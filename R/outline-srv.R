# Outline extension server. The JS side announces itself via the `ready`
# input (the panel UI is moved into its dock panel after page load, so the
# client, not the server, knows when it can render); from then on every
# board change pushes the full model. User gestures come back as
# event-priority inputs and are translated into blockr.core `update()`
# deltas or blockr.dock action triggers.
outline_ext_srv <- function(id, board, update, actions, ...) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      send <- function(type, payload) {
        payload$el <- session$ns("outline")
        session$sendCustomMessage(paste0("outline-", type), payload)
      }

      ready <- shiny::reactive(isTRUE(input$ready))

      shiny::observeEvent(
        list(board$board, input$ready),
        {
          shiny::req(isTRUE(input$ready))
          send("data", outline_payload(board$board))
        }
      )

      # The catalogue does not depend on the board, so it travels once, when
      # the client announces itself -- not on every board change with the
      # model. Until it lands the picker declines to open and the gestures
      # fall back to the board's own block browser.
      shiny::observeEvent(input$ready, {
        shiny::req(isTRUE(input$ready))
        send("registry", outline_registry())
      })

      # Connect: the client proposes an input slot (it knows the free
      # slots), the server re-derives the free set from the committed
      # board and only honours the proposal when it is (still) free --
      # the authoritative gate, same as blockr.dag's draw_link_action.
      shiny::observeEvent(input$link_add, {
        msg <- input$link_add
        blocks <- blockr.core::board_blocks(board$board)

        if (!all(c(msg$from, msg$to) %in% names(blocks))) {
          return()
        }

        inps <- blockr.dock::block_input_select(
          blocks[[msg$to]],
          msg$to,
          blockr.core::board_links(board$board),
          mode = "inputs"
        )

        if (length(inps) == 0L) {
          shiny::showNotification(
            sprintf("No free inputs on block %s.", msg$to),
            type = "warning"
          )
          return()
        }

        slot <- if (length(msg$input) == 1L && msg$input %in% inps) {
          msg$input
        } else {
          inps[1L]
        }

        update(list(links = list(add = blockr.core::as_links(
          blockr.core::new_link(from = msg$from, to = msg$to, input = slot)
        ))))
      })

      shiny::observeEvent(input$link_rm, {
        ids <- intersect(
          unlist(input$link_rm$ids),
          names(blockr.core::board_links(board$board))
        )
        if (length(ids)) {
          update(list(links = list(rm = ids)))
        }
      })

      # Remove a block; when it sits in a straight chain (exactly one
      # incoming and one outgoing link) the chain is healed by wiring the
      # parent into the freed slot of the child. `augment_board_update()`
      # cascades the incident link removals and stack pruning.
      shiny::observeEvent(input$block_rm, {
        id_rm <- input$block_rm$id

        if (!id_rm %in% names(blockr.core::board_blocks(board$board))) {
          return()
        }

        links <- blockr.core::board_links(board$board)
        ins <- links[links$to == id_rm]
        outs <- links[links$from == id_rm]

        upd <- list(blocks = list(rm = id_rm))

        dup <- length(ins) == 1L && length(outs) == 1L && any(
          links$from == ins$from &
            links$to == outs$to &
            links$input == outs$input
        )

        if (length(ins) == 1L && length(outs) == 1L && !dup) {
          upd$links <- list(add = blockr.core::as_links(
            blockr.core::new_link(
              from = ins$from,
              to = outs$to,
              input = outs$input
            )
          ))
        }

        update(upd)
      })

      shiny::observeEvent(input$block_rename, {
        msg <- input$block_rename
        nm <- trimws(as.character(msg$name))
        if (!nzchar(nm)) {
          return()
        }
        update(list(blocks = list(
          mod = stats::setNames(list(list(block_name = nm)), msg$id)
        )))
      })

      shiny::observeEvent(input$block_select, {
        delta <- outline_reveal_delta(board$board, input$block_select$id)
        if (!is.null(delta)) {
          update(delta)
        }
      })

      # An extension row answers the click the same way a block row does: the
      # panel appears on the view you are on, mounted there first if the view
      # did not hold it. The row menu stays the surface for putting it on the
      # views you are NOT looking at.
      shiny::observeEvent(input$ext_select, {
        delta <- outline_reveal_delta(
          board$board,
          extension = as.character(input$ext_select$id)
        )
        if (!is.null(delta)) {
          update(delta)
        }
      })

      # Drag released on empty canvas: open the block browser, wired from
      # the drag source (the outline's drop-on-canvas append, same flow the
      # DAG extension triggers for an edge dropped on the canvas).
      # The outline's picker handles both gestures itself, so `block_append` and
      # `block_add` only reach here when the catalogue never arrived (the
      # client falls back rather than swallowing the gesture). The board's
      # own browser is then the safety net.
      shiny::observeEvent(input$block_append, {
        actions[["append_block_action"]](input$block_append$from)
      })

      shiny::observeEvent(input$block_add, {
        actions[["add_block_action"]](input$block_add)
      })

      # Insert a block chosen in the outline. Adding and appending are one
      # operation: the origin decides only whether a link is made, and which
      # of its free input slots receives it.
      shiny::observeEvent(input$block_insert, {

        msg <- input$block_insert
        type <- as.character(msg$type)

        if (!length(type) || !nzchar(type) ||
              !type %in% names(blockr.core::available_blocks())) {
          return()
        }

        blk <- tryCatch(
          blockr.core::create_block(type),
          error = function(e) {
            shiny::showNotification(
              sprintf("Could not create a %s: %s", type, conditionMessage(e)),
              type = "error"
            )
            NULL
          }
        )

        if (is.null(blk)) {
          return()
        }

        blocks <- blockr.core::board_blocks(board$board)
        blk_id <- blockr.core::rand_names(names(blocks))
        upd <- list(
          blocks = list(
            add = blockr.core::as_blocks(stats::setNames(list(blk), blk_id))
          )
        )

        from <- msg$from

        if (length(from) == 1L && !is.na(from) && from %in% names(blocks)) {

          # The link lands on a free slot of the NEW block, so the choice is
          # made against the block just built, not against anything on the
          # board yet. A variadic block reports no named slots but still
          # accepts a link, on a fresh one.
          inps <- blockr.core::block_inputs(blk)
          slot <- if (length(inps)) {
            inps[[1L]]
          } else if (is.na(blockr.core::block_arity(blk))) {
            "1"
          } else {
            NULL
          }

          if (!is.null(slot)) {
            upd$links <- list(
              add = blockr.core::as_links(
                blockr.core::new_link(from = from, to = blk_id, input = slot)
              )
            )
          }
        }

        upd$views <- outline_place_delta(board$board, blk_id, from)

        # An insert made inside a focused stack view joins that stack in the
        # SAME update: landing loose first and joining a roundtrip later left
        # the block outside the focused view for a beat and cost a second
        # full model push on a large board.
        stk <- as.character(msg$stack)
        stks <- blockr.core::board_stacks(board$board)

        if (length(stk) == 1L && nzchar(stk) && stk %in% names(stks)) {
          upd$stacks <- list(
            mod = stats::setNames(
              list(
                list(
                  blocks = union(
                    blockr.core::stack_blocks(stks[[stk]]),
                    blk_id
                  )
                )
              ),
              stk
            )
          )
        }

        update(upd)
      })

      # Grouping a selection. Blocks already in a stack are not refused: they
      # move, which is how a stack gets split.
      shiny::observeEvent(input$stack_add, {
        delta <- outline_stack_new(board$board, unlist(input$stack_add$blocks))
        if (!is.null(delta)) {
          update(delta)
        }
      })

      # Dragged into a frame (or the "⚠ n between" fix on a stack header):
      # the blocks join that stack and leave whatever they were in.
      shiny::observeEvent(input$stack_join, {
        delta <- outline_stack_delta(
          board$board,
          unlist(input$stack_join$blocks),
          as.character(input$stack_join$stack)
        )
        if (!is.null(delta)) {
          update(delta)
        }
      })

      # Dragged out of every frame.
      shiny::observeEvent(input$stack_leave, {
        delta <- outline_stack_delta(
          board$board, unlist(input$stack_leave$blocks)
        )
        if (!is.null(delta)) {
          update(delta)
        }
      })

      shiny::observeEvent(input$stack_rename, {
        msg <- input$stack_rename
        nm <- trimws(as.character(msg$name))
        if (!nzchar(nm)) {
          return()
        }
        update(list(stacks = list(
          mod = stats::setNames(list(list(name = nm)), msg$id)
        )))
      })

      shiny::observeEvent(input$stack_rm, {
        id_rm <- input$stack_rm$id
        if (id_rm %in% names(blockr.core::board_stacks(board$board))) {
          update(list(stacks = list(rm = id_rm)))
        }
      })

      # --- clipboard ---------------------------------------------------
      #
      # The payload is prepared when the SELECTION changes, not when the copy
      # key is pressed. A `copy` event has to hand its data over
      # synchronously, and only the server can build it -- so it is built
      # ahead of time and cached client-side. `navigator.clipboard` would
      # avoid that, but it needs a permission Safari largely refuses and
      # Chrome prompts for, and its read is async, which loses the gesture.
      shiny::observeEvent(input$block_selection, {

        ids <- intersect(
          unlist(input$block_selection$ids),
          names(blockr.core::board_blocks(board$board))
        )

        if (!length(ids)) {
          return()
        }

        json <- outline_clip_json(
          board$board, ids, outline_live_states(board, ids)
        )

        if (!is.null(json)) {
          send("clipboard", list(json = json))
        }
      })

      # Cut is copy plus remove, and the copy half already happened on the
      # client from the cached payload. `augment_board_update()` cascades the
      # incident links and prunes any stack left short.
      shiny::observeEvent(input$block_cut, {

        ids <- intersect(
          unlist(input$block_cut$ids),
          names(blockr.core::board_blocks(board$board))
        )

        if (!length(ids)) {
          return()
        }

        # A stack whose every member is going goes too. The core cascade
        # prunes the members out of it but keeps the stack, which after
        # cutting a whole stack leaves an empty husk on the board -- and the
        # gesture plainly meant "remove this stack".
        stacks <- blockr.core::board_stacks(board$board)
        gone <- names(stacks)[vapply(
          stacks,
          function(s) all(blockr.core::stack_blocks(s) %in% ids),
          logical(1)
        )]

        upd <- list(blocks = list(rm = ids))

        if (length(gone)) {
          upd$stacks <- list(rm = gone)
        }

        update(upd)
      })

      shiny::observeEvent(input$block_paste, {
        delta <- outline_paste_delta(board$board, input$block_paste$json)
        if (!is.null(delta)) {
          update(delta)
        }
      })

      # --- membership ---------------------------------------------------
      #
      # One message for every placement write the row menu can make: the three
      # presets (all views / no view / this view only) and a single checkbox
      # being ticked or cleared. The client proposes; the server re-derives what
      # is actually a member from the committed board and emits only the
      # difference, so a stale client (a box clicked twice before the round-trip
      # lands) cannot ask to add a member or remove a non-member -- both of which
      # `validate_view_mod()` rejects outright.
      #
      # There are no view CRUD handlers here on purpose. Creating, renaming,
      # reordering and removing views belongs to the dock's own navbar, which has
      # had all four since before the outline existed; the outline says which
      # panels go where, and nothing about the pages themselves.
      shiny::observeEvent(input$membership, {
        msg <- input$membership
        delta <- outline_membership_delta(
          board$board,
          blocks = unlist(msg$blocks),
          extensions = unlist(msg$extensions),
          mode = if (isTRUE(msg$mode %in%
                              c("all", "none", "only", "add", "rm"))) {
            msg$mode
          } else {
            "add"
          },
          view = msg$view
        )
        if (!is.null(delta)) {
          update(delta)
        }
      })

      list(
        state = list(),
        ready = ready,
        send = send
      )
    }
  )
}
