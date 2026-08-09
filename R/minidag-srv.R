# Minidag extension server. The JS side announces itself via the `ready`
# input (the panel UI is moved into its dock panel after page load, so the
# client, not the server, knows when it can render); from then on every
# board change pushes the full model. User gestures come back as
# event-priority inputs and are translated into blockr.core `update()`
# deltas or blockr.dock action triggers.
minidag_ext_srv <- function(id, board, update, actions, ...) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      send <- function(type, payload) {
        payload$el <- session$ns("deck")
        session$sendCustomMessage(paste0("minidag-", type), payload)
      }

      ready <- shiny::reactive(isTRUE(input$ready))

      shiny::observeEvent(
        list(board$board, input$ready),
        {
          shiny::req(isTRUE(input$ready))
          send("data", minidag_payload(board$board))
        }
      )

      # The catalogue does not depend on the board, so it travels once, when
      # the client announces itself -- not on every board change with the
      # model. Until it lands the picker declines to open and the gestures
      # fall back to the board's own block browser.
      shiny::observeEvent(input$ready, {
        shiny::req(isTRUE(input$ready))
        send("registry", minidag_registry())
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
        delta <- minidag_reveal_delta(board$board, input$block_select$id)
        if (!is.null(delta)) {
          update(delta)
        }
      })

      # Drag released on empty canvas: open the block browser, wired from
      # the drag source (the deck's drop-on-canvas append, same flow the
      # DAG extension triggers for an edge dropped on the canvas).
      # The deck's picker handles both gestures itself, so `block_append` and
      # `block_add` only reach here when the catalogue never arrived (the
      # client falls back rather than swallowing the gesture). The board's
      # own browser is then the safety net.
      shiny::observeEvent(input$block_append, {
        actions[["append_block_action"]](input$block_append$from)
      })

      shiny::observeEvent(input$block_add, {
        actions[["add_block_action"]](input$block_add)
      })

      # Insert a block chosen in the deck. Adding and appending are one
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

        update(upd)
      })

      shiny::observeEvent(input$stack_add, {
        members <- unlist(input$stack_add$blocks)
        members <- intersect(
          members,
          names(blockr.core::board_blocks(board$board))
        )
        if (length(members) < 2L) {
          return()
        }
        update(list(stacks = list(add = blockr.core::stacks(
          blockr.dock::new_dock_stack(blocks = members, name = "New stack")
        ))))
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

      # --- views -------------------------------------------------------
      #
      # Membership is the only view property the deck writes per row. The
      # client proposes a set of blocks and a direction; the server re-derives
      # what is actually a member from the committed board and emits only the
      # difference, so a stale client (a tri-state stack toggled twice before
      # the round-trip lands) cannot ask to add a member or remove a
      # non-member -- both of which `validate_view_mod()` rejects outright.
      shiny::observeEvent(input$view_toggle, {
        msg <- input$view_toggle
        views <- blockr.dock::board_views(board$board)

        if (!isTRUE(msg$view %in% names(views))) {
          return()
        }

        ids <- intersect(
          unlist(msg$blocks),
          names(blockr.core::board_blocks(board$board))
        )

        if (!length(ids)) {
          return()
        }

        pids <- as.character(blockr.dock::as_block_panel_id(ids))
        members <- blockr.dock::view_members(views[[msg$view]])

        ops <- if (isTRUE(msg$add)) {
          add <- setdiff(pids, members)
          if (length(add)) {
            # No placement hint: an un-landed member renders through the
            # default grid and the client echo mirrors back wherever the
            # user drops it. Pinning a side here would fight that.
            list(add = stats::setNames(rep(list(list()), length(add)), add))
          }
        } else {
          rm <- intersect(pids, members)
          if (length(rm)) list(rm = rm)
        }

        if (is.null(ops)) {
          return()
        }

        update(
          list(views = list(mod = stats::setNames(list(ops), msg$view)))
        )
      })

      shiny::observeEvent(input$view_rename, {
        msg <- input$view_rename
        nm <- trimws(as.character(msg$name))
        if (!nzchar(nm) ||
              !isTRUE(msg$id %in% names(blockr.dock::board_views(board$board)))) {
          return()
        }
        update(list(views = list(rename = stats::setNames(list(nm), msg$id))))
      })

      shiny::observeEvent(input$view_add, {
        nm <- trimws(as.character(input$view_add$name))
        if (!nzchar(nm)) {
          nm <- "New view"
        }

        # The new view carries THIS deck. A view created from the deck that
        # does not contain the deck is a trap: it is empty by definition, so
        # the moment you switch to it the one tool that could fill it is gone.
        me <- blockr.dock::extension_ids(
          shiny::isolate(board$board), "minidag_extension"
        )

        members <- if (length(me)) {
          as.character(blockr.dock::as_ext_panel_id(me[[1L]]))
        } else {
          character()
        }

        # Deliberately NOT active. `views$add` accepts an `active` naming the
        # add key, but switching there on create drops you into a page whose
        # only panel is this one -- every block you were looking at is gone,
        # and the view you were curating is no longer on screen. Creating a
        # view and going to it are two decisions; the list marks the active
        # one, and the board's own nav is where you travel.
        update(
          list(
            views = list(
              add = stats::setNames(
                list(blockr.dock::dock_view(members)), nm
              )
            )
          )
        )
      })

      shiny::observeEvent(input$view_rm, {
        id_rm <- input$view_rm$id
        views <- blockr.dock::board_views(board$board)
        # The last view cannot go (`dock_views_delta_remove_all`); refusing
        # here keeps that an inert click rather than an error notification.
        if (id_rm %in% names(views) && length(views) > 1L) {
          update(list(views = list(rm = id_rm)))
        }
      })

      shiny::observeEvent(input$view_order, {
        order <- unlist(input$view_order$ids)
        current <- names(blockr.dock::board_views(board$board))
        # `views$order` must be a TOTAL permutation of the post-state ids.
        if (setequal(order, current) && !anyDuplicated(order)) {
          update(list(views = list(order = order)))
        }
      })

      shiny::observeEvent(input$view_activate, {
        id <- input$view_activate$id
        views <- blockr.dock::board_views(board$board)
        if (id %in% names(views) &&
              !identical(id, blockr.dock::active_view(views))) {
          update(list(views = list(active = id)))
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
