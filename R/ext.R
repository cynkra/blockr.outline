#' Outline extension
#'
#' A dock extension rendering the board as a linked outline: block chips
#' (name, include-in-report switch, reorder arrows) aligned with the
#' generated code sections, per-block markdown descriptions, stack
#' chapters, and rendering of the inverted document. Descriptions, the
#' report flags and the user-chosen block order are extension state and
#' serialize with the board; blocks themselves are untouched.
#'
#' @param annotations Named list keyed by block id, each entry a list with
#'   `description` (markdown string) and/or `report` (logical).
#' @param block_order Character vector of block ids: the preferred document
#'   order, applied as the tie-break of the topological sort.
#' @param title Document title.
#' @param ... Forwarded to [blockr.dock::new_dock_extension()]
#'
#' @export
new_outline_extension <- function(annotations = list(),
                                  block_order = character(),
                                  title = "Board report",
                                  stack_annotations = list(),
                                  ...) {

  blockr.dock::new_dock_extension(
    outline_ext_srv(annotations, block_order, title, stack_annotations),
    outline_ext_ui,
    name = "Outline",
    description = paste(
      "Report outline of the board: per-block markdown descriptions,",
      "include-in-report flags, document order, and html/pptx/pdf render",
      "of the generated report."
    ),
    class = "outline_extension",
    ...
  )
}

outline_ext_ui <- function(id, board, ...) {

  ns <- NS(id)

  formats <- c("html", "pptx")

  if (report_pdf_available()) {
    formats <- c(formats, "pdf")
  }

  div(
    class = "blockr-otl-panel",
    outline_dep(),
    md_editor_dep(),
    div(
      class = "blockr-otl-toolbar",
      shinyWidgets::radioGroupButtons(
        inputId = ns("code_view"),
        label = NULL,
        size = "sm",
        status = "light",
        choices = c(
          "Outline" = "outline",
          "R script" = "script",
          "Document" = "qmd"
        ),
        selected = "outline"
      ),
      div(
        class = "blockr-otl-toolbar-right",
        selectInput(
          ns("code_render_format"),
          label = NULL,
          choices = formats,
          selected = "html",
          width = "110px"
        ),
        downloadButton(
          ns("code_render"),
          "Render",
          class = "btn-sm btn-outline-success"
        ),
        tags$button(
          type = "button",
          class = "btn btn-sm btn-light",
          title = "Copy code to clipboard",
          onclick = sprintf(
            paste0(
              "navigator.clipboard.writeText(",
              "document.getElementById('%s').innerText);"
            ),
            ns("code_pre")
          ),
          icon("clipboard")
        )
      )
    ),
    uiOutput(ns("outline_out")),
    outline_js(ns)
  )
}

outline_ext_srv <- function(annotations, block_order, title,
                            stack_annotations = list()) {

  function(id, board, update, session, parent, actions = NULL, ...) {
    moduleServer(
      id,
      function(input, output, session) {

        rv_ann <- reactiveVal(sanitize_annotations(annotations))
        rv_stack_ann <- reactiveVal(sanitize_annotations(stack_annotations))
        rv_order <- reactiveVal(as.character(unlist(block_order)))
        rv_title <- reactiveVal(
          if (is.character(title) && length(title)) title[[1L]] else
            "Board report"
        )
        editing <- reactiveVal(NULL)
        # "Insert after X" intent: the id the add link was clicked on, and
        # the block ids last seen on the board. A new block must land where
        # the user pointed -- the topological order alone cannot know that
        # (stack contiguity would keep the source's run together first).
        pending_after <- reactiveVal(NULL)
        known_ids <- reactiveVal(NULL)

        # Garbage-collect annotations / order entries for removed blocks;
        # the id-keyed map must follow the board's block lifecycle.
        observeEvent(
          board$board,
          {
            ids <- blockr.core::board_block_ids(board$board)

            ann <- rv_ann()
            keep <- intersect(names(ann), ids)
            if (!identical(names(ann), keep)) {
              rv_ann(ann[keep])
            }

            ord <- intersect(rv_order(), ids)
            if (!identical(rv_order(), ord)) {
              rv_order(ord)
            }

            stk_ids <- names(blockr.core::board_stacks(board$board))

            sann <- rv_stack_ann()
            skeep <- intersect(names(sann), stk_ids)
            if (!identical(names(sann), skeep)) {
              rv_stack_ann(sann[skeep])
            }

            # Place a block that has just been added right after the block
            # its add link was clicked on. Always a valid order: the new
            # block is a successor of that source.
            prev <- known_ids()
            known_ids(ids)

            src <- pending_after()
            added <- if (is.null(prev)) character() else setdiff(ids, prev)

            if (length(added) && !is.null(src) && src %in% ids) {

              cur <- rv_order()

              if (!length(cur)) {
                shown <- sections_store()
                cur <- if (is.null(shown)) prev else shown$ids
              }

              cur <- setdiff(cur, added)
              at <- match(src, cur)

              if (!is.na(at)) {
                rv_order(append(cur, added, after = at))
              }

              # Inherit the source's stack: a block added from inside a
              # chapter belongs to that chapter, so it does not split the
              # stack's run the moment it appears.
              stks <- blockr.core::board_stacks(board$board)
              own <- Filter(
                function(s) src %in% blockr.core::stack_blocks(stks[[s]]),
                names(stks)
              )

              if (length(own)) {
                stk_id <- own[[1L]]
                members <- union(
                  blockr.core::stack_blocks(stks[[stk_id]]),
                  added
                )
                update(
                  list(
                    stacks = list(
                      mod = setNames(list(list(blocks = members)), stk_id)
                    )
                  )
                )
              }

              pending_after(NULL)
            }
          }
        )

        # Removing a block leaves its dependents without input for a
        # flush, and reading such a block's expression throws. Evaluate
        # each expression defensively: a block that cannot report one is
        # dropped, so the outline redraws (showing the removal) instead of
        # freezing on the last good projection.
        # Last known expression per block id. A block's expr reactive
        # reports NULL whenever it cannot produce one right now -- a
        # ggplot block req()s while its upstream data settles, and a block
        # whose dock panel is not the visible tab stops reporting
        # altogether. A report surface covers the whole board, so a block
        # going quiet must not remove it from the document; the last
        # expression it did report stays until it reports a new one.
        # Without this the outline loses blocks as soon as you switch
        # tabs. (The real fix is upstream: an extension cannot ask core to
        # construct or evaluate a hidden block. Recorded as a core
        # follow-up; the demo sets background_construction_delay = 0.)
        expr_cache <- new.env(parent = emptyenv())

        board_exprs <- reactive(
          {
            ex <- lapply(
              blockr.core::lst_xtr(board$blocks, "server", "expr"),
              function(e) tryCatch(blockr.core::reval(e), error = function(err) NULL)
            )

            for (id in names(ex)) {
              if (!is.null(ex[[id]])) {
                assign(id, ex[[id]], envir = expr_cache)
              }
            }

            # Keyed on the live board, so a removed block is gone for good
            # rather than resurrected from the cache.
            live <- blockr.core::board_block_ids(board$board)

            out <- lapply(
              setNames(nm = live),
              function(id) {
                if (!is.null(ex[[id]])) {
                  ex[[id]]
                } else if (exists(id, envir = expr_cache, inherits = FALSE)) {
                  get(id, envir = expr_cache)
                }
              }
            )

            rm(
              list = setdiff(ls(expr_cache), live),
              envir = expr_cache
            )

            # A block that has not reported yet still takes its place in
            # the document, holding a placeholder expression. Otherwise
            # the block set grows as blocks report in (the ggplot block
            # trails the others by about a second at startup), each
            # arrival is a structural change, and the outline visibly
            # redraws after its first paint. With placeholders the
            # skeleton is complete from the first projection and the real
            # code arrives as a push.
            pending <- names(out)[vapply(out, is.null, logical(1L))]

            for (id in pending) {
              # NOT quote(NULL): NULL is self-evaluating, so quote(NULL)
              # IS NULL and assigning it deletes the element instead of
              # filling it. Any non-NULL call works -- it is never
              # deparsed, since a pending cell renders as a placeholder.
              out[[id]] <- quote(invisible(NULL))
            }

            structure(out, pending = pending)
          }
        )

        sections_calc <- reactive(
          {
            req(length(board_exprs()) > 0L)

            # During startup (deferred construction) the block servers and
            # the committed board can disagree for a flush; retry on the
            # next one instead of surfacing a transient error.
            tryCatch(
              outline_sections(
                board_exprs(),
                board$board,
                rv_ann(),
                rv_order(),
                rv_stack_ann()
              ),
              error = function(e) {
                message(
                  "blockr.outline: sections unavailable this flush (",
                  conditionMessage(e),
                  ") at: ",
                  paste(
                    utils::head(deparse(conditionCall(e)), 2L),
                    collapse = " "
                  )
                )
                req(FALSE)
              }
            )
          }
        )

        # Identical-skip store: board updates that do not change the
        # projected sections (e.g. the views-delta a click-to-open commits)
        # must not redraw the outline. The renderUI below depends on this
        # store, never on board reactives directly.
        sections_store <- reactiveVal(NULL)

        observe(
          {
            new <- sections_calc()
            if (!identical(new, isolate(sections_store()))) {
              sections_store(new)

              # Remember the order actually shown. Without this, a block
              # that loses its last dependency becomes unconstrained and
              # drifts (removing a block could reshuffle the document);
              # with it, the displayed order is sticky and only explicit
              # drags or inserts change it.
              if (!length(isolate(rv_order()))) {
                rv_order(new$ids)
              }
            }
          }
        )

        sections <- reactive(
          {
            req(!is.null(sections_store()))
            sections_store()
          }
        )

        spin_txt <- reactive(export_spin(sections()))
        qmd_txt <- reactive(export_qmd(sections(), rv_title()))

        # Split the projection into the structural skeleton and the
        # per-block code markup, each with its own identical-skip store.
        # Editing a block value (a row count, say) changes only the code,
        # so the skeleton store stays put, renderUI does not fire, and the
        # observer below pushes just the changed chunks. Structural edits
        # (drag, chapter changes, add/remove) move the skeleton and redraw
        # wholesale, which is correct and rare.
        skel_store <- reactiveVal(NULL)
        code_store <- reactiveVal(NULL)

        observe(
          {
            full <- sections()

            skel <- full
            skel$code <- NULL
            # Pending-ness only changes the code cell, so it rides with
            # the pushed code map. In the skeleton it would redraw the
            # outline the moment a block reports.
            skel$pending <- NULL

            if (!identical(skel, isolate(skel_store()))) {
              skel_store(skel)
            }

            codes <- outline_code_map(full)

            if (!identical(codes, isolate(code_store()))) {
              code_store(codes)
            }
          }
        )

        # Diff against what the client already has. This compares every
        # block, not just the edited one: assignment names flow downstream
        # (`mut <- dplyr::mutate(filt, ...)`), so an upstream edit
        # legitimately rewrites its dependents' code too.
        pushed <- reactiveVal(NULL)

        observe(
          {
            codes <- code_store()
            req(!is.null(codes))

            prev <- isolate(pushed())
            pushed(codes)

            # First paint: renderUI carries the markup itself.
            req(!is.null(prev))

            changed <- Filter(
              function(n) !identical(codes[[n]], prev[[n]]),
              names(codes)
            )

            if (!length(changed)) {
              return()
            }

            session$sendCustomMessage(
              "blockr-outline-code",
              list(
                items = lapply(
                  changed,
                  function(n) {
                    list(
                      id = session$ns(paste0("code-", n)),
                      html = codes[[n]]
                    )
                  }
                ),
                # The hidden full script backs the copy button, so it has
                # to track the pushed chunks or copying yields stale code.
                script_id = session$ns("code_pre"),
                script = isolate(spin_txt())
              )
            )
          }
        )

        output$outline_out <- renderUI(
          {
            view <- coal(input$code_view, "outline")

            if (identical(view, "outline")) {

              # Skeleton reactively, code isolated: a code-only change must
              # not re-render here (the push handles it), but when the
              # skeleton does redraw it has to paint the current code.
              sects <- skel_store()
              req(!is.null(sects))
              sects$code_html <- isolate(code_store())

              return(
                tagList(
                  outline_tags(sects, session$ns, editing()),
                  # Hidden full script so the copy button works here too.
                  # Isolated: reading it reactively would re-couple this
                  # renderUI to every code change, which is exactly what
                  # the skeleton/code split exists to avoid. The push
                  # keeps this node's text current instead.
                  tags$pre(
                    id = session$ns("code_pre"),
                    style = "display: none;",
                    isolate(spin_txt())
                  )
                )
              )
            }

            txt <- if (identical(view, "qmd")) qmd_txt() else spin_txt()

            hl <- if (identical(view, "script")) {
              highlight_r_code(txt)
            }

            if (is.null(hl)) {
              tags$pre(
                class = "blockr-otl-raw",
                tags$code(id = session$ns("code_pre"), txt)
              )
            } else {
              div(
                class = "blockr-otl-raw",
                id = session$ns("code_pre"),
                HTML(hl)
              )
            }
          }
        )

        outputOptions(output, "outline_out", suspendWhenHidden = FALSE)

        observeEvent(
          input$outline_toggle,
          {
            tog <- input$outline_toggle
            req(is.character(tog$id), is.logical(tog$report))

            ann <- rv_ann()

            if (identical(ann_report(ann, tog$id), tog$report)) {
              return()
            }

            entry <- coal(ann[[tog$id]], list())
            entry$report <- tog$report
            ann[[tog$id]] <- entry
            rv_ann(ann)
          }
        )

        observeEvent(
          input$outline_edit,
          editing(input$outline_edit$id)
        )

        observeEvent(input$desc_cancel, editing(NULL))

        observeEvent(
          input$desc_save,
          {
            key <- editing()
            req(is.character(key))

            if (startsWith(key, "stack:")) {
              stk_id <- sub("^stack:", "", key)
              sann <- rv_stack_ann()
              entry <- coal(sann[[stk_id]], list())
              entry$description <- coal(input$desc_edit, "")
              sann[[stk_id]] <- entry
              rv_stack_ann(sann)
            } else {
              ann <- rv_ann()
              entry <- coal(ann[[key]], list())
              entry$description <- coal(input$desc_edit, "")
              ann[[key]] <- entry
              rv_ann(ann)
            }

            editing(NULL)
          }
        )

        # Rename a stack: name is a stack ctor argument, so the standard
        # stacks-mod update rebuilds it with the new name.
        observeEvent(
          input$outline_rename_stack,
          {
            ren <- input$outline_rename_stack
            req(is.character(ren$stack), is.character(ren$name))
            req(nzchar(trimws(ren$name)))

            stks <- blockr.core::board_stacks(board$board)
            req(ren$stack %in% names(stks))

            if (identical(
              blockr.core::stack_name(stks[[ren$stack]]),
              ren$name
            )) {
              return()
            }

            update(
              list(
                stacks = list(
                  mod = setNames(
                    list(list(name = ren$name)),
                    ren$stack
                  )
                )
              )
            )
          }
        )

        # Reorder by drag: remove the dragged id from the displayed order
        # and reinsert before / after the drop target, storing the whole
        # permutation; the Kahn tie-break snaps invalid wishes back to the
        # nearest valid order.
        observeEvent(
          input$outline_move,
          {
            mv <- input$outline_move
            req(is.character(mv$id), is.character(mv$target))
            req(!identical(mv$id, mv$target))

            disp <- setdiff(sections()$ids, mv$id)
            at <- match(mv$target, disp)
            req(!is.na(at))

            if (isTRUE(mv$after)) {
              at <- at + 1L
            }

            ord <- append(disp, mv$id, after = at - 1L)
            rv_order(ord)

            # Rule 1: position implies membership. The block joins the
            # chapter surrounding where it landed -- the one above it, or
            # the one below when it landed first. Without this a drop
            # inside another chapter would leave a "(continued)" split
            # nobody asked for.
            pos <- match(mv$id, ord)
            neighbour <- if (pos > 1L) ord[pos - 1L] else ord[pos + 1L]
            req(!is.na(neighbour))

            stks <- blockr.core::board_stacks(board$board)
            target <- Filter(
              function(s) neighbour %in% blockr.core::stack_blocks(stks[[s]]),
              names(stks)
            )
            target <- if (length(target)) target[[1L]] else NULL

            current <- Filter(
              function(s) mv$id %in% blockr.core::stack_blocks(stks[[s]]),
              names(stks)
            )
            current <- if (length(current)) current[[1L]] else NULL

            if (identical(target, current)) {
              return()
            }

            members <- list()

            if (!is.null(current)) {
              members[[current]] <- setdiff(stack_members(current), mv$id)
            }

            if (!is.null(target)) {
              members[[target]] <- c(stack_members(target), mv$id)
            }

            commit_stacks(members)
          }
        )

        # Chapter drag: the whole run moves as one unit, membership
        # untouched, landing before another chapter or at the end. The
        # legality was computed with the sections (chapter-level DAG
        # slack), so any arriving payload is already valid.
        observeEvent(
          input$outline_movechap,
          {
            mv <- input$outline_movechap
            req(is.character(mv$stack), is.character(mv$before))

            s <- sections()
            unit <- s$ids[!is.na(s$stack_ids) & s$stack_ids == mv$stack]
            req(length(unit))

            rest <- setdiff(s$ids, unit)

            at <- if (identical(mv$before, "__end__")) {
              length(rest)
            } else {
              match(mv$before, rest) - 1L
            }

            req(!is.na(at))

            rv_order(append(rest, unit, after = at))
          }
        )

        # Rename on double-click: block_name is externally controllable on
        # every block, so the standard blocks-mod update applies it.
        observeEvent(
          input$outline_rename,
          {
            ren <- input$outline_rename
            req(is.character(ren$id), is.character(ren$name))
            req(nzchar(trimws(ren$name)))

            blk <- blockr.core::board_blocks(board$board)[[ren$id]]
            req(!is.null(blk))

            if (identical(blockr.core::block_name(blk), ren$name)) {
              return()
            }

            update(
              list(
                blocks = list(
                  mod = setNames(
                    list(list(block_name = ren$name)),
                    ren$id
                  )
                )
              )
            )
          }
        )

        # Append after a block: hand the block id to the dock's own append
        # action, which opens the block browser scoped to that source and
        # commits the new block plus its link. Graph surgery beyond a
        # straight append stays in the Workflow (dag) view.
        observeEvent(
          input$outline_add,
          {
            blk_id <- input$outline_add$id
            req(is.character(blk_id))

            append <- if (is.list(actions)) actions[["append_block_action"]]

            pending_after(blk_id)

            if (is.null(append)) {
              showNotification(
                "Adding blocks needs the dock's append action.",
                type = "warning"
              )
              return()
            }

            append(blk_id)
          }
        )

        # ---- section (stack) management -------------------------------
        # Sections are defined by where they START: "New chapter" takes the
        # clicked block plus the rest of its current run into a fresh
        # stack. Chapter actions merge it into the one above, dissolve it,
        # or flip every member's report flag. Dropping a chip on a chapter
        # heading moves that block into it.
        # Blocks that move into a chapter started at `blk_id`. Normally the
        # cut goes ABOVE the block, so it takes the rest of its run. When
        # the block ALREADY starts a chapter there is no boundary to add
        # above it, so it becomes a chapter on its own and the rest of the
        # run stays behind -- otherwise the whole run would move, the old
        # chapter would empty out, and the action would read as a rename.
        run_of <- function(blk_id) {
          s <- sections()
          i <- match(blk_id, s$ids)
          if (is.na(i)) {
            return(character())
          }
          stk <- s$stack_ids[i]
          starts_run <- i == 1L || !identical(s$stack_ids[i - 1L], stk)
          if (starts_run && !is.na(stk)) {
            return(s$ids[i])
          }
          j <- i
          while (j < length(s$ids) && identical(s$stack_ids[j + 1L], stk)) {
            j <- j + 1L
          }
          s$ids[i:j]
        }

        stack_members <- function(stk_id) {
          stks <- blockr.core::board_stacks(board$board)
          if (stk_id %in% names(stks)) {
            blockr.core::stack_blocks(stks[[stk_id]])
          } else {
            character()
          }
        }

        # Apply a whole new membership map at once: every touched stack is
        # modified, emptied stacks are dropped, so the partition invariant
        # (a block sits in at most one stack) always holds on commit.
        commit_stacks <- function(members) {

          stks <- blockr.core::board_stacks(board$board)
          mod <- list()
          rm <- character()

          for (s in names(members)) {
            if (!s %in% names(stks)) {
              next
            }
            if (length(members[[s]])) {
              mod[[s]] <- list(blocks = members[[s]])
            } else {
              rm <- c(rm, s)
            }
          }

          payload <- list()
          if (length(mod)) payload$mod <- mod
          if (length(rm)) payload$rm <- rm

          if (length(payload)) {
            update(list(stacks = payload))
          }
        }

        observeEvent(
          input$outline_newchapter,
          {
            blk_id <- input$outline_newchapter$id
            req(is.character(blk_id))

            members <- run_of(blk_id)
            req(length(members))

            stks <- blockr.core::board_stacks(board$board)
            old <- Filter(
              function(s) blk_id %in% blockr.core::stack_blocks(stks[[s]]),
              names(stks)
            )

            keep <- list()
            if (length(old)) {
              keep[[old[[1L]]]] <- setdiff(stack_members(old[[1L]]), members)
            }

            new_id <- paste0(
              "chap_",
              substr(gsub("[^a-z0-9]", "", tolower(blk_id)), 1L, 8L),
              length(stks) + 1L
            )

            colors <- tryCatch(
              blockr.dock::suggest_new_colors(),
              error = function(e) "#6b7280"
            )

            # Two updates, one flush apart. Core validates `add` against
            # the CURRENT board, so a combined payload still sees the
            # blocks in their old stack ("Blocks cannot be in multiple
            # stacks at the same time") -- the detach has to be committed
            # before the new stack is offered. Same onFlushed sequencing
            # the dock's own add-stack action uses.
            payload <- list(
              # stacks() takes the stacks as `...`, one named argument per
              # stack -- passing a single list makes one malformed entry,
              # which the update path then drops without a word.
              add = do.call(
                blockr.core::stacks,
                setNames(
                  list(
                    blockr.dock::new_dock_stack(
                      members,
                      name = "New chapter",
                      color = colors[[1L]]
                    )
                  ),
                  new_id
                )
              )
            )

            if (!length(keep)) {
              update(list(stacks = payload))
              return()
            }

            stk_id <- names(keep)[[1L]]

            detach <- if (length(keep[[1L]])) {
              list(mod = setNames(list(list(blocks = keep[[1L]])), stk_id))
            } else {
              list(rm = stk_id)
            }

            update(list(stacks = detach))

            session$onFlushed(
              function() isolate(update(list(stacks = payload))),
              once = TRUE
            )
          }
        )

        observeEvent(
          input$outline_chapter,
          {
            act <- input$outline_chapter
            req(is.character(act$stack), is.character(act$act))

            s <- sections()
            idx <- which(s$stack_ids %in% act$stack)
            req(length(idx))

            if (act$act %in% c("include", "exclude")) {

              ann <- rv_ann()
              want <- identical(act$act, "include")

              for (bid in s$ids[idx]) {
                entry <- coal(ann[[bid]], list())
                entry$report <- want
                ann[[bid]] <- entry
              }

              rv_ann(ann)
              return()
            }

            if (identical(act$act, "ungroup")) {
              commit_stacks(setNames(list(character()), act$stack))
              return()
            }

            if (identical(act$act, "merge")) {

              # The chapter above in the DOCUMENT, which may be another run
              # of the same stack (a split chapter) -- then there is
              # nothing to merge.
              above <- s$stack_ids[seq_len(min(idx) - 1L)]
              above <- above[!is.na(above) & above != act$stack]
              req(length(above))

              target <- above[[length(above)]]

              commit_stacks(
                setNames(
                  list(
                    union(stack_members(target), stack_members(act$stack)),
                    character()
                  ),
                  c(target, act$stack)
                )
              )
            }
          }
        )

        observeEvent(
          input$outline_tostack,
          {
            mv <- input$outline_tostack
            req(is.character(mv$id), is.character(mv$stack))

            stks <- blockr.core::board_stacks(board$board)
            req(mv$stack %in% names(stks))

            if (mv$id %in% stack_members(mv$stack)) {
              return()
            }

            members <- list()

            for (s in names(stks)) {
              cur <- blockr.core::stack_blocks(stks[[s]])
              if (mv$id %in% cur) {
                members[[s]] <- setdiff(cur, mv$id)
              }
            }

            members[[mv$stack]] <- c(stack_members(mv$stack), mv$id)

            commit_stacks(members)
          }
        )

        # Remove a block through the dock's own remove action (which
        # confirms and cleans up links / panels).
        observeEvent(
          input$outline_rm,
          {
            blk_id <- input$outline_rm$id
            req(is.character(blk_id))

            rm_act <- if (is.list(actions)) actions[["remove_block_action"]]

            if (is.null(rm_act)) {
              showNotification(
                "Removing blocks needs the dock's remove action.",
                type = "warning"
              )
              return()
            }

            rm_act(blk_id)
          }
        )

        # Reveal the block's panel in the active view, exactly like
        # clicking a dag node (focus-or-add, never switch views).
        observeEvent(
          input$outline_open,
          {
            blk_id <- input$outline_open$id
            req(is.character(blk_id))
            req(blk_id %in% blockr.core::board_block_ids(board$board))

            views <- blockr.dock::board_views(board$board)
            view <- blockr.dock::active_view(views)

            if (is.null(view)) {
              return()
            }

            pid <- as.character(blockr.dock::as_block_panel_id(blk_id))

            ops <- if (pid %in% blockr.dock::view_members(views[[view]])) {
              list(select = pid)
            } else {
              list(add = setNames(list(list()), pid), select = pid)
            }

            update(list(views = list(mod = setNames(list(ops), view))))
          }
        )

        output$code_render <- downloadHandler(
          filename = function() {
            paste0(
              "board-report-",
              format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
              ".",
              input$code_render_format
            )
          },
          content = function(file) {
            render_report(
              qmd_txt(),
              spin_txt(),
              input$code_render_format,
              file,
              rv_title()
            )
          }
        )

        list(
          state = list(
            annotations = rv_ann,
            block_order = rv_order,
            title = rv_title,
            stack_annotations = rv_stack_ann
          )
        )
      }
    )
  }
}

sanitize_annotations <- function(x) {

  if (!is.list(x) || !length(x)) {
    return(list())
  }

  lapply(
    x,
    function(entry) {
      list(
        description = as.character(
          coal(unlist(entry[["description"]]), "")
        )[1L],
        report = isTRUE(coal(unlist(entry[["report"]]), TRUE))
      )
    }
  )
}
