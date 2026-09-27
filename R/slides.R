#' Slide builder extension
#'
#' A dock extension that turns a board into a deck: pick the blocks whose
#' output should become slides, order them, download a PowerPoint file or an
#' HTML deck. One block, one slide.
#'
#' The lightweight counterpart to a full document renderer: a report builds
#' a whole Quarto document -- prose, chapters, the generated code -- and can
#' render it as a deck among other things. This does only the deck, and the
#' difference shows in what it asks of the user: a picker and a list, no
#' writing surface and nothing to read.
#'
#' Two consequences worth knowing about, both of which follow from having no
#' document:
#'
#' * **Slides are freely orderable.** The deck emits every block's code up
#'   front, hidden, and each slide carries only its exhibit -- so slide order
#'   and evaluation order are independent. A deck may open on its conclusion.
#' * **Nothing is evaluated until you download.** The picker and the list
#'   read block names off the board; block expressions are read once, when
#'   the download is clicked. A slide builder sitting in a closed dock panel
#'   costs nothing.
#'
#' Blocks upstream of a picked block are still evaluated -- picking a table
#' means running what feeds it -- but they are not shown and take no slide.
#' Branches nothing picked depends on are never evaluated at all.
#'
#' @section Tables that do not fit:
#' A table too tall for its slide is carried onto the next one, and blockr.viz
#' shrinks the type before it does that: one slide at 10pt beats two at 13pt.
#' How far it may shrink is the board's `exhibit_min_font_size` option
#' (`blockr.viz::new_exhibit_font_option()`, "Smallest table font" in the
#' board settings), so a deck that must not split its tables asks for it
#' there rather than here. It is a board option and not a field in this
#' panel because the same number governs the PowerPoint download on a table
#' or summarize block: a slide and the block it came from have to be the same
#' table.
#'
#' What still does not fit at that size is split, and the download says which
#' tables and at what size they would have stayed whole.
#'
#' @param slides Character vector of block ids, in slide order. Both the
#'   picking and the ordering: a block is a slide iff it is named here.
#' @param title Deck title. Names the file, titles the html deck and appears
#'   as the running footer on every html slide.
#' @param format Download format: `"pptx"` (PowerPoint) or `"html"`
#'   (a self-contained HTML deck). Both are written in this process, so
#'   neither needs the quarto CLI on the machine. `"revealjs"`, the format
#'   string of the quarto render the HTML deck replaced, still restores as
#'   `"html"`.
#' @param template LEGACY, ignored. The reference deck is a property of the
#'   deployment, not of a board: it comes from
#'   `getOption("blockr.outline.template")` (an app sets it once, typically
#'   from `blockr.theme::theme_template()`), falling back to the bundled
#'   widescreen deck. Accepted only so boards saved while the deck panel
#'   still offered a template field restore without error.
#' @param ... Forwarded to [blockr.dock::new_dock_extension()]
#'
#' @return A dock extension object, to be passed in a board's `extensions`
#'   list (see [blockr.dock::new_dock_board()]).
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   library(blockr.dock)
#'
#'   board <- new_dock_board(
#'     blocks = c(
#'       data = new_dataset_block("iris"),
#'       audit = new_head_block(n = 3L)
#'     ),
#'     links = links(from = "data", to = "audit"),
#'     extensions = list(
#'       new_slides_extension(slides = "audit", title = "Iris pilot")
#'     )
#'   )
#'
#'   serve(board)
#' }
#'
#' @export
new_slides_extension <- function(slides = character(),
                                 title = "Deck",
                                 format = "pptx",
                                 template = "",
                                 ...) {

  blockr.dock::new_dock_extension(
    slides_ext_srv(slides, title, format),
    slides_ext_ui,
    name = "Slides",
    description = slides_ext_meta(),
    class = "slides_extension",
    external_ctrl = c("slides", "title", "format"),
    ...
  )
}

# Model-facing documentation, read by blockr.dock's external-control tooling
# (`list_extensions` reports the description, `describe_extension` the
# arguments, examples and guidance) so a client driving the deck through
# `modify_extension` is told the one thing it cannot infer: `slides` is the
# WHOLE deck, not a queue to append to.
slides_ext_meta <- function() {
  blockr.dock::new_ext_meta(
    description = paste(
      "Deck builder: pick the blocks whose output becomes a slide, order",
      "them, download as PowerPoint or HTML."
    ),
    arguments = c(
      slides = paste(
        "Block ids, in slide order. A block is a slide iff it is named",
        "here; one id, one slide, rendered from that block's output.",
        "Ids naming blocks that are not on the board are dropped."
      ),
      title = paste(
        "Deck title. Names the downloaded file, titles the HTML deck and",
        "appears as its running footer."
      ),
      format = "Download format: \"pptx\" or \"html\"."
    ),
    examples = list(
      list(slides = c("demographics", "response_rates", "safety_summary")),
      list(slides = c("response_rates", "demographics"), title = "Interim")
    ),
    guidance = paste(
      "`slides` is the whole deck, always. There is no add, move or remove",
      "verb: read the current order from the `values` field of",
      "list_extensions, then write back the full vector in the order you",
      "want. Sending one id replaces the deck with a one-slide deck.",
      "Only blocks with a visible output -- a table, a chart, an exhibit --",
      "make sensible slides. The reads and transforms feeding them still",
      "run at download time and must NOT be listed; picking a table is what",
      "runs its chain. Slide order and evaluation order are independent, so",
      "a deck may open on its conclusion. Deck styling is not set here: the",
      "master template and the look come from the deployment's theme."
    )
  )
}

# The formats a deck offers. Deliberately two, and both written in-process:
# officer for the PowerPoint, this package's own writer for the HTML (see
# R/deck-html.R for why that is not quarto + revealjs). Neither needs a CLI
# on the machine.
deck_formats <- function() {
  c("PowerPoint" = "pptx", "HTML" = "html")
}

# LEGACY: the HTML deck was a quarto revealjs render before it was written
# here, and a board saved in between carries quarto's word for it.
deck_format <- function(fmt) {
  fmt <- coal(fmt, "pptx")
  if (identical(fmt, "revealjs")) "html" else fmt
}

slides_ext_ui <- function(id, board, ...) {

  ns <- NS(id)

  div(
    class = "blockr-otl-pnl blockr-sld-panel",
    id = ns("sld_root"),
    `data-ns` = ns(""),
    panels_dep(),
    slides_dep(),
    div(
      class = "blockr-otl-head",
      otl_name(
        ns("sld_title"),
        label = "Deck title",
        empty_msg = "A deck needs a title",
        placeholder = "Untitled deck"
      ),
      div(
        class = "blockr-otl-tools",
        # The download is a tool; with two formats it opens an action menu,
        # one row per format with the extension as meta. A row goes to the
        # server first (demand the picked blocks, wait for their code) and
        # the hidden link below is clicked once the deck is ready. No gear:
        # the deck's look comes from the deployment's template (see
        # effective_template()), so there is nothing left to configure.
        do.call(
          blockr.ui::action_menu,
          c(
            list(blockr.ui::tool_button(HTML(otl_icon("download")), "Download")),
            lapply(
              seq_along(deck_formats()),
              function(i) {
                fmt <- unname(deck_formats()[[i]])
                blockr.ui::menu_item(
                  tags$button(
                    class = "blockr-sld-fmt",
                    `data-format` = fmt,
                    deck_format_label(fmt)
                  ),
                  meta = paste0(".", report_ext(fmt))
                )
              }
            )
          )
        )
      )
    ),
    downloadLink(ns("sld_dl"), label = NULL, style = "display: none;"),
    div(class = "blockr-otl-list", uiOutput(ns("sld_list"))),
    otl_button(
      "Add slide",
      kind = "quiet",
      size = "s",
      icon = otl_icon("plus"),
      class = "blockr-otl-add"
    ),
    tags$script(
      HTML(sprintf("BlockrSlides.init('%s');", ns("sld_root")))
    )
  )
}

# The name a download menu row gives a format.
deck_format_label <- function(fmt) {
  switch(fmt, pptx = "PowerPoint", html = "Web page", fmt)
}

slides_dep <- function() {
  htmlDependency(
    "blockr-slides",
    pkg_version(),
    src = pkg_file("assets"),
    script = "js/blockr-slides.js",
    all_files = FALSE
  )
}

slides_ext_srv <- function(slides, title, format = "pptx") {

  function(id, board, update, session, parent, actions = NULL, ...) {
    moduleServer(
      id,
      function(input, output, session) {

        rv_slides <- reactiveVal(as.character(unlist(slides)))
        rv_title <- reactiveVal(
          if (is.character(title) && length(title)) title[[1L]] else "Deck"
        )
        rv_format <- reactiveVal(
          if (deck_format(format) %in% deck_formats()) {
            deck_format(format)
          } else {
            "pptx"
          }
        )

        # ---- the board, as names ------------------------------------
        #
        # Everything the panel DRAWS comes from here, and nothing here is an
        # expression. A block's name and its exhibit kind are properties of
        # the block object, readable off the board whether or not the block
        # has ever been constructed -- which is what lets a slide builder on
        # a deferred board cost nothing until the download.

        block_meta <- reactive(
          {
            blks <- blockr.core::board_blocks(board$board)

            lapply(
              setNames(nm = names(blks)),
              function(i) {
                list(
                  name = blockr.core::block_name(blks[[i]]),
                  kind = block_exhibit_kind(blks[[i]]),
                  mark = block_mark(blks[[i]])
                )
              }
            )
          }
        )

        # Drop picks for blocks that have left the board: the id-keyed state
        # has to follow the board's block lifecycle, same as the outline's
        # annotations.
        observeEvent(
          board$board,
          {
            ids <- blockr.core::board_block_ids(board$board)
            keep <- intersect(rv_slides(), ids)
            if (!identical(rv_slides(), keep)) {
              rv_slides(keep)
            }
          }
        )

        # ---- picking and ordering -----------------------------------

        # The "Add slide" menu is filled client-side from this payload:
        # every board block, its mark, name and type. Which blocks are in
        # the deck rides in its own, tiny message, because that flips on
        # every add; glyphs are shared by key because a board repeats each
        # type's glyph per block.
        #
        # Identical-skip: `board$board` is reassigned by EVERY board update,
        # including the state a block commits as it constructs, and a plain
        # reactive re-emits regardless of whether anything it reads moved.
        # Both messages carry the panel's root id, so two panels on a page
        # do not fill each other's menus.
        root_id <- session$ns("sld_root")
        catalog_sig <- NULL

        observe(
          {
            msg <- board_catalog(block_meta())

            if (!identical(msg, catalog_sig)) {
              catalog_sig <<- msg
              session$sendCustomMessage(
                "blockr-slides-catalog",
                c(list(root = root_id), msg)
              )
            }
          }
        )

        observe(
          {
            session$sendCustomMessage(
              "blockr-slides-picked",
              list(root = root_id, ids = as.list(rv_slides()))
            )
          }
        )

        observeEvent(
          input$sld_add,
          {
            id <- input$sld_add
            req(is.character(id), length(id) == 1L, nzchar(id))

            if (!id %in% rv_slides()) {
              rv_slides(c(rv_slides(), id))
            }
          }
        )

        observeEvent(
          input$sld_act,
          {
            blk <- input$sld_act$id
            act <- input$sld_act$act
            req(is.character(blk), is.character(act))

            cur <- rv_slides()
            at <- match(blk, cur)
            req(!is.na(at))

            rv_slides(
              switch(
                act,
                rm = cur[-at],
                up = if (at > 1L) append(cur[-at], blk, after = at - 2L) else cur,
                down = if (at < length(cur)) {
                  append(cur[-at], blk, after = at)
                } else {
                  cur
                },
                cur
              )
            )
          }
        )

        observeEvent(
          input$sld_move,
          {
            blk <- input$sld_move$id
            target <- input$sld_move$target
            req(is.character(blk), is.character(target), !identical(blk, target))

            cur <- rv_slides()
            req(blk %in% cur, target %in% cur)

            rest <- setdiff(cur, blk)
            at <- match(target, rest)
            req(!is.na(at))

            rv_slides(
              append(rest, blk, after = if (isTRUE(input$sld_move$after)) at else at - 1L)
            )
          }
        )

        # ---- rename a block ------------------------------------------
        #
        # A double-click on a row's name renames the block itself, as in
        # the outline.
        observeEvent(
          input$sld_rename,
          {
            delta <- rename_block_delta(board$board, input$sld_rename)
            if (!is.null(delta)) update(delta)
          }
        )

        # ---- open a block --------------------------------------------
        #
        # The outline's open move, which the report extension's rows make
        # too: reveal the block's panel in the ACTIVE view -- focus it when
        # it is already there, add it when it is not -- and never switch
        # views. A deck row names a block; clicking it should show that
        # block, not navigate somewhere else.
        observeEvent(
          input$sld_open,
          {
            blk_id <- input$sld_open$id
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

        # ---- the list -----------------------------------------------
        #
        # renderUI replaces the list WHOLESALE, so it must fire only when the
        # list actually changes -- anything else is a visible white flash.
        # Its inputs re-emit far more often than they change: `block_meta()`
        # reads `board$board`, which is reassigned by every board update,
        # and a block committing its state as it constructs is one. So the
        # deck repainted whenever a block it slides loaded, having read back
        # the same names, kinds and icons.
        #
        # The store in front is the same mechanism as the outline's
        # `board_shape` (R/ext.R): the observer pays the read on every flush,
        # and a reactiveVal handed a value identical to the one it holds does
        # not notify -- so the render only re-runs when the drawing changes.
        list_state <- reactiveVal(NULL)

        observe(
          list_state(list(picked = rv_slides(), meta = block_meta()))
        )

        output$sld_list <- renderUI({

          state <- list_state()
          req(!is.null(state))

          picked <- state$picked
          meta <- state$meta

          if (!length(picked)) {
            return(
              div(
                class = "blockr-empty blockr-empty--panel",
                "No slides yet. Add a block to make it one."
              )
            )
          }

          div(
            class = "blockr-sld-list",
            lapply(
              seq_along(picked),
              function(k) slides_row(picked[[k]], k, meta[[picked[[k]]]])
            )
          )
        })

        # ---- settings ------------------------------------------------

        # Both directions, for both fields. The reverse leg used to be a
        # one-shot update call that seeded the field at server start, which is
        # enough while the user is the only writer -- but `title` and `format`
        # are externally controllable, and a value arriving from a controller
        # has to reach the field too, or the panel goes on showing the old
        # title while the download carries the new one. `slides` needs no
        # equivalent: its list is repainted from a push observer already.
        # The `identical` guards on both legs are what stops the echo.
        observeEvent(input$sld_title, {
          if (!identical(input$sld_title, rv_title())) {
            rv_title(input$sld_title)
          }
        }, ignoreInit = TRUE)

        # The title field never echoes a value the server sets, so this
        # leg sends unconditionally; again when the panel announces itself
        # (sld_sync), because a message to a field not on the page yet is
        # dropped.
        send_title <- function() {
          session$sendInputMessage("sld_title", list(value = rv_title()))
        }

        observeEvent(rv_title(), send_title())

        observeEvent(input$sld_sync, send_title(), ignoreInit = TRUE)

        # The format is the download menu's last pick. A controller can set
        # it too; it then names the format a saved board last downloaded.
        observeEvent(input$sld_format, {
          if (deck_format(input$sld_format) %in% deck_formats() &&
                !identical(input$sld_format, rv_format())) {
            rv_format(deck_format(input$sld_format))
          }
        }, ignoreInit = TRUE)

        # ---- the projection, on demand -------------------------------
        #
        # A lazy reactive that nothing reads while the panel is idle. The
        # download observer reads it on click; the wait observer below reads
        # it only once a demand is in flight (req() on `awaiting` first, so
        # no dependency is taken while it is NULL). That is the whole of the
        # outline's visibility gate, obtained by not needing one: the deck
        # never draws anything derived from an expression.

        # Last known expression per block id.
        #
        # A block's expr reactive reports NULL whenever it cannot produce one
        # right now, and "right now" is shorter than it sounds: a block whose
        # dock panel is not the visible tab stops reporting altogether.
        #
        # It is load-bearing for the DOWNLOAD specifically, which is the only
        # thing here that reads expressions at all. Core drops an `evaluate`
        # request once the block it names has run, so the block goes quiet
        # again between the wait observer seeing the expression and the
        # download handler building its own projection -- and the deck would
        # render without the slide it had just waited for, silently, because
        # a pending block is skipped rather than raised. The cache is what
        # carries the expression across that gap.
        expr_cache <- new.env(parent = emptyenv())

        board_exprs <- reactive(
          {
            ex <- lapply(
              blockr.core::lst_xtr(board$blocks, "server", "expr"),
              function(e) {
                tryCatch(blockr.core::reval(e), error = function(err) NULL)
              }
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

            rm(list = setdiff(ls(expr_cache), live), envir = expr_cache)

            # A block that has never reported an expression is PENDING, not
            # absent: it holds a placeholder, so the download can see it is
            # missing and demand it rather than quietly rendering a deck
            # without that slide.
            pending <- names(out)[vapply(out, is.null, logical(1L))]

            for (id in pending) {
              # NOT quote(NULL): NULL is self-evaluating, so quote(NULL) IS
              # NULL and assigning it deletes the element instead of filling
              # it.
              out[[id]] <- quote(invisible(NULL))
            }

            structure(out, pending = pending)
          }
        )

        # "static" is the deck's renderer style, not the document's: every
        # non-figure exhibit goes through blockr.viz::static_exhibit(), so a
        # function or code block returning a composer table lands on the
        # slide as a table rather than a bare print. See slide_sections().
        sections <- reactive(
          slide_sections(
            board_exprs(),
            board$board,
            rv_slides(),
            renderer = getOption("blockr.outline.report_renderer", "static")
          )
        )

        qmd_txt <- reactive(export_deck_qmd(sections(), rv_title()))

        # ---- the two-stage download ----------------------------------

        awaiting <- reactiveVal(FALSE)
        wait_note <- reactiveVal(NULL)

        # The browser draws the charts for this deck: a picture of what is
        # on the screen, at the box a SLIDE gives it, for every chart on the
        # deck whether or not its panel was ever opened. The exchange holds
        # the tokens between the ask and the write (R/captures.R).
        caps <- capture_exchange("deck")
        captures <- reactiveVal(NULL)

        # Ask core to bring the pending blocks up to date.
        #
        # `evaluate` is core's one-off evaluation request (see the "Evaluation
        # requests" section of blockr.core::board_server): it names blocks,
        # core joins them and their upstream closure to the eval set, and
        # drops the request once each has run or reported why it cannot. It
        # travels on the board-update channel every extension already holds,
        # needs no handle the dock does not hand out, and is orthogonal to the
        # front-end's `required` visibility axis -- so it neither competes
        # with the dock's card-build ledger nor leaves anything behind to put
        # back. A locked board still accepts it, because it carries no state
        # change.
        #
        # This is what makes the download work on a block whose panel is not
        # the visible tab. Without it a picked block on another view reports
        # no expression, stays pending forever, and the click can only refuse.
        demand_blocks <- function(pending) {

          if (!is.function(update)) {
            return(FALSE)
          }

          update(list(evaluate = pending))

          TRUE
        }

        drop_wait_note <- function() {
          wait_since(NULL)
          note <- wait_note()
          if (!is.null(note)) {
            removeNotification(note)
            wait_note(NULL)
          }
        }

        # When the demand went out, and over how many blocks. NULL = no
        # demand in flight.
        #
        # The notification below is a LATENESS report, not a progress bar.
        # Almost every demand is served in the flush after the click, and a
        # notification that appears and disappears within one frame reads as
        # something having gone wrong -- it is on screen too briefly to be
        # read, so all it conveys is that something flashed. Nobody needs to
        # be told that a click they just made is being worked on; they need
        # to be told when it is taking longer than they expect. So the click
        # records the wait and this observer shows the note only once the
        # wait has actually got long, which on a deferred board with slow
        # blocks is exactly when it earns its place.
        wait_since <- reactiveVal(NULL)

        observe({

          held <- wait_since()

          req(awaiting(), held)

          if (!is.null(isolate(wait_note()))) {
            return()
          }

          left <- wait_note_delay() -
            as.numeric(difftime(Sys.time(), held$at, units = "secs"))

          if (left > 0) {
            invalidateLater(ceiling(left * 1000), session)
            return()
          }

          wait_note(
            showNotification(
              sprintf(
                paste(
                  "Evaluating %d block%s\u2026 the download starts when",
                  "the deck is ready."
                ),
                held$n,
                if (held$n == 1L) "" else "s"
              ),
              duration = NULL,
              closeButton = FALSE
            )
          )
        })

        pending_exported <- function(sects) {
          sects$ids[sects$exported & sects$pending]
        }

        fire_download <- function() {
          session$sendCustomMessage(
            "blockr-slides-download",
            list(id = session$ns("sld_dl"))
          )
        }

        # The last step before the file is written: ask the browser to draw
        # the deck's charts. Nothing to draw (no chart blocks, no capture
        # service) goes straight to the download, so a deck of tables is
        # exactly as it was.
        capture_then_fire <- function(sects) {
          if (caps$request(sects)) {
            return(invisible(NULL))
          }
          fire_download()
        }

        observeEvent(
          input$sld_go,
          {
            # A download menu row sends the format it names; a bare click
            # count downloads the current one.
            fmt <- if (is.list(input$sld_go)) input$sld_go$format
            if (is.character(fmt) && deck_format(fmt) %in% deck_formats()) {
              rv_format(deck_format(fmt))
            }

            if (!length(rv_slides())) {
              showNotification(
                "Pick at least one block before downloading a deck.",
                type = "warning"
              )
              return()
            }

            sects <- tryCatch(sections(), error = function(e) NULL)

            if (is.null(sects)) {
              showNotification(
                "The board is not ready yet; try again in a moment.",
                type = "warning"
              )
              return()
            }

            pending <- pending_exported(sects)

            if (!length(pending)) {
              capture_then_fire(sects)
              return()
            }

            if (!demand_blocks(pending)) {
              showNotification(
                paste(
                  "Some slide blocks are not initialized yet. Open their",
                  "views to initialize them, then download again."
                ),
                type = "warning",
                duration = 10
              )
              return()
            }

            drop_wait_note()
            awaiting(TRUE)
            wait_since(list(at = Sys.time(), n = length(pending)))
          }
        )

        observe(
          {
            req(awaiting())

            if (length(pending_exported(sections()))) {
              return()
            }

            awaiting(FALSE)
            drop_wait_note()
            capture_then_fire(sections())
          }
        )

        # The pictures land one by one; the deck goes when the set is
        # complete.
        observe({
          req(caps$pending())
          got <- caps$value()
          req(!is.null(got))
          captures(got)
          caps$clear()
          fire_download()
        })

        output$sld_dl <- downloadHandler(
          filename = function() {
            paste0(
              deck_filename(rv_title()),
              "-",
              format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
              ".",
              report_ext(rv_format())
            )
          },
          content = function(file) {
            # Both formats are written here, in this process. The HTML deck
            # is this package's own writer (R/deck-html.R); the PowerPoint is
            # officer. render_report()'s quarto path is the outline's, for
            # documents, and a deck never takes it.
            if (identical(rv_format(), "html")) {
              return(
                with_render_guard(
                  render_deck_html(sections(), file, rv_title())
                )
              )
            }

            with_render_guard(
              render_report(
                qmd_txt(),
                # The rmarkdown fallback (no quarto on the machine) renders a
                # document rather than a deck, and in document order: the
                # spin script cannot express a free slide order. A degraded
                # deck beats no download, and quarto is present everywhere
                # this actually ships.
                export_spin(sections()),
                rv_format(),
                file,
                rv_title(),
                template = effective_template(),
                sects = sections_with_captures(sections(), captures()),
                captures = captures()
              )
            )
          }
        )

        # A downloadHandler behind a display:none link is a hidden output,
        # and Shiny suspends those -- which here means the href is never
        # populated and the JS click navigates to the bare page URL.
        outputOptions(output, "sld_dl", suspendWhenHidden = FALSE)

        list(
          state = list(
            slides = rv_slides,
            title = rv_title,
            format = rv_format
          )
        )
      }
    )
  }
}

# One row of the deck list (design system, "Block lists"). Pure markup, no
# Shiny inputs: the "..." and its menu report through one delegated handler
# (blockr-slides.js), so the list can be re-rendered without anything to
# rebind. A click opens the block, a double-click on the name renames it, the
# whole row drags; Alt+Up and Alt+Down move the focused row.
slides_row <- function(id, k, meta) {

  meta <- coal(meta, list())
  name <- coal(na_blank(meta$name), id)

  div(
    class = "blockr-otl-row blockr-sld-row",
    `data-blk` = id,
    `data-kind` = coal(meta$kind, ""),
    draggable = "true",
    tabindex = "0",
    # The slide number is what makes this a deck rather than a set. It is
    # positional, drawn from the row's place in the list and never stored.
    span(class = "blockr-otl-row__num blockr-sld-num", k),
    otl_mark(meta$mark),
    otl_row_name(name),
    span(class = "blockr-otl-row__end", otl_row_more())
  )
}

# The board update that renames a block from a list row: `msg` is the
# client's {id, name}. NULL for a block that is not on the board, or an
# empty name (the row refuses that in place already).
rename_block_delta <- function(board, msg) {

  id <- msg$id
  nm <- trimws(as.character(coal(msg$name, "")))

  if (!is.character(id) || length(id) != 1L || length(nm) != 1L ||
        !nzchar(nm) || !id %in% blockr.core::board_block_ids(board)) {
    return(NULL)
  }

  list(blocks = list(mod = setNames(list(list(block_name = nm)), id)))
}

# The title, as a filename stem. A deck called "Q3 review / EU" must not
# produce a path separator, and a title of nothing at all must still produce
# a name.
deck_filename <- function(title) {

  out <- gsub("^-+|-+$", "", gsub("-+", "-", gsub("[^A-Za-z0-9]+", "-", title)))

  if (!nzchar(out)) "deck" else tolower(out)
}
