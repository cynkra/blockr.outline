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
#' @param stack_annotations Named list keyed by stack id, each entry a list
#'   with a `description` (markdown string) used as the chapter intro.
#' @param stack_title_level How stack titles render in the document: `"#"`
#'   or `"##"` for a heading of that level, `"none"` to omit them.
#' @param block_title_level How block titles render: `"caption"` (the
#'   exhibit caption), `"#"`, `"##"` or `"###"` for a heading of that
#'   level, `"none"` to omit them. A block title is a heading or a
#'   caption, never both.
#' @param template LEGACY, ignored. The reference document is a property of
#'   the deployment, not of a board: it comes from
#'   `getOption("blockr.outline.template")` (an app sets it once, typically
#'   from `blockr.theme::theme_template()`), falling back to the bundled
#'   widescreen deck. Accepted only so boards saved while the gear still
#'   offered a template field restore without error.
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
#'       new_outline_extension(
#'         title = "Iris pilot report",
#'         annotations = list(
#'           data = list(description = "The classic **iris** dataset."),
#'           audit = list(description = "Quick QC check.", report = FALSE)
#'         )
#'       )
#'     )
#'   )
#'
#'   serve(board)
#' }
#'
#' @export
new_outline_extension <- function(annotations = list(),
                                  block_order = character(),
                                  title = "Board report",
                                  stack_annotations = list(),
                                  stack_title_level = "#",
                                  block_title_level = "caption",
                                  template = "",
                                  ...) {

  blockr.dock::new_dock_extension(
    outline_ext_srv(
      annotations, block_order, title, stack_annotations,
      stack_title_level, block_title_level
    ),
    outline_ext_ui,
    name = "Outline",
    description = paste(
      "Report outline of the board: per-block markdown descriptions,",
      "include-in-report flags, document order, and html/pptx render",
      "of the generated report."
    ),
    class = "outline_extension",
    ...
  )
}

outline_ext_ui <- function(id, board, ...) {

  ns <- NS(id)

  formats <- report_formats()

  div(
    class = "blockr-otl-panel",
    # Id so the client can report when this panel is on screen. The dock
    # mounts every extension eagerly (in a hidden offcanvas) and MOVES the
    # DOM into a dock panel when shown, so the outline's outputs are live
    # from board startup. Without a visibility gate the O(n^2) projection
    # (board_exprs -> sections_calc -> outline_sections) would run on every
    # block-expression change even while the panel is closed -- a real cost
    # on a large deferred board (see the perf note in the server). An
    # IntersectionObserver on this element drives `otl_visible`, gating the
    # projection so a present-but-unopened outline costs nothing.
    id = ns("otl_panel"),
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
      # Outline body: generated Code (default) or the activated blocks'
      # rendered Output -- the deck-builder view. Only meaningful in the
      # Outline view, so it rides in a conditionalPanel keyed on it.
      # Icon-only (</> vs eye): two states on a strip that only appears in
      # one of three views reads better as glyphs than a second word pair
      # competing with the view switcher beside it.
      conditionalPanel(
        condition = sprintf("input['%s'] == 'outline'", ns("code_view")),
        class = "blockr-otl-bodytoggle",
        shinyWidgets::radioGroupButtons(
          inputId = ns("otl_body"),
          label = NULL,
          size = "sm",
          status = "light",
          choiceValues = c("code", "output"),
          choiceNames = list(
            tags$span(
              class = "blockr-otl-bodyicon", title = "Code",
              HTML(paste0(
                "<svg width='14' height='14' viewBox='0 0 24 24' fill='none' ",
                "stroke='currentColor' stroke-width='2' stroke-linecap='round' ",
                "stroke-linejoin='round'><polyline points='16 18 22 12 16 6'/>",
                "<polyline points='8 6 2 12 8 18'/></svg>"
              ))
            ),
            tags$span(
              class = "blockr-otl-bodyicon", title = "Output",
              HTML(paste0(
                "<svg width='14' height='14' viewBox='0 0 24 24' fill='none' ",
                "stroke='currentColor' stroke-width='2' stroke-linecap='round' ",
                "stroke-linejoin='round'><path d='M1 12s4-8 11-8 11 8 11 8-4 8-",
                "11 8-11-8-11-8z'/><circle cx='12' cy='12' r='3'/></svg>"
              ))
            )
          ),
          selected = "code"
        )
      ),
      div(
        class = "blockr-otl-toolbar-right",
        # Split button (decision record:
        # design-system/target/outline-render-group-proposals.html, A +
        # label 2): the format picker and the action read as one control,
        # neutral picker fused to the green action. The label is
        # "Download", not "Render" -- render is the internal step, the
        # file is what the user walks away with.
        div(
          class = "blockr-otl-rendergroup",
          # selectize = FALSE: a native <select> can be sized to match the
          # buttons beside it; selectize's wrapper cannot without fighting
          # its own layout (it rendered 42px against their 28px).
          selectInput(
            ns("code_render_format"),
            label = NULL,
            choices = formats,
            # The deck is what this is used for: a board's exhibits are sized,
            # placed and styled for slides. html is the one you take when you
            # do not want a deck.
            selected = "pptx",
            selectize = FALSE,
            width = "88px"
          ),
          # The visible half of a two-stage download. A plain
          # downloadButton would GET immediately on click; on a deferred
          # board the reported blocks may not be constructed yet, so the
          # click first goes to the server (require blocks, wait for their
          # code) which then clicks the hidden link below from JS. See the
          # code_render_go observer.
          actionButton(
            ns("code_render_go"),
            "Download",
            icon = icon("download"),
            class = "blockr-otl-renderbtn"
          ),
          downloadLink(
            ns("code_render"),
            label = NULL,
            style = "display: none;"
          )
        ),
        # Gear: opens the in-flow settings band below the toolbar (the
        # blockr.viz / blockr.dplyr settings-band pattern). Output-shaping
        # options that do not belong on the always-visible toolbar.
        # Same cog + treatment as blockr.dplyr's .blockr-gear-btn, and the
        # band below grows the same beak pointing back at it.
        tags$button(
          id = ns("otl_gear"),
          type = "button",
          class = "blockr-gear-btn blockr-otl-gearbtn",
          title = "Report settings",
          HTML(paste0(
            "<svg xmlns='http://www.w3.org/2000/svg' width='14' height='14' ",
            "fill='currentColor' viewBox='0 0 16 16'><path d='M9.405 1.05c-",
            ".413-1.4-2.397-1.4-2.81 0l-.1.34a1.464 1.464 0 0 1-2.105.872l-",
            ".31-.17c-1.283-.698-2.686.705-1.987 1.987l.169.311c.446.82.023 ",
            "1.841-.872 2.105l-.34.1c-1.4.413-1.4 2.397 0 2.81l.34.1a1.464 ",
            "1.464 0 0 1 .872 2.105l-.17.31c-.698 1.283.705 2.686 1.987 ",
            "1.987l.311-.169a1.464 1.464 0 0 1 2.105.872l.1.34c.413 1.4 ",
            "2.397 1.4 2.81 0l.1-.34a1.464 1.464 0 0 1 2.105-.872l.31.17c",
            "1.283.698 2.686-.705 1.987-1.987l-.169-.311a1.464 1.464 0 0 1 ",
            ".872-2.105l.34-.1c1.4-.413 1.4-2.397 0-2.81l-.34-.1a1.464 ",
            "1.464 0 0 1-.872-2.105l.17-.31c.698-1.283-.705-2.686-1.987-",
            "1.987l-.311.169a1.464 1.464 0 0 1-2.105-.872zM8 10.93a2.929 ",
            "2.929 0 1 1 0-5.86 2.929 2.929 0 0 1 0 5.858z'/></svg>"
          ))
        )
      )
    ),
    outline_settings_band(ns),
    # The report title and the search box sit above the outline body and
    # only in the Outline view (the title is state; the search is client
    # logic over a pushed catalogue). They live outside outline_out so the
    # input stays focus-stable across the body's renderUI re-renders --
    # which is also why the menu is filled by JS rather than rendered
    # server-side: re-rendering the control would drop focus mid-query.
    conditionalPanel(
      condition = sprintf("input['%s'] == 'outline'", ns("code_view")),
      uiOutput(ns("otl_title")),
      # The field and the menu are the block browser's: same classes, hence
      # the same magnifier, focus ring, rows, icon tiles and section
      # headers, from the stylesheet blockr.dock already puts on the page.
      # `type = "search"` also gives the native clear affordance the
      # browser relies on.
      div(
        # NOT .blockr-block-browser: that class is the block browser's
        # Shiny input binding and its search JS, which would adopt this
        # control as a browser instance and filter it by data attributes
        # these cards do not carry. The card classes below are inert
        # styling, and blockr-outline.css maps the --bb-* tokens they read.
        class = "blockr-otl-search",
        tags$input(
          type = "search",
          class = "blockr-block-browser-search blockr-otl-searchinput",
          # Short on purpose: a dock panel is often 375px wide, and the
          # pool count sits on the same line.
          placeholder = "Search or add a block\u2026",
          `aria-label` = "Search blocks",
          autocomplete = "off",
          spellcheck = "false"
        ),
        span(class = "blockr-otl-searchcount"),
        div(class = "blockr-otl-searchmenu")
      )
    ),
    uiOutput(ns("outline_out")),
    outline_js(ns)
  )
}

# The gear's settings band. Uses blockr.dplyr's settings-band selectors
# (.blockr-settings / __title / __grid / __field) -- the shared layer bound
# for blockr.ui -- so it matches every other gear band in the ecosystem.
# A __title spans the row, forcing each section onto its own line; Template
# takes a --full field so it runs the whole width. Hidden until the gear
# toggles it (outline_js). Defaults reproduce today's output.
outline_settings_band <- function(ns) {
  div(
    class = "blockr-settings blockr-settings--beak",
    id = ns("otl_settings"),

    div(class = "blockr-settings__title", "Headings"),
    div(
      class = "blockr-settings__grid",
      div(
        class = "blockr-settings__field",
        tags$label("Stack titles", `for` = ns("otl_stack_level")),
        selectInput(
          ns("otl_stack_level"), label = NULL,
          choices = c("Heading 1 (#)" = "#", "Heading 2 (##)" = "##",
                      "None" = "none"),
          selected = "#", selectize = FALSE, width = "100%"
        )
      ),
      div(
        class = "blockr-settings__field",
        tags$label("Block titles", `for` = ns("otl_block_level")),
        selectInput(
          ns("otl_block_level"), label = NULL,
          choices = c("Caption" = "caption", "Heading 1 (#)" = "#",
                      "Heading 2 (##)" = "##", "Heading 3 (###)" = "###",
                      "None" = "none"),
          selected = "caption", selectize = FALSE, width = "100%"
        )
      )
    ),

    div(class = "blockr-settings__title", "Outline"),
    div(
      class = "blockr-settings__grid",
      div(
        class = "blockr-settings__field blockr-settings__field--full",
        checkboxInput(
          ns("otl_show_all"),
          "Show all blocks",
          value = FALSE
        ),
        div(
          class = "blockr-settings__hint",
          paste(
            "By default the outline lists the document: the report's",
            "blocks plus the upstream blocks they need, the latter with",
            "their switch off. Branches outside the report are added",
            "through the picker under the list. Show-all restores the",
            "full board overview."
          )
        )
      )
    )

    # No Template section. The reference doc styles every render of a
    # deployment, so it is the app's to declare, not the reader's to type:
    # see effective_template().
  )
}

outline_ext_srv <- function(annotations, block_order, title,
                            stack_annotations = list(),
                            stack_title_level = "#",
                            block_title_level = "caption") {

  function(id, board, update, session, parent, actions = NULL,
           visibility = NULL, ...) {
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
        rv_stack_level <- reactiveVal(coal(stack_title_level, "#"))
        rv_block_level <- reactiveVal(coal(block_title_level, "caption"))
        editing <- reactiveVal(NULL)
        # "Insert after X" intent: the id the add link was clicked on, and
        # the block ids last seen on the board. A new block must land where
        # the user pointed -- the topological order alone cannot know that
        # (stack contiguity would keep the source's run together first).
        pending_after <- reactiveVal(NULL)
        known_ids <- reactiveVal(NULL)

        # Excluded rows whose code cell the user has opened. An
        # `include: false` chunk is in the document so it RUNS, not so it
        # is read: its code is collapsed to the badge line until asked for
        # (on a long prep chain those cells are most of the column, and
        # they are also the bulk of the highlighting cost). Presentation
        # only, and deliberately not persisted -- it says what the reader
        # is looking at right now, not what the document is.
        rv_open_code <- reactiveVal(character())

        # Whether the outline panel is on screen (client-reported via the
        # IntersectionObserver in outline_js). Seeded TRUE so the first
        # projection runs before the client has reported and a broken /
        # absent observer degrades to the old always-on behaviour rather
        # than a blank panel. `sections_calc` reads this to skip the
        # expensive projection while the panel is closed.
        panel_visible <- reactiveVal(TRUE)

        observeEvent(
          input$otl_visible,
          panel_visible(isTRUE(input$otl_visible)),
          ignoreNULL = TRUE
        )

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

            opn <- intersect(rv_open_code(), ids)
            if (!identical(rv_open_code(), opn)) {
              rv_open_code(opn)
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

              # Report the new blocks. Added FROM the outline means added
              # to the document, and the flag is what puts them there: a
              # fresh leaf has no reported descendant, so it is outside
              # the export closure and the listing (see `listed` below)
              # would drop it -- the add link would redraw nothing.
              ann <- rv_ann()

              for (id in added) {
                entry <- coal(ann[[id]], list())
                entry$report <- TRUE
                ann[[id]] <- entry
              }

              rv_ann(ann)

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
        # tabs. The cache is also what lets the download flow withdraw its
        # demand (see restore_demanded): a block demoted back to dormant
        # keeps the last expression it reported.
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

        # Memoises the projection's reachability geometry across flushes:
        # an expression-only change (the common case -- any value edit on
        # any constructed block) reuses it and the projection drops from
        # ~300ms to ~30ms at 80 blocks. Structural edits change the key
        # and pay the full sweep, which is when it is actually owed.
        geometry_cache <- new.env(parent = emptyenv())

        # The projection's two inputs, each behind an identical-skip store.
        # `sections_store` below already stops an unchanged projection from
        # redrawing anything -- but by then the projection has been RUN, and
        # at 44 blocks that is 30-120ms. These two stop it being run at all.
        #
        # Both inputs re-emit far more often than they change:
        #   * board_exprs re-reads every block server's expr reactive, and a
        #     block parked behind a dock tab stops reporting one, so merely
        #     fronting a different tab invalidates it -- the expr_cache then
        #     refills the gap with the identical expression.
        #   * board$board is rebuilt by every board update, including the
        #     views delta a tab click commits, which says nothing about the
        #     document.
        # A plain reactive() propagates both regardless of whether the value
        # moved. These do not.
        #
        # Both observers carry the panel-visibility gate that used to sit
        # only on sections_calc. They are now the sole eager readers of
        # board_exprs / board$board, so without it a closed outline would
        # go back to re-reading every block's expression on every flush --
        # the exact cost the gate was added to remove.
        exprs_store <- reactiveVal(NULL)

        observe(
          {
            req(panel_visible())

            ex <- board_exprs()
            if (!identical(ex, isolate(exprs_store()))) {
              exprs_store(ex)
            }
          }
        )

        # Keyed on what outline_sections actually reads off the board:
        # blocks (narrowing, names, icons, export_code), links and stacks.
        # Views are deliberately NOT in the key -- they are the noise this
        # store exists to filter. KEEP THIS IN STEP with outline_sections:
        # a field it starts reading and this signature omits would leave the
        # outline stale.
        board_shape <- reactiveVal(NULL)
        shape_sig <- NULL

        observe(
          {
            req(panel_visible())

            brd <- board$board

            sig <- list(
              blocks = blockr.core::board_blocks(brd),
              links = blockr.core::board_links(brd),
              stacks = blockr.core::board_stacks(brd)
            )

            if (!identical(sig, shape_sig)) {
              shape_sig <<- sig
              board_shape(brd)
            }
          }
        )

        sections_calc <- reactive(
          {
            # Gate FIRST: the projection is the outline's one expensive
            # step (O(n^2) drag geometry). The same gate rides on the two
            # store observers above, which are what actually read the board
            # -- between them, a present-but-unopened outline costs nothing.
            # The next time the panel is shown, panel_visible flips, the
            # stores fill and the projection runs.
            req(panel_visible())

            exprs <- exprs_store()
            brd <- board_shape()

            req(!is.null(brd), length(exprs) > 0L)

            # During startup (deferred construction) the block servers and
            # the committed board can disagree for a flush; retry on the
            # next one instead of surfacing a transient error.
            tryCatch(
              outline_sections(
                exprs,
                brd,
                rv_ann(),
                rv_order(),
                rv_stack_ann(),
                geometry_cache = geometry_cache
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

        spin_txt <- reactive(
          export_spin(sections(), rv_stack_level(), rv_block_level())
        )
        # The Document view shows the qmd that WILL be rendered, slide breaks
        # included, so it reads the format picker sitting right beside it
        # rather than taking a format at download time. Slides are a document
        # shape, not a render flag.
        qmd_txt <- reactive(
          export_qmd(
            sections(), rv_title(), rv_stack_level(), rv_block_level(),
            slides = slide_format(coal(input$code_render_format, "html"))
          )
        )

        # Gear -> Headings. Value-guarded so a no-op update
        # (e.g. a re-render restoring the same choice) does not churn.
        #
        # ignoreInit is load-bearing, and the reason is not obvious. The two
        # selectInputs below declare a HARDCODED `selected` ("#" and
        # "caption") -- outline_settings_band() takes only `ns`, so it cannot
        # see the constructor arguments. At session start Shiny delivers that
        # hardcoded default as the input value; without ignoreInit these
        # observers fire immediately and overwrite rv_stack_level /
        # rv_block_level, silently discarding whatever the caller passed to
        # new_outline_extension() -- or whatever a saved board restored. The
        # arguments were accepted, stored, and then thrown away one flush
        # later. The seed below then pushes the real value back INTO the
        # control, so the gear displays what is actually in effect; that
        # update re-delivers the input, this time not as init, and the
        # identical-guard makes it a no-op.
        observeEvent(input$otl_stack_level, {
          if (!identical(input$otl_stack_level, rv_stack_level())) {
            rv_stack_level(input$otl_stack_level)
          }
        }, ignoreInit = TRUE)
        observeEvent(input$otl_block_level, {
          if (!identical(input$otl_block_level, rv_block_level())) {
            rv_block_level(input$otl_block_level)
          }
        }, ignoreInit = TRUE)

        # Seed both selects from state, so the gear shows the level that is
        # actually in effect rather than the markup's placeholder. Sent
        # directly rather than from session$onFlushed(): Shiny queues input
        # messages and delivers them on the next flush either way, and the
        # direct call is observable under testServer(), where MockShinySession
        # never runs onFlushed callbacks at all.
        updateSelectInput(
          session, "otl_stack_level", selected = isolate(rv_stack_level())
        )
        updateSelectInput(
          session, "otl_block_level", selected = isolate(rv_block_level())
        )

        # Which blocks are ACTIVE in the outline: the ones whose panel is a
        # member of the active dock view. Activation is expressed as view
        # membership -- the same thing a dag-node click commits -- so the
        # dock stays the single writer of core's `required` channel and an
        # activated block is evaluated exactly like any other block of the
        # view (upstream closure riding along). Membership, NOT the front
        # tab: front flips on every tab click inside a view, which must not
        # churn the outline's skeleton; a member parked behind a tab keeps
        # its last-reported code from expr_cache. NULL means "no gating"
        # (not a dock board, or views unreadable) and degrades to the old
        # show-everything behaviour.
        view_active_calc <- reactive(
          {
            brd <- board$board

            views <- tryCatch(
              blockr.dock::board_views(brd),
              error = function(e) NULL
            )

            if (is.null(views) || !length(views)) {
              return(NULL)
            }

            view <- tryCatch(
              blockr.dock::active_view(views),
              error = function(e) NULL
            )

            if (is.null(view) || !view %in% names(views)) {
              return(NULL)
            }

            members <- blockr.dock::view_members(views[[view]])
            ids <- blockr.core::board_block_ids(brd)

            pids <- chr_ply(
              ids,
              function(i) as.character(blockr.dock::as_block_panel_id(i))
            )

            ids[pids %in% members]
          }
        )

        # Identical-skip in front of the two board reads the content
        # observer below makes. Both are keyed on `board$board`, so ANY
        # board update invalidates them -- and most board updates say
        # nothing about the document: clicking a dock tab commits a views
        # delta (front tab), which changes neither the active membership
        # nor the links, yet re-ran the whole content pass (display
        # geometry, catalogue, and the full code map) to produce byte-
        # identical output. A plain reactive() cannot stop that; it
        # re-emits whenever its dependency invalidates, regardless of
        # whether its value moved. Wrapping each in a reactiveVal that is
        # only written on a real change gives the observer a dependency
        # that stays put across those gestures.
        #
        # The ids are wrapped in a list because NULL is a MEANINGFUL value
        # here -- "no view gating" -- and a reactiveVal cannot distinguish
        # a NULL payload from an unset one.
        view_active_store <- reactiveVal(list(ids = NULL))

        observe(
          {
            new <- list(ids = view_active_calc())
            if (!identical(new, isolate(view_active_store()))) {
              view_active_store(new)
            }
          }
        )

        view_active_ids <- reactive(view_active_store()$ids)

        links_store <- reactiveVal(NULL)

        observe(
          {
            lnks <- blockr.core::board_links(board$board)
            if (!identical(lnks, isolate(links_store()))) {
              links_store(lnks)
            }
          }
        )

        # Memoises the highlighted code markup per block, so a redraw that
        # changed no code costs nothing (see sect_code_html). Session-owned
        # like the two geometry caches.
        code_html_cache <- new.env(parent = emptyenv())

        # Split the projection into the structural skeleton and the
        # per-block code markup, each with its own identical-skip store.
        # Editing a block value (a row count, say) changes only the code,
        # so the skeleton store stays put, renderUI does not fire, and the
        # observer below pushes just the changed chunks. Structural edits
        # (drag, chapter changes, add/remove) move the skeleton and redraw
        # wholesale, which is correct and rare.
        skel_store <- reactiveVal(NULL)
        code_store <- reactiveVal(NULL)
        catalog_store <- reactiveVal(NULL)

        # Memoises the display-subset drag geometry (see display_sections),
        # the same trade as geometry_cache above.
        display_geometry_cache <- new.env(parent = emptyenv())

        observe(
          {
            full <- sections()

            # NULL means the links store has not been written yet (an empty
            # board still has a zero-row links table, never NULL), so this
            # only skips the flush before its observer above has run.
            lnks <- links_store()
            req(!is.null(lnks))

            show_all <- isTRUE(input$otl_show_all)

            # The outline LISTS the document, one row per chunk the qmd
            # gets: the reported blocks plus the ancestors they need. That
            # is the export closure (`exported` in outline_sections), and
            # the mapping is exact -- a reported block is a chunk with
            # output, an ancestor is an `#| include: false` chunk, so it
            # shows as a row with the toggle OFF. Only blocks the document
            # does not contain at all (branches no reported block depends
            # on) stay out of the list and live in the include picker
            # below it. Show-all restores the full board overview.
            listed <- if (show_all) full$ids else full$ids[full$exported]

            skel <- display_sections(
              full, listed,
              lnks = lnks,
              cache = display_geometry_cache
            )
            skel$code <- NULL

            # The search catalogue: EVERY board block, listed ones first, in
            # document order. The search box is one control over the whole
            # board -- a listed block is a "go to", an unlisted one an "add"
            # -- so it needs the pool and the document in one payload. It is
            # pushed to the client (below) rather than rendered, because the
            # control is static UI: re-rendering it would drop focus
            # mid-query.
            catalog_store(outline_catalog(full, listed))

            # Dormant-by-default: a block outside the active view condenses
            # to title + description and carries no code cell. `active`
            # rides in the SKELETON so activation (the views-delta a row
            # click commits) redraws exactly the rows it changes -- which
            # also means the identical-skip for pure views-deltas no longer
            # holds by construction, the identical() below decides.
            # `gated` gates the hide affordance: without view gating (or
            # with the show-all override) there is nothing to hide from.
            shown <- view_active_ids()
            gated <- !show_all && !is.null(shown)

            skel$active <- if (gated) {
              skel$ids %in% shown
            } else {
              rep(TRUE, length(skel$ids))
            }

            # An excluded row is collapsed on top of that: it carries a
            # chunk the document runs but does not show, so its code is
            # behind the badge twisty (rv_open_code) rather than filling
            # the column. Reported rows are the document's content and
            # always render.
            skel$active <- unname(
              skel$active &
                (skel$report | skel$ids %in% rv_open_code())
            )

            skel$gated <- gated
            # Keep `pending` IN the skeleton so a block flipping pending ->
            # code redraws the outline. That redraw is how a deferred board
            # (background_construction_delay = Inf) ever shows real code: the
            # incremental code push cannot carry it, because on a deferred
            # board a block's push arrives before renderUI has inserted its
            # cell (so applyCode finds no node), and the outline output does
            # not emit the shiny:value the re-apply hooks -- the dock mounts
            # the extension in an offcanvas and moves its DOM, skipping the
            # event -- so the cached markup would sit unapplied forever.
            # Re-rendering paints the current code straight from code_store.
            # A plain code EDIT does not touch `pending`, so the skeleton is
            # unchanged and the push still handles it without a redraw.

            if (!identical(skel, isolate(skel_store()))) {
              skel_store(skel)
            }

            # Only listed AND active blocks get code markup: dormant and
            # unlisted rows render no code cell, so highlighting their
            # chunks would be pure waste -- on a large board that waste is
            # most of the content cost (the map re-runs on every code
            # change). Activation redraws the skeleton, and that same
            # flush recomputes this map with the new block included, so
            # renderUI always finds its cell.
            codes <- outline_code_map(
              full, skel$ids[skel$active], cache = code_html_cache
            )

            if (!identical(codes, isolate(code_store()))) {
              code_store(codes)
            }
          }
        )

        # The search menu is filled client-side from this payload. Pushed
        # whole rather than diffed: it is one small array (id, name, icon
        # key, chapter, one description line per block) plus the icon table
        # those keys point into, and it only moves when the projection does.
        observe(
          {
            cat <- catalog_store()
            req(!is.null(cat))

            session$sendCustomMessage(
              "blockr-outline-catalog",
              list(items = cat$items, icons = cat$icons)
            )
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
                )
              )
            )
          }
        )

        # Output mode: render each activated block's exhibit inline. Gated
        # on the toggle so the (heavy) evaluation only runs when the view is
        # actually asked for; in Code mode this reactive never fires. It
        # depends on sections() reactively, so editing a block value while
        # in Output mode refreshes the preview -- a full renderUI redraw,
        # which is fine (the lightweight incremental push is Code-mode only,
        # and exhibits are heavier and change less often anyway).
        output_map <- reactive({
          req(identical(coal(input$otl_body, "code"), "output"))
          # Board ids, not just the projected ones: a block that reports no
          # expression is dropped from the projection, and only the board
          # still knows its id (see eval_env).
          outline_output_map(
            sections(),
            blockr.core::board_block_ids(board$board),
            blockr.core::board_links(board$board)
          )
        })

        # The report title heads the column, above the search. Kept out of
        # outline_out so it is not torn down on every body re-render.
        output$otl_title <- renderUI(outline_title_row(rv_title()))

        # Row-level incremental render. A skeleton change used to redraw the
        # whole document through renderUI, which reads as a blank-then-
        # repaint flash and throws away scroll and hover state -- and the
        # commonest skeleton change by far is a row click, which activates
        # ONE block. So: compose the sections here, and only hand renderUI a
        # new value when the change is one a row swap cannot express (see
        # outline_layout_key). Everything else travels as a push.
        #
        # Code is isolated, exactly as the renderUI below used to isolate it:
        # a code-only change must not come through here, the code push
        # already handles it -- but a row that IS pushed has to carry the
        # current code, because the swap replaces the cell it lives in.
        # The current sections, held in a PLAIN variable rather than a
        # reactiveVal. renderUI has to paint the latest state whenever it
        # runs -- and it runs for reasons of its own, notably the code_view
        # switcher coming back to Outline. A reactiveVal written only on
        # layout changes would hand it a snapshot from before every row that
        # has been pushed since, silently undoing them. So: content in a
        # plain variable, and a separate token as the only reactive reason
        # to re-render.
        render_sects <- NULL
        render_token <- reactiveVal(0L)
        rows_pushed <- reactiveVal(NULL)
        layout_last <- NULL

        bump_render <- function() {
          render_token(isolate(render_token()) + 1L)
        }

        observe(
          {
            skel <- skel_store()
            req(!is.null(skel))

            edit <- editing()

            sects <- skel
            sects$body_mode <- coal(input$otl_body, "code")
            sects$code_html <- isolate(code_store())

            render_sects <<- list(sects = sects, editing = edit)

            # Output mode stays wholly on the full-render path: the cells
            # hold evaluated exhibits, not the code markup composed here, so
            # a row built from it would be wrong to push. Exhibits are also
            # heavy and change rarely, which is what the mode is for.
            if (identical(sects$body_mode, "output")) {
              layout_last <<- NULL
              rows_pushed(NULL)
              bump_render()
              return()
            }

            key <- outline_layout_key(sects, edit)
            rows <- outline_row_map(sects, session$ns, edit)

            prev <- isolate(rows_pushed())
            rows_pushed(rows)

            if (!identical(key, layout_last)) {
              layout_last <<- key
              # renderUI paints from exactly these sections, so `rows` above
              # is an honest record of what ends up on screen and the next
              # diff has something true to compare against.
              bump_render()
              return()
            }

            # First pass after a full render carries no baseline yet.
            req(!is.null(prev))

            changed <- Filter(
              function(n) !identical(rows[[n]], prev[[n]]),
              names(rows)
            )

            if (!length(changed)) {
              return()
            }

            session$sendCustomMessage(
              "blockr-outline-rows",
              list(
                items = lapply(
                  changed,
                  function(n) list(id = n, html = rows[[n]])
                )
              )
            )
          }
        )

        output$outline_out <- renderUI(
          {
            view <- coal(input$code_view, "outline")

            if (identical(view, "outline")) {

              # Reactive on the token only; the content itself is read
              # plainly, so this always paints what is current rather than
              # what was current when the layout last moved.
              render_token()

              rs <- render_sects
              req(!is.null(rs))

              sects <- rs$sects

              if (identical(coal(sects$body_mode, "code"), "output")) {
                sects$code_html <- output_map()
              }

              if (!length(sects$ids)) {
                return(
                  div(
                    class = "blockr-otl-emptydoc",
                    "Nothing in the report yet: search for a block above."
                  )
                )
              }

              # rs$editing, not editing(): the sections were composed with
              # it, and reading the reactive here would make a render fire
              # on an edit that the observer above has already accounted for.
              return(outline_tags(sects, session$ns, rs$editing))
            }

            txt <- if (identical(view, "qmd")) qmd_txt() else spin_txt()

            hl <- switch(
              view,
              script = highlight_r_code(txt),
              qmd = highlight_qmd_code(txt),
              NULL
            )

            body <- if (is.null(hl)) {
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

            # One frame, not two: the header replaces the nested card and
            # names the file, which neither view stated before. Copy moves
            # onto the code it copies; the toolbar's button hides itself
            # while this header is on screen (see outline_js).
            div(
              class = "blockr-otl-fileblock",
              div(
                class = "blockr-otl-filehead",
                span(
                  class = "blockr-otl-filename",
                  if (identical(view, "qmd")) "report.qmd" else "report.R"
                ),
                tags$button(
                  type = "button",
                  class = "blockr-otl-headbtn",
                  title = "Copy to clipboard",
                  onclick = sprintf(
                    paste0(
                      "navigator.clipboard.writeText(",
                      "document.getElementById('%s').innerText);"
                    ),
                    session$ns("code_pre")
                  ),
                  HTML("&#10697;"),
                  "Copy"
                )
              ),
              body
            )
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

        # The include picker: pulling a block out of the pool flips its
        # report flag, which lists it (and, via the exporters, puts it in
        # the document). The control re-renders with the next skeleton, so
        # no reset round trip is needed.
        observeEvent(
          input$otl_include,
          {
            blk_id <- input$otl_include
            req(is.character(blk_id), nzchar(blk_id))
            req(blk_id %in% blockr.core::board_block_ids(board$board))

            ann <- rv_ann()

            if (isTRUE(ann_report(ann, blk_id))) {
              return()
            }

            entry <- coal(ann[[blk_id]], list())
            entry$report <- TRUE
            ann[[blk_id]] <- entry
            rv_ann(ann)
          },
          ignoreInit = TRUE
        )

        # The badge twisty on an excluded row: open or close its code cell.
        # Presentation only -- no board update, no report flag, no views
        # delta. Clicking the ROW still opens the block's panel (and reveals
        # the code with it, see outline_open below).
        observeEvent(
          input$otl_show_code,
          {
            blk_id <- input$otl_show_code$id
            req(is.character(blk_id))

            cur <- rv_open_code()

            rv_open_code(
              if (blk_id %in% cur) setdiff(cur, blk_id) else c(cur, blk_id)
            )
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

        # Rename the report title in place (double-click the top heading).
        # The title is outline state (see the state list returned below), so
        # this is all that is needed for it to persist.
        observeEvent(
          input$outline_rename_title,
          {
            nm <- input$outline_rename_title$name
            req(is.character(nm), nzchar(trimws(nm)))
            if (!identical(rv_title(), nm)) rv_title(nm)
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

        # Board-wide include/exclude from the settings band: set the report
        # flag on every block at once (the per-chapter version is
        # input$outline_chapter below).
        observeEvent(
          input$outline_bulk,
          {
            act <- input$outline_bulk$act
            req(is.character(act), act %in% c("include", "exclude"))

            want <- identical(act, "include")
            ann <- rv_ann()
            for (bid in sections()$ids) {
              entry <- coal(ann[[bid]], list())
              entry$report <- want
              ann[[bid]] <- entry
            }
            rv_ann(ann)
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

            # "Show me this block": on an excluded row that also means
            # opening its code, so the click does something visible in the
            # outline itself -- and on a board with no views (nothing to
            # reveal) it is the whole of the gesture.
            if (!blk_id %in% rv_open_code()) {
              rv_open_code(c(rv_open_code(), blk_id))
            }

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

        # The inverse: take the block's panel OUT of the active view, which
        # returns its outline row to the condensed dormant state. The block
        # itself is untouched (and stays constructed -- evaluation idles
        # through the dock's own view bookkeeping, the outline keeps its
        # last code from expr_cache).
        observeEvent(
          input$outline_hide,
          {
            blk_id <- input$outline_hide$id
            req(is.character(blk_id))

            views <- blockr.dock::board_views(board$board)
            view <- blockr.dock::active_view(views)

            if (is.null(view)) {
              return()
            }

            pid <- as.character(blockr.dock::as_block_panel_id(blk_id))

            if (!pid %in% blockr.dock::view_members(views[[view]])) {
              return()
            }

            update(list(views = list(mod = setNames(list(list(rm = pid)), view))))
          }
        )

        # The action half of the two-stage download (see the UI). On a
        # deferred board (background_construction_delay = Inf) a block no
        # view has shown is never constructed, reports no expression, and
        # sits in the document as a pending placeholder. The click demands
        # exactly the export closure through core's visibility channel --
        # `visibility$required[[id]](TRUE)`, the same mechanism core's own
        # generate_code plugin uses -- which constructs the pending blocks
        # and (their ancestors riding along via core's upstream closure)
        # evaluates the reported branches. Once nothing exported is
        # pending, the hidden download link is clicked from JS and the
        # render runs against complete code. Branches outside the export
        # closure are never constructed, mirroring the app's lazy views.
        #
        # The Output preview shares this machinery (see the otl_body
        # observer below): same closure, same demand, no download at the
        # end.
        #
        # The demanded slots are snapshotted and RESTORED once the demand
        # is served. The dock overloads the `required` axis as its card build
        # ledger (non-NA = card built, see blockr.dock::built_cards), so a
        # TRUE left on a block whose card was never built would make the
        # first visit to its view skip the card build -- a blank panel.
        # Constructed-server-with-unbuilt-card is a state the dock already
        # supports (finite-delay background construction produces it), so
        # putting the prior value back is safe: core keeps the constructed
        # block, the dock keeps an honest ledger.
        # What the demand is FOR: NULL (idle), "download" (fire the file
        # download once the closure reports) or "output" (the Output
        # preview asked for the closure; nothing to fire, the output map
        # recomputes by itself as the expressions arrive).
        awaiting <- reactiveVal(NULL)
        wait_note <- reactiveVal(NULL)
        demanded <- reactiveVal(list())

        # Demand construction of `pending` through core's visibility
        # channel, snapshotting each slot's prior value for the restore.
        # FALSE when there is no channel to demand through (an old
        # container, or none at all).
        demand_blocks <- function(pending) {

          slots <- if (!is.null(visibility)) visibility$required

          if (is.null(slots)) {
            return(FALSE)
          }

          snap <- demanded()

          for (blk_id in pending) {
            slot <- slots[[blk_id]]
            if (is.function(slot)) {
              # A re-demand while already waiting must keep the ORIGINAL
              # prior value, not the TRUE of the first demand.
              if (!blk_id %in% names(snap)) {
                snap[[blk_id]] <- isolate(slot())
              }
              slot(TRUE)
            }
          }

          demanded(snap)

          TRUE
        }

        drop_wait_note <- function() {
          note <- wait_note()
          if (!is.null(note)) {
            removeNotification(note)
            wait_note(NULL)
          }
        }

        restore_demanded <- function() {
          snap <- demanded()

          for (blk_id in names(snap)) {
            slot <- visibility$required[[blk_id]]
            if (is.function(slot)) {
              slot(snap[[blk_id]])
            }
          }

          demanded(list())
        }

        pending_exported <- function(sects) {
          sects$ids[sects$exported & sects$pending]
        }

        fire_download <- function() {
          session$sendCustomMessage(
            "blockr-outline-download",
            list(id = session$ns("code_render"))
          )
        }

        observeEvent(
          input$code_render_go,
          {
            sects <- tryCatch(sections(), error = function(e) NULL)

            if (is.null(sects)) {
              showNotification(
                "The document is not ready yet; try again in a moment.",
                type = "warning"
              )
              return()
            }

            pending <- pending_exported(sects)

            if (!length(pending)) {
              fire_download()
              return()
            }

            if (!demand_blocks(pending)) {
              # No channel (an old container, or none at all): blocks can
              # only construct through their views, so say so instead of
              # waiting on something that cannot happen.
              showNotification(
                paste(
                  "Some report blocks are not initialized yet. Open their",
                  "views to initialize them, then download again."
                ),
                type = "warning",
                duration = 10
              )
              return()
            }

            # "download" unconditionally: a click while an Output-preview
            # demand is in flight upgrades it -- same closure, and the
            # user now wants the file.
            awaiting("download")
            drop_wait_note()
            wait_note(
              showNotification(
                sprintf(
                  paste(
                    "Generating R code for %d block%s\u2026 the download",
                    "starts when it is ready."
                  ),
                  length(pending),
                  if (length(pending) == 1L) "" else "s"
                ),
                duration = NULL,
                closeButton = FALSE
              )
            )
          }
        )

        # The Output preview's pre-step, the same demand the download runs:
        # on a deferred board the report closure may contain blocks no view
        # has constructed, which the output map would skip (and their
        # dependents would fail to evaluate). Flipping to Output demands
        # exactly those; the preview fills in as their expressions report,
        # and the demand is withdrawn once the closure is complete. The
        # side effect is the point: the preview IS the pre-render check.
        observeEvent(
          input$otl_body,
          {
            if (!identical(input$otl_body, "output")) {

              # Flipping back to Code cancels a preview-only wait (a
              # pending download keeps going).
              if (identical(awaiting(), "output")) {
                awaiting(NULL)
                restore_demanded()
                drop_wait_note()
              }

              return()
            }

            sects <- tryCatch(sections(), error = function(e) NULL)

            if (is.null(sects)) {
              return()
            }

            pending <- pending_exported(sects)

            if (!length(pending)) {
              return()
            }

            if (!demand_blocks(pending)) {
              showNotification(
                paste(
                  "Some report blocks are not initialized yet. Open their",
                  "views to initialize them and the preview fills in."
                ),
                type = "warning",
                duration = 10
              )
              return()
            }

            if (is.null(awaiting())) {
              awaiting("output")
              wait_note(
                showNotification(
                  sprintf(
                    "Evaluating %d report block%s for the preview\u2026",
                    length(pending),
                    if (length(pending) == 1L) "" else "s"
                  ),
                  duration = NULL,
                  closeButton = FALSE
                )
              )
            }
          }
        )

        observe(
          {
            req(!is.null(awaiting()))

            if (length(pending_exported(sections()))) {
              return()
            }

            why <- awaiting()
            awaiting(NULL)
            restore_demanded()
            drop_wait_note()

            if (identical(why, "download")) {
              fire_download()
            }
          }
        )

        output$code_render <- downloadHandler(
          filename = function() {
            paste0(
              "board-report-",
              format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
              ".",
              report_ext(input$code_render_format)
            )
          },
          # Guarded HERE rather than inside render_report(): this is the one
          # place every format passes through, and pptx short-circuits to
          # officer long before render_report's own error handling. A guard
          # per renderer would have to be written three times and would still
          # miss whatever is added fourth.
          content = function(file) {
            with_render_guard(
              render_report(
                qmd_txt(),
                spin_txt(),
                input$code_render_format,
                file,
                rv_title(),
                template = effective_template(),
                sects = sections()
              )
            )
          }
        )

        # The download link is display:none (the visible button is the
        # action half, see above), and Shiny suspends hidden outputs --
        # which for a downloadHandler means the link's href is never
        # populated and a click navigates to the bare page URL instead of
        # the handler. Keep it live.
        outputOptions(output, "code_render", suspendWhenHidden = FALSE)

        list(
          state = list(
            annotations = rv_ann,
            block_order = rv_order,
            title = rv_title,
            stack_annotations = rv_stack_ann,
            stack_title_level = rv_stack_level,
            block_title_level = rv_block_level
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
        report = isTRUE(coal(unlist(entry[["report"]]), FALSE))
      )
    }
  )
}
