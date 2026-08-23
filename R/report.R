#' Report builder extension
#'
#' A dock extension that turns a board into a quarto document: an ordered
#' list of items, each either a block or a piece of markdown. Pick blocks,
#' write text between them, decide per block whether the document shows its
#' code, its output, both or neither, and download the result -- rendered
#' HTML, or the quarto / R-script source itself.
#'
#' The document model is one list. A block item carries two independent
#' switches -- `code` and `output`, quarto's `echo` / `output` pair -- with
#' output-only as the default when a block is added. A text item is a
#' markdown paragraph that stands on its own: above a block, between two
#' exhibits, closing a section. There is no separate intro; the first text
#' item is the intro. Blocks nobody added are still evaluated when a picked
#' block depends on them, and ride along as `include: false` chunks.
#'
#' Two views sit beside the builder: **report.R** (a knitr spin script) and
#' **report.qmd** (the quarto document). Both show the same text their
#' download writes, one chunk per block, each chunk carrying its block's
#' registry icon in the gutter -- click it to open that block's panel. The
#' emitted code is canonical R: dplyr stays dplyr, a ggplot prints itself, a
#' data frame renders through `df-print: kable`. Blocks whose only faithful
#' form is a styled exhibit (display tables, canvas charts) keep their
#' renderer calls; everything else is plain.
#'
#' Like the slides extension, nothing is evaluated until a download asks for
#' it: the builder reads names and icons off the board, never expressions.
#'
#' @param items Ordered list of document items. Each element is either a
#'   block item -- `list(block = "<id>")`, optionally with `code` /
#'   `output` flags (defaults `FALSE` / `TRUE`), `fig_width` / `fig_height`
#'   overrides and `full_width` -- or a text item, `list(text = "<md>")`.
#'   List order is document order (block order snapped to a valid
#'   evaluation order; text travels with the block that follows it).
#' @param title Report title: the document title, and the download's
#'   filename stem.
#' @param settings Named list of document-wide settings; missing entries
#'   take defaults, unknown names error. `block_titles` (`"headings"`,
#'   `"captions"` or `"none"`), `fig_width` / `fig_height` (default figure
#'   size in inches), `toc`, `number_sections`, `code_fold` and `warnings`
#'   (whether warnings/messages appear in the document; default `FALSE`).
#' @param blocks LEGACY. Character vector of block ids; converted to block
#'   items (output on, code off) ahead of `items`.
#' @param annotations LEGACY. Named list keyed by block id with a
#'   `description` entry; each becomes a text item placed directly before
#'   its block's item.
#' @param intro LEGACY. Markdown intro; becomes the leading text item.
#' @param format LEGACY, ignored. The download format is picked in the
#'   panel and is no longer state.
#' @param template LEGACY, ignored.
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
#'       new_report_extension(
#'         items = list(
#'           list(text = "The first rows, as a sanity check."),
#'           list(block = "audit")
#'         ),
#'         title = "Iris pilot"
#'       )
#'     )
#'   )
#'
#'   serve(board)
#' }
#'
#' @export
new_report_extension <- function(items = list(),
                                 title = "Report",
                                 settings = list(),
                                 blocks = character(),
                                 annotations = list(),
                                 intro = "",
                                 format = "html",
                                 template = "",
                                 ...) {

  items <- sanitize_items(
    if (length(items)) items else legacy_report_items(blocks, annotations, intro)
  )

  settings <- sanitize_settings(settings)

  blockr.dock::new_dock_extension(
    report_ext_srv(items, title, settings),
    report_ext_ui,
    name = "Report",
    description = paste(
      "Report builder: blocks and markdown in one ordered list, per-block",
      "code/output switches, and a quarto document to take away."
    ),
    class = "report_extension",
    ...
  )
}

# What a report can be downloaded as: the rendered document, then the two
# source forms -- the source IS a deliverable here: a runnable, readable
# document in canonical R is half the point of this extension. No deck:
# report makes documents, the slides extension makes decks.
# The RENDER targets, and only those. `qmd` and `r` used to sit in this
# list, which put the two code views in the toolbar a second time under
# different words -- and lied besides: picking "Quarto source" handed over
# a file whose own YAML said `format: html`, because report_yaml() wrote
# that regardless. The source is not a format, it is the view; it is
# downloaded from the view's own header now (report_code_ui()), and what
# stays here is the one question this control was ever asking: which
# document does Render produce.
#
# NOT report_formats(): render.R already owns that name for the retired
# outline extension's html/revealjs/pptx list. Two definitions in one
# package is one definition -- the later collation wins silently -- and
# this list is the report's, not that one's.
#
# The VOCABULARY. Every format the report knows how to describe, which is
# not the same as every format a given machine can produce -- see
# report_render_formats(). Validation reads this one, deliberately: a
# board saved against pdf must still LOAD on a deployment without TeX,
# and be told no when it renders, rather than refuse to open at all.
report_known_formats <- function() {
  c(
    "HTML" = "html",
    "Word" = "docx",
    "PDF" = "pdf",
    "Typst PDF" = "typst"
  )
}

# The vocabulary, less what this machine cannot keep. Offering a format is
# a promise -- render.R argues that at length, having once offered pdf off
# a PATH probe for a toolchain quarto does not use, which reached users as
# a download that simply failed. So the question is put to the renderer
# instead of to the PATH: format_renderable() renders a three-line
# document and looks for the file.
#
# Once per process. The probe costs about a render apiece and the answer
# cannot change under a running R session -- installing TeX mid-session and
# expecting the select to notice is not a case worth a re-probe, and a
# control that changes its mind while open is worse than one that does not.
report_render_formats <- memoise0(function() {
  log_render_capability()
  known <- report_known_formats()
  known[vapply(known, format_renderable, logical(1L), USE.NAMES = FALSE)]
})

# The button says what the gear decided, so it needs the label back. Reads
# the vocabulary, not the offered set: a format that is set but cannot be
# rendered here still has a name, and "Render PDF" is the honest label for
# it right up until the render says no.
report_format_name <- function(fmt) {
  fmts <- report_known_formats()
  coal(names(fmts)[match(coal(fmt, "html"), fmts)], "HTML")
}

# typst renders THROUGH typst TO a pdf; the file the reader receives is a
# pdf and must be named one.
report_dl_ext <- function(fmt) {
  switch(fmt, typst = "pdf", fmt)
}

# ---- items -------------------------------------------------------------

item_is_block <- function(x) {
  is.list(x) && is.character(x$block) && length(x$block) == 1L &&
    nzchar(x$block)
}

item_is_text <- function(x) {
  is.list(x) && is.character(x$text) && length(x$text) == 1L
}

item_block_ids <- function(items) {
  chr_ply(Filter(item_is_block, items), function(x) x$block)
}

new_block_item <- function(id) {
  list(block = id, code = FALSE, output = TRUE,
       fig_width = NULL, fig_height = NULL, full_width = FALSE)
}

# One pass over a ctor's (or a restore payload's) item list: coerce the
# flags, drop anything that is neither a block nor a text item, and keep
# only the FIRST item for a block id -- a block appears at most once.
sanitize_items <- function(items) {

  if (!is.list(items)) {
    return(list())
  }

  seen <- character()
  out <- list()

  for (x in items) {
    if (item_is_block(x)) {
      if (x$block %in% seen) {
        next
      }
      seen <- c(seen, x$block)
      it <- new_block_item(x$block)
      it$code <- isTRUE(x$code)
      it$output <- !isFALSE(x$output)
      if (is.numeric(x$fig_width) && length(x$fig_width) == 1L) {
        it$fig_width <- as.numeric(x$fig_width)
      }
      if (is.numeric(x$fig_height) && length(x$fig_height) == 1L) {
        it$fig_height <- as.numeric(x$fig_height)
      }
      it$full_width <- isTRUE(x$full_width)
      out <- c(out, list(it))
    } else if (item_is_text(x)) {
      out <- c(out, list(list(text = x$text)))
    }
  }

  out
}

# A v1 payload (blocks + annotations + intro) as an item list: the intro
# leads, each pick becomes an output-only block item, and its annotation --
# v1's note above the block -- becomes a text item directly before it.
legacy_report_items <- function(blocks, annotations, intro) {

  blocks <- as.character(unlist(blocks))

  items <- if (is.character(intro) && length(intro) && nzchar(intro[[1L]])) {
    list(list(text = intro[[1L]]))
  } else {
    list()
  }

  for (id in blocks) {
    desc <- ann_description(annotations, id)
    if (nzchar(desc)) {
      items <- c(items, list(list(text = desc)))
    }
    items <- c(items, list(new_block_item(id)))
  }

  items
}

# ---- settings ----------------------------------------------------------

report_settings_default <- function() {
  list(
    # First, because it gates: the format decides which of the keys below
    # quarto will even read.
    format = "html",
    embed_resources = TRUE,
    block_titles = "headings",
    fig_width = 8,
    fig_height = 4.5,
    toc = FALSE,
    number_sections = FALSE,
    code_fold = FALSE,
    warnings = FALSE
  )
}

# Unknown names ERROR (the no-runtime-feature-guards rule: a misspelled
# setting must fail loudly, not silently mean the default); missing names
# take defaults; values are coerced to their field's type.
sanitize_settings <- function(settings) {

  def <- report_settings_default()

  if (!is.list(settings)) {
    settings <- list()
  }

  unknown <- setdiff(names(settings), names(def))
  if (length(unknown)) {
    stop(
      "Unknown report setting(s): ", paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }

  out <- def

  if (!is.null(settings$format)) {
    val <- as.character(settings$format)[[1L]]
    if (!val %in% report_known_formats()) {
      stop(
        "`format` must be one of ",
        paste0("\"", report_known_formats(), "\"", collapse = ", "), ".",
        call. = FALSE
      )
    }
    out$format <- val
  }

  if (!is.null(settings$block_titles)) {
    val <- as.character(settings$block_titles)[[1L]]
    if (!val %in% c("headings", "captions", "none")) {
      stop(
        "`block_titles` must be \"headings\", \"captions\" or \"none\".",
        call. = FALSE
      )
    }
    out$block_titles <- val
  }

  for (f in c("fig_width", "fig_height")) {
    if (!is.null(settings[[f]])) {
      val <- suppressWarnings(as.numeric(settings[[f]])[[1L]])
      if (is.finite(val) && val > 0) {
        out[[f]] <- val
      }
    }
  }

  for (f in c("embed_resources", "toc", "number_sections", "code_fold",
              "warnings")) {
    if (!is.null(settings[[f]])) {
      out[[f]] <- isTRUE(settings[[f]])
    }
  }

  out
}

# The block-title mode as the emitters' `block_level`: a document heading,
# the exhibit caption, or nothing (any non-heading, non-"caption" value
# yields neither a heading nor a caption in the emitters).
report_block_level <- function(settings) {
  switch(
    settings$block_titles,
    headings = "##",
    captions = "caption",
    "none"
  )
}

# Per-block chunk flags from the item list, keyed by id -- what the
# emitters' `flags` argument wants.
report_flags <- function(items) {

  out <- list()

  for (x in items) {
    if (item_is_block(x)) {
      out[[x$block]] <- x[c("code", "output", "fig_width", "fig_height",
                            "full_width")]
    }
  }

  out
}

# The report projection IS the deck projection: added blocks drive the
# export closure (they are evaluated even when both switches are off --
# being in the document is a claim on the data), ancestors ride along
# hidden, item order is the ordering preference. Prose lives in the item
# list, not in annotations, so the descriptions field stays empty.
report_sections <- function(expressions, board, picked) {
  slide_sections(expressions, board, picked, list())
}

report_ext_srv <- function(items, title, settings) {

  function(id, board, update, session, parent, actions = NULL,
           visibility = NULL, ...) {
    moduleServer(
      id,
      function(input, output, session) {

        rv_items <- reactiveVal(sanitize_items(items))
        rv_title <- reactiveVal(
          if (is.character(title) && length(title)) title[[1L]] else "Report"
        )
        rv_settings <- reactiveVal(sanitize_settings(settings))

        # Which item's text editor is open (list index, or NULL). UI state,
        # not persisted -- an editor does not survive a board save.
        editing <- reactiveVal(NULL)

        # The index of a JUST-inserted text item whose editor has not been
        # saved yet: cancelling that one removes the item again, so "+ then
        # click elsewhere" does not litter empty rows. Cancelling an item
        # that existed before keeps it -- deleting is the x's job.
        fresh <- reactiveVal(NULL)

        # ---- the board, as names ------------------------------------
        #
        # Names, kinds and icons are properties of the block OBJECT,
        # readable off the board whether or not the block was ever
        # constructed -- the builder view costs nothing on a deferred
        # board. (The full rationale is slides.R, which this server is a
        # clone of; comments here note only what the report adds.)
        block_meta <- reactive(
          {
            blks <- blockr.core::board_blocks(board$board)

            lapply(
              setNames(nm = names(blks)),
              function(i) {
                list(
                  name = blockr.core::block_name(blks[[i]]),
                  kind = block_exhibit_kind(blks[[i]]),
                  icon = block_icon_html(blks[[i]])
                )
              }
            )
          }
        )

        # Block items follow the board's block lifecycle: the item of a
        # removed block goes with it. Text items stay -- an orphaned
        # paragraph is visible in the list and the author deletes it, a
        # silently vanished one is a lost edit.
        observeEvent(
          board$board,
          {
            ids <- blockr.core::board_block_ids(board$board)

            cur <- rv_items()
            keep <- Filter(
              function(x) !item_is_block(x) || x$block %in% ids,
              cur
            )

            if (!identical(cur, keep)) {
              rv_items(keep)
            }
          }
        )

        # ---- picking and ordering -----------------------------------
        #
        # Catalogue (the board, as searchable cards) and picked set ride in
        # two messages, identical-skipped, exactly as in slides.R -- but
        # under blockr-report-* names: both extensions can sit on one page,
        # and shared message names would cross-wire their menus.
        catalog_sig <- NULL

        observe(
          {
            meta <- block_meta()
            ids <- names(meta)
            tbl <- icon_key_table(
              chr_ply(ids, function(i) na_blank(meta[[i]]$icon))
            )

            items_msg <- lapply(
              seq_along(ids),
              function(k) {
                i <- ids[[k]]
                list(
                  id = i,
                  name = coal(na_blank(meta[[i]]$name), i),
                  icon_key = tbl$keys[[k]],
                  kind = coal(meta[[i]]$kind, "")
                )
              }
            )

            if (!identical(items_msg, catalog_sig)) {
              catalog_sig <<- items_msg
              session$sendCustomMessage(
                "blockr-report-catalog",
                list(items = items_msg, icons = tbl$icons)
              )
            }
          }
        )

        observe(
          {
            session$sendCustomMessage(
              "blockr-report-picked",
              list(ids = as.list(item_block_ids(rv_items())))
            )
          }
        )

        observeEvent(
          input$rpt_add,
          {
            blk <- input$rpt_add
            req(is.character(blk), length(blk) == 1L, nzchar(blk))

            if (!blk %in% item_block_ids(rv_items())) {
              rv_items(c(rv_items(), list(new_block_item(blk))))
            }
          }
        )


        # ---- row actions --------------------------------------------
        #
        # All row messages carry the item INDEX: text items have no id, and
        # the index is unambiguous for both kinds. The renderUI redraws on
        # every items write, so an index can never outlive its list.
        observeEvent(
          input$rpt_act,
          {
            at <- input$rpt_act$idx
            act <- input$rpt_act$act
            req(is.numeric(at), is.character(act))
            at <- as.integer(at)

            cur <- rv_items()
            req(at >= 1L, at <= length(cur))
            it <- cur[[at]]

            if (identical(act, "rm")) {
              editing(NULL)
              fresh(NULL)
              rv_items(cur[-at])
              return()
            }

            if (identical(act, "edit")) {
              req(item_is_text(it))
              editing(if (identical(editing(), at)) NULL else at)
              return()
            }

            if (act %in% c("text_above", "text_below")) {
              after <- if (identical(act, "text_below")) at else at - 1L
              rv_items(append(cur, list(list(text = "")), after = after))
              editing(after + 1L)
              fresh(after + 1L)
              return()
            }

            req(item_is_block(it))

            if (identical(act, "code")) {
              it$code <- !isTRUE(it$code)
            } else if (identical(act, "output")) {
              it$output <- !isTRUE(it$output)
            } else if (identical(act, "fullw")) {
              it$full_width <- !isTRUE(it$full_width)
            } else {
              return()
            }

            cur[[at]] <- it
            rv_items(cur)
          }
        )

        # Figure size, from the dots menu: a preset (w/h pair) or NULL to
        # fall back to the document default.
        observeEvent(
          input$rpt_fig,
          {
            at <- input$rpt_fig$idx
            req(is.numeric(at))
            at <- as.integer(at)

            cur <- rv_items()
            req(at >= 1L, at <= length(cur), item_is_block(cur[[at]]))

            w <- input$rpt_fig$w
            h <- input$rpt_fig$h

            cur[[at]]$fig_width <-
              if (is.numeric(w) && length(w) == 1L) as.numeric(w)
            cur[[at]]$fig_height <-
              if (is.numeric(h) && length(h) == 1L) as.numeric(h)

            rv_items(cur)
          }
        )

        observeEvent(
          input$rpt_move,
          {
            from <- input$rpt_move$from
            to <- input$rpt_move$to
            req(is.numeric(from), is.numeric(to))
            from <- as.integer(from)
            to <- as.integer(to)
            req(!identical(from, to))

            cur <- rv_items()
            req(from >= 1L, from <= length(cur), to >= 1L, to <= length(cur))

            it <- cur[[from]]
            rest <- cur[-from]
            at <- to - (from < to)
            after <- at - !isTRUE(input$rpt_move$after)

            editing(NULL)
            fresh(NULL)
            rv_items(append(rest, list(it), after = max(0L, after)))
          }
        )

        # ---- the text editor ----------------------------------------
        #
        # Save-then-close, the outline's model: the Milkdown editor commits
        # its text to `rpt_desc_edit` (focusout, plus an explicit flush from
        # the click-outside handler in report_js), and only the save writes
        # the item -- never a keystroke, so the row list's renderUI cannot
        # fight the open editor.
        observeEvent(
          input$rpt_desc_cancel,
          {
            at <- editing()
            editing(NULL)

            if (is.numeric(at) && identical(fresh(), at)) {
              cur <- rv_items()
              if (at <= length(cur) && item_is_text(cur[[at]]) &&
                    !nzchar(trimws(coal(cur[[at]]$text, "")))) {
                rv_items(cur[-at])
              }
            }
            fresh(NULL)
          }
        )

        observeEvent(
          input$rpt_desc_save,
          {
            at <- editing()
            req(is.numeric(at))

            cur <- rv_items()
            req(at >= 1L, at <= length(cur), item_is_text(cur[[at]]))

            cur[[at]]$text <- coal(input$rpt_desc_edit, "")
            rv_items(cur)

            editing(NULL)
            fresh(NULL)
          }
        )

        # ---- the list -----------------------------------------------
        #
        # NOT a renderUI: a full re-render repaints every card on any
        # items write, so a toggle click flashes the whole list. The rows
        # are rendered to HTML strings instead, diffed server-side against
        # the previous push, and only the changed ones cross the wire; the
        # client swaps exactly those nodes. A toggle touches one row, an
        # append one, an insert the renumbered tail. `rpt_rows_sync` (the
        # client announcing itself on shiny:connected) forces a full send,
        # so a late or re-attached panel starts complete.
        rows_html <- reactive(
          {
            items <- rv_items()
            meta <- block_meta()
            ed <- editing()

            lapply(
              seq_along(items),
              function(k) {
                it <- items[[k]]
                tag <- if (item_is_block(it)) {
                  report_row(it, k, meta[[it$block]], session$ns)
                } else {
                  report_text_row(
                    it, k,
                    editing = identical(ed, k),
                    ns = session$ns
                  )
                }
                as.character(tag)
              }
            )
          }
        )

        rows_prev <- NULL
        rows_sync_seen <- NULL

        observe(
          {
            rows <- rows_html()
            sync <- input$rpt_rows_sync

            full <- !identical(sync, rows_sync_seen)
            rows_sync_seen <<- sync

            prev <- if (full) NULL else rows_prev
            rows_prev <<- rows

            n <- length(rows)
            changed <- Filter(
              function(i) {
                i > length(prev) || !identical(rows[[i]], prev[[i]])
              },
              seq_len(n)
            )

            if (!full && !length(changed) && length(prev) == n) {
              return()
            }

            session$sendCustomMessage(
              "blockr-report-rows",
              list(
                n = n,
                set = lapply(
                  changed,
                  function(i) list(i = i, html = rows[[i]])
                )
              )
            )
          }
        )

        # ---- title and settings -------------------------------------

        observeEvent(input$rpt_title, {
          if (!identical(input$rpt_title, rv_title())) {
            rv_title(input$rpt_title)
          }
        }, ignoreInit = TRUE)

        updateTextInput(session, "rpt_title", value = isolate(rv_title()))

        # The gear band's fields, each writing its slot through the
        # sanitizer -- one reactiveVal, so a settings change is one
        # invalidation for the emitters downstream.
        set_setting <- function(field, value) {
          cur <- rv_settings()
          cur[[field]] <- value
          new <- sanitize_settings(cur)
          if (!identical(new, rv_settings())) {
            rv_settings(new)
          }
        }

        observeEvent(input$rpt_set_format,
          set_setting("format", input$rpt_set_format),
          ignoreInit = TRUE
        )
        observeEvent(input$rpt_set_embed,
          set_setting("embed_resources", input$rpt_set_embed),
          ignoreInit = TRUE
        )
        observeEvent(input$rpt_set_titles,
          set_setting("block_titles", input$rpt_set_titles),
          ignoreInit = TRUE
        )
        observeEvent(input$rpt_set_figw,
          set_setting("fig_width", input$rpt_set_figw),
          ignoreInit = TRUE
        )
        observeEvent(input$rpt_set_figh,
          set_setting("fig_height", input$rpt_set_figh),
          ignoreInit = TRUE
        )
        observeEvent(input$rpt_set_toc,
          set_setting("toc", input$rpt_set_toc),
          ignoreInit = TRUE
        )
        observeEvent(input$rpt_set_numbers,
          set_setting("number_sections", input$rpt_set_numbers),
          ignoreInit = TRUE
        )
        observeEvent(input$rpt_set_fold,
          set_setting("code_fold", input$rpt_set_fold),
          ignoreInit = TRUE
        )
        observeEvent(input$rpt_set_warnings,
          set_setting("warnings", input$rpt_set_warnings),
          ignoreInit = TRUE
        )

        # Seed the band's inputs from the restored settings, once.
        local({
          s <- isolate(rv_settings())
          updateSelectInput(session, "rpt_set_format", selected = s$format)
          updateCheckboxInput(session, "rpt_set_embed", value = s$embed_resources)
          updateSelectInput(session, "rpt_set_titles", selected = s$block_titles)
          updateNumericInput(session, "rpt_set_figw", value = s$fig_width)
          updateNumericInput(session, "rpt_set_figh", value = s$fig_height)
          updateCheckboxInput(session, "rpt_set_toc", value = s$toc)
          updateCheckboxInput(session, "rpt_set_numbers", value = s$number_sections)
          updateCheckboxInput(session, "rpt_set_fold", value = s$code_fold)
          updateCheckboxInput(session, "rpt_set_warnings", value = s$warnings)
        })

        # Narrow the format select to what this deployment can actually
        # produce. Off the UI's critical path on purpose: the probe is a
        # render per format, so it runs here, after the page exists, and
        # once per process (report_render_formats() is memoised) -- only
        # the first session in a worker pays it.
        #
        # The current setting stays in the list even when it is not
        # renderable here. A board saved against pdf on a machine with TeX
        # must keep saying pdf on a machine without it: silently rewriting
        # the author's choice to html would make the panel lie about the
        # document, and the render already has an honest way to say no.
        observe({

          cur <- isolate(rv_settings()$format)
          known <- report_known_formats()
          choices <- known[known %in% c(report_render_formats(), cur)]

          updateSelectInput(
            session,
            "rpt_set_format",
            choices = choices,
            selected = cur
          )
        })

        # The button reports the gear's decision. Not a picker and not a
        # second place to change it -- the toolbar's job here is to say what
        # pressing it will produce, which is the one thing the old format
        # select was good for.
        observeEvent(
          rv_settings()$format,
          updateActionButton(
            session,
            "rpt_go",
            label = paste("Render", report_format_name(rv_settings()$format)),
            icon = icon("play")
          )
        )

        # ---- the projection, on demand -------------------------------
        #
        # Lazy, as in slides.R: nothing here is read while the builder is on
        # screen. The code views read it when opened, the download observers
        # on click. expr_cache carries expressions across the gap in which a
        # backgrounded block stops reporting (the full story is slides.R).
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

            pending <- names(out)[vapply(out, is.null, logical(1L))]

            for (id in pending) {
              # NOT quote(NULL): NULL is self-evaluating, so assigning
              # quote(NULL) deletes the element instead of filling it.
              out[[id]] <- quote(invisible(NULL))
            }

            structure(out, pending = pending)
          }
        )

        sections <- reactive(
          report_sections(
            board_exprs(), board$board, item_block_ids(rv_items())
          )
        )

        # One emission, two consumers: the pieces feed the gutter views, and
        # the downloads join the SAME pieces -- what is on screen and what
        # is in the file cannot drift.
        qmd_pieces <- reactive(
          export_qmd(
            sections(),
            rv_title(),
            block_level = report_block_level(rv_settings()),
            collapse = FALSE,
            flags = report_flags(rv_items()),
            items = rv_items(),
            settings = rv_settings()
          )
        )

        spin_pieces <- reactive(
          export_spin(
            sections(),
            block_level = report_block_level(rv_settings()),
            title = rv_title(),
            collapse = FALSE,
            flags = report_flags(rv_items()),
            items = rv_items(),
            settings = rv_settings()
          )
        )

        qmd_txt <- reactive(paste0(qmd_pieces(), collapse = "\n\n"))

        spin_txt <- reactive(paste0(spin_pieces(), collapse = "\n\n"))

        # ---- the code views ------------------------------------------
        #
        # The projection is re-DERIVED far more often than it CHANGES, and
        # the difference is what the reader sees. board_exprs() reads every
        # block's expr reactive, and selecting a dock panel invalidates
        # those: the active view decides the board's needed set, so a tab
        # click re-evaluates blocks and their expr reactives fire again
        # carrying the same expression. Shiny propagates invalidation, not
        # value equality, so a renderUI reading sections() directly tore
        # down and rebuilt the whole file on every tab click -- the code
        # view blanked and came back, while the code in it was byte for
        # byte the one already on screen.
        #
        # reactiveVal is the brake: it compares with identical() and stays
        # silent when the value has not changed, so expr churn that lands
        # on the same code stops here instead of reaching the DOM. Cheap,
        # because the comparison is on the emitted pieces -- the thing that
        # actually decides what the view looks like.
        #
        # The observe is gated on a code view being open rather than
        # reading sections() unconditionally, which keeps the projection as
        # lazy as it was: nothing is demanded while the builder is on
        # screen, and a deferred board still costs nothing until the reader
        # asks for the code.
        code_view <- reactiveVal(NULL)

        observe({

          view <- coal(input$rpt_view, "builder")

          if (!view %in% c("script", "qmd")) {
            return()
          }

          sects <- tryCatch(sections(), error = function(e) NULL)

          # No pieces is the empty state; the render arm reads it off the
          # missing element rather than a second flag.
          if (is.null(sects) || !length(item_block_ids(rv_items()))) {
            code_view(list(view = view))
            return()
          }

          code_view(
            list(
              view = view,
              sects = sects,
              pieces = if (identical(view, "qmd")) {
                qmd_pieces()
              } else {
                spin_pieces()
              }
            )
          )
        })

        output$rpt_code <- renderUI({

          state <- code_view()
          req(state)

          if (is.null(state$pieces)) {
            return(
              div(
                class = "blockr-rpt-empty",
                "Nothing in the report yet, so nothing to show here."
              )
            )
          }

          report_code_ui(
            state$pieces, state$view, state$sects, session$ns
          )
        })

        # ---- open a block --------------------------------------------
        #
        # The outline's open move: reveal the block's panel in the active
        # view, focus-or-add, never switch views. Fired from a row click
        # and from the code views' gutter chips alike.
        observeEvent(
          input$rpt_open,
          {
            blk_id <- input$rpt_open$id
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

        # ---- the two-stage download ----------------------------------
        #
        # Demand pending blocks, wait for their code, then click the hidden
        # link -- including the visibility$required snapshot/restore. All of
        # it verbatim from slides.R, where the comments explain the traps.
        awaiting <- reactiveVal(FALSE)
        wait_note <- reactiveVal(NULL)
        demanded <- reactiveVal(list())

        demand_blocks <- function(pending) {

          slots <- if (!is.null(visibility)) visibility$required

          if (is.null(slots)) {
            return(FALSE)
          }

          snap <- demanded()

          for (blk_id in pending) {
            slot <- slots[[blk_id]]
            if (is.function(slot)) {
              if (!blk_id %in% names(snap)) {
                snap[[blk_id]] <- isolate(slot())
              }
              slot(TRUE)
            }
          }

          demanded(snap)

          TRUE
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

        drop_wait_note <- function() {
          note <- wait_note()
          if (!is.null(note)) {
            removeNotification(note)
            wait_note(NULL)
          }
        }

        pending_exported <- function(sects) {
          sects$ids[sects$exported & sects$pending]
        }

        fire_download <- function() {
          session$sendCustomMessage(
            "blockr-report-download",
            list(id = session$ns("rpt_dl"))
          )
        }

        observeEvent(
          input$rpt_go,
          {
            if (!length(item_block_ids(rv_items()))) {
              showNotification(
                "Add at least one block before downloading a report.",
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
              fire_download()
              return()
            }

            if (!demand_blocks(pending)) {
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

            awaiting(TRUE)
            drop_wait_note()
            wait_note(
              showNotification(
                sprintf(
                  paste(
                    "Evaluating %d block%s… the download starts when",
                    "the report is ready."
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

        observe(
          {
            req(awaiting())

            if (length(pending_exported(sections()))) {
              return()
            }

            awaiting(FALSE)
            restore_demanded()
            drop_wait_note()
            fire_download()
          }
        )

        dl_stem <- function() {
          stem <- if (nzchar(trimws(rv_title()))) rv_title() else "report"
          paste0(
            deck_filename(stem), "-",
            format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
          )
        }

        output$rpt_dl <- downloadHandler(
          filename = function() {
            paste0(dl_stem(), ".", report_dl_ext(rv_settings()$format))
          },
          content = function(file) {
            # No source branch: the format is a RENDER target now, and the
            # source leaves by its own view's header (rpt_src below).
            with_render_guard(
              render_report(
                qmd_txt(),
                spin_txt(),
                rv_settings()$format,
                file,
                rv_title(),
                sects = sections()
              )
            )
          }
        )

        # The source, from the code view's own header. One handler for both
        # views because only one is ever on screen, and the same reactives
        # the view reads -- so the file and the panel cannot disagree, which
        # is the property the old format-picker route quietly lacked.
        output$rpt_src <- downloadHandler(
          filename = function() {
            qmd <- identical(coal(input$rpt_view, "builder"), "qmd")
            paste0(dl_stem(), if (qmd) ".qmd" else ".R")
          },
          content = function(file) {
            qmd <- identical(coal(input$rpt_view, "builder"), "qmd")
            writeLines(if (qmd) qmd_txt() else spin_txt(), file)
            invisible(file)
          }
        )

        # A hidden downloadLink's handler must not be suspended, or its href
        # is never populated and the JS click navigates to the page URL.
        outputOptions(output, "rpt_dl", suspendWhenHidden = FALSE)

        list(
          state = list(
            items = rv_items,
            title = rv_title,
            settings = rv_settings
          )
        )
      }
    )
  }
}
