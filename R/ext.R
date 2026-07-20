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
                                  ...) {

  blockr.dock::new_dock_extension(
    outline_ext_srv(annotations, block_order, title),
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

outline_ext_srv <- function(annotations, block_order, title) {

  function(id, board, update, session, parent, ...) {
    moduleServer(
      id,
      function(input, output, session) {

        rv_ann <- reactiveVal(sanitize_annotations(annotations))
        rv_order <- reactiveVal(as.character(unlist(block_order)))
        rv_title <- reactiveVal(
          if (is.character(title) && length(title)) title[[1L]] else
            "Board report"
        )
        editing <- reactiveVal(NULL)

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
          }
        )

        board_exprs <- reactive(
          lapply(
            blockr.core::lst_xtr(board$blocks, "server", "expr"),
            blockr.core::reval
          )
        )

        sections <- reactive(
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
                rv_order()
              ),
              error = function(e) {
                message(
                  "blockr.outline: sections unavailable this flush (",
                  conditionMessage(e),
                  ")"
                )
                req(FALSE)
              }
            )
          }
        )

        spin_txt <- reactive(export_spin(sections()))
        qmd_txt <- reactive(export_qmd(sections(), rv_title()))

        output$outline_out <- renderUI(
          {
            view <- coal(input$code_view, "outline")

            if (identical(view, "outline")) {
              return(
                tagList(
                  outline_tags(
                    sections(), board$board, session$ns, editing()
                  ),
                  # Hidden full script so the copy button works here too.
                  tags$pre(
                    id = session$ns("code_pre"),
                    style = "display: none;",
                    spin_txt()
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
            blk_id <- editing()
            req(is.character(blk_id))

            ann <- rv_ann()
            entry <- coal(ann[[blk_id]], list())
            entry$description <- coal(input$desc_edit, "")
            ann[[blk_id]] <- entry
            rv_ann(ann)
            editing(NULL)
          }
        )

        # Reorder: swap with the neighbour in the *displayed* order and
        # store the whole permutation; the Kahn tie-break snaps invalid
        # wishes back to the nearest valid order.
        observeEvent(
          input$outline_move,
          {
            mv <- input$outline_move
            req(is.character(mv$id), is.character(mv$dir))

            disp <- sections()$ids
            i <- match(mv$id, disp)
            req(!is.na(i))

            j <- if (identical(mv$dir, "up")) i - 1L else i + 1L
            req(j >= 1L, j <= length(disp))

            disp[c(i, j)] <- disp[c(j, i)]
            rv_order(disp)
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
            title = rv_title
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
