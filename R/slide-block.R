# The slide block: an ordinary blockr block whose value is one composed
# slide. Spec: _blockr.design/open/slide-block/ (motivation, requirements,
# design); mockups: _scratch/slide-block/.

# Mirrors blockr.core's (unexported) variadic helpers, the way blockr.dplyr
# does: a variadic server receives `...args` as a reactives object whose
# unnamed slots (edges dragged in the DAG UI) have no display name, and each
# slot is bound in the eval environment under the link name or `.arg1`,
# `.arg2`, ... Keep in sync with blockr.core R/utils-misc.R.
dot_sym <- function(i) {
  paste0(".arg", i)
}

arg_refs <- function(nms) {
  unnamed <- !nzchar(nms)
  replace(nms, unnamed, dot_sym(seq_len(sum(unnamed))))
}

dot_arg_refs <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    nms <- character(length(x))
  }
  stats::setNames(arg_refs(nms), nms)
}

as_dot_call <- function(x) {
  call(".", as.name(x))
}

#' Slide block
#'
#' A block that composes one PowerPoint slide: pick a layout, fill its
#' fields, link exhibits, and the block's output is the finished slide at
#' true 16:9 -- the same inch geometry the pptx download places, painted in
#' HTML (see [slide()]). Variadic: a layout states how many exhibits it
#' takes, and zero-input text slides are legal.
#'
#' @param layout Layout id, one of `names(slide_layouts())`.
#' @param title,subtitle,footnote Slide chrome; empty strings render nothing.
#' @param text The Details field: one field for every layout, so the words
#'   survive a layout switch. `- ` starts a bullet, `1. ` a numbered item,
#'   plain lines stay plain; `**bold**`, `*italic*`.
#' @param labels Compare layout only: panel headings, `|`-separated.
#' @param paginate Single-exhibit layouts: page an overflowing table over
#'   further slides (see [slide()]). Ignored by other layouts.
#' @param ... Forwarded to [blockr.core::new_block()].
#'
#' @export
new_slide_block <- function(layout = "exhibit-full", title = "",
                            subtitle = "", footnote = "", text = "",
                            labels = "", paginate = TRUE, ...) {

  blockr.core::new_block(

    function(id, ...args) {
      shiny::moduleServer(id, function(input, output, session) {

        lay <- shiny::reactiveVal(layout)
        tit <- shiny::reactiveVal(title)
        sub <- shiny::reactiveVal(subtitle)
        fno <- shiny::reactiveVal(footnote)
        txt <- shiny::reactiveVal(text)
        pgn <- shiny::reactiveVal(isTRUE(paginate))
        lbl <- shiny::reactiveVal(labels)

        shiny::observeEvent(input$layout, lay(input$layout))
        shiny::observeEvent(input$paginate, pgn(isTRUE(input$paginate)),
                            ignoreInit = TRUE)
        shiny::observeEvent(input$labels, lbl(input$labels),
                            ignoreInit = TRUE)
        shiny::observeEvent(input$title, tit(input$title), ignoreInit = TRUE)
        shiny::observeEvent(input$subtitle, sub(input$subtitle),
                            ignoreInit = TRUE)
        shiny::observeEvent(input$footnote, fno(input$footnote),
                            ignoreInit = TRUE)
        shiny::observeEvent(input$text, txt(input$text), ignoreInit = TRUE)

        arg_names <- shiny::reactive(
          dot_arg_refs(...args)
        )

        # The layout-dependent fields are STATIC -- rendered once in the
        # UI function, shown / hidden client-side by the tile click (see
        # slide_layout_picker). A layout switch must not rebuild the
        # Details textarea: the field's whole story is that its words
        # survive the switch, and a teardown-and-rebuild flashes exactly
        # the opposite.

        # The capacity indicator (spec req. 12): the quiet meter -- one
        # cell per slot the layout expects, filled left to right, an amber
        # cell per surplus input, the text as the accessible label. An
        # indicator, not a validation: a link short is an ordinary state
        # while authoring, and the preview below already SHOWS the empty
        # slot -- the meter only has to count.
        output$capacity <- shiny::renderUI({
          spec <- slide_layout_spec(lay())
          n <- length(arg_names())
          want <- spec$inputs

          cells <- c(
            lapply(seq_len(want), function(i) {
              htmltools::span(class = paste0(
                "slb-cell", if (i <= n) " slb-cell--on"
              ))
            }),
            lapply(seq_len(max(0L, n - want)), function(i) {
              htmltools::span(class = "slb-cell slb-cell--over")
            })
          )

          label <- if (want == 0L) {
            if (n > 0L) {
              paste0(n, " linked, this layout draws none")
            } else {
              "No inputs"
            }
          } else {
            paste0(
              n, " of ", want, " linked",
              if (n > want) paste0(", ", n - want, " not drawn")
            )
          }

          htmltools::span(
            class = "slb-meter",
            if (length(cells)) htmltools::span(class = "slb-cells", cells),
            htmltools::span(label)
          )
        })

        cur_slide <- shiny::reactive(
          slide(
            layout = lay(), title = tit(), subtitle = sub(),
            footnote = fno(), text = txt(), labels = lbl(),
            paginate = pgn(),
            # By position, not via unname() (the reactives names<- method
            # rejects NULL), and no call: the store binds each slot as an
            # active binding, so indexing already yields the current value.
            exhibits = lapply(seq_along(...args), function(i) ...args[[i]])
          )
        )

        dl_name <- function(ext) {
          function() {
            nm <- if (nzchar(tit())) tit() else "slide"
            paste0(gsub("[^[:alnum:]]+", "-", tolower(nm)), ".", ext)
          }
        }

        output$dl_pptx <- shiny::downloadHandler(
          filename = dl_name("pptx"),
          content = function(file) write_slide_pptx(cur_slide(), file)
        )

        output$dl_html <- shiny::downloadHandler(
          filename = dl_name("html"),
          content = function(file) write_slide_html(cur_slide(), file)
        )

        # The links wait, hidden, in the closed download menu; a suspended
        # hidden output would never get its href.
        shiny::outputOptions(output, "dl_pptx", suspendWhenHidden = FALSE)
        shiny::outputOptions(output, "dl_html", suspendWhenHidden = FALSE)

        list(
          expr = shiny::reactive(
            bquote(
              blockr.outline::slide(
                layout = .(lay), title = .(tit), subtitle = .(sub),
                footnote = .(fno), text = .(txt), labels = .(lbl),
                paginate = .(pgn),
                exhibits = list(..(dat))
              ),
              list(
                lay = lay(), tit = tit(), sub = sub(), fno = fno(),
                txt = txt(), lbl = lbl(), pgn = pgn(),
                dat = lapply(unname(arg_names()), as_dot_call)
              ),
              splice = TRUE
            )
          ),
          state = list(
            layout = lay, title = tit, subtitle = sub,
            footnote = fno, text = txt, labels = lbl, paginate = pgn
          )
        )
      })
    },

    function(id) {

      ns <- function(x) shiny::NS(id, x)
      tray <- ns("tray")

      htmltools::div(
        class = "slb-root",
        panels_dep(),
        slide_block_dep(),
        # The header row: how many exhibits the layout draws (a status, on
        # the left), then the tools, gear last.
        htmltools::div(
          class = "blockr-otl-head slb-head",
          htmltools::div(
            class = "slb-status",
            shiny::uiOutput(ns("capacity"), inline = TRUE)
          ),
          htmltools::div(
            class = "blockr-otl-tools",
            slide_dl_menu(id),
            otl_gear(tray)
          )
        ),
        # Every option, in the gear tray. The layout-dependent fields are
        # rendered once and shown or hidden client-side by the layout pick
        # (data-layouts), so a switch never rebuilds the Details field:
        # its words survive the switch.
        otl_tray(
          tray,
          otl_section(
            "Layout",
            otl_field(
              NULL,
              slide_layout_picker(ns("layout"), selected = layout),
              size = "full"
            ),
            slide_layout_field(
              "paginate", layout,
              otl_checkbox(
                ns("paginate"),
                "Page a long table over further slides",
                value = isTRUE(paginate),
                size = "full"
              )
            )
          ),
          otl_section(
            "Text",
            otl_text_field(ns("title"), "Title", value = title),
            otl_text_field(ns("subtitle"), "Subtitle", value = subtitle),
            otl_text_field(ns("footnote"), "Footnote", value = footnote),
            slide_layout_field(
              "labels", layout,
              otl_text_field(
                ns("labels"), "Panel labels", value = labels,
                placeholder = "left | right"
              )
            ),
            slide_layout_field(
              "text", layout,
              otl_textarea_field(
                ns("text"), "Details", value = text, rows = 3L,
                placeholder = paste(
                  "- bullet, 1. numbered, **bold**, *italic*.",
                  "Kept when the layout changes."
                )
              )
            )
          )
        )
      )
    },

    class = "slide_block",
    expr_type = "bquoted",
    allow_empty_state = list(input = TRUE, data = list(`...args` = 0L)),
    ...
  )
}

#' @importFrom blockr.core block_output
#' @export
block_output.slide_block <- function(x, result, session) {
  shiny::renderUI(slide_html_fit(result))
}

#' @importFrom blockr.core block_ui
#' @export
block_ui.slide_block <- function(id, x, ...) {
  htmltools::tagList(
    shiny::uiOutput(shiny::NS(id, "result"))
  )
}


# A layout-dependent field, rendered ONCE: the field carries the
# space-separated list of layout ids it belongs to (data-layouts), its
# initial visibility resolved here from the ctor's layout, every later switch
# toggled client-side by the layout pick (blockr-panels.js). The control
# keeps its identity, and its value, for the life of the block.
slide_layout_field <- function(feature, layout, field) {

  ids <- slide_layout_features()[[feature]]
  shown <- layout %in% strsplit(ids, " ", fixed = TRUE)[[1L]]

  htmltools::tagAppendAttributes(
    field,
    `data-layouts` = ids,
    hidden = if (!shown) NA
  )
}

slide_block_dep <- function() {
  htmltools::htmlDependency(
    "blockr-slide-block",
    pkg_version(),
    src = pkg_file("assets", "css"),
    stylesheet = "blockr-slide-block.css"
  )
}

# The download: a tool that opens an action menu, one row per format with
# the extension as meta.
slide_dl_menu <- function(id) {
  blockr.ui::action_menu(
    blockr.ui::tool_button(htmltools::HTML(otl_icon("download")), "Download"),
    blockr.ui::menu_item(
      shiny::downloadLink(shiny::NS(id, "dl_pptx"), "PowerPoint"),
      meta = ".pptx"
    ),
    blockr.ui::menu_item(
      shiny::downloadLink(shiny::NS(id, "dl_html"), "Web page"),
      meta = ".html"
    )
  )
}
