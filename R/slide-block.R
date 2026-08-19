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
#' @param text The chosen layout's text field (takeaway, bullets, kicker).
#' @param paginate Single-exhibit layouts: page an overflowing table over
#'   further slides (see [slide()]). Ignored by other layouts.
#' @param ... Forwarded to [blockr.core::new_block()].
#'
#' @export
new_slide_block <- function(layout = "exhibit-full", title = "",
                            subtitle = "", footnote = "", text = "",
                            paginate = TRUE, ...) {

  blockr.core::new_block(

    function(id, ...args) {
      shiny::moduleServer(id, function(input, output, session) {

        lay <- shiny::reactiveVal(layout)
        tit <- shiny::reactiveVal(title)
        sub <- shiny::reactiveVal(subtitle)
        fno <- shiny::reactiveVal(footnote)
        txt <- shiny::reactiveVal(text)
        pgn <- shiny::reactiveVal(isTRUE(paginate))

        shiny::observeEvent(input$layout, lay(input$layout))
        shiny::observeEvent(input$paginate, pgn(isTRUE(input$paginate)),
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

        # The layout's own text field: label and visibility follow the
        # chosen layout (a takeaway line, a bullet list, a kicker -- or
        # nothing at all for the pure exhibit layouts).
        output$text_field <- shiny::renderUI({
          spec <- slide_layout_spec(lay())
          if (is.null(spec$text)) {
            return(NULL)
          }
          shiny::textAreaInput(
            session$ns("text"), spec$text$label,
            value = shiny::isolate(txt()), rows = 3L, width = "100%"
          )
        })

        # The pagination toggle exists only where pagination can: a layout
        # whose sole content slot is the exhibit (no option, no control --
        # the no-gear-when-no-options rule).
        output$paginate_field <- shiny::renderUI({
          spec <- slide_layout_spec(lay())
          kinds <- chr_ply(
            spec$build(rect(0, 0, 1, 1)),
            function(s) s$kind
          )
          if (!identical(kinds, "exhibit")) {
            return(NULL)
          }
          shiny::checkboxInput(
            session$ns("paginate"),
            "Page a long table over further slides",
            value = shiny::isolate(pgn())
          )
        })

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
              paste(n, "linked — this layout draws none")
            } else {
              "no inputs"
            }
          } else {
            paste0(
              n, " of ", want, " linked",
              if (n > want) paste0(" — ", n - want, " not drawn")
            )
          }

          htmltools::div(
            class = "slb-meter",
            if (length(cells)) htmltools::span(class = "slb-cells", cells),
            htmltools::span(label)
          )
        })

        cur_slide <- shiny::reactive(
          slide(
            layout = lay(), title = tit(), subtitle = sub(),
            footnote = fno(), text = txt(), paginate = pgn(),
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

        list(
          expr = shiny::reactive(
            bquote(
              blockr.outline::slide(
                layout = .(lay), title = .(tit), subtitle = .(sub),
                footnote = .(fno), text = .(txt), paginate = .(pgn),
                exhibits = list(..(dat))
              ),
              list(
                lay = lay(), tit = tit(), sub = sub(), fno = fno(),
                txt = txt(), pgn = pgn(),
                dat = lapply(unname(arg_names()), as_dot_call)
              ),
              splice = TRUE
            )
          ),
          state = list(
            layout = lay, title = tit, subtitle = sub,
            footnote = fno, text = txt, paginate = pgn
          )
        )
      })
    },

    function(id) {
      htmltools::tagList(
        # The layout picker: schematic tiles (the chart block's type-picker
        # pattern), drawn from the layouts' own slot rects. Clicking a tile
        # sets input$layout, same as the select it replaces.
        slide_layout_picker(shiny::NS(id, "layout"), selected = layout),
        shiny::textInput(
          shiny::NS(id, "title"), "Title", value = title, width = "100%"
        ),
        shiny::textInput(
          shiny::NS(id, "subtitle"), "Subtitle", value = subtitle,
          width = "100%"
        ),
        shiny::textInput(
          shiny::NS(id, "footnote"), "Footnote", value = footnote,
          width = "100%"
        ),
        shiny::uiOutput(shiny::NS(id, "text_field")),
        shiny::uiOutput(shiny::NS(id, "paginate_field")),
        htmltools::div(
          style = paste0(
            "display:flex;align-items:center;gap:10px;margin-bottom:8px;"
          ),
          htmltools::div(
            style = "flex:1 1 auto;",
            shiny::uiOutput(shiny::NS(id, "capacity"))
          ),
          slide_dl_menu(id)
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


# The download control: the table block's single toggle, verbatim in shape
# -- one 30px icon button that is the <summary> of a <details> menu, no JS
# (the open / close, keyboard handling and focus order are the browser's).
# The chrome CSS is restated from blockr.viz R/html-table.R dl_chrome_css()
# the way chart.css restates it: blockr.viz is a Suggests, and a block's
# controls must not depend on which other blocks share the page.
slide_dl_menu <- function(id) {

  dl_icon <- htmltools::HTML(paste0(
    '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" ',
    'stroke="currentColor" stroke-width="1.6" stroke-linecap="round" ',
    'stroke-linejoin="round">',
    '<path d="M8 2.5 V10 M4.8 7 L8 10.2 L11.2 7"/>',
    '<path d="M2.5 11.5 V12.8 A1.2 1.2 0 0 0 3.7 14 H12.3 ',
    'A1.2 1.2 0 0 0 13.5 12.8 V11.5"/></svg>'
  ))

  item <- function(output_id, label) {
    htmltools::tags$a(
      id = shiny::NS(id, output_id),
      class = "blockr-dl-item shiny-download-link",
      href = "", target = "_blank", download = NA,
      title = paste0("Download as ", label),
      `aria-label` = paste0("Download as ", label),
      label
    )
  }

  htmltools::tagList(
    htmltools::tags$style(htmltools::HTML("
a.blockr-dl-xlsx, summary.blockr-dl-xlsx { appearance: none;
  box-sizing: border-box; display: inline-flex; align-items: center;
  justify-content: center; width: 30px; height: 30px; flex: 0 0 auto;
  padding: 0; margin: 0;
  border: 1px solid var(--blockr-color-border, #e5e7eb);
  border-radius: 4px;
  background-color: var(--blockr-color-bg-input, #f9fafb);
  color: var(--blockr-grey-500, #6b7280); line-height: 1; cursor: pointer;
  transition: border-color 0.12s, background-color 0.12s, color 0.12s; }
a.blockr-dl-xlsx:hover, summary.blockr-dl-xlsx:hover {
  background-color: #fff;
  border-color: var(--blockr-grey-300, #d1d5db);
  color: var(--blockr-color-text-primary, #374151);
  text-decoration: none; }
summary.blockr-dl-xlsx:focus-visible { outline: none;
  border-color: var(--blockr-color-primary, #2563eb);
  box-shadow: 0 0 0 3px rgba(37, 99, 235, 0.12); }
a.blockr-dl-xlsx svg, summary.blockr-dl-xlsx svg { display: block; }
details.blockr-dl-menu { position: relative; flex: 0 0 auto; }
details.blockr-dl-menu > summary { list-style: none; user-select: none; }
details.blockr-dl-menu > summary::-webkit-details-marker { display: none; }
details.blockr-dl-menu > summary::marker { content: \"\"; }
details.blockr-dl-menu[open] > summary { background-color: #fff;
  border-color: var(--blockr-grey-300, #d1d5db);
  color: var(--blockr-color-text-primary, #374151); }
.blockr-dl-menu-list { position: absolute; top: calc(100% + 4px); right: 0;
  z-index: 20; min-width: 172px; padding: 4px;
  border: 1px solid var(--blockr-color-border, #e5e7eb); border-radius: 6px;
  background-color: #fff; box-shadow: 0 6px 16px rgba(17, 24, 39, 0.12);
  display: flex; flex-direction: column; gap: 1px; }
a.blockr-dl-item { display: block; padding: 6px 10px; border-radius: 4px;
  font-size: var(--blockr-font-size-sm, 0.8125rem);
  color: var(--blockr-color-text-primary, #111827); text-decoration: none;
  white-space: nowrap; cursor: pointer; }
a.blockr-dl-item:hover {
  background-color: var(--blockr-color-bg-hover, #f3f4f6); }
    ")),
    htmltools::tags$details(
      class = "blockr-dl-menu",
      htmltools::tags$summary(
        class = "blockr-dl-xlsx",
        title = "Download", `aria-label` = "Download",
        dl_icon
      ),
      htmltools::tags$div(
        class = "blockr-dl-menu-list", role = "menu",
        item("dl_pptx", "PowerPoint (.pptx)"),
        item("dl_html", "Web page (.html)")
      )
    )
  )
}
