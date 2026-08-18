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
#' @param ... Forwarded to [blockr.core::new_block()].
#'
#' @export
new_slide_block <- function(layout = "exhibit-full", title = "",
                            subtitle = "", footnote = "", text = "", ...) {

  blockr.core::new_block(

    function(id, ...args) {
      shiny::moduleServer(id, function(input, output, session) {

        lay <- shiny::reactiveVal(layout)
        tit <- shiny::reactiveVal(title)
        sub <- shiny::reactiveVal(subtitle)
        fno <- shiny::reactiveVal(footnote)
        txt <- shiny::reactiveVal(text)

        shiny::observeEvent(input$layout, lay(input$layout))
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

        # The capacity indicator (spec req. 12): what the layout wants
        # against what is linked. An indicator, not a validation -- being a
        # link short is an ordinary state while authoring.
        output$capacity <- shiny::renderText({
          spec <- slide_layout_spec(lay())
          n <- length(arg_names())
          want <- spec$inputs
          paste0(
            "Layout takes ", want,
            if (want == 1L) " exhibit" else " exhibits",
            " · ", n, " linked",
            if (n > want) " — extra inputs are not drawn",
            if (n < want) " — unfilled slots stay empty"
          )
        })

        cur_slide <- shiny::reactive(
          slide(
            layout = lay(), title = tit(), subtitle = sub(),
            footnote = fno(), text = txt(),
            # By position, not via unname() (the reactives names<- method
            # rejects NULL), and no call: the store binds each slot as an
            # active binding, so indexing already yields the current value.
            exhibits = lapply(seq_along(...args), function(i) ...args[[i]])
          )
        )

        output$download <- shiny::downloadHandler(
          filename = function() {
            nm <- if (nzchar(tit())) tit() else "slide"
            paste0(gsub("[^[:alnum:]]+", "-", tolower(nm)), ".pptx")
          },
          content = function(file) {
            write_slide_pptx(cur_slide(), file)
          }
        )

        list(
          expr = shiny::reactive(
            bquote(
              blockr.outline::slide(
                layout = .(lay), title = .(tit), subtitle = .(sub),
                footnote = .(fno), text = .(txt),
                exhibits = list(..(dat))
              ),
              list(
                lay = lay(), tit = tit(), sub = sub(), fno = fno(),
                txt = txt(),
                dat = lapply(unname(arg_names()), as_dot_call)
              ),
              splice = TRUE
            )
          ),
          state = list(
            layout = lay, title = tit, subtitle = sub,
            footnote = fno, text = txt
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
        htmltools::div(
          style = "font-size:12px;color:#6b7280;margin-bottom:8px;",
          shiny::textOutput(shiny::NS(id, "capacity"), inline = TRUE)
        ),
        shiny::downloadButton(
          shiny::NS(id, "download"), "Download",
          class = "btn-sm"
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
