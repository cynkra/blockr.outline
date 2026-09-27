# The report panel: the document's title and its one main action (Render)
# in the header row with the gear last; the document settings in the gear
# tray; then a view switch between the builder (one ordered list of block
# and text rows) and two code views (report.R / report.qmd) that show the
# document the downloads write, one chunk per block, each chunk's block
# mark in a gutter.
#
# Built on blockr.ui's design system (see R/outline-ui.R for the fields).
# The rows are pushed as html by the server and patched in place
# (blockr-report.js); every row message carries the item INDEX, because
# text rows have no id.
report_ext_ui <- function(id, board, ...) {

  ns <- NS(id)

  div(
    class = "blockr-otl-pnl blockr-rpt-panel",
    id = ns("rpt_root"),
    `data-ns` = ns(""),
    panels_dep(),
    report_dep(),
    # The file frame and the syntax colours of the code views.
    outline_dep(),
    div(
      class = "blockr-otl-head",
      otl_name(
        ns("rpt_title"),
        label = "Report title",
        empty_msg = "A report needs a title",
        placeholder = "Untitled report"
      ),
      div(
        class = "blockr-otl-tools",
        # The view's one main action. What gets rendered is a property of
        # the document and lives in the gear; the label says which
        # (updated from the settings). Two-stage, as in slides.R: the
        # click goes to the server first (demand pending blocks, wait for
        # their code) and the hidden link is clicked once the report is
        # ready.
        otl_button(
          "Render HTML",
          kind = "main",
          id = ns("rpt_go"),
          action = TRUE
        ),
        otl_gear(ns("rpt_tray"))
      )
    ),
    downloadLink(ns("rpt_dl"), label = NULL, style = "display: none;"),
    report_settings_tray(ns),
    div(
      class = "blockr-rpt-views",
      otl_segmented(
        ns("rpt_view"),
        c(Builder = "builder", report.R = "script", report.qmd = "qmd"),
        selected = "builder",
        label = "View",
        xs = TRUE,
        icons = report_view_icons()
      )
    ),
    div(
      class = "blockr-rpt-pane",
      `data-pane` = "builder",
      # The row host: filled and PATCHED by the blockr-report-rows push,
      # never re-rendered wholesale.
      # The empty line shows while the host has no rows (the stylesheet).
      div(
        class = "blockr-otl-list",
        div(class = "blockr-rpt-rows", id = ns("rpt_rows")),
        div(
          class = "blockr-empty blockr-empty--panel blockr-rpt-empty",
          "Nothing in the report yet. Add a block to start."
        )
      ),
      otl_button(
        "Add block",
        kind = "quiet",
        size = "s",
        icon = otl_icon("plus"),
        class = "blockr-otl-add"
      )
    ),
    div(
      class = "blockr-rpt-pane",
      `data-pane` = "code",
      hidden = NA,
      uiOutput(ns("rpt_code"))
    ),
    tags$script(
      HTML(sprintf("BlockrReport.init('%s');", ns("rpt_root")))
    )
  )
}

# The three views, icon only. The glyphs are drawn for 14px: rows, then
# `</>`, then a page -- a list, a script, a document. Icon only because
# report_code_ui() prints `report.R` / `report.qmd` in the file header right
# below; the name is each segment's tooltip.
report_view_icons <- function() {
  svg <- function(...) {
    paste0(
      "<svg width=\"14\" height=\"14\" viewBox=\"0 0 16 16\" fill=\"none\" ",
      "stroke=\"currentColor\" stroke-linecap=\"round\" ",
      "stroke-linejoin=\"round\" aria-hidden=\"true\" focusable=\"false\" ",
      ..., "</svg>"
    )
  }
  list(
    builder = svg(
      "stroke-width=\"1.4\">",
      "<rect x=\"2\" y=\"2.9\" width=\"3.4\" height=\"3.4\" rx=\"0.9\" ",
      "fill=\"currentColor\" stroke=\"none\"/><path d=\"M7.6 4.6 h6.4\"/>",
      "<rect x=\"2\" y=\"9.7\" width=\"3.4\" height=\"3.4\" rx=\"0.9\" ",
      "fill=\"currentColor\" stroke=\"none\"/><path d=\"M7.6 11.4 h6.4\"/>"
    ),
    script = svg(
      "stroke-width=\"1.5\">",
      "<path d=\"M4.9 4.1 L1.7 8 L4.9 11.9 M11.1 4.1 L14.3 8 L11.1 11.9\"/>",
      "<path d=\"M10 1.7 L6 14.3\"/>"
    ),
    qmd = svg(
      "stroke-width=\"1.4\">",
      "<path d=\"M3.6 2.4 h5.2 L12.4 5.8 V13.6 H3.6 Z\"/>",
      "<path d=\"M8.8 2.4 v3.4 h3.6\"/>",
      "<path d=\"M5.8 8.9 h4.6 M5.8 11.2 h3.2\"/>"
    )
  )
}

report_dep <- function() {
  htmlDependency(
    "blockr-report",
    pkg_version(),
    src = pkg_file("assets"),
    script = "js/blockr-report.js",
    stylesheet = "css/blockr-report.css",
    all_files = FALSE
  )
}

# The document settings, in the gear tray. The controls start at the
# defaults and report nothing until used; the server sets them to the
# restored settings (see report.R).
report_settings_tray <- function(ns) {
  otl_tray(
    ns("rpt_tray"),
    otl_section(
      "Output",
      # The format gates what quarto reads from everything below it. The
      # vocabulary, not the probed set: probing costs a render per format,
      # so the server narrows this list once the page is up.
      otl_select_field(
        ns("rpt_set_format"),
        "Format",
        report_known_formats(),
        selected = "html"
      ),
      # Only html embeds; hidden for the other formats (blockr-report.js).
      div(
        class = "blockr-settings__field blockr-settings__field--small",
        `data-rpt-html-only` = NA,
        tags$label(
          id = ns("rpt_set_embed"),
          class = "blockr-otl-field blockr-checkbox",
          `data-kind` = "checkbox",
          tags$input(type = "checkbox", checked = NA),
          span(class = "blockr-checkbox__box", HTML(otl_check_svg())),
          span(class = "blockr-checkbox__label", "One self-contained file")
        )
      )
    ),
    otl_section(
      "Document",
      otl_select_field(
        ns("rpt_set_titles"),
        "Block titles",
        c(Headings = "headings", Captions = "captions", None = "none"),
        selected = "headings"
      ),
      otl_checkbox(ns("rpt_set_toc"), "Table of contents"),
      otl_checkbox(ns("rpt_set_numbers"), "Number sections")
    ),
    otl_section(
      "Figures",
      otl_text_field(
        ns("rpt_set_figw"), "Width (in)", value = 8, type = "number",
        min = 1, step = 0.5, size = "small"
      ),
      otl_text_field(
        ns("rpt_set_figh"), "Height (in)", value = 4.5, type = "number",
        min = 1, step = 0.5, size = "small"
      )
    ),
    otl_section(
      "Code",
      otl_checkbox(ns("rpt_set_fold"), "Fold shown code"),
      otl_checkbox(ns("rpt_set_warnings"), "Show warnings and messages")
    )
  )
}

report_tool <- function(label, ico, ...) {
  tags$button(
    type = "button",
    class = "blockr-tool",
    `aria-label` = label,
    `data-blockr-tooltip` = label,
    ...,
    HTML(ico)
  )
}

# The row's "..." menu, described for blockr-report.js, which draws it with
# Blockr.menu: figure size for chart rows, full width for block rows, the
# text inserts for all, Edit for a text row, and Remove last. Code and
# output are on the row, so the menu does not repeat them.
report_menu_btn <- function(item, meta) {

  is_text <- is.null(meta)

  spec <- if (is_text) {
    "{\"text\":true}"
  } else {
    sprintf(
      "{\"fig\":%s,\"w\":%s,\"h\":%s,\"full\":%s}",
      if (identical(coal(meta$kind, ""), "fig")) "true" else "false",
      if (is.null(item$fig_width)) "null" else item$fig_width,
      if (is.null(item$fig_height)) "null" else item$fig_height,
      if (isTRUE(item$full_width)) "true" else "false"
    )
  }

  otl_row_more(`data-menu` = spec)
}

# One block row (design system, "Block lists"): number, the block's mark,
# its name, then the row's end. The end holds what the document shows of
# the block, code and output, as two pressed tools that are always there:
# pressed means shown, a click toggles it. The "..." follows them on hover
# or keyboard focus.
report_row <- function(item, k, meta, ns) {

  meta <- coal(meta, list())
  id <- item$block
  name <- coal(na_blank(meta$name), id)
  code <- isTRUE(item$code)
  out <- isTRUE(item$output)

  div(
    class = "blockr-otl-row blockr-rpt-row",
    `data-idx` = k,
    `data-blk` = id,
    `data-kind` = coal(meta$kind, ""),
    draggable = "true",
    tabindex = "0",
    span(class = "blockr-otl-row__num", k),
    otl_mark(meta$mark),
    otl_row_name(name),
    span(
      class = "blockr-otl-row__end",
      otl_row_tool(
        if (code) "Code shown in the report" else "Code hidden from the report",
        otl_icon("code"),
        pressed = code,
        `data-act` = "code"
      ),
      otl_row_tool(
        if (out) "Output shown in the report" else "Output hidden from the report",
        otl_icon(if (out) "eye" else "eye_off"),
        pressed = out,
        `data-act` = "output"
      ),
      report_menu_btn(item, meta)
    )
  )
}

# One text row: markdown that stands on its own, two lines of it at most. A
# click opens its editor (save-then-close, see the server).
report_text_row <- function(item, k, editing, ns) {

  body <- if (editing) {
    report_desc_editor(ns, k, coal(item$text, ""))
  } else if (nzchar(trimws(coal(item$text, "")))) {
    div(
      class = "blockr-rpt-prose",
      HTML(commonmark::markdown_html(item$text))
    )
  } else {
    div(class = "blockr-rpt-prose is-empty", "Empty text. Click to write.")
  }

  div(
    class = paste(
      "blockr-otl-row blockr-rpt-row blockr-rpt-textrow",
      if (editing) "is-editing"
    ),
    `data-idx` = k,
    draggable = if (!editing) "true",
    tabindex = if (!editing) "0",
    span(class = "blockr-otl-row__num", k),
    span(
      class = "blockr-otl-mark blockr-otl-mark--text",
      HTML(otl_icon("text"))
    ),
    body,
    if (!editing) {
      span(
        class = "blockr-otl-row__end",
        report_menu_btn(item, NULL)
      )
    }
  )
}

# The text editor: raw markdown in a field set in the UI font, so the text
# keeps the width and rhythm it will have rendered. The footer names the bits
# of markdown people use, and Done / Discard; a click outside saves too, and
# Escape discards. The textarea grows with its text (blockr-report.js).
report_desc_editor <- function(ns, key, value) {

  cheat <- function(x) span(class = "blockr-rpt-cheatbit", x)

  div(
    class = "blockr-rpt-editor",
    tags$textarea(
      class = "blockr-rpt-mdedit",
      rows = max(3L, length(strsplit(coal(value, ""), "\n")[[1L]]) + 1L),
      placeholder = "Write in markdown…",
      `aria-label` = "Text, in markdown",
      spellcheck = "false",
      value
    ),
    div(
      class = "blockr-rpt-edfoot",
      span(
        class = "blockr-rpt-cheat",
        cheat("**bold**"),
        cheat("*italic*"),
        cheat("## heading"),
        cheat("- list")
      ),
      div(
        class = "blockr-rpt-edbtns",
        otl_button("Discard", kind = "quiet", class = "blockr-rpt-edcancel"),
        otl_button("Done", kind = "secondary", class = "blockr-rpt-edsave")
      )
    )
  )
}

# A code view: the file frame around one row per emitted piece -- gutter
# cell (the block's mark as a button; a click opens the block's panel)
# beside the highlighted code. Pieces and their block ids come from
# export_qmd/export_spin(collapse = FALSE); a piece with no block (YAML,
# text items) gets an empty gutter cell.
report_code_ui <- function(pieces, view, sects, ns, marks = list()) {

  ids <- attr(pieces, "ids")
  name_map <- setNames(sects$names, sects$ids)

  hl_fun <- if (identical(view, "qmd")) highlight_qmd_code else highlight_r_code

  rows <- lapply(
    seq_along(pieces),
    function(i) {

      hl <- hl_fun(pieces[[i]])

      code <- if (is.null(hl)) {
        tags$pre(class = "blockr-rpt-code", tags$code(pieces[[i]]))
      } else {
        div(class = "blockr-rpt-code", HTML(hl))
      }

      blk <- ids[[i]]

      gut <- if (!is.na(blk)) {
        label <- paste("Open", coal(na_blank(name_map[[blk]]), blk))
        tags$button(
          type = "button",
          class = "blockr-rpt-chip",
          `data-blk` = blk,
          `aria-label` = label,
          `data-blockr-tooltip` = label,
          otl_mark(marks[[blk]])
        )
      }

      div(
        class = "blockr-rpt-coderow",
        div(class = "blockr-rpt-gutter", gut),
        code
      )
    }
  )

  body_id <- ns("rpt_codebody")

  fname <- if (identical(view, "qmd")) "report.qmd" else "report.R"

  div(
    class = "blockr-otl-fileblock blockr-rpt-fileblock",
    div(
      class = "blockr-otl-filehead",
      span(class = "blockr-otl-filename", fname),
      # The file's two actions, as tools: copying the source and saving it
      # are the same act against the same bytes. The filename is the label
      # they share.
      report_tool(
        "Copy to clipboard",
        otl_icon("copy"),
        # Join the CODE cells only: innerText on the whole body would drag
        # the gutter's accessible names into the clipboard.
        onclick = sprintf(
          paste0(
            "var b=document.getElementById('%s');",
            "navigator.clipboard.writeText(",
            "[].map.call(b.querySelectorAll('.blockr-rpt-code'),",
            "function(n){return n.innerText.replace(/\\s+$/,'');}",
            ").join('\\n\\n'));"
          ),
          body_id
        )
      ),
      downloadLink(
        ns("rpt_src"),
        label = HTML(otl_icon("download")),
        class = "blockr-tool",
        `aria-label` = paste("Download", fname),
        `data-blockr-tooltip` = paste("Download", fname)
      )
    ),
    div(class = "blockr-rpt-codebody", id = body_id, rows)
  )
}
