# The report panel: a builder view (one ordered list of block and text
# rows, a gear band for the document settings), and two code views
# (report.R / report.qmd) that show the document the downloads write, one
# chunk per block, each chunk's block icon in a gutter.
#
# The builder markup is the slides extension's, under blockr-rpt-* classes.
# The rename is not cosmetic: slides_js and report_js both delegate from
# document by class, so shared class names would let either extension drive
# the other's picker when both are mounted on one page. The block-browser
# CARD classes are kept -- they are inert styling from blockr.dock's
# stylesheet, with no JS attached. The row controls and the dots menu reuse
# the dock's ghost-button and block-dropdown styling for the same reason:
# the eye on a report row IS the eye on a block card.
report_ext_ui <- function(id, board, ...) {

  ns <- NS(id)

  div(
    class = "blockr-rpt-panel",
    id = ns("rpt_root"),
    report_dep(),
    # The fileblock frame + chroma syntax theme ride in with the outline
    # dependency. No Milkdown here: text items are edited as plain
    # markdown in a textarea (the WYSIWYG round trip confused more than it
    # helped) and only RENDERED as html in the resting row.
    outline_dep(),
    div(
      class = "blockr-rpt-toolbar",
      shinyWidgets::radioGroupButtons(
        inputId = ns("rpt_view"),
        label = NULL,
        size = "sm",
        status = "light",
        choiceNames = report_view_names(),
        choiceValues = names(report_view_icons()),
        selected = "builder"
      ),
      div(
        class = "blockr-rpt-toolbar-right",
        tags$button(
          type = "button",
          class = "blockr-rpt-gear",
          title = "Document settings",
          `aria-label` = "Document settings",
          HTML(report_gear_icon())
        ),
        div(
          class = "blockr-rpt-rendergroup",
          selectInput(
            ns("rpt_format"),
            label = NULL,
            choices = report_dl_formats(),
            selected = "html",
            selectize = FALSE,
            width = "132px"
          ),
          # Two-stage, as in slides.R: the click goes to the server first
          # (demand pending blocks, wait for their code), and the hidden
          # link is clicked from JS once the report is ready.
          actionButton(
            ns("rpt_go"),
            "Download",
            icon = icon("download"),
            class = "blockr-rpt-renderbtn"
          ),
          downloadLink(ns("rpt_dl"), label = NULL, style = "display: none;")
        )
      )
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'builder'", ns("rpt_view")),
      report_settings_band(ns),
      div(
        class = "blockr-rpt-head",
        textInput(
          ns("rpt_title"),
          label = NULL,
          placeholder = "Report title",
          width = "100%"
        )
      ),
      # No standalone add-text button: every row's + inserts text below
      # it, which covers the whole list including the end.
      div(
        class = "blockr-rpt-search",
        tags$input(
          type = "search",
          class = "blockr-block-browser-search blockr-rpt-searchinput",
          placeholder = "Search or add a block…",
          `aria-label` = "Search blocks",
          autocomplete = "off",
          spellcheck = "false"
        ),
        span(class = "blockr-rpt-searchcount"),
        div(class = "blockr-rpt-searchmenu")
      ),
      # The row host: filled and PATCHED by the blockr-report-rows push
      # (see report.R), never re-rendered wholesale. The empty message is
      # the :empty::before rule in the stylesheet.
      div(class = "blockr-rpt-list blockr-rpt-rows", id = ns("rpt_rows"))
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] != 'builder'", ns("rpt_view")),
      uiOutput(ns("rpt_code"))
    ),
    report_js(ns)
  )
}

# The three view segments, icon only. The glyphs are hand-drawn 15px
# currentColor SVG in blockr.viz's TYPE_ICONS idiom (chart.js) rather than
# fontawesome: the same three shapes have to read at 15px inside a 28px
# segment, which is a drawing decision, not an icon-set lookup. Rows, then
# angle brackets, then a page: a list, a script, a document.
report_view_icons <- function() {
  svg <- function(...) {
    paste0(
      '<svg width="15" height="15" viewBox="0 0 16 16" fill="none" ',
      'stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" ',
      'aria-hidden="true" focusable="false" ', ..., "</svg>"
    )
  }
  list(
    builder = svg(
      'stroke-width="1.4">',
      '<rect x="2" y="2.9" width="3.4" height="3.4" rx="0.9" ',
      'fill="currentColor" stroke="none"/><path d="M7.6 4.6 h6.4"/>',
      '<rect x="2" y="9.7" width="3.4" height="3.4" rx="0.9" ',
      'fill="currentColor" stroke="none"/><path d="M7.6 11.4 h6.4"/>'
    ),
    # `</>`, slash included -- the glyph the row's own code toggle shows
    # and the one the ecosystem reads as "source". Brackets alone are a
    # different sign.
    script = svg(
      'stroke-width="1.5">',
      '<path d="M4.9 4.1 L1.7 8 L4.9 11.9 M11.1 4.1 L14.3 8 L11.1 11.9"/>',
      '<path d="M10 1.7 L6 14.3"/>'
    ),
    qmd = svg(
      'stroke-width="1.4">',
      '<path d="M3.6 2.4 h5.2 L12.4 5.8 V13.6 H3.6 Z"/>',
      '<path d="M8.8 2.4 v3.4 h3.6"/>',
      '<path d="M5.8 8.9 h4.6 M5.8 11.2 h3.2"/>'
    )
  )
}

# The gear glyph, matching Blockr.icons.gear (blockr.dplyr's
# blockr-core.js) rather than fontawesome: the settings gear is one sign
# across the ecosystem, and every block on a board draws THAT one. Path
# restated here because the shared set is a JS object and this button is
# rendered in R -- it moves with the rest of the shared layer when that
# lands in blockr.ui.
report_gear_icon <- function() {
  paste0(
    '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" ',
    'fill="currentColor" viewBox="0 0 16 16" aria-hidden="true" ',
    'focusable="false"><path d="M9.405 1.05c-.413-1.4-2.397-1.4-2.81 0',
    "l-.1.34a1.464 1.464 0 0 1-2.105.872l-.31-.17c-1.283-.698-2.686.705",
    "-1.987 1.987l.169.311c.446.82.023 1.841-.872 2.105l-.34.1c-1.4.413",
    "-1.4 2.397 0 2.81l.34.1a1.464 1.464 0 0 1 .872 2.105l-.17.31c-.698 ",
    "1.283.705 2.686 1.987 1.987l.311-.169a1.464 1.464 0 0 1 2.105.872",
    "l.1.34c.413 1.4 2.397 1.4 2.81 0l.1-.34a1.464 1.464 0 0 1 2.105",
    "-.872l.31.17c1.283.698 2.686-.705 1.987-1.987l-.169-.311a1.464 ",
    "1.464 0 0 1 .872-2.105l.34-.1c1.4-.413 1.4-2.397 0-2.81l-.34-.1a",
    "1.464 1.464 0 0 1-.872-2.105l.17-.31c.698-1.283-.705-2.686-1.987",
    "-1.987l-.311.169a1.464 1.464 0 0 1-2.105-.872zM8 10.93a2.929 2.929 ",
    '0 1 1 0-5.86 2.929 2.929 0 0 1 0 5.858z"/></svg>'
  )
}

# A segment carries no visible text, so the tooltip is its NAME and must not
# be the only copy of it -- `title` alone never reaches a keyboard or screen
# reader (ux-principles, tooltip tier 1). Each segment therefore ships the
# name twice: once as the tooltip, once visually hidden.
#
# Dropping the labels is licensed by report_code_ui(): a code view restates
# its filename in the fileblock header a few pixels below the toolbar, so a
# `report.qmd` segment label would be the same string twice on one screen.
report_view_names <- function() {

  nms <- c(builder = "Builder", script = "report.R", qmd = "report.qmd")
  ico <- report_view_icons()

  unname(
    Map(
      function(svg, nm) {
        span(
          class = "blockr-rpt-viewicon",
          title = nm,
          HTML(svg),
          span(class = "blockr-rpt-sr", nm)
        )
      },
      ico,
      nms[names(ico)]
    )
  )
}

report_dep <- function() {
  htmlDependency(
    "blockr-report",
    pkg_version(),
    src = pkg_file("assets", "css"),
    stylesheet = "blockr-report.css"
  )
}

# The document settings, between toolbar and title. Closed by default; the
# toolbar's gear toggles the `open` class (pure client state -- whether the
# band is open is not worth a round trip, let alone persisting).
report_settings_band <- function(ns) {

  fld <- function(label, control, hint = NULL) {
    div(
      class = "blockr-rpt-fld",
      tags$label(
        label,
        if (!is.null(hint)) span(class = "blockr-rpt-fldhint", hint)
      ),
      control
    )
  }

  div(
    class = "blockr-rpt-band",
    div(
      class = "blockr-rpt-bandgrid",
      fld(
        "Block titles",
        selectInput(
          ns("rpt_set_titles"),
          label = NULL,
          choices = c(
            "Headings" = "headings",
            "Captions" = "captions",
            "None" = "none"
          ),
          selected = "headings",
          selectize = FALSE,
          width = "110px"
        ),
        hint = "how a block's name appears"
      ),
      fld(
        "Figure size",
        div(
          class = "blockr-rpt-figsize",
          numericInput(
            ns("rpt_set_figw"),
            label = NULL,
            value = 8,
            min = 1,
            step = 0.5,
            width = "64px"
          ),
          span("×"),
          numericInput(
            ns("rpt_set_figh"),
            label = NULL,
            value = 4.5,
            min = 1,
            step = 0.5,
            width = "64px"
          ),
          span("in")
        ),
        hint = "default; per block in its menu"
      ),
      fld(
        "Table of contents",
        checkboxInput(ns("rpt_set_toc"), label = NULL, value = FALSE)
      ),
      fld(
        "Number sections",
        checkboxInput(ns("rpt_set_numbers"), label = NULL, value = FALSE)
      ),
      fld(
        "Fold shown code",
        checkboxInput(ns("rpt_set_fold"), label = NULL, value = FALSE),
        hint = "collapsed, reader can open"
      ),
      fld(
        "Warnings and messages",
        checkboxInput(ns("rpt_set_warnings"), label = NULL, value = FALSE),
        hint = "off keeps them out of the document"
      )
    )
  )
}

# A ghost icon button, the dock's header-icon idiom: 28px transparent
# square, grey when off, blue when on. The eye on a row means exactly what
# it means on a block card -- the output is shown.
report_gh <- function(act, label, ico, on = FALSE, extra = NULL) {
  tags$button(
    type = "button",
    class = paste0(
      "blockr-rpt-gh",
      if (on) " is-on",
      if (!is.null(extra)) paste0(" ", extra)
    ),
    `data-act` = act,
    title = label,
    `aria-label` = label,
    ico
  )
}

# One block row: number (the drag grip on hover), the block's registry
# icon, name and kind, then the ghost controls -- code, eye, the dots menu,
# remove. A row whose two switches are both off is in the document and
# renders nothing; it draws dimmed.
report_row <- function(item, k, meta, ns) {

  meta <- coal(meta, list())
  id <- item$block
  silent <- !isTRUE(item$code) && !isTRUE(item$output)

  div(
    class = "blockr-rpt-item",
    div(
      class = paste0(
        "blockr-rpt-row blockr-block-browser-card",
        if (silent) " is-silent"
      ),
      `data-idx` = k,
      `data-blk` = id,
      draggable = "true",
      div(
        class = "blockr-block-browser-card-header",
        span(class = "blockr-rpt-num", k),
        if (nzchar(coal(meta$icon, ""))) {
          span(class = "blockr-block-browser-card-icon", HTML(meta$icon))
        },
        div(
          class = "blockr-block-browser-card-body",
          div(
            class = "blockr-block-browser-card-titles",
            span(
              class = "blockr-block-browser-card-name",
              coal(na_blank(meta$name), id)
            ),
            if (nzchar(coal(meta$kind, ""))) {
              span(class = "blockr-block-browser-card-package", meta$kind)
            }
          )
        ),
        div(
          class = "blockr-rpt-ctrls",
          report_gh(
            "code", "Show the code", icon("code"), on = isTRUE(item$code)
          ),
          report_gh(
            "output", "Show the output", icon("eye"), on = isTRUE(item$output)
          ),
          report_gh("text_below", "Add text below", icon("plus")),
          report_menu(item, k, meta),
          report_gh(
            "rm", "Remove from report", icon("xmark"), extra = "is-rm"
          )
        )
      )
    )
  )
}

# One text row: markdown that stands on its own. Clicking the row opens its
# editor (save-then-close, see the server); the dots menu offers edit and
# the inserts, remove stays in the row.
report_text_row <- function(item, k, editing, ns) {

  body <- if (editing) {
    report_desc_editor(ns, k, coal(item$text, ""))
  } else if (nzchar(trimws(coal(item$text, "")))) {
    div(
      class = "blockr-rpt-prose",
      HTML(commonmark::markdown_html(item$text))
    )
  } else {
    div(class = "blockr-rpt-prose is-empty", "Empty text — click to write")
  }

  div(
    class = "blockr-rpt-item",
    div(
      class = paste0(
        "blockr-rpt-row blockr-rpt-textrow blockr-block-browser-card",
        if (editing) " is-editing"
      ),
      `data-idx` = k,
      draggable = if (!editing) "true",
      div(
        class = "blockr-block-browser-card-header",
        span(class = "blockr-rpt-num", k),
        span(
          class = "blockr-block-browser-card-icon blockr-rpt-texticon",
          icon("markdown")
        ),
        div(class = "blockr-block-browser-card-body", body),
        if (!editing) {
          div(
            class = "blockr-rpt-ctrls",
            report_gh("text_below", "Add text below", icon("plus")),
            report_menu(item, k, NULL),
            report_gh(
              "rm", "Remove from report", icon("xmark"), extra = "is-rm"
            )
          )
        }
      )
    )
  )
}

# The dots menu, the block header's dropdown under the report's classes:
# figure size (chart-kinded rows only -- a table has no figure), full
# width, and the text inserts. Remove is NOT here (the row's own x) and
# neither is open-the-block (a click on the row). `meta = NULL` marks a
# text row: edit plus the inserts.
report_menu <- function(item, k, meta) {

  is_text <- is.null(meta)
  kind <- coal(meta$kind, "")

  size_block <- if (identical(kind, "fig")) {
    w <- item$fig_width
    h <- item$fig_height
    preset <- function(lab, pw, ph) {
      on <- identical(w, pw) && identical(h, ph)
      tags$button(
        type = "button",
        class = paste0("blockr-rpt-size", if (on) " is-on"),
        `data-w` = pw,
        `data-h` = ph,
        lab
      )
    }
    list(
      tags$li(tags$h6(class = "dropdown-header", "Figure size")),
      tags$li(
        div(
          class = "blockr-rpt-sizes",
          preset("S", 5, 3.5),
          preset("M", 8, 4.5),
          preset("L", 10, 6),
          preset("Wide", 12, 4)
        )
      ),
      tags$li(
        div(
          class = "blockr-rpt-sizenote",
          if (is.null(w) && is.null(h)) {
            "document default"
          } else {
            paste0(coal(w, "–"), " × ", coal(h, "–"), " in")
          }
        )
      ),
      if (!is.null(w) || !is.null(h)) {
        menu_item("figdefault", "Document default", icon("rotate-left"))
      },
      tags$li(tags$hr(class = "dropdown-divider my-2"))
    )
  }

  full_block <- if (!is_text) {
    list(
      menu_item(
        "fullw", "Full width", icon("expand"),
        on = isTRUE(item$full_width)
      ),
      tags$li(tags$hr(class = "dropdown-divider my-2"))
    )
  }

  edit_block <- if (is_text) {
    list(menu_item("edit", "Edit text", icon("pen")))
  }

  div(
    class = "dropdown blockr-rpt-menuwrap",
    tags$button(
      type = "button",
      class = "blockr-rpt-gh",
      `data-bs-toggle` = "dropdown",
      `aria-expanded` = "false",
      title = "More",
      icon("ellipsis-vertical")
    ),
    tags$ul(
      class = paste(
        "dropdown-menu dropdown-menu-end blockr-block-dropdown",
        "blockr-rpt-menu shadow-sm rounded-3 border-1"
      ),
      size_block,
      full_block,
      edit_block,
      menu_item("text_above", "Add text above", icon("plus"), cls = "good"),
      menu_item("text_below", "Add text below", icon("plus"), cls = "good")
    )
  )
}

menu_item <- function(act, label, ico, on = FALSE, cls = NULL) {
  tags$li(
    tags$a(
      class = paste(
        "dropdown-item blockr-rpt-menuitem",
        if (on) "is-on",
        if (!is.null(cls)) paste0("is-", cls)
      ),
      href = "#",
      `data-act` = act,
      span(class = "blockr-rpt-menuicon", if (on) icon("check") else ico),
      label
    )
  )
}

# The text editor: a writer card around a plain markdown textarea. Raw
# markdown is easier to edit than a WYSIWYG round trip (typing `## Season`
# beats hunting for a heading control), and the resting row still RENDERS
# the html. The card is set in the UI font, not monospace, so the text
# keeps the width and rhythm it will have rendered; the footer carries the
# four bits of markdown people actually use, and Done / Discard so nobody
# has to know the click-outside convention (which still works, as does
# Esc). The textarea auto-grows from report_js.
report_desc_editor <- function(ns, key, value) {

  cheat <- function(x) span(class = "blockr-rpt-cheatbit", x)

  div(
    class = "blockr-rpt-editor",
    tags$textarea(
      class = "blockr-rpt-mdedit",
      rows = max(3L, length(strsplit(coal(value, ""), "\n")[[1L]]) + 1L),
      placeholder = "Write in markdown…",
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
        tags$button(
          type = "button",
          class = "blockr-rpt-edbtn blockr-rpt-edcancel",
          "Discard"
        ),
        tags$button(
          type = "button",
          class = "blockr-rpt-edbtn blockr-rpt-edsave",
          "Done"
        )
      )
    )
  )
}

# A code view: the outline's fileblock frame around one row per emitted
# piece -- gutter cell (the block's registry icon as a button; click opens
# the block's panel) beside the highlighted code cell. Pieces and their
# block ids come from export_qmd/export_spin(collapse = FALSE); a piece
# with no block (YAML, text items) gets an empty gutter cell.
report_code_ui <- function(pieces, view, sects, ns) {

  ids <- attr(pieces, "ids")
  icon_map <- setNames(sects$icons, sects$ids)
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
        tags$button(
          type = "button",
          class = "blockr-rpt-chip",
          `data-blk` = blk,
          title = paste0("Open ", coal(na_blank(name_map[[blk]]), blk)),
          HTML(coal(na_blank(icon_map[[blk]]), ""))
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

  div(
    class = "blockr-otl-fileblock blockr-rpt-fileblock",
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
        ),
        HTML("&#10697;"),
        "Copy"
      )
    ),
    div(class = "blockr-rpt-codebody", id = body_id, rows)
  )
}

# Delegated client logic: the slides picker, the text editor's save/cancel
# (click outside applies, Esc discards, with an explicit flush so a click
# that never moves focus cannot save stale text), and the report's row
# protocol -- everything about a row rides its item INDEX, because text
# rows have no id. Same shape as slides_js -- duplicated deliberately; if
# the report supersedes slides, this file is the survivor.
report_js <- function(ns) {

  consts <- sprintf(
    paste0(
      "var ACT = '%s', MOVE = '%s', DL = '%s', ADD = '%s', ROOT = '%s', ",
      "OPEN = '%s', SAVE = '%s', CANCEL = '%s', FIG = '%s', ",
      "ROWS = '%s', SYNC = '%s', EDIT = '%s';"
    ),
    ns("rpt_act"),
    ns("rpt_move"),
    ns("rpt_dl"),
    ns("rpt_add"),
    ns("rpt_root"),
    ns("rpt_open"),
    ns("rpt_desc_save"),
    ns("rpt_desc_cancel"),
    ns("rpt_fig"),
    ns("rpt_rows"),
    ns("rpt_rows_sync"),
    ns("rpt_desc_edit")
  )

  tags$script(HTML(paste0(
    "$(function() {",
    consts,
    "
      function fire(id) {
        Shiny.setInputValue(id, Math.random(), {priority: 'event'});
      }
      function openEditor() {
        return document.querySelector('.blockr-rpt-editor');
      }
      function rowIdx(row) {
        return parseInt(row.dataset.idx, 10);
      }
      // Commit the textarea's markdown before a save: both inputs travel
      // in one client message, so the save observer reads the fresh text.
      function flushEditor(ed) {
        var ta = ed.querySelector('.blockr-rpt-mdedit');
        if (ta) Shiny.setInputValue(EDIT, ta.value);
      }

      // ---- the row host: patched, never re-rendered ------------------
      // The server diffs the rendered rows against its previous push and
      // sends only the changed ones ({n, set: [{i, html}]}); this handler
      // swaps exactly those nodes, so a toggle repaints one row and an
      // append adds one. On shiny:connected the client announces itself
      // and the server answers with a full send.
      Shiny.addCustomMessageHandler('blockr-report-rows', function(msg) {
        var host = document.getElementById(ROWS);
        if (!host) return;
        var n = msg.n || 0;
        while (host.children.length > n) {
          host.removeChild(host.lastElementChild);
        }
        var tpl = document.createElement('template');
        (msg.set || []).forEach(function(e) {
          tpl.innerHTML = e.html;
          var node = tpl.content.firstElementChild;
          if (!node) return;
          if (e.i <= host.children.length) {
            host.replaceChild(node, host.children[e.i - 1]);
          } else {
            host.appendChild(node);
          }
        });
        var ta = host.querySelector('.blockr-rpt-mdedit');
        if (ta) {
          growEditor(ta);
          if (document.activeElement !== ta) ta.focus();
        }
      });

      $(document).on('shiny:connected', function() {
        fire(SYNC);
      });

      // Dirty tracking, the outline's: any edit inside the editor arms the
      // save; a pristine editor closed from outside just cancels.
      function markDirty(ev) {
        var ed = ev.target.closest && ev.target.closest('.blockr-rpt-editor');
        if (ed) ed.classList.add('dirty');
      }
      document.addEventListener('input', markDirty);
      document.addEventListener('paste', markDirty, true);
      document.addEventListener('cut', markDirty, true);

      // The textarea grows with its text: no scrollbar inside the card,
      // no resize handle to drag.
      function growEditor(ta) {
        ta.style.height = 'auto';
        ta.style.height = ta.scrollHeight + 'px';
      }
      document.addEventListener('input', function(ev) {
        if (ev.target && ev.target.classList &&
            ev.target.classList.contains('blockr-rpt-mdedit')) {
          growEditor(ev.target);
        }
      });
      document.addEventListener('keydown', function(ev) {
        if (ev.key && (ev.key.length === 1 ||
            ev.key === 'Backspace' || ev.key === 'Delete' ||
            ev.key === 'Enter')) {
          markDirty(ev);
        }
      }, true);
      document.addEventListener('keydown', function(ev) {
        if (ev.key !== 'Escape') return;
        var ed = ev.target.closest && ev.target.closest('.blockr-rpt-editor');
        if (ed) {
          ev.stopPropagation();
          fire(CANCEL);
        }
      }, true);

      document.addEventListener('click', function(e) {

        // The card's own buttons first: Done saves regardless of dirty
        // state, Discard cancels.
        var edbtn = e.target.closest ?
          e.target.closest('.blockr-rpt-edbtn') : null;
        if (edbtn) {
          var bed = edbtn.closest('.blockr-rpt-editor');
          if (edbtn.classList.contains('blockr-rpt-edsave')) {
            flushEditor(bed);
            fire(SAVE);
          } else {
            fire(CANCEL);
          }
          return;
        }

        // An open editor eats the click that lands outside it: commit (or
        // discard, if nothing changed) and stop. Flush first, so the save
        // observer reads the textarea's current text.
        var ed = openEditor();
        if (ed && !ed.contains(e.target)) {
          flushEditor(ed);
          fire(ed.classList.contains('dirty') ? SAVE : CANCEL);
          return;
        }

        var chip = e.target.closest ?
          e.target.closest('.blockr-rpt-chip[data-blk]') : null;
        if (chip) {
          Shiny.setInputValue(
            OPEN, {id: chip.dataset.blk}, {priority: 'event'}
          );
          return;
        }

        // The gear: the settings band's open state is pure client state.
        var gear = e.target.closest ?
          e.target.closest('.blockr-rpt-gear') : null;
        if (gear) {
          var p = panel();
          var band = p && p.querySelector('.blockr-rpt-band');
          if (band) {
            band.classList.toggle('open');
            gear.classList.toggle('is-on', band.classList.contains('open'));
          }
          return;
        }


        // Figure size presets, inside the dots menu.
        var size = e.target.closest ?
          e.target.closest('.blockr-rpt-size') : null;
        if (size) {
          var srow = size.closest('.blockr-rpt-item')
            .querySelector('.blockr-rpt-row');
          Shiny.setInputValue(
            FIG,
            {
              idx: rowIdx(srow),
              w: parseFloat(size.dataset.w),
              h: parseFloat(size.dataset.h)
            },
            {priority: 'event'}
          );
          return;
        }

        // The dots button itself: bootstrap opens the menu; the click must
        // not fall through to the row (which would open the block).
        if (e.target.closest && e.target.closest('[data-bs-toggle]')) return;

        // Menu items and the row's ghost buttons both carry data-act.
        var btn = e.target.closest ?
          e.target.closest('[data-act]') : null;
        if (btn) {
          e.preventDefault();
          var brow = btn.closest('.blockr-rpt-item') &&
            btn.closest('.blockr-rpt-item').querySelector('.blockr-rpt-row');
          if (!brow) return;
          if (btn.dataset.act === 'figdefault') {
            Shiny.setInputValue(
              FIG, {idx: rowIdx(brow), w: null, h: null}, {priority: 'event'}
            );
            return;
          }
          Shiny.setInputValue(
            ACT,
            {idx: rowIdx(brow), act: btn.dataset.act},
            {priority: 'event'}
          );
          return;
        }

        // A plain click on the row: a block row opens the block's panel
        // (the outline's chip move, promoted to the whole row), a text row
        // opens its editor.
        var row = e.target.closest ? e.target.closest('.blockr-rpt-row') : null;
        if (!row || row.classList.contains('is-editing')) return;
        if (e.target.closest('.dropdown-menu')) return;
        if (row.classList.contains('blockr-rpt-textrow')) {
          Shiny.setInputValue(
            ACT, {idx: rowIdx(row), act: 'edit'}, {priority: 'event'}
          );
        } else if (row.dataset.blk) {
          Shiny.setInputValue(
            OPEN, {id: row.dataset.blk}, {priority: 'event'}
          );
        }
      });

      var dragged = null;

      document.addEventListener('dragstart', function(e) {
        var row = e.target.closest ? e.target.closest('.blockr-rpt-row') : null;
        if (!row || !row.dataset.idx) return;
        dragged = rowIdx(row);
        row.classList.add('is-dragging');
        if (e.dataTransfer) e.dataTransfer.effectAllowed = 'move';
      });

      // The pointer's half of the row decides which gap the drop targets;
      // the indicator and the drop share this one rule, so the line never
      // promises a spot the drop will not use.
      function dropAfter(e, row) {
        var box = row.getBoundingClientRect();
        return (e.clientY - box.top) > box.height / 2;
      }

      // ONE caret per list, moved to the targeted gap -- not a pair of
      // pseudo-elements on the rows. The rows here are flush and
      // borderless, which is the only condition under which anchoring the
      // line to a row works: a row's bottom edge is then literally the next
      // row's top edge, so `bottom: -1px` on one and `top: -1px` on the
      // next land on the same pixel. Give a row a border or a margin and
      // they do not -- a pseudo-element is laid out against the padding
      // box, which a border insets -- and the same gap gets drawn 2px apart
      // depending on which row the pointer is over, so the caret jumps
      // between clinging to one row and the other. That is what it did in
      // the patient profile sidebar, which is where this rewrite comes
      // from. Nothing visible changes here; what changes is that the
      // appearance no longer depends on the rows staying flush.
      function caretFor(row) {
        var list = row.closest('.blockr-rpt-list');
        if (!list) return null;
        var caret = list.querySelector('.blockr-rpt-caret');
        if (!caret) {
          caret = document.createElement('div');
          caret.className = 'blockr-rpt-caret is-hidden';
          list.appendChild(caret);
        }
        return caret;
      }

      function hideCarets() {
        document.querySelectorAll('.blockr-rpt-caret').forEach(function(c) {
          c.classList.add('is-hidden');
        });
      }

      // The gap's centre, read off what is actually on screen, so this
      // knows nothing about the rows' border, margin or height.
      function moveCaret(row, after) {
        var caret = caretFor(row);
        if (!caret) return;
        var box = row.getBoundingClientRect();
        var sib = after ? row.nextElementSibling : row.previousElementSibling;
        while (sib && !sib.classList.contains('blockr-rpt-row')) {
          sib = after ? sib.nextElementSibling : sib.previousElementSibling;
        }
        var y;
        if (sib) {
          var sb = sib.getBoundingClientRect();
          y = after ? (box.bottom + sb.top) / 2 : (sb.bottom + box.top) / 2;
        } else {
          var edge = parseFloat(getComputedStyle(row).marginBottom) || 0;
          y = after ? box.bottom + edge / 2 : box.top - edge / 2;
        }
        var lb = row.closest('.blockr-rpt-list').getBoundingClientRect();
        caret.style.top = (y - lb.top) + 'px';
        caret.classList.remove('is-hidden');
      }

      document.addEventListener('dragend', function() {
        dragged = null;
        hideCarets();
        document.querySelectorAll('.blockr-rpt-row').forEach(function(r) {
          r.classList.remove('is-dragging');
        });
      });

      document.addEventListener('dragover', function(e) {
        if (dragged === null) return;
        var row = e.target.closest ? e.target.closest('.blockr-rpt-row') : null;
        if (!row) return;
        e.preventDefault();
        moveCaret(row, dropAfter(e, row));
      });

      document.addEventListener('drop', function(e) {
        if (dragged === null) return;
        var row = e.target.closest ? e.target.closest('.blockr-rpt-row') : null;
        if (!row) return;
        e.preventDefault();
        hideCarets();
        Shiny.setInputValue(
          MOVE,
          {from: dragged, to: rowIdx(row), after: dropAfter(e, row)},
          {priority: 'event'}
        );
        dragged = null;
      });

      // ---- search: the picker, as the block browser's box ------------
      // The catalogue/picked split, menu rendering and keyboard handling
      // are the slides picker's (slides.R has the full commentary); only
      // the class names and message names differ.
      var catalog = [];
      var icons = {};
      var picked = [];
      var pickedAt = {};
      var hot = 0;

      function isPicked(b) {
        return pickedAt[b.id] !== undefined;
      }

      function panel() {
        return document.getElementById(ROOT);
      }
      function searchRoot() {
        var p = panel();
        return p && p.querySelector('.blockr-rpt-search');
      }
      function searchInput() {
        var p = panel();
        return p && p.querySelector('.blockr-rpt-searchinput');
      }
      function searchQuery() {
        var inp = searchInput();
        return (inp && inp.value ? inp.value : '').trim().toLowerCase();
      }
      function esc(s) {
        return String(s == null ? '' : s).replace(/[&<>]/g, function(c) {
          return {'&': '&amp;', '<': '&lt;', '>': '&gt;'}[c];
        });
      }
      function mark(text, q) {
        var t = String(text == null ? '' : text);
        if (!q) return esc(t);
        var i = t.toLowerCase().indexOf(q);
        if (i < 0) return esc(t);
        return esc(t.slice(0, i)) + '<mark>' + esc(t.slice(i, i + q.length)) +
          '</mark>' + esc(t.slice(i + q.length));
      }
      // Match on the name and the id, which is what an entry SHOWS plus the
      // thing that disambiguates two blocks sharing a name. Deliberately not
      // the registry description: it is boilerplate per block TYPE -- every
      // dm block carries the same sentence -- so matching it returns dozens
      // of identical-looking hits for a word the user cannot see on any of
      // them.
      function searchHits(q) {
        return catalog.filter(function(b) {
          if (!q) return true;
          return (b.name + ' ' + b.id).toLowerCase().indexOf(q) >= 0;
        });
      }
      function cardHtml(b, q, idx) {
        return '<div class=\"blockr-block-browser-card\" data-blk=\"' +
          esc(b.id) + '\" data-idx=\"' + idx + '\">' +
          '<div class=\"blockr-block-browser-card-header\">' +
            '<span class=\"blockr-block-browser-card-icon\">' +
              (icons[b.icon_key] || '') + '</span>' +
            '<div class=\"blockr-block-browser-card-body\">' +
              '<div class=\"blockr-block-browser-card-titles\">' +
                '<span class=\"blockr-block-browser-card-name\">' +
                  mark(b.name, q) + '</span>' +
                (b.kind ?
                  '<span class=\"blockr-block-browser-card-package\">' +
                  esc(b.kind) + '</span>' : '') +
              '</div>' +
            '</div>' +
          '</div>' +
        '</div>';
      }
      function orderedHits(q) {
        var hits = searchHits(q);
        var out = hits.filter(function(b) { return !isPicked(b); });
        var inn = hits.filter(isPicked).sort(function(a, b) {
          return pickedAt[a.id] - pickedAt[b.id];
        });
        return {out: out, inn: inn, all: out.concat(inn)};
      }
      function sectionHtml(title, items, q, start) {
        if (!items.length) return '';
        var html = '<div class=\"blockr-block-browser-category\"><h3>' +
          title + '</h3><div class=\"blockr-block-browser-cards\">';
        items.forEach(function(b, k) { html += cardHtml(b, q, start + k); });
        return html + '</div></div>';
      }
      function selectHot(scroll) {
        var root = searchRoot();
        if (!root) return;
        root.querySelectorAll('.blockr-block-browser-card').forEach(
          function(c, i) {
            var on = i === hot;
            c.classList.toggle('card-selected', on);
            if (on && scroll) c.scrollIntoView({block: 'nearest'});
          });
      }
      function renderMenu() {
        var root = searchRoot();
        var menu = root && root.querySelector('.blockr-rpt-searchmenu');
        if (!menu) return;

        var q = searchQuery();
        var split = orderedHits(q);
        var out = split.out, inn = split.inn, all = split.all;
        if (hot >= all.length) hot = Math.max(0, all.length - 1);

        var count = root.querySelector('.blockr-rpt-searchcount');
        if (count) {
          var pool = catalog.filter(function(b) { return !isPicked(b); }).length;
          count.textContent = pool ? pool + ' not in the report' : '';
        }
        root.classList.toggle('has-value', !!q);

        menu.classList.toggle('is-empty', !all.length);
        menu.innerHTML =
          '<div class=\"blockr-block-browser-categories\">' +
            sectionHtml('Add to the report', out, q, 0) +
            sectionHtml('Already in the report', inn, q, out.length) +
          '</div>' +
          '<div class=\"blockr-block-browser-empty\">' +
            'No blocks match your search.</div>';
        selectHot(false);
      }
      function searchOpen() {
        var root = searchRoot();
        if (!root) return;
        root.classList.add('open');
        renderMenu();
      }
      function searchClose() {
        var root = searchRoot();
        if (root) root.classList.remove('open');
      }
      function gotoRow(id) {
        var p = panel();
        var row = p && p.querySelector(
          '.blockr-rpt-row[data-blk=\"' + id + '\"]'
        );
        if (!row) return;
        row.scrollIntoView({block: 'center', behavior: 'smooth'});
        row.classList.remove('blockr-rpt-flash');
        void row.offsetWidth;
        row.classList.add('blockr-rpt-flash');
      }
      function searchChoose(idx) {
        var b = orderedHits(searchQuery()).all[idx];
        if (!b) return;
        if (isPicked(b)) {
          searchClose();
          var inp = searchInput();
          if (inp) inp.blur();
          gotoRow(b.id);
          return;
        }
        Shiny.setInputValue(ADD, b.id, {priority: 'event'});
      }

      Shiny.addCustomMessageHandler('blockr-report-catalog', function(msg) {
        catalog = msg.items || [];
        icons = msg.icons || {};
        renderMenu();
      });

      Shiny.addCustomMessageHandler('blockr-report-picked', function(msg) {
        picked = msg.ids || [];
        if (typeof picked === 'string') picked = [picked];
        pickedAt = {};
        picked.forEach(function(id, i) { pickedAt[id] = i; });
        renderMenu();
      });

      document.addEventListener('input', function(ev) {
        if (ev.target && ev.target.classList &&
            ev.target.classList.contains('blockr-rpt-searchinput')) {
          hot = 0;
          searchOpen();
        }
      });
      document.addEventListener('focusin', function(ev) {
        if (ev.target && ev.target.classList &&
            ev.target.classList.contains('blockr-rpt-searchinput')) {
          searchOpen();
        }
      });
      // mousedown, not click: click fires after blur, and blurring the
      // input would have to close the menu first.
      document.addEventListener('mousedown', function(ev) {
        var card = ev.target.closest &&
          ev.target.closest('.blockr-rpt-searchmenu .blockr-block-browser-card');
        if (!card) return;
        ev.preventDefault();
        searchChoose(parseInt(card.dataset.idx, 10));
      });
      // CAPTURE phase: choosing an entry re-renders the menu and detaches
      // the clicked node, so a bubble-phase listener would close on every
      // add.
      document.addEventListener('mousedown', function(ev) {
        var root = searchRoot();
        if (root && !root.contains(ev.target)) searchClose();
      }, true);
      document.addEventListener('keydown', function(ev) {
        if (!(ev.target && ev.target.classList &&
              ev.target.classList.contains('blockr-rpt-searchinput'))) {
          return;
        }
        var n = searchHits(searchQuery()).length;
        if (ev.key === 'ArrowDown') {
          hot = n ? (hot + 1) % n : 0;
          selectHot(true); ev.preventDefault();
        } else if (ev.key === 'ArrowUp') {
          hot = n ? (hot + n - 1) % n : 0;
          selectHot(true); ev.preventDefault();
        } else if (ev.key === 'Enter') {
          searchChoose(hot); ev.preventDefault();
        } else if (ev.key === 'Escape') {
          if (ev.target.value) {
            ev.target.value = ''; hot = 0; searchOpen();
          } else {
            searchClose(); ev.target.blur();
          }
        }
      });

      Shiny.addCustomMessageHandler('blockr-report-download', function(msg) {
        var el = document.getElementById(msg.id);
        if (el) el.click();
      });
    });"
  )))
}
