# The UI the report and slides panels and the slide block share, built from
# R on blockr.ui's design system: the fields of a gear tray, buttons, block
# marks and the icons they carry. blockr-panels.js binds the fields to Shiny
# (see its header for why a field reports NULL until it is used or set).

# The report's code views: the file frame and the syntax colours.
outline_dep <- function() {
  htmlDependency(
    "blockr-outline",
    pkg_version(),
    src = pkg_file("assets", "css"),
    stylesheet = c("blockr-outline.css", "syntax-highlight.css")
  )
}

# blockr.ui's controls, then this package's binding and shared styles.
panels_dep <- function() {
  tagList(
    blockr.ui::controls_dep(),
    htmlDependency(
      "blockr-outline-panels",
      pkg_version(),
      src = pkg_file("assets"),
      script = "js/blockr-panels.js",
      stylesheet = "css/blockr-panels.css",
      all_files = FALSE
    )
  )
}

# ---- fields ---------------------------------------------------------------

# A field in the gear tray's grid: its label, 12px muted and 4px above, and
# the control. `size` is the grid's: small takes one column, large two, full
# the row.
otl_field <- function(label, control, size = c("large", "small", "full"),
                      ...) {

  size <- match.arg(size)

  div(
    class = paste(
      "blockr-settings__field",
      if (!identical(size, "large")) paste0("blockr-settings__field--", size)
    ),
    ...,
    if (!is.null(label)) div(class = "blockr-label", label),
    control
  )
}

# A text or number field that commits on Enter or blur.
otl_text_input <- function(id, label, value = "", placeholder = NULL,
                           type = c("text", "number"), min = NULL,
                           step = NULL) {

  type <- match.arg(type)

  div(
    id = id,
    class = "blockr-otl-field blockr-commit-field",
    `data-kind` = type,
    tags$input(
      type = type,
      class = "blockr-text-input",
      value = value,
      placeholder = placeholder,
      min = min,
      step = step,
      `aria-label` = label,
      autocomplete = "off",
      spellcheck = "false"
    )
  )
}

otl_text_field <- function(id, label, value = "", placeholder = NULL,
                           size = "large", ...) {
  otl_field(
    label,
    otl_text_input(id, label, value, placeholder, ...),
    size = size
  )
}

# Several lines: commits on blur, or Ctrl/Cmd+Enter.
otl_textarea_field <- function(id, label, value = "", rows = 3L,
                               placeholder = NULL) {
  otl_field(
    label,
    div(
      id = id,
      class = "blockr-otl-field",
      `data-kind` = "textarea",
      tags$textarea(
        class = "blockr-text-input blockr-otl-textarea",
        rows = rows,
        placeholder = placeholder,
        `aria-label` = label,
        value
      )
    ),
    size = "full"
  )
}

# Four or more values: Blockr.Select. `choices` is named by label, as in
# selectInput(); the list shows the labels, the input reports the values.
otl_select_field <- function(id, label, choices, selected) {
  otl_field(
    label,
    div(
      id = id,
      class = "blockr-otl-field",
      `data-kind` = "select",
      `data-options` = otl_options_json(choices),
      `data-value` = selected
    )
  )
}

otl_options_json <- function(choices) {

  labs <- names(choices)

  if (is.null(labs)) {
    labs <- rep("", length(choices))
  }

  q <- function(x) {
    paste0("\"", gsub("([\"\\\\])", "\\\\\\1", x), "\"")
  }

  paste0(
    "[",
    paste0(
      "{\"value\":", q(unname(choices)), ",\"label\":", q(labs), "}",
      collapse = ","
    ),
    "]"
  )
}

# Two or three short values. `choices` is named by label; `icons`, when
# given, draws icon-only segments whose names become their tooltips.
otl_segmented <- function(id, choices, selected, label, xs = FALSE,
                          icons = NULL) {

  labs <- names(choices)

  segs <- lapply(
    seq_along(choices),
    function(i) {
      val <- unname(choices[[i]])
      ico <- if (!is.null(icons)) icons[[val]]
      tags$button(
        type = "button",
        class = "blockr-segmented__seg",
        role = "radio",
        `data-value` = val,
        `aria-label` = if (!is.null(ico)) labs[[i]],
        `data-blockr-tooltip` = if (!is.null(ico)) labs[[i]],
        if (!is.null(ico)) HTML(ico) else labs[[i]]
      )
    }
  )

  div(
    id = id,
    class = paste(
      "blockr-otl-field blockr-segmented",
      if (xs) "blockr-segmented--xs"
    ),
    role = "radiogroup",
    `aria-label` = label,
    `data-kind` = "segmented",
    `data-value` = selected,
    segs
  )
}

# On or off: the box and its words, no field shell and no label row. The
# label names the "on" state.
otl_checkbox <- function(id, label, value = FALSE, size = "small") {
  otl_field(
    NULL,
    tags$label(
      id = id,
      class = "blockr-otl-field blockr-checkbox",
      `data-kind` = "checkbox",
      tags$input(type = "checkbox", checked = if (isTRUE(value)) NA),
      span(class = "blockr-checkbox__box", HTML(otl_check_svg())),
      span(class = "blockr-checkbox__label", label)
    ),
    size = size
  )
}

otl_check_svg <- function() {
  paste0(
    "<svg width=\"10\" height=\"10\" viewBox=\"0 0 16 16\" ",
    "fill=\"currentColor\" aria-hidden=\"true\"><path d=\"M13.854 3.646a.5.5 ",
    "0 0 1 0 .708l-7 7a.5.5 0 0 1-.708 0l-3.5-3.5a.5.5 0 1 1 .708-.708L6.5 ",
    "10.293l6.646-6.647a.5.5 0 0 1 .708 0\"/></svg>"
  )
}

# A name that renames in place: plain text at rest, a field on double-click
# or Enter. The server sets it (sendInputMessage); it reports on rename.
otl_name <- function(id, label, empty_msg, placeholder = "") {
  span(
    id = id,
    class = "blockr-otl-field blockr-otl-name",
    `data-kind` = "name",
    `data-empty` = empty_msg,
    `data-placeholder` = placeholder,
    `aria-label` = label,
    tabindex = "0"
  )
}

# ---- the gear and its tray ------------------------------------------------

# The gear: 26px, framed, gear last in the header row, tooltip "Settings".
# blockr-panels.js pairs it with the tray of id `tray`.
otl_gear <- function(tray) {
  tags$button(
    type = "button",
    class = "blockr-gear-btn",
    `data-blockr-tray` = tray,
    `aria-controls` = tray,
    `aria-label` = "Settings",
    `aria-expanded` = "false",
    `data-blockr-tooltip` = "Settings",
    HTML(otl_icon("gear"))
  )
}

# The tray: in flow under the header row, bg-subtle, a beak at the gear.
otl_tray <- function(id, ...) {
  div(
    id = id,
    class = "blockr-settings blockr-settings--beak",
    ...
  )
}

otl_section <- function(title, ...) {
  tagList(
    div(class = "blockr-settings__title", title),
    div(class = "blockr-settings__grid", ...)
  )
}

# ---- buttons --------------------------------------------------------------

# A button with words: main (the accent tint, one per view), secondary or
# quiet, at 26px (xs) or 30px (s). `action = TRUE` makes it an actionButton
# (Shiny binds .action-button; the label sits in .action-label so
# updateActionButton() can change it).
otl_button <- function(label, kind = c("secondary", "main", "quiet"),
                       size = c("xs", "s"), id = NULL, icon = NULL,
                       action = FALSE, ...) {

  kind <- match.arg(kind)
  size <- match.arg(size)

  tags$button(
    type = "button",
    id = id,
    class = paste(
      "blockr-otl-btn",
      paste0("blockr-otl-btn--", kind),
      paste0("blockr-otl-btn--", size),
      if (action) "action-button"
    ),
    ...,
    if (!is.null(icon)) span(class = "blockr-otl-btn__icon", HTML(icon)),
    span(class = "action-label", label)
  )
}

# ---- block list rows --------------------------------------------------------

# The parts of a row in a block list (design system, "Block lists"), shared
# by the report and the deck. The row itself is the caller's: number, mark,
# name, then the row's end.

# A block's name on its row: one line, cut with an ellipsis. It renames in
# place on a double-click (blockr.ui's `data-blockr-editable` gives it the
# tooltip, which leads with the whole name while the name is cut).
otl_row_name <- function(name) {
  span(
    class = "blockr-otl-row__name",
    `data-blockr-editable` = "",
    name
  )
}

# A 24px tool on a row's end. `pressed` makes it a pressed icon button: the
# accent tint while on.
otl_row_tool <- function(label, icon, class = NULL, pressed = NULL, ...) {
  tags$button(
    type = "button",
    class = paste(c("blockr-otl-rtool", class), collapse = " "),
    `aria-label` = label,
    `aria-pressed` = if (!is.null(pressed)) {
      if (isTRUE(pressed)) "true" else "false"
    },
    `data-blockr-tooltip` = label,
    ...,
    HTML(icon)
  )
}

# The row's "…": shown on hover and keyboard focus, last on the row. It
# opens the row's action menu, drawn by the panel's script.
otl_row_more <- function(...) {
  otl_row_tool(
    "Actions",
    otl_icon("dots"),
    class = "blockr-otl-row__more",
    `aria-haspopup` = "menu",
    ...
  )
}

# ---- block marks ----------------------------------------------------------

# A block's mark in a list row: 24px, its glyph in the category colour on an
# 18% tint of it (blockr.ui's menu mark, on the panel's surface).
otl_mark <- function(mark, ...) {

  if (is.null(mark) || !nzchar(coal(mark$icon, ""))) {
    return(span(class = "blockr-otl-mark", ...))
  }

  span(
    class = "blockr-otl-mark",
    style = paste0("--blockr-outline-mark: ", mark$color, ";"),
    ...,
    HTML(mark$icon)
  )
}

# What a block's mark needs, and its type's name: registry metadata, the
# same per class, so memoised per class. blockr.dock keeps blks_metadata()
# internal; read it the way block_icon_html() does.
block_mark_cache <- new.env(parent = emptyenv())

block_mark <- function(blk) {

  key <- paste(class(blk), collapse = "|")
  hit <- block_mark_cache[[key]]

  if (!is.null(hit)) {
    return(hit)
  }

  val <- tryCatch(
    {
      meta <- utils::getFromNamespace("blks_metadata", "blockr.dock")(blk)
      list(
        icon = as.character(meta$icon[[1L]]),
        color = as.character(meta$color[[1L]]),
        type = as.character(meta$name[[1L]])
      )
    },
    error = function(e) list(icon = "", color = "", type = "")
  )

  block_mark_cache[[key]] <- val

  val
}

# ---- icons ----------------------------------------------------------------

# The glyphs these panels draw, at 14px in currentColor: the gear is
# blockr.ui's Blockr.icons.gear, the rest follow its thin-stroke small icons.
otl_icon <- function(name) {

  svg <- function(body, fill = FALSE, width = "1.3") {
    paste0(
      "<svg width=\"14\" height=\"14\" viewBox=\"0 0 16 16\" ",
      if (fill) {
        "fill=\"currentColor\" "
      } else {
        paste0(
          "fill=\"none\" stroke=\"currentColor\" stroke-width=\"", width,
          "\" stroke-linecap=\"round\" stroke-linejoin=\"round\" "
        )
      },
      "aria-hidden=\"true\" focusable=\"false\">", body, "</svg>"
    )
  }

  switch(
    name,
    gear = svg(
      paste0(
        "<path d=\"M9.405 1.05c-.413-1.4-2.397-1.4-2.81 0l-.1.34a1.464 1.464 ",
        "0 0 1-2.105.872l-.31-.17c-1.283-.698-2.686.705-1.987 1.987l.169",
        ".311c.446.82.023 1.841-.872 2.105l-.34.1c-1.4.413-1.4 2.397 0 2.81l",
        ".34.1a1.464 1.464 0 0 1 .872 2.105l-.17.31c-.698 1.283.705 2.686 ",
        "1.987 1.987l.311-.169a1.464 1.464 0 0 1 2.105.872l.1.34c.413 1.4 ",
        "2.397 1.4 2.81 0l.1-.34a1.464 1.464 0 0 1 2.105-.872l.31.17c1.283",
        ".698 2.686-.705 1.987-1.987l-.169-.311a1.464 1.464 0 0 1 .872-2.105",
        "l.34-.1c1.4-.413 1.4-2.397 0-2.81l-.34-.1a1.464 1.464 0 0 1-.872-",
        "2.105l.17-.31c.698-1.283-.705-2.686-1.987-1.987l-.311.169a1.464 ",
        "1.464 0 0 1-2.105-.872zM8 10.93a2.929 2.929 0 1 1 0-5.86 2.929 ",
        "2.929 0 0 1 0 5.858z\"/>"
      ),
      fill = TRUE
    ),
    download = svg(
      paste0(
        "<path d=\"M8 1.8v7.4M5.2 6.4 8 9.2l2.8-2.8\"/>",
        "<path d=\"M2.6 10.6v2.2c0 .5.4.8.8.8h9.2c.5 0 .8-.3.8-.8v-2.2\"/>"
      ),
      width = "1.4"
    ),
    copy = svg(
      paste0(
        "<rect x=\"5.5\" y=\"1.8\" width=\"8.7\" height=\"8.7\" rx=\"1.4\"/>",
        "<path d=\"M10.5 12.7v.9a1.4 1.4 0 0 1-1.4 1.4H3.2a1.4 1.4 0 0 1-1.4",
        "-1.4V6.9a1.4 1.4 0 0 1 1.4-1.4h.9\"/>"
      )
    ),
    plus = svg("<path d=\"M8 3v10M3 8h10\"/>"),
    x = svg("<path d=\"M4 4l8 8M12 4l-8 8\"/>", width = "1.1"),
    dots = svg(
      paste0(
        "<circle cx=\"3\" cy=\"8\" r=\"1.25\"/><circle cx=\"8\" cy=\"8\" ",
        "r=\"1.25\"/><circle cx=\"13\" cy=\"8\" r=\"1.25\"/>"
      ),
      fill = TRUE
    ),
    code = svg(
      paste0(
        "<path d=\"M5 4.2 1.8 8 5 11.8M11 4.2 14.2 8 11 11.8\"/>",
        "<path d=\"M9.6 2.4 6.4 13.6\"/>"
      ),
      width = "1.4"
    ),
    eye = svg(
      paste0(
        "<path d=\"M1.2 8S3.8 3.2 8 3.2 14.8 8 14.8 8 12.2 12.8 8 12.8 1.2 8 ",
        "1.2 8z\"/><circle cx=\"8\" cy=\"8\" r=\"2.2\"/>"
      )
    ),
    eye_off = svg(
      paste0(
        "<path d=\"M6.4 3.4A7 7 0 0 1 8 3.2c4.2 0 6.8 4.8 6.8 4.8a12.7 12.7 0 ",
        "0 1-1.9 2.5M4.3 4.4C2.3 5.7 1.2 8 1.2 8s2.6 4.8 6.8 4.8c1.4 0 2.6",
        "-.5 3.6-1.2\"/><path d=\"M6.5 6.6a2.2 2.2 0 0 0 3 3\"/>",
        "<path d=\"M2 2l12 12\"/>"
      )
    ),
    text = svg(
      paste0(
        "<path d=\"M2.5 3.5h11M2.5 6.5h11M2.5 9.5h11M2.5 12.5h6.5\"/>"
      )
    ),
    stop("Unknown icon: ", name, call. = FALSE)
  )
}

# The rows of the "Add block" / "Add slide" menu, for the client: one entry
# per board block (mark, name, type) and the distinct glyphs, shared by key
# (icon_key_table()) because a board repeats each type's glyph per block.
board_catalog <- function(meta) {

  ids <- names(meta)
  marks <- lapply(ids, function(i) coal(meta[[i]]$mark, list()))
  tbl <- icon_key_table(
    chr_ply(marks, function(m) na_blank(coal(m$icon, "")))
  )

  list(
    items = lapply(
      seq_along(ids),
      function(k) {
        list(
          id = ids[[k]],
          name = coal(na_blank(meta[[ids[[k]]]]$name), ids[[k]]),
          type = coal(marks[[k]]$type, ""),
          color = coal(marks[[k]]$color, ""),
          icon_key = tbl$keys[[k]]
        )
      }
    ),
    icons = tbl$icons
  )
}
