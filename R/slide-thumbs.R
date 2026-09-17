# Layout schematics for the slide block's picker: the blockr.viz chart
# type-picker pattern (icon-over-label tiles, hand-drawn glyphs in
# currentColor-ish neutrals, dimmed under the label) with the glyph drawn
# FROM the layout's own slot rects -- a new layout costs no artwork, and the
# thumbnail cannot drift from the geometry because it IS the geometry.

# One layout as an inline SVG schematic. The viewBox is the slide's inches
# times ten, so every rect below is the geometry's numbers verbatim.
slide_layout_thumb <- function(layout) {

  spec <- slide_layout_spec(layout)
  fr <- slide_frame()
  sz <- slide_size()
  cw <- slide_content_width(fr)
  bare <- identical(spec$chrome, "none")

  acc <- new.env(parent = emptyenv())
  acc$rects <- list()
  add <- function(x, y, w, h, fill, rx = 1.5) {
    acc$rects[[length(acc$rects) + 1L]] <- sprintf(
      '<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" rx="%.1f" fill="%s"/>',
      x * 10, y * 10, w * 10, h * 10, rx, fill
    )
  }

  if (!bare) {
    add(fr$x, fr$title_y + 0.1, cw * 0.55, 0.42, "#374151")
    add(fr$x, fr$sub_y + 0.05, cw * 0.38, 0.26, "#d1d5db")
  }

  top <- if (bare) fr$body_top_bare else fr$body_top_sub
  body <- rect(fr$x, top, cw, fr$body_bottom - top)

  for (s in spec$build(body)) {
    r <- s$rect
    if (s$kind == "exhibit") {
      add(r$x, r$y, r$w, r$h, "#c7d2de")
    } else if (s$kind == "panelhead") {
      add(r$x, r$y + 0.08, r$w * 0.5, 0.2, "#374151")
    } else if (s$kind == "text") {
      add(r$x, r$y + 0.05, r$w * 0.94, 0.14, "#c3c8d0")
      add(r$x, r$y + 0.35, r$w * 0.72, 0.14, "#c3c8d0")
    } else if (s$kind == "callout") {
      add(r$x, r$y, r$w, r$h, "#9dc3dd")
    } else if (s$kind == "section") {
      add(r$x, r$y, r$w * 0.28, 0.22, "#0072b2")
      add(r$x, r$y + 0.5, r$w * 0.82, 0.62, "#374151")
      add(r$x, r$y + 1.4, r$w * 0.18, 0.12, "#0072b2")
    } else {
      # bullets / notes: ruled lines, alternating length
      lh <- 0.34
      n <- max(2L, min(5L, floor(r$h / lh / 1.9)))
      for (i in seq_len(n) - 1L) {
        add(r$x, r$y + i * lh * 1.9, r$w * if (i %% 2) 0.74 else 0.94,
            0.16, "#c3c8d0")
      }
    }
  }

  if (!bare) {
    add(fr$x, fr$foot_y + 0.04, cw * 0.3, 0.18, "#e5e7eb")
  }

  htmltools::HTML(paste0(
    '<svg class="slb-thumb" viewBox="0 0 ', sz[["w"]] * 10, " ",
    sz[["h"]] * 10, '" preserveAspectRatio="none" aria-hidden="true">',
    paste0(unlist(acc$rects), collapse = ""),
    "</svg>"
  ))
}

# The picker: a grid of tiles, one per layout, the chosen one marked. A
# plain Shiny input underneath (Shiny.setInputValue on click), so the block
# server's observeEvent(input$layout) is unchanged from the select it
# replaces. Styled after blockr.viz's .dd-type-tile (the chart type picker),
# with literal fallbacks so it renders standalone.
slide_layout_picker <- function(input_id, selected) {

  tiles <- lapply(names(slide_layouts()), function(id) {
    htmltools::tags$button(
      type = "button",
      class = paste0("slb-tile", if (identical(id, selected)) " slb-tile-active"),
      `data-value` = id,
      onclick = paste0(
        "var g=this.closest('.slb-layouts');",
        "g.querySelectorAll('.slb-tile').forEach(function(b){",
        "b.classList.remove('slb-tile-active')});",
        "this.classList.add('slb-tile-active');",
        # Show / hide the layout-dependent controls CLIENT-SIDE: they are
        # rendered once and only their visibility follows the layout, so a
        # switch never rebuilds the fields (no flash, focus survives, and
        # the Details value visibly stays -- which is the point of the
        # shared field).
        "var v=this.dataset.value,",
        "r=this.closest('.slb-root')||document;",
        "r.querySelectorAll('[data-layouts]').forEach(function(el){",
        "el.style.display=el.dataset.layouts.split(' ').indexOf(v)>=0",
        "?'':'none'});",
        "Shiny.setInputValue(g.dataset.input, this.dataset.value);"
      ),
      slide_layout_thumb(id),
      htmltools::tags$span(class = "slb-tile-label",
                           slide_layouts()[[id]]$name)
    )
  })

  htmltools::tagList(
    htmltools::tags$style(htmltools::HTML("
.slb-layouts { display: grid;
  grid-template-columns: repeat(auto-fill, minmax(104px, 1fr));
  gap: 6px; margin-bottom: 10px; }
.slb-tile { display: flex; flex-direction: column; gap: 5px;
  padding: 7px 7px 6px; background: #fff;
  border: 1px solid var(--blockr-color-border, #e5e7eb);
  border-radius: 6px; /* --blockr-radius-md */
  font: inherit; font-size: var(--blockr-font-size-xs, 0.75rem);
  color: var(--blockr-grey-500, #6b7280); cursor: pointer; text-align: left;
  transition: background 0.15s ease, border-color 0.15s ease; }
.slb-tile:hover { background: var(--blockr-grey-50, #f9fafb);
  border-color: var(--blockr-grey-300, #d1d5db); }
.slb-tile:focus-visible { outline: none;
  border-color: var(--blockr-color-primary, #2563eb);
  box-shadow: var(--blockr-focus-ring, 0 0 0 3px rgba(37,99,235,0.12)); }
.slb-tile-active, .slb-tile-active:hover {
  background: var(--blockr-blue-50, #eff6ff);
  border-color: var(--blockr-color-primary, #2563eb);
  color: var(--blockr-color-primary, #2563eb);
  font-weight: var(--blockr-font-weight-medium, 500); }
.slb-thumb { display: block; width: 100%; height: auto; aspect-ratio: 16/9;
  border: 1px solid var(--blockr-grey-100, #f3f4f6); border-radius: 3px;
  background: #fff; opacity: 0.75; }
.slb-tile-active .slb-thumb { opacity: 1;
  border-color: var(--blockr-blue-100, #dbeafe); }
.slb-tile-label { overflow: hidden; white-space: nowrap;
  text-overflow: ellipsis; }
.slb-meter { display: flex; align-items: center; gap: 8px;
  font-size: var(--blockr-font-size-xs, 0.75rem);
  color: var(--blockr-grey-500, #6b7280); }
.slb-cells { display: inline-flex; gap: 3px; }
.slb-cell { width: 14px; height: 10px; border-radius: 2px;
  border: 1px solid var(--blockr-grey-300, #d1d5db); background: #fff; }
.slb-cell--on { background: #0072b2; border-color: #0072b2; }
.slb-cell--over { background: var(--blockr-color-warning-bg, #fffbeb);
  border-color: var(--blockr-color-warning, #f59e0b); }
    ")),
    htmltools::div(
      class = "slb-layouts",
      `data-input` = input_id,
      tiles
    )
  )
}
