outline_dep <- function() {
  htmlDependency(
    "blockr-outline",
    pkg_version(),
    src = pkg_file("assets", "css"),
    stylesheet = c("blockr-outline.css", "syntax-highlight.css")
  )
}

# Delegated client logic: hover pairs a row's chip and section, clicks are
# routed by target -- switch = include-in-report toggle, arrows = reorder,
# pencil = edit description, anything else = reveal the block's dock panel
# (dag-node semantics). Delegation on document stays valid across renderUI
# re-renders; the script is part of the static extension UI and runs once.
outline_js <- function(ns) {
  tags$script(HTML(sprintf(
    "$(function() {
      var TOGGLE = '%s', OPEN = '%s', MOVE = '%s', EDIT = '%s';
      function rowOf(el) {
        return el.closest && el.closest('.blockr-otl-grow');
      }
      function cells(row) {
        return [
          row.querySelector('.blockr-otl-chip'),
          row.querySelector('.blockr-otl-sect')
        ];
      }
      document.addEventListener('mouseover', function(ev) {
        var row = rowOf(ev.target);
        if (!row) return;
        cells(row).forEach(function(c) { if (c) c.classList.add('hot'); });
      });
      document.addEventListener('mouseout', function(ev) {
        var row = rowOf(ev.target);
        if (!row) return;
        cells(row).forEach(function(c) { if (c) c.classList.remove('hot'); });
      });
      document.addEventListener('click', function(ev) {
        var row = rowOf(ev.target);
        if (!row) return;
        var id = row.dataset.blk;
        if (ev.target.closest('.blockr-otl-sw')) {
          Shiny.setInputValue(TOGGLE, {
            id: id, report: !row.classList.contains('on')
          }, {priority: 'event'});
          return;
        }
        var mv = ev.target.closest('.blockr-otl-mv');
        if (mv) {
          Shiny.setInputValue(MOVE, {
            id: id, dir: mv.dataset.dir
          }, {priority: 'event'});
          return;
        }
        if (ev.target.closest('.blockr-otl-pencil')) {
          Shiny.setInputValue(EDIT, {id: id}, {priority: 'event'});
          return;
        }
        if (ev.target.closest('.blockr-otl-editor')) return;
        Shiny.setInputValue(OPEN, {id: id}, {priority: 'event'});
      });
    });",
    ns("outline_toggle"),
    ns("outline_open"),
    ns("outline_move"),
    ns("outline_edit")
  )))
}

# The gutter outline: one grid where every block is a row of
# [gutter chip | code section]; stacks render as colored spines with
# chapter rows. `editing` holds the id of the block whose description is
# currently in edit mode (or NULL).
outline_tags <- function(sects, board, ns, editing = NULL) {

  stks <- blockr.core::board_stacks(board)

  accent_of <- function(stk_id) {
    if (is.na(stk_id) || !stk_id %in% names(stks)) {
      return("#9ca3af")
    }
    col <- tryCatch(
      blockr.dock::stack_color(stks[[stk_id]]),
      error = function(e) NULL
    )
    coal(col, "#2563eb")
  }

  runs <- rle(ifelse(is.na(sects$stack_ids), "", sects$stack_ids))
  starts <- cumsum(c(1L, head(runs$lengths, -1L)))

  run_labels <- character(length(runs$values))
  seen <- character()
  for (r in seq_along(runs$values)) {
    if (nzchar(runs$values[r])) {
      nme <- sects$stack_names[starts[r]]
      run_labels[r] <- if (runs$values[r] %in% seen) {
        paste(nme, "(continued)")
      } else {
        nme
      }
      seen <- c(seen, runs$values[r])
    }
  }

  chip_ui <- function(i) {
    div(
      class = "blockr-otl-chip",
      span(
        class = "blockr-otl-tile",
        toupper(substr(sects$names[i], 1L, 1L))
      ),
      span(class = "blockr-otl-rname", sects$names[i]),
      span(
        class = "blockr-otl-mvs",
        tags$button(
          class = "blockr-otl-mv", `data-dir` = "up",
          type = "button", title = "Move up", HTML("&#9650;")
        ),
        tags$button(
          class = "blockr-otl-mv", `data-dir` = "down",
          type = "button", title = "Move down", HTML("&#9660;")
        )
      ),
      span(class = "blockr-otl-sw")
    )
  }

  sect_ui <- function(i) {

    if (identical(editing, sects$ids[i])) {
      return(
        div(
          class = "blockr-otl-sect blockr-otl-editor",
          textAreaInput(
            ns("desc_edit"),
            label = NULL,
            value = sects$descriptions[i],
            rows = 5L,
            width = "100%",
            placeholder = "Block description (markdown)"
          ),
          div(
            class = "d-flex gap-2 justify-content-end",
            actionButton(
              ns("desc_cancel"), "Cancel",
              class = "btn-sm btn-light"
            ),
            actionButton(
              ns("desc_save"), "Save",
              class = "btn-sm btn-primary"
            )
          )
        )
      )
    }

    chunk <- paste(
      c(
        paste0(
          "#+ ", sects$ids[i],
          if (!sects$report[i]) ", include=FALSE"
        ),
        sects$code[i],
        if (sects$report[i]) sects$ids[i]
      ),
      collapse = "\n"
    )

    hl <- highlight_r_code(chunk)
    code_tag <- if (is.null(hl)) {
      tags$pre(chunk)
    } else {
      HTML(hl)
    }

    prose <- if (sects$report[i] && nzchar(sects$descriptions[i])) {
      div(
        class = "blockr-otl-prose",
        HTML(commonmark::markdown_html(sects$descriptions[i]))
      )
    }

    div(
      class = "blockr-otl-sect",
      tags$button(
        class = "blockr-otl-pencil",
        type = "button",
        title = "Edit description",
        icon("pen")
      ),
      if (!sects$report[i]) {
        span(
          class = "blockr-otl-offchip",
          "include=FALSE · runs, not shown"
        )
      },
      prose,
      code_tag
    )
  }

  grid_rows <- lapply(seq_along(runs$values), function(r) {

    idx <- seq(starts[r], length.out = runs$lengths[r])
    stk_id <- sects$stack_ids[idx[1L]]
    accent <- accent_of(stk_id)
    grouped <- !is.na(stk_id)

    chapter <- if (grouped) {
      tagList(
        div(
          class = "blockr-otl-chaphead",
          style = paste0("--accent: ", accent, ";"),
          span(
            class = "blockr-otl-gpill",
            paste(sum(sects$report[idx]), "in report")
          )
        ),
        div(
          class = "blockr-otl-chap",
          style = paste0("--accent: ", accent, ";"),
          span(class = "blockr-otl-gbar"),
          span(class = "blockr-otl-chlabel", run_labels[r])
        )
      )
    }

    rows <- lapply(seq_along(idx), function(j) {

      i <- idx[j]

      spine <- if (!grouped) {
        "nospine"
      } else if (length(idx) == 1L) {
        "spine-only"
      } else if (j == 1L) {
        "spine-start"
      } else if (j == length(idx)) {
        "spine-end"
      }

      div(
        class = paste(
          "blockr-otl-grow",
          if (sects$report[i]) "on"
        ),
        `data-blk` = sects$ids[i],
        style = paste0("--accent: ", accent, ";"),
        div(
          class = paste("blockr-otl-gutter", spine),
          chip_ui(i)
        ),
        div(class = "blockr-otl-gsect", sect_ui(i))
      )
    })

    tagList(chapter, rows)
  })

  div(class = "blockr-otl", grid_rows)
}
