outline_dep <- function() {
  htmlDependency(
    "blockr-outline",
    pkg_version(),
    src = pkg_file("assets", "css"),
    stylesheet = c("blockr-outline.css", "syntax-highlight.css")
  )
}

# Delegated client logic: hover pairs a row's chip and section; clicks are
# routed by target -- switch = include-in-report toggle, pencil = edit
# description, anything else = reveal the block's dock panel (dag-node
# semantics). Chips are draggable to reorder (drop before a target row;
# drop on the last row's lower half appends). Double-clicking a block name
# turns it into an inline rename input committing on Enter/blur (Escape
# cancels), matching the text-commit decision record. Delegation on
# document stays valid across renderUI re-renders; the script is part of
# the static extension UI and runs once.
outline_js <- function(ns) {
  tags$script(HTML(sprintf(
    "$(function() {
      var TOGGLE = '%s', OPEN = '%s', MOVE = '%s', EDIT = '%s', REN = '%s';
      var dragId = null;
      function rowOf(el) {
        return el.closest && el.closest('.blockr-otl-grow');
      }
      function cells(row) {
        return [
          row.querySelector('.blockr-otl-chip'),
          row.querySelector('.blockr-otl-sect')
        ];
      }
      function clearDrop() {
        document.querySelectorAll('.blockr-otl-grow.drop-before, ' +
          '.blockr-otl-grow.drop-after').forEach(function(r) {
          r.classList.remove('drop-before', 'drop-after');
        });
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
      document.addEventListener('dragstart', function(ev) {
        var chip = ev.target.closest && ev.target.closest('.blockr-otl-chip');
        if (!chip) return;
        dragId = rowOf(chip).dataset.blk;
        ev.dataTransfer.effectAllowed = 'move';
        ev.dataTransfer.setData('text/plain', dragId);
      });
      document.addEventListener('dragover', function(ev) {
        var row = rowOf(ev.target);
        if (!row || !dragId) return;
        ev.preventDefault();
        clearDrop();
        var r = row.querySelector('.blockr-otl-gutter').getBoundingClientRect();
        var below = ev.clientY > r.top + r.height / 2;
        row.classList.add(below ? 'drop-after' : 'drop-before');
      });
      document.addEventListener('drop', function(ev) {
        var row = rowOf(ev.target);
        clearDrop();
        if (!row || !dragId) return;
        ev.preventDefault();
        var r = row.querySelector('.blockr-otl-gutter').getBoundingClientRect();
        var below = ev.clientY > r.top + r.height / 2;
        Shiny.setInputValue(MOVE, {
          id: dragId, target: row.dataset.blk, after: below
        }, {priority: 'event'});
        dragId = null;
      });
      document.addEventListener('dragend', function() {
        clearDrop();
        dragId = null;
      });
      document.addEventListener('dblclick', function(ev) {
        var name = ev.target.closest && ev.target.closest('.blockr-otl-rname');
        if (!name || name.querySelector('input')) return;
        var row = rowOf(name);
        var cur = name.textContent;
        name.innerHTML = '';
        var inp = document.createElement('input');
        inp.className = 'blockr-otl-rname-input';
        inp.value = cur;
        name.appendChild(inp);
        inp.focus();
        inp.select();
        var done = false;
        function commit(save) {
          if (done) return;
          done = true;
          var val = inp.value.trim();
          name.textContent = save && val ? val : cur;
          if (save && val && val !== cur) {
            Shiny.setInputValue(REN, {
              id: row.dataset.blk, name: val
            }, {priority: 'event'});
          }
        }
        inp.addEventListener('keydown', function(e) {
          if (e.key === 'Enter') commit(true);
          if (e.key === 'Escape') commit(false);
          e.stopPropagation();
        });
        inp.addEventListener('blur', function() { commit(true); });
        inp.addEventListener('click', function(e) { e.stopPropagation(); });
      });
      document.addEventListener('click', function(ev) {
        var row = rowOf(ev.target);
        if (!row) return;
        if (ev.target.closest('.blockr-otl-rname-input')) return;
        var id = row.dataset.blk;
        if (ev.target.closest('.blockr-otl-sw')) {
          Shiny.setInputValue(TOGGLE, {
            id: id, report: !row.classList.contains('on')
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
    ns("outline_edit"),
    ns("outline_rename")
  )))
}

# The gutter outline: one grid where every block is a row of
# [gutter chip | code section]; stacks render as colored spines with
# chapter rows. `editing` holds the id of the block whose description is
# currently in edit mode (or NULL).
outline_tags <- function(sects, ns, editing = NULL) {

  accent_of <- function(stk_id) {
    if (is.na(stk_id) || !stk_id %in% names(sects$stack_colors)) {
      return("#9ca3af")
    }
    sects$stack_colors[[stk_id]]
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

    tile <- if (is.na(sects$icons[i])) {
      toupper(substr(sects$names[i], 1L, 1L))
    } else {
      HTML(sects$icons[i])
    }

    div(
      class = "blockr-otl-chip",
      draggable = "true",
      title = "Drag to reorder · double-click the name to rename",
      span(class = "blockr-otl-tile", tile),
      span(class = "blockr-otl-rname", sects$names[i]),
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
      div(
        class = "blockr-otl-chap",
        style = paste0("--accent: ", accent, ";"),
        span(class = "blockr-otl-gbar"),
        span(class = "blockr-otl-chlabel", run_labels[r])
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
