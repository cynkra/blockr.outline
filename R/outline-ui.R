outline_dep <- function() {
  htmlDependency(
    "blockr-outline",
    pkg_version(),
    src = pkg_file("assets", "css"),
    stylesheet = c("blockr-outline.css", "syntax-highlight.css", "md-editor.css")
  )
}

# The Milkdown WYSIWYG markdown editor, vendored from blockr.md's
# feat/milkdown-editor prototype (commit d72f9e2; markdown stays canonical,
# auto-inits .blockr-md-editor[data-input-id] nodes via MutationObserver,
# debounced commit to the named Shiny input). Recorded follow-up: extract
# it as a shared markdown-input component consumed by blockr.md, the
# prose-block and this package, instead of three vendored copies.
md_editor_dep <- function() {
  htmlDependency(
    "blockr-outline-md-editor",
    pkg_version(),
    src = pkg_file("js"),
    script = "md-editor.js"
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
      var COLLAPSE = '%s', RENSTACK = '%s';
      var dragId = null;
      var collapseTimer = null;
      function inlineRename(holder, cur, commit) {
        if (holder.querySelector('input')) return;
        holder.innerHTML = '';
        var inp = document.createElement('input');
        inp.className = 'blockr-otl-rname-input';
        inp.value = cur;
        holder.appendChild(inp);
        inp.focus();
        inp.select();
        var done = false;
        function fin(save) {
          if (done) return;
          done = true;
          var val = inp.value.trim();
          holder.textContent = save && val ? val : cur;
          if (save && val && val !== cur) commit(val);
        }
        inp.addEventListener('keydown', function(e) {
          if (e.key === 'Enter') fin(true);
          if (e.key === 'Escape') fin(false);
          e.stopPropagation();
        });
        inp.addEventListener('blur', function() { fin(true); });
        inp.addEventListener('click', function(e) { e.stopPropagation(); });
      }
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
        if (name) {
          var row = rowOf(name);
          inlineRename(name, name.textContent, function(val) {
            Shiny.setInputValue(REN, {
              id: row.dataset.blk, name: val
            }, {priority: 'event'});
          });
          return;
        }
        var chl = ev.target.closest && ev.target.closest('.blockr-otl-chlabel');
        if (chl) {
          clearTimeout(collapseTimer);
          var chap = chl.closest('.blockr-otl-chap');
          // A split stack's later runs read 'Name (continued)'; rename
          // edits the plain name.
          var cur = chl.textContent.replace(/ \\(continued\\)$/, '');
          inlineRename(chl, cur, function(val) {
            Shiny.setInputValue(RENSTACK, {
              stack: chap.dataset.stack, name: val
            }, {priority: 'event'});
          });
        }
      });
      document.addEventListener('click', function(ev) {
        var chap = ev.target.closest && ev.target.closest('.blockr-otl-chap');
        if (chap) {
          if (ev.target.closest('.blockr-otl-rname-input')) return;
          if (ev.target.closest('.blockr-otl-chap-pencil')) {
            Shiny.setInputValue(EDIT, {
              id: 'stack:' + chap.dataset.stack
            }, {priority: 'event'});
            return;
          }
          // Delay so a double-click (rename) can cancel the collapse.
          clearTimeout(collapseTimer);
          collapseTimer = setTimeout(function() {
            Shiny.setInputValue(COLLAPSE, {
              stack: chap.dataset.stack
            }, {priority: 'event'});
          }, 250);
          return;
        }
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
    ns("outline_rename"),
    ns("outline_collapse"),
    ns("outline_rename_stack")
  )))
}

# The gutter outline: one grid where every block is a row of
# [gutter chip | code section]; stacks render as colored spines with
# chapter rows. `editing` holds the id of the block whose description is
# currently in edit mode (or NULL).
# Milkdown editor block shared by block descriptions and chapter intros.
# The element id carries a nonce because the bundle keeps an instance
# registry keyed by id (a reused id would be skipped).
desc_editor_ui <- function(ns, key, value) {
  div(
    class = "blockr-otl-sect blockr-otl-editor",
    div(
      id = ns(paste0(
        "desc_milkdown_", gsub("[^a-zA-Z0-9]", "_", key), "_",
        format(Sys.time(), "%H%M%OS3")
      )),
      class = "blockr-md-editor",
      `data-input-id` = ns("desc_edit"),
      `data-initial` = value
    ),
    div(
      class = "d-flex gap-2 justify-content-end mt-2",
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
}

outline_tags <- function(sects, ns, editing = NULL, collapsed = character()) {

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
      return(desc_editor_ui(ns, sects$ids[i], sects$descriptions[i]))
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
    is_collapsed <- grouped && stk_id %in% collapsed
    continued <- grepl("\\(continued\\)$", coal(run_labels[r], ""))

    chapter <- if (grouped) {
      div(
        class = "blockr-otl-chap",
        `data-stack` = stk_id,
        style = paste0("--accent: ", accent, ";"),
        span(
          class = "blockr-otl-caret",
          if (is_collapsed) HTML("&#9656;") else HTML("&#9662;")
        ),
        span(class = "blockr-otl-gbar"),
        span(class = "blockr-otl-chlabel", run_labels[r]),
        if (is_collapsed) {
          span(
            class = "blockr-otl-collapsed-count",
            paste(length(idx), "blocks")
          )
        },
        tags$button(
          class = "blockr-otl-pencil blockr-otl-chap-pencil",
          type = "button",
          title = "Edit chapter intro",
          icon("pen")
        )
      )
    }

    stack_desc <- if (grouped) {
      coal(sects$stack_descriptions[[stk_id]], "")
    } else {
      ""
    }

    intro <- if (grouped && !continued && !is_collapsed) {
      if (identical(editing, paste0("stack:", stk_id))) {
        div(
          class = "blockr-otl-gsect",
          desc_editor_ui(ns, paste0("stack:", stk_id), stack_desc)
        )
      } else if (nzchar(stack_desc)) {
        div(
          class = "blockr-otl-gsect",
          div(
            class = "blockr-otl-prose blockr-otl-chapintro",
            HTML(commonmark::markdown_html(stack_desc))
          )
        )
      }
    }

    if (is_collapsed) {
      return(tagList(chapter))
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

    tagList(chapter, intro, rows)
  })

  div(class = "blockr-otl", grid_rows)
}
