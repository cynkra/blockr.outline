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
  # The id constants are the only dynamic part; the body stays a plain
  # string (sprintf caps its format at 8192 chars).
  consts <- sprintf(
    paste0(
      "var TOGGLE = '%s', OPEN = '%s', MOVE = '%s', EDIT = '%s', ",
      "REN = '%s', RENSTACK = '%s', SAVE = '%s', CANCEL = '%s', ",
      "ADD = '%s';"
    ),
    ns("outline_toggle"),
    ns("outline_open"),
    ns("outline_move"),
    ns("outline_edit"),
    ns("outline_rename"),
    ns("outline_rename_stack"),
    ns("desc_save"),
    ns("desc_cancel"),
    ns("outline_add")
  )

  tags$script(HTML(paste0(
    "$(function() {",
    consts,
    "
      var dragId = null;
      var collapseTimer = null;
      var openTimer = null;
      // Collapse is CLIENT-ONLY: no server round trip, no redraw. The set
      // survives renderUI re-renders via the shiny:value re-apply below.
      var collapsedStacks = new Set();
      function applyCollapsed() {
        document.querySelectorAll('.blockr-otl-chap[data-stack]')
          .forEach(function(ch) {
            ch.classList.toggle(
              'collapsed', collapsedStacks.has(ch.dataset.stack)
            );
          });
        document.querySelectorAll(
          '.blockr-otl-grow[data-stack], .blockr-otl-introrow[data-stack]'
        ).forEach(function(el) {
          el.classList.toggle(
            'blockr-otl-hidden', collapsedStacks.has(el.dataset.stack)
          );
        });
      }
      $(document).on('shiny:value', function(ev) {
        if (ev.name && /outline_out$/.test(ev.name)) {
          setTimeout(applyCollapsed, 0);
        }
      });
      function fire(id) {
        Shiny.setInputValue(id, Math.random(), {priority: 'event'});
      }
      function openEditor() {
        return document.querySelector('.blockr-otl-editor');
      }
      // Dirty tracking: any edit inside the editor shows the Enter chip.
      // ProseMirror handles some edits through beforeinput/transactions, so
      // key/paste/cut are tracked alongside plain input events.
      function markDirty(ev) {
        var ed = ev.target.closest && ev.target.closest('.blockr-otl-editor');
        if (ed) ed.classList.add('dirty');
      }
      document.addEventListener('input', markDirty);
      document.addEventListener('paste', markDirty, true);
      document.addEventListener('cut', markDirty, true);
      document.addEventListener('keydown', function(ev) {
        if (ev.key && (ev.key.length === 1 ||
            ev.key === 'Backspace' || ev.key === 'Delete' ||
            ev.key === 'Enter')) {
          markDirty(ev);
        }
      }, true);
      document.addEventListener('keydown', function(ev) {
        if (ev.key !== 'Escape') return;
        var ed = ev.target.closest && ev.target.closest('.blockr-otl-editor');
        if (ed) {
          ev.stopPropagation();
          fire(CANCEL);
        }
      }, true);
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
      // Legal landing spots: the server hands each block the gap range it
      // may occupy (after its last ancestor, before its first descendant).
      // On dragstart every legal gap is marked, so the drag shows where it
      // is allowed to go; illegal targets refuse the drop outright.
      var legal = {before: new Set(), after: new Set()};
      function rowsInOrder() {
        return [].slice.call(
          document.querySelectorAll('.blockr-otl-grow[data-blk]')
        );
      }
      function computeLegal() {
        legal = {before: new Set(), after: new Set()};
        var rows = rowsInOrder();
        var origPos = rows.findIndex(function(r) {
          return r.dataset.blk === dragId;
        });
        var rest = rows.filter(function(r) { return r.dataset.blk !== dragId; });
        var src = rows[origPos];
        var lo = parseInt(src.dataset.droplo, 10);
        var hi = parseInt(src.dataset.drophi, 10);
        rest.forEach(function(r, k) {
          // gap k = before this row, gap k+1 = after it; origPos is the
          // no-op gap (the block's current place).
          if (k >= lo && k <= hi && k !== origPos) {
            legal.before.add(r.dataset.blk);
          }
          if (k + 1 >= lo && k + 1 <= hi && k + 1 !== origPos) {
            legal.after.add(r.dataset.blk);
          }
        });
        rest.forEach(function(r) {
          if (legal.before.has(r.dataset.blk) || legal.after.has(r.dataset.blk)) {
            r.classList.add('drop-legal');
          }
        });
      }
      function clearLegal() {
        document.querySelectorAll('.blockr-otl-grow.drop-legal')
          .forEach(function(r) { r.classList.remove('drop-legal'); });
      }
      function dropSide(row, ev) {
        var r = row.querySelector('.blockr-otl-gutter').getBoundingClientRect();
        var below = ev.clientY > r.top + r.height / 2;
        var id = row.dataset.blk;
        if (below && legal.after.has(id)) return 'after';
        if (!below && legal.before.has(id)) return 'before';
        // Fall back to the other side of the same row when only it is
        // legal, so a near-miss still lands.
        if (legal.after.has(id)) return 'after';
        if (legal.before.has(id)) return 'before';
        return null;
      }
      document.addEventListener('dragstart', function(ev) {
        var grip = ev.target.closest && ev.target.closest('.blockr-otl-grip');
        if (!grip) return;
        dragId = rowOf(grip).dataset.blk;
        ev.dataTransfer.effectAllowed = 'move';
        ev.dataTransfer.setData('text/plain', dragId);
        computeLegal();
      });
      document.addEventListener('dragover', function(ev) {
        var row = rowOf(ev.target);
        if (!row || !dragId) return;
        clearDrop();
        var side = dropSide(row, ev);
        if (!side) return;
        ev.preventDefault();
        row.classList.add(side === 'after' ? 'drop-after' : 'drop-before');
      });
      document.addEventListener('drop', function(ev) {
        var row = rowOf(ev.target);
        clearDrop();
        if (!row || !dragId) return;
        var side = dropSide(row, ev);
        if (!side) return;
        ev.preventDefault();
        Shiny.setInputValue(MOVE, {
          id: dragId, target: row.dataset.blk, after: side === 'after'
        }, {priority: 'event'});
        dragId = null;
        clearLegal();
      });
      document.addEventListener('dragend', function() {
        clearDrop();
        clearLegal();
        dragId = null;
      });
      document.addEventListener('dblclick', function(ev) {
        clearTimeout(openTimer);
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
          return;
        }
        if (ev.target.closest('.blockr-otl-editor')) return;
        var intro = ev.target.closest &&
          ev.target.closest('.blockr-otl-chapintro');
        if (intro) {
          Shiny.setInputValue(EDIT, {
            id: 'stack:' + intro.dataset.stack
          }, {priority: 'event'});
          return;
        }
        var sect = ev.target.closest && ev.target.closest('.blockr-otl-sect');
        if (sect) {
          var row = rowOf(sect);
          if (row) {
            Shiny.setInputValue(EDIT, {
              id: row.dataset.blk
            }, {priority: 'event'});
          }
        }
      });
      document.addEventListener('click', function(ev) {
        var add = ev.target.closest && ev.target.closest('.blockr-otl-addrow');
        if (add) {
          Shiny.setInputValue(ADD, {
            id: add.dataset.blk
          }, {priority: 'event'});
          return;
        }
        // Clicking outside an open editor commits (text-commit
        // convention: blur applies); a pristine editor just closes.
        var ed = openEditor();
        if (ed && !ed.contains(ev.target)) {
          fire(ed.classList.contains('dirty') ? SAVE : CANCEL);
          return;
        }
        var chap = ev.target.closest && ev.target.closest('.blockr-otl-chap');
        if (chap) {
          if (ev.target.closest('.blockr-otl-rname-input')) return;
          // Delay so a double-click (rename) can cancel the collapse.
          clearTimeout(collapseTimer);
          collapseTimer = setTimeout(function() {
            var s = chap.dataset.stack;
            if (collapsedStacks.has(s)) collapsedStacks.delete(s);
            else collapsedStacks.add(s);
            applyCollapsed();
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
        if (ev.target.closest('.blockr-otl-editor')) return;
        // Delay so a double-click (edit / rename) can cancel the open.
        clearTimeout(openTimer);
        openTimer = setTimeout(function() {
          Shiny.setInputValue(OPEN, {id: id}, {priority: 'event'});
        }, 250);
      });
    });"
  )))
}

# The gutter outline: one grid where every block is a row of
# [gutter chip | code section]; stacks render as colored spines with
# chapter rows. `editing` holds the id of the block whose description is
# currently in edit mode (or NULL).
# Milkdown editor block shared by block descriptions and chapter intros.
# Edit-mode chrome matches the inline rename inputs: a small border, no
# wash. Clicking outside commits (blur applies, the way every text field
# in blockr commits), Escape discards. No buttons, no raw source view
# (the R script / Document views show the markdown). The element id
# carries a nonce because the bundle keeps an instance registry keyed by
# id (a reused id would be skipped).
desc_editor_ui <- function(ns, key, value) {
  div(
    class = "blockr-otl-sect blockr-otl-editor",
    title = "Click outside to apply; Esc discards",
    div(
      id = ns(paste0(
        "desc_milkdown_", gsub("[^a-zA-Z0-9]", "_", key), "_",
        format(Sys.time(), "%H%M%OS3")
      )),
      class = "blockr-md-editor",
      `data-input-id` = ns("desc_edit"),
      `data-initial` = value
    )
  )
}

# Collapse chevron, blockr.viz structured-table style (html-table.R
# section_chevron_svg): rotation is purely CSS off the chapter's
# .collapsed class; collapsing itself is client-side only.
outline_chevron <- function() {
  HTML(paste0(
    "<svg class=\"blockr-otl-chev\" viewBox=\"0 0 24 24\" fill=\"none\" ",
    "stroke=\"currentColor\" stroke-width=\"2.4\" stroke-linecap=\"round\" ",
    "stroke-linejoin=\"round\" aria-hidden=\"true\">",
    "<path d=\"M6 9l6 6 6-6\"/></svg>"
  ))
}

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
      title = "Double-click the name to rename",
      span(class = "blockr-otl-tile", tile),
      span(class = "blockr-otl-rname", sects$names[i]),
      span(class = "blockr-otl-sw"),
      if (sects$movable[i]) span(
        class = "blockr-otl-grip",
        draggable = "true",
        title = "Drag to reorder",
        HTML(paste0(
          "<svg viewBox=\"0 0 10 16\" width=\"8\" height=\"13\" ",
          "fill=\"currentColor\" aria-hidden=\"true\">",
          "<circle cx=\"2.5\" cy=\"3\" r=\"1.3\"/>",
          "<circle cx=\"7.5\" cy=\"3\" r=\"1.3\"/>",
          "<circle cx=\"2.5\" cy=\"8\" r=\"1.3\"/>",
          "<circle cx=\"7.5\" cy=\"8\" r=\"1.3\"/>",
          "<circle cx=\"2.5\" cy=\"13\" r=\"1.3\"/>",
          "<circle cx=\"7.5\" cy=\"13\" r=\"1.3\"/></svg>"
        ))
      )
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
      title = "Double-click to edit the description",
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
    continued <- grepl("\\(continued\\)$", coal(run_labels[r], ""))

    # Chapter heading row: the stack's thread (decision: document-styling
    # variant B, revised) starts at the chapter's gutter cell and runs as
    # ONE line down to the run's last block. Collapsed chapters show the
    # hidden blocks' icons inline (pre-rendered, CSS-revealed).
    chapter <- if (grouped) {
      tagList(
        div(
          class = "blockr-otl-gutter blockr-otl-chapgutter",
          style = paste0("--accent: ", accent, ";")
        ),
        div(
          class = "blockr-otl-chap",
          `data-stack` = stk_id,
          style = paste0("--accent: ", accent, ";"),
          span(class = "blockr-otl-chevwrap", outline_chevron()),
          span(class = "blockr-otl-chlabel", run_labels[r]),
          span(
            class = "blockr-otl-chapicons",
            lapply(idx, function(i) {
              span(
                class = "blockr-otl-minitile",
                if (is.na(sects$icons[i])) {
                  toupper(substr(sects$names[i], 1L, 1L))
                } else {
                  HTML(sects$icons[i])
                }
              )
            })
          )
        )
      )
    }

    stack_desc <- if (grouped) {
      coal(sects$stack_descriptions[[stk_id]], "")
    } else {
      ""
    }

    # Chapter intro: plain body prose, exactly what quarto renders (no
    # standfirst, no tint). The gutter cell continues the thread.
    intro <- if (grouped && !continued) {
      tagList(
        div(
          class = "blockr-otl-gutter blockr-otl-introrow",
          `data-stack` = stk_id,
          style = paste0("--accent: ", accent, ";")
        ),
        if (identical(editing, paste0("stack:", stk_id))) {
          div(
            class = "blockr-otl-gsect blockr-otl-introrow",
            `data-stack` = stk_id,
            desc_editor_ui(ns, paste0("stack:", stk_id), stack_desc)
          )
        } else {
          # Always rendered (placeholder when empty) so there is a
          # double-click target for the chapter intro.
          div(
            class = "blockr-otl-gsect blockr-otl-introrow",
            `data-stack` = stk_id,
            div(
              class = "blockr-otl-sect blockr-otl-chapintro",
              `data-stack` = stk_id,
              title = "Double-click to edit the chapter intro",
              div(
                class = "blockr-otl-prose",
                if (nzchar(stack_desc)) {
                  HTML(commonmark::markdown_html(stack_desc))
                } else {
                  span(
                    class = "blockr-otl-placeholder",
                    "Chapter intro (double-click to add)"
                  )
                }
              )
            )
          )
        }
      )
    }

    rows <- lapply(seq_along(idx), function(j) {

      i <- idx[j]

      # The thread starts at the chapter's gutter cell, so block rows only
      # continue it; the run's last block closes it.
      spine <- if (!grouped) {
        "nospine"
      } else if (j == length(idx)) {
        "spine-end"
      }

      # Insert affordance (decision: outline-insert-proposals.html variant
      # A, revised -- revealed on the block's own hover and placed BELOW
      # it, styled like blockr.dplyr's add links). Appends after this
      # block through the dock's own append flow.
      add_link <- div(
        class = "blockr-otl-addrow",
        `data-blk` = sects$ids[i],
        span(
          class = "blockr-otl-addlink",
          span(class = "blockr-otl-addicon", HTML(
            paste0(
              "<svg viewBox=\"0 0 16 16\" width=\"12\" height=\"12\" ",
              "fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" ",
              "stroke-linecap=\"round\"><path d=\"M8 3v10M3 8h10\"/></svg>"
            )
          )),
          "Add block"
        )
      )

      div(
        class = paste(
          "blockr-otl-grow",
          if (sects$report[i]) "on"
        ),
        `data-blk` = sects$ids[i],
        `data-stack` = if (grouped) stk_id,
        `data-droplo` = sects$drop_lo[i],
        `data-drophi` = sects$drop_hi[i],
        style = paste0("--accent: ", accent, ";"),
        div(
          class = paste("blockr-otl-gutter", spine),
          chip_ui(i)
        ),
        div(class = "blockr-otl-gsect", sect_ui(i), add_link)
      )
    })

    tagList(chapter, intro, rows)
  })

  div(class = "blockr-otl", grid_rows)
}
