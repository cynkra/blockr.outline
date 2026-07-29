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
      "ADD = '%s', RM = '%s', CHAP = '%s', NEWCHAP = '%s', ",
      "TOSTACK = '%s', MOVECHAP = '%s', GEAR = '%s', SETTINGS = '%s', ",
      "PANEL = '%s', VISIBLE = '%s', BULK = '%s', RENTITLE = '%s', ",
      "HIDE = '%s';"
    ),
    ns("outline_toggle"),
    ns("outline_open"),
    ns("outline_move"),
    ns("outline_edit"),
    ns("outline_rename"),
    ns("outline_rename_stack"),
    ns("desc_save"),
    ns("desc_cancel"),
    ns("outline_add"),
    ns("outline_rm"),
    ns("outline_chapter"),
    ns("outline_newchapter"),
    ns("outline_tostack"),
    ns("outline_movechap"),
    ns("otl_gear"),
    ns("otl_settings"),
    ns("otl_panel"),
    ns("otl_visible"),
    ns("outline_bulk"),
    ns("outline_rename_title"),
    ns("outline_hide")
  )

  tags$script(HTML(paste0(
    "$(function() {",
    consts,
    "
      // Incremental code updates: editing a block value changes only the
      // generated code, never the outline's structure, so the server
      // pushes the changed chunks instead of re-rendering the whole
      // outline. Keeps the DOM (and any open editor, scroll and hover
      // state) intact. Structural changes still go through renderUI.
      // Latest code markup per node id. Held client-side because a push
      // can arrive before the node it targets exists: at startup the
      // server projects again as blocks report in, and that second push
      // can beat the first renderUI insert. Applying blind would drop it
      // silently with nothing to re-send it, leaving cells stuck on
      // their placeholder. Same re-apply pattern as collapsedStacks.
      var codeById = {};
      function applyCode() {
        Object.keys(codeById).forEach(function(id) {
          var el = document.getElementById(id);
          if (!el) return;
          // Output mode: the cell holds an exhibit painted by renderUI
          // (class set server-side), not code -- applying the cached code
          // markup would stomp it. Pushes still land in codeById, so the
          // cache stays current and flipping back to Code replays cleanly.
          if (el.classList.contains('blockr-otl-outwrap')) return;
          if (el.innerHTML !== codeById[id]) el.innerHTML = codeById[id];
        });
      }
      Shiny.addCustomMessageHandler('blockr-outline-code', function(msg) {
        (msg.items || []).forEach(function(it) { codeById[it.id] = it.html; });
        applyCode();
      });

      // Fire the real download. The visible Download button is an action
      // button: on a deferred board the server first has to construct the
      // reported blocks and wait for their code, so the browser-initiated
      // GET a plain download button issues would race the render. The
      // server sends this message once the document is complete.
      //
      // Not a.click(): the shiny-download-link is target=_blank, and by
      // the time this message arrives (a websocket round trip later) the
      // user activation from the button click has expired -- browsers
      // popup-block the programmatic _blank click and the download dies
      // silently. Navigate a same-tab throwaway anchor instead. No
      // download attribute: it would override the handler's
      // Content-Disposition filename with one derived from the URL; the
      // attachment disposition alone already makes this a download, not
      // a navigation.
      Shiny.addCustomMessageHandler('blockr-outline-download', function(msg) {
        var a = document.getElementById(msg.id);
        if (!a) return;
        var href = a.getAttribute('href');
        if (!href || href === '#') return;
        var tmp = document.createElement('a');
        tmp.href = a.href;
        document.body.appendChild(tmp);
        tmp.click();
        tmp.remove();
      });

      // Report whether the outline panel is on screen, so the server can
      // gate its (O(n^2)) projection: idle while the panel is closed, live
      // when it is shown. The dock parks an unshown extension in an
      // offcanvas that hides it with `visibility: hidden` -- NOT
      // display:none -- and moves the DOM into a dock panel when shown. So
      // IntersectionObserver is no use (it ignores visibility:hidden); the
      // reliable test is the visibility CSS property plus a client-rect
      // check (which also catches display:none / detached). A light poll
      // (cheap getComputedStyle, only pushes the input on CHANGE) covers
      // the offcanvas move, dockview tab switches and view changes alike.
      // Seeded server-side as TRUE, so a broken poll degrades to the old
      // always-on cost rather than a blank panel.
      (function() {
        var last = null;
        function tick() {
          // Guard against Shiny not being initialised yet: the immediate
          // tick() below runs at DOM ready, which can beat Shiny's own
          // ready handler, so Shiny.setInputValue is not a function yet.
          // Calling it there threw and aborted the whole outline_js body,
          // taking every handler defined AFTER this poll (gear, drag, and
          // the OPEN / ADD / chapter click handlers) with it -- the outline
          // rendered but was completely inert. Skip until Shiny is up; the
          // setInterval keeps trying.
          if (!(window.Shiny && Shiny.setInputValue)) return;
          // Resolve the panel inside the tick, not once at setup: at DOM
          // ready the dock may not have mounted / moved the extension node
          // yet, so a setup-time lookup can miss it and never recover. A
          // per-tick lookup self-heals.
          var panel = document.getElementById(PANEL);
          if (!panel) return;
          var vis = getComputedStyle(panel).visibility !== 'hidden' &&
                    panel.getClientRects().length > 0;
          if (vis === last) return;
          last = vis;
          Shiny.setInputValue(VISIBLE, vis, {priority: 'event'});
        }
        setInterval(tick, 400);
        tick();
      })();

      // Gear toggles the settings band. Client-only, like the collapse:
      // opening a settings panel is not board state, so no round trip.
      var gear = document.getElementById(GEAR);
      var settings = document.getElementById(SETTINGS);
      if (gear && settings) {
        gear.addEventListener('click', function() {
          var open = settings.classList.toggle('blockr-settings--open');
          gear.classList.toggle('blockr-gear-active', open);
        });
      }

      var dragId = null;
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
      // Instant search: CLIENT-ONLY filter over the outline. Matches a
      // block's name / description / code and its chapter name; non-matching
      // rows get 'blockr-otl-filtered' (display:none). While a query is
      // active collapse is overridden so matches inside a collapsed chapter
      // still show; clearing the box restores the collapsed state. Like
      // collapse, it re-applies after every renderUI via shiny:value.
      function applyFilter() {
        var input = document.querySelector('.blockr-otl-searchinput');
        var q = (input && input.value ? input.value : '').trim().toLowerCase();

        var box = input && input.closest('.blockr-otl-search');
        if (box) box.classList.toggle('has-value', !!(input && input.value));

        if (!q) {
          document.querySelectorAll('.blockr-otl-filtered').forEach(
            function(el) { el.classList.remove('blockr-otl-filtered'); });
          document.querySelectorAll('.blockr-otl-addrow').forEach(
            function(el) { el.classList.remove('blockr-otl-filtered'); });
          document.body.classList.remove('blockr-otl-filtering');
          applyCollapsed();          // restore collapsed chapters
          return;
        }

        document.body.classList.add('blockr-otl-filtering');
        // Filtering overrides collapse so matches are always visible.
        document.querySelectorAll('.blockr-otl-hidden').forEach(
          function(el) { el.classList.remove('blockr-otl-hidden'); });

        var stackHit = {};
        document.querySelectorAll('.blockr-otl-grow[data-blk]').forEach(
          function(row) {
            var hit = (row.textContent || '').toLowerCase().indexOf(q) !== -1;
            row.classList.toggle('blockr-otl-filtered', !hit);
            if (hit && row.dataset.stack) stackHit[row.dataset.stack] = true;
          });

        // Add rows are chrome; hide them while searching.
        document.querySelectorAll('.blockr-otl-addrow').forEach(
          function(el) { el.classList.add('blockr-otl-filtered'); });

        // A chapter shows if any member matched or its own name matches;
        // a name match reveals all of its blocks.
        document.querySelectorAll('.blockr-otl-chap[data-stack]').forEach(
          function(ch) {
            var s = ch.dataset.stack;
            var lbl = (ch.querySelector('.blockr-otl-chlabel') || {}).textContent || '';
            var nameHit = lbl.toLowerCase().indexOf(q) !== -1;
            var show = nameHit || stackHit[s];
            ch.classList.toggle('blockr-otl-filtered', !show);
            if (nameHit) {
              document.querySelectorAll(
                '.blockr-otl-grow[data-stack=\\\"' + s + '\\\"]'
              ).forEach(function(row) {
                row.classList.remove('blockr-otl-filtered');
              });
            }
            var intro = document.querySelector(
              '.blockr-otl-introrow[data-stack=\\\"' + s + '\\\"]'
            );
            if (intro) intro.classList.toggle('blockr-otl-filtered', !show);
          });
      }
      document.addEventListener('input', function(ev) {
        if (ev.target && ev.target.classList &&
            ev.target.classList.contains('blockr-otl-searchinput')) {
          applyFilter();
        }
      });
      document.addEventListener('click', function(ev) {
        var clr = ev.target.closest &&
          ev.target.closest('.blockr-otl-searchclear');
        if (clr) {
          var inp = document.querySelector('.blockr-otl-searchinput');
          if (inp) { inp.value = ''; applyFilter(); inp.focus(); }
        }
      });
      document.addEventListener('keydown', function(ev) {
        if (ev.key === 'Escape' && ev.target && ev.target.classList &&
            ev.target.classList.contains('blockr-otl-searchinput')) {
          ev.target.value = '';
          applyFilter();
        }
      });

      $(document).on('shiny:value', function(ev) {
        if (ev.name && /outline_out$/.test(ev.name)) {
          setTimeout(applyCollapsed, 0);
          setTimeout(applyFilter, 0);

          // Fill in any push that landed before this render inserted its
          // nodes. renderUI carries current code, so this is a no-op
          // whenever the two are already in step.
          setTimeout(applyCode, 0);
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
          '.blockr-otl-grow.drop-after, .blockr-otl-chap.drop-chap')
          .forEach(function(r) {
            r.classList.remove('drop-before', 'drop-after', 'drop-chap');
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
      // Chapter drag: the heading's grip moves the WHOLE chapter. Only
      // other chapter headings (and the end marker) are legal targets --
      // a chapter cannot land inside a chapter. Legal targets come from
      // the server as anchor block ids.
      var dragChap = null;
      document.addEventListener('dragstart', function(ev) {
        var cgrip = ev.target.closest &&
          ev.target.closest('.blockr-otl-chapgrip');
        if (cgrip) {
          var chap = cgrip.closest('.blockr-otl-chap');
          dragChap = {
            stack: chap.dataset.stack,
            anchor: chap.dataset.anchor,
            targets: (chap.dataset.targets || '').split(',').filter(Boolean)
          };
          ev.dataTransfer.effectAllowed = 'move';
          ev.dataTransfer.setData('text/plain', dragChap.stack);
          document.querySelectorAll('.blockr-otl-chap').forEach(function(c) {
            if (dragChap.targets.indexOf(c.dataset.anchor) > -1) {
              c.classList.add('chap-legal');
            }
          });
          if (dragChap.targets.indexOf('__end__') > -1) {
            var rows = rowsInOrder();
            if (rows.length) {
              rows[rows.length - 1].classList.add('chap-endlegal');
            }
          }
          return;
        }
        var grip = ev.target.closest && ev.target.closest('.blockr-otl-grip');
        if (!grip) return;
        dragId = rowOf(grip).dataset.blk;
        ev.dataTransfer.effectAllowed = 'move';
        ev.dataTransfer.setData('text/plain', dragId);
        computeLegal();
      });
      function clearChap() {
        document.querySelectorAll('.chap-legal, .chap-hot, .chap-endlegal, ' +
          '.chap-endhot').forEach(function(c) {
          c.classList.remove('chap-legal', 'chap-hot', 'chap-endlegal',
            'chap-endhot');
        });
      }
      document.addEventListener('dragover', function(ev) {
        if (dragChap) {
          var tgt = ev.target.closest &&
            ev.target.closest('.blockr-otl-chap.chap-legal');
          var endRow = ev.target.closest &&
            ev.target.closest('.blockr-otl-grow.chap-endlegal');
          document.querySelectorAll('.chap-hot, .chap-endhot')
            .forEach(function(c) { c.classList.remove('chap-hot', 'chap-endhot'); });
          if (tgt) {
            ev.preventDefault();
            tgt.classList.add('chap-hot');
          } else if (endRow) {
            ev.preventDefault();
            endRow.classList.add('chap-endhot');
          }
          return;
        }
        // A chapter heading is a membership target: dropping a chip on it
        // moves the block into that chapter.
        var chap = ev.target.closest && ev.target.closest('.blockr-otl-chap');
        if (chap && dragId) {
          ev.preventDefault();
          clearDrop();
          chap.classList.add('drop-chap');
          return;
        }
        var row = rowOf(ev.target);
        if (!row || !dragId) return;
        clearDrop();
        var side = dropSide(row, ev);
        if (!side) return;
        ev.preventDefault();
        row.classList.add(side === 'after' ? 'drop-after' : 'drop-before');
      });
      document.addEventListener('drop', function(ev) {
        if (dragChap) {
          var hot = document.querySelector('.blockr-otl-chap.chap-hot');
          var endHot = document.querySelector('.blockr-otl-grow.chap-endhot');
          if (hot || endHot) {
            ev.preventDefault();
            Shiny.setInputValue(MOVECHAP, {
              stack: dragChap.stack,
              anchor: dragChap.anchor,
              before: hot ? hot.dataset.anchor : '__end__'
            }, {priority: 'event'});
          }
          clearChap();
          dragChap = null;
          return;
        }
        var chap = ev.target.closest && ev.target.closest('.blockr-otl-chap');
        if (chap && dragId) {
          ev.preventDefault();
          clearDrop();
          clearLegal();
          Shiny.setInputValue(TOSTACK, {
            id: dragId, stack: chap.dataset.stack
          }, {priority: 'event'});
          dragId = null;
          return;
        }
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
        clearChap();
        dragId = null;
        dragChap = null;
      });
      document.addEventListener('dblclick', function(ev) {
        clearTimeout(openTimer);
        var dtl = ev.target.closest && ev.target.closest('.blockr-otl-doctitle');
        if (dtl) {
          inlineRename(dtl, dtl.textContent, function(val) {
            Shiny.setInputValue(RENTITLE, {name: val}, {priority: 'event'});
          });
          return;
        }
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
        // Bulk include/exclude every block, from the settings band. The
        // per-chapter version of this is .blockr-otl-chapact above.
        var bulk = ev.target.closest && ev.target.closest('.blockr-otl-bulk');
        if (bulk) {
          Shiny.setInputValue(BULK, {
            act: bulk.dataset.bulk
          }, {priority: 'event'});
          return;
        }
        var chapAct = ev.target.closest &&
          ev.target.closest('.blockr-otl-chapact');
        if (chapAct) {
          Shiny.setInputValue(CHAP, {
            stack: chapAct.closest('.blockr-otl-chap').dataset.stack,
            act: chapAct.dataset.act
          }, {priority: 'event'});
          return;
        }
        var newChap = ev.target.closest &&
          ev.target.closest('.blockr-otl-newchap');
        if (newChap) {
          Shiny.setInputValue(NEWCHAP, {
            id: newChap.closest('.blockr-otl-addrow').dataset.blk
          }, {priority: 'event'});
          return;
        }
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
          // Push the editor's current text before asking the server to save
          // it. The editor no longer streams every keystroke (it commits on
          // focusout), and focusout does fire ahead of this click -- but a
          // click that never moves focus would otherwise save a stale value.
          if (window.blockrMdEditor && window.blockrMdEditor.flush) {
            window.blockrMdEditor.flush();
          }
          fire(ed.classList.contains('dirty') ? SAVE : CANCEL);
          return;
        }
        // Collapse is chevron-only: the twisty owns the toggle, the title owns
        // rename (dblclick). Separate targets mean no single-vs-double
        // ambiguity, so this fires instantly -- no debounce.
        var chev = ev.target.closest && ev.target.closest('.blockr-otl-chevwrap');
        if (chev) {
          var chap = chev.closest('.blockr-otl-chap');
          if (chap) {
            var s = chap.dataset.stack;
            if (collapsedStacks.has(s)) collapsedStacks.delete(s);
            else collapsedStacks.add(s);
            applyCollapsed();
          }
          return;
        }
        var row = rowOf(ev.target);
        if (!row) return;
        if (ev.target.closest('.blockr-otl-rname-input')) return;
        var id = row.dataset.blk;
        if (ev.target.closest('.blockr-otl-rm')) {
          Shiny.setInputValue(RM, {id: id}, {priority: 'event'});
          return;
        }
        if (ev.target.closest('.blockr-otl-eyeoff')) {
          Shiny.setInputValue(HIDE, {id: id}, {priority: 'event'});
          return;
        }
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
# Highlighted HTML for one section's chunk. Sole producer of block code
# markup: the initial render calls it through outline_code_map(), and the
# incremental push sends its output for the blocks whose code changed.
# Depends on the chunk header too, so a report-flag flip regenerates the
# markup (that flip also redraws the skeleton, which is what shows the
# prose and the include=FALSE chip).
sect_code_html <- function(sects, i) {

  # Not reported yet: hold the row with a muted placeholder rather than
  # deparsing the stand-in expression, which would flash `x <- NULL`.
  if (isTRUE(sects$pending[i])) {
    return(
      as.character(
        div(class = "blockr-otl-pending", "Evaluating\u2026")
      )
    )
  }

  hl_or_pre <- function(txt) {
    hl <- highlight_r_code(txt)
    if (is.null(hl)) as.character(tags$pre(txt)) else hl
  }

  chunk <- paste(
    c(
      paste0(
        "#+ ", sects$ids[i],
        if (!sects$report[i]) ", include=FALSE"
      ),
      sects$code[i]
    ),
    collapse = "\n"
  )

  # The cell mirrors the document chunk exactly: transform code, then --
  # for a reported block -- the same presentation line the qmd chunk ends
  # with (report_call / renderer / bare variable, see sect_output). It is
  # highlighted separately so it can carry its own class and render
  # dimmed: the transform is the substance, the exhibit call its footer.
  # Code view is thus the chunk source; Output view its evaluated result.
  paste0(
    hl_or_pre(chunk),
    if (sects$report[i]) {
      paste0(
        "<div class=\"blockr-otl-exline\">",
        hl_or_pre(sect_output(sects, i)),
        "</div>"
      )
    }
  )
}

# `ids` narrows the map to the blocks that render a code cell (the active
# ones): dormant rows have no cell to fill, so highlighting their chunks
# would be pure waste on a large board.
outline_code_map <- function(sects, ids = sects$ids) {
  keep <- match(intersect(ids, sects$ids), sects$ids)
  setNames(
    lapply(keep, function(i) sect_code_html(sects, i)),
    sects$ids[keep]
  )
}

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

  # Older callers (and the qmd/script exporters' sections) carry no
  # activation info: everything active, nothing gated -- the pre-dormancy
  # rendering.
  active <- coal(sects$active, rep(TRUE, length(sects$ids)))
  gated <- isTRUE(sects$gated)

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
      # Return the row to its condensed state by taking the block's panel
      # out of the active view (the inverse of the row click). Only on
      # gated boards -- without view gating there is no dormant state to
      # return to.
      if (gated && isTRUE(active[i])) {
        tags$button(
          class = "blockr-otl-eyeoff",
          type = "button",
          title = "Hide from this view (condenses the row)",
          HTML(paste0(
            "<svg viewBox=\"0 0 24 24\" width=\"12\" height=\"12\" ",
            "fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" ",
            "stroke-linecap=\"round\" stroke-linejoin=\"round\">",
            "<path d=\"M17.94 17.94A10.07 10.07 0 0 1 12 20c-7 0-11-8-11-8",
            "a18.45 18.45 0 0 1 5.06-5.94M9.9 4.24A9.12 9.12 0 0 1 12 4",
            "c7 0 11 8 11 8a18.5 18.5 0 0 1-2.16 3.19\"/>",
            "<path d=\"M14.12 14.12a3 3 0 1 1-4.24-4.24\"/>",
            "<line x1=\"1\" y1=\"1\" x2=\"23\" y2=\"23\"/></svg>"
          ))
        )
      },
      tags$button(
        class = "blockr-otl-rm",
        type = "button",
        title = "Remove this block",
        HTML(paste0(
          "<svg viewBox=\"0 0 16 16\" width=\"11\" height=\"11\" fill=\"none\" ",
          "stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\">",
          "<path d=\"M4 4l8 8M12 4l-8 8\"/></svg>"
        ))
      ),
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

    output_mode <- identical(coal(sects$body_mode, "code"), "output")

    # Dormant: not in the active view, so no code cell, no prose block --
    # one condensed line of description next to the chip. Clicking the row
    # opens the block's panel in the view (the standard row click), which
    # activates it. Output mode is exempt: the preview is the document,
    # and the document does not care which panels are open.
    if (!output_mode && !isTRUE(active[i])) {
      desc <- sects$descriptions[i]

      return(
        div(
          class = "blockr-otl-sect blockr-otl-dormant",
          title = paste(
            "Click to open this block in the view and show its code;",
            "double-click to edit the description"
          ),
          div(
            class = "blockr-otl-dormline",
            if (!sects$report[i]) {
              span(
                class = "blockr-otl-offchip",
                if (isTRUE(sects$exported[i])) {
                  "include=FALSE \u00b7 runs, not shown"
                } else {
                  "not in report \u00b7 not evaluated"
                }
              )
            },
            span(
              class = "blockr-otl-dormdesc",
              if (nzchar(desc)) {
                # One plain-text line; the markdown structure belongs to
                # the expanded row.
                gsub(
                  "\\s+", " ",
                  trimws(commonmark::markdown_text(desc, extensions = TRUE))
                )
              } else {
                span(
                  class = "blockr-otl-placeholder",
                  "No description"
                )
              }
            )
          )
        )
      )
    }

    # Rendered from the pre-computed map so the initial paint and the
    # incremental push (see the code observer in ext.R) go through the
    # same producer and can never drift apart. Code mode holds an HTML
    # string (also the target of the incremental push); Output mode holds
    # a tag object, so a flextable's html dependency survives -- render it
    # directly rather than through HTML().
    body <- sects$code_html[[sects$ids[i]]]
    code_tag <- div(
      id = ns(paste0("code-", sects$ids[i])),
      class = if (output_mode) "blockr-otl-codewrap blockr-otl-outwrap" else
        "blockr-otl-codewrap",
      if (is.character(body)) HTML(coal(body, "")) else body
    )

    prose <- if (sects$report[i] && nzchar(sects$descriptions[i])) {
      div(
        class = "blockr-otl-prose",
        HTML(commonmark::markdown_html(sects$descriptions[i], extensions = TRUE))
      )
    }

    div(
      class = "blockr-otl-sect",
      title = "Double-click to edit the description",
      if (!sects$report[i]) {
        span(
          class = "blockr-otl-offchip",
          # Two different exclusions: a block the report still depends on
          # runs invisibly (include=FALSE); one nothing reported needs is
          # pruned from the document and never evaluated.
          if (isTRUE(sects$exported[i])) {
            "include=FALSE \u00b7 runs, not shown"
          } else {
            "not in report \u00b7 not evaluated"
          }
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
          `data-anchor` = sects$ids[idx[1L]],
          `data-targets` = paste(
            coal(sects$chap_targets[[r]], character()),
            collapse = ","
          ),
          style = paste0("--accent: ", accent, ";"),
          span(
            class = "blockr-otl-chapgrip",
            draggable = "true",
            title = "Drag to move this chapter",
            HTML(paste0(
              "<svg viewBox=\"0 0 10 16\" width=\"8\" height=\"13\" ",
              "fill=\"currentColor\" aria-hidden=\"true\">",
              "<circle cx=\"2.5\" cy=\"3\" r=\"1.3\"/><circle cx=\"7.5\" cy=\"3\" r=\"1.3\"/>",
              "<circle cx=\"2.5\" cy=\"8\" r=\"1.3\"/><circle cx=\"7.5\" cy=\"8\" r=\"1.3\"/>",
              "<circle cx=\"2.5\" cy=\"13\" r=\"1.3\"/><circle cx=\"7.5\" cy=\"13\" r=\"1.3\"/>",
              "</svg>"
            ))
          ),
          span(class = "blockr-otl-chevwrap", outline_chevron()),
          span(class = "blockr-otl-chlabel", run_labels[r]),
          span(
            class = "blockr-otl-chapacts",
            if (r > 1L) {
              span(
                class = "blockr-otl-chapact",
                `data-act` = "merge",
                title = "Merge into the chapter above",
                "merge up"
              )
            },
            span(
              class = "blockr-otl-chapact",
              `data-act` = "ungroup",
              title = "Dissolve this chapter; its blocks keep their order",
              "ungroup"
            ),
            span(
              class = "blockr-otl-chapact",
              `data-act` = if (all(sects$report[idx])) "exclude" else "include",
              title = "Include or exclude every block of this chapter",
              if (all(sects$report[idx])) "exclude all" else "include all"
            )
          ),
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
                  HTML(commonmark::markdown_html(stack_desc, extensions = TRUE))
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
        ),
        # Sections are defined by where they START, so the create gesture
        # lives on a block, styled exactly like "Add block": this block
        # and the rest of its run become a new chapter.
        span(
          class = "blockr-otl-addlink blockr-otl-newchap",
          title = "Start a new chapter at this block",
          span(class = "blockr-otl-addicon", HTML(
            paste0(
              "<svg viewBox=\"0 0 16 16\" width=\"12\" height=\"12\" ",
              "fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" ",
              "stroke-linecap=\"round\"><path d=\"M8 3v10M3 8h10\"/></svg>"
            )
          )),
          "New chapter"
        )
      )

      div(
        class = paste(
          "blockr-otl-grow",
          if (sects$report[i]) "on",
          if (!isTRUE(active[i])) "dormant"
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

# The report title as the top-level "chapter": document title on the left
# (double-click to rename -- it is outline state), the board-wide include /
# exclude actions inline right after it, revealed on hover exactly like the
# per-chapter actions. Rendered separately from the outline body so the
# search box can sit under it while staying static (focus-stable).
outline_title_row <- function(title) {
  doc_title <- if (is.character(title) && length(title) && nzchar(title[[1L]])) {
    title[[1L]]
  } else {
    "Board report"
  }
  div(
    class = "blockr-otl-doctitle-row",
    span(
      class = "blockr-otl-doctitle",
      title = "Double-click to rename the report",
      doc_title
    ),
    span(
      class = "blockr-otl-docacts",
      span(class = "blockr-otl-bulk", `data-bulk` = "include", "include all"),
      span(class = "blockr-otl-bulk", `data-bulk` = "exclude", "exclude all")
    )
  )
}
