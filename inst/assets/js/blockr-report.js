// The report panel's client (R/report-ui.R builds the markup): the row
// list the server patches, the rows' tools and "..." menu, drag and
// keyboard reordering, the text editor, the "Add block" menu, the view
// switch. One instance per panel, found by its root id; the server's
// messages carry that id.
//
// Every row message to the server carries the item INDEX: text rows have no
// id, and the index is unambiguous for both kinds.
(function () {
  'use strict';

  var panels = {};
  // Messages that arrived before their panel did (the catalogue is sent
  // only when it changes, so it must not be lost).
  var early = {};

  function later(type, msg) {
    (early[msg.root] = early[msg.root] || []).push({ type: type, msg: msg });
  }

  var handlers = {
    catalog: function (msg) {
      var p = panels[msg.root];
      if (!p) { later('catalog', msg); return; }
      p.state.catalog = msg.items || [];
      p.state.icons = msg.icons || {};
    },
    picked: function (msg) {
      var p = panels[msg.root];
      if (!p) { later('picked', msg); return; }
      var ids = msg.ids || [];
      if (typeof ids === 'string') ids = [ids];
      p.state.picked = {};
      ids.forEach(function (b) { p.state.picked[b] = true; });
    }
  };

  function fire(id, value) {
    Shiny.setInputValue(id, value === undefined ? Math.random() : value,
                        { priority: 'event' });
  }

  function init(rootId) {
    var root = document.getElementById(rootId);
    if (!root || panels[rootId]) return;
    var ns = root.getAttribute('data-ns');
    var id = function (x) { return ns + x; };
    var host = root.querySelector('.blockr-rpt-rows');
    var list = root.querySelector('.blockr-otl-list');

    var state = { catalog: [], icons: {}, picked: {} };
    panels[rootId] = { root: root, host: host, state: state, grow: growEditor };
    (early[rootId] || []).forEach(function (m) { handlers[m.type](m.msg); });
    delete early[rootId];

    function rowIdx(row) { return parseInt(row.getAttribute('data-idx'), 10); }
    function openEditor() { return root.querySelector('.blockr-rpt-editor'); }
    function flushEditor(ed) {
      var ta = ed.querySelector('.blockr-rpt-mdedit');
      if (ta) Shiny.setInputValue(id('rpt_desc_edit'), ta.value);
    }
    function growEditor(ta) {
      ta.style.height = 'auto';
      ta.style.height = ta.scrollHeight + 'px';
    }

    // ---- the server handshake: a full send of the rows ----------------
    var sync = function () { fire(id('rpt_rows_sync')); };
    if (Shiny.shinyapp && Shiny.shinyapp.isConnected && Shiny.shinyapp.isConnected()) {
      sync();
    } else {
      $(document).one('shiny:connected', sync);
    }

    // ---- views ---------------------------------------------------------
    function showView(v) {
      root.querySelectorAll('.blockr-rpt-pane').forEach(function (p) {
        var pane = p.getAttribute('data-pane');
        p.hidden = v === 'builder' ? pane !== 'builder' : pane !== 'code';
      });
    }
    root.addEventListener('blockr-otl-change', function (e) {
      var f = e.target;
      if (f.id === id('rpt_view')) showView(e.detail.value || 'builder');
      if (f.id === id('rpt_set_format')) {
        root.querySelectorAll('[data-rpt-html-only]').forEach(function (x) {
          x.hidden = e.detail.value !== 'html';
        });
      }
    });

    // ---- the text editor -----------------------------------------------
    function markDirty(ev) {
      var ed = ev.target.closest && ev.target.closest('.blockr-rpt-editor');
      if (ed && root.contains(ed)) ed.classList.add('dirty');
    }
    root.addEventListener('input', function (ev) {
      markDirty(ev);
      if (ev.target.classList && ev.target.classList.contains('blockr-rpt-mdedit')) {
        growEditor(ev.target);
      }
    });
    root.addEventListener('paste', markDirty, true);
    root.addEventListener('cut', markDirty, true);
    root.addEventListener('keydown', function (ev) {
      var ed = ev.target.closest && ev.target.closest('.blockr-rpt-editor');
      if (!ed) return;
      if (ev.key === 'Escape') {
        ev.stopPropagation();
        fire(id('rpt_desc_cancel'));
      }
    }, true);

    // An open editor eats the click that lands outside it: commit (or
    // discard, if nothing changed) and stop. Flush first, so the save
    // observer reads the textarea's current text.
    document.addEventListener('click', function (e) {
      if (!root.isConnected) return;
      var ed = openEditor();
      if (!ed || ed.contains(e.target)) return;
      // The editor's own row opened it; a click on a menu is not "outside".
      if (e.target.closest && e.target.closest('.blockr-menu')) return;
      flushEditor(ed);
      fire(id(ed.classList.contains('dirty') ? 'rpt_desc_save' : 'rpt_desc_cancel'));
      // The click did its job; it opens nothing else in this panel.
      e.blockrRptEaten = true;
    }, true);

    // ---- clicks in the panel -------------------------------------------
    root.addEventListener('click', function (e) {
      if (e.blockrRptEaten) return;
      var t = e.target;

      var edbtn = t.closest('.blockr-rpt-edsave, .blockr-rpt-edcancel');
      if (edbtn) {
        var bed = edbtn.closest('.blockr-rpt-editor');
        if (edbtn.classList.contains('blockr-rpt-edsave')) {
          flushEditor(bed);
          fire(id('rpt_desc_save'));
        } else {
          fire(id('rpt_desc_cancel'));
        }
        return;
      }

      var chip = t.closest('.blockr-rpt-chip[data-blk]');
      if (chip) {
        fire(id('rpt_open'), { id: chip.getAttribute('data-blk'), n: Math.random() });
        return;
      }

      if (t.closest('.blockr-otl-add')) {
        addMenu(t.closest('.blockr-otl-add'));
        return;
      }

      // A name being renamed keeps its clicks.
      if (t.closest('.blockr-otl-row__name.is-editing')) return;

      var more = t.closest('.blockr-otl-row__more');
      if (more) {
        rowMenu(more);
        return;
      }

      // The row's pressed tools (code, output) toggle in place.
      var btn = t.closest('.blockr-otl-row [data-act]');
      if (btn) {
        e.preventDefault();
        var brow = btn.closest('.blockr-otl-row');
        fire(id('rpt_act'), { idx: rowIdx(brow), act: btn.getAttribute('data-act') });
        return;
      }

      var row = t.closest('.blockr-rpt-row');
      if (row) openRow(row);
    });

    // A plain click on a row: a block row opens the block's panel, a text
    // row opens its editor.
    function openRow(row) {
      if (row.classList.contains('is-editing')) return;
      if (row.classList.contains('blockr-rpt-textrow')) {
        fire(id('rpt_act'), { idx: rowIdx(row), act: 'edit' });
      } else if (row.getAttribute('data-blk')) {
        fire(id('rpt_open'), { id: row.getAttribute('data-blk'), n: Math.random() });
      }
    }

    root.addEventListener('keydown', function (e) {
      if (e.key !== 'Enter') return;
      var row = e.target;
      if (row.classList && row.classList.contains('blockr-rpt-row')) {
        e.preventDefault();
        openRow(row);
      }
    });

    // ---- the row's "..." menu ------------------------------------------
    // The actions that are not on the row: figure size (charts), full
    // width (blocks), the text inserts, Edit (text rows), and Remove last
    // after a divider.
    function rowMenu(btn) {
      var row = btn.closest('.blockr-otl-row');
      var at = rowIdx(row);
      var spec = {};
      try { spec = JSON.parse(btn.getAttribute('data-menu')); } catch (err) { spec = {}; }
      var act = function (a) {
        return function () { fire(id('rpt_act'), { idx: at, act: a }); };
      };
      var items = [];
      if (spec.text) {
        items.push({ label: 'Edit text', onSelect: act('edit') });
        items.push({ divider: true });
      }
      if (spec.fig) {
        var fig = function (w, h) {
          return function () { fire(id('rpt_fig'), { idx: at, w: w, h: h }); };
        };
        var presets = [
          ['Small', 5, 3.5], ['Medium', 8, 4.5], ['Large', 10, 6], ['Wide', 12, 4]
        ];
        var own = spec.w !== null || spec.h !== null;
        items.push({ title: 'Figure size' });
        items.push({ label: 'Document default', current: !own, onSelect: fig(null, null) });
        presets.forEach(function (p) {
          items.push({
            label: p[0],
            meta: p[1] + ' × ' + p[2] + ' in',
            current: spec.w === p[1] && spec.h === p[2],
            onSelect: fig(p[1], p[2])
          });
        });
        items.push({ divider: true });
      }
      if (!spec.text) {
        items.push({ label: 'Full width', checked: !!spec.full, onSelect: act('fullw') });
        items.push({ divider: true });
      }
      items.push({ label: 'Add text above', onSelect: act('text_above') });
      items.push({ label: 'Add text below', onSelect: act('text_below') });
      items.push({ divider: true });
      items.push({ label: 'Remove from report', icon: 'trash', danger: true, onSelect: act('rm') });
      BlockrOutline.rowMenu(btn, items);
    }

    // ---- renaming a block row's block ---------------------------------
    function rename(row) {
      var blk = row && row.getAttribute('data-blk');
      if (!blk) return;
      BlockrOutline.renameRow(row.querySelector('.blockr-otl-row__name'), function (v) {
        fire(id('rpt_rename'), { id: blk, name: v, n: Math.random() });
      });
    }
    root.addEventListener('dblclick', function (e) {
      var nm = e.target.closest && e.target.closest('.blockr-rpt-row[data-blk] .blockr-otl-row__name');
      if (nm && root.contains(nm)) rename(nm.closest('.blockr-rpt-row'));
    });
    root.addEventListener('keydown', function (e) {
      if (e.key !== 'F2') return;
      var row = e.target;
      if (row.classList && row.classList.contains('blockr-rpt-row')) {
        e.preventDefault();
        rename(row);
      }
    });

    BlockrOutline.watchCurrent(list);

    // ---- "Add block": the board's blocks ------------------------------
    function addMenu(btn) {
      BlockrOutline.boardMenu(btn, {
        items: state.catalog,
        icons: state.icons,
        picked: state.picked,
        onPick: function (blk, was) {
          if (!was) {
            fire(id('rpt_add'), blk);
            return;
          }
          var row = host.querySelector('.blockr-rpt-row[data-blk="' + blk + '"]');
          if (row) fire(id('rpt_act'), { idx: rowIdx(row), act: 'rm' });
        }
      });
    }

    // ---- reordering: drag, and Alt+Up / Alt+Down ----------------------
    var dragged = null;

    function dropAfter(e, row) {
      var box = row.getBoundingClientRect();
      return (e.clientY - box.top) > box.height / 2;
    }

    // ONE caret per list, moved to the targeted gap, its position the
    // midpoint between the two rows bounding it.
    function caret() {
      var c = list.querySelector('.blockr-otl-caret');
      if (!c) {
        c = document.createElement('div');
        c.className = 'blockr-otl-caret';
        c.hidden = true;
        list.appendChild(c);
      }
      return c;
    }
    function moveCaret(row, after) {
      var c = caret();
      var box = row.getBoundingClientRect();
      var sib = after ? row.nextElementSibling : row.previousElementSibling;
      var y;
      if (sib) {
        var sb = sib.getBoundingClientRect();
        y = after ? (box.bottom + sb.top) / 2 : (sb.bottom + box.top) / 2;
      } else {
        y = after ? box.bottom : box.top;
      }
      c.style.top = (y - list.getBoundingClientRect().top) + 'px';
      c.hidden = false;
    }

    root.addEventListener('dragstart', function (e) {
      var row = e.target.closest && e.target.closest('.blockr-rpt-row');
      if (!row) return;
      dragged = rowIdx(row);
      row.classList.add('is-dragging');
      if (e.dataTransfer) e.dataTransfer.effectAllowed = 'move';
    });
    root.addEventListener('dragend', function () {
      dragged = null;
      caret().hidden = true;
      root.querySelectorAll('.blockr-rpt-row.is-dragging').forEach(function (r) {
        r.classList.remove('is-dragging');
      });
    });
    root.addEventListener('dragover', function (e) {
      if (dragged === null) return;
      var row = e.target.closest && e.target.closest('.blockr-rpt-row');
      if (!row) return;
      e.preventDefault();
      moveCaret(row, dropAfter(e, row));
    });
    root.addEventListener('drop', function (e) {
      if (dragged === null) return;
      var row = e.target.closest && e.target.closest('.blockr-rpt-row');
      if (!row) return;
      e.preventDefault();
      caret().hidden = true;
      fire(id('rpt_move'), { from: dragged, to: rowIdx(row), after: dropAfter(e, row) });
      dragged = null;
    });

    BlockrOutline.rowKeys(root, '.blockr-rpt-row', function (row, dir) {
      var at = rowIdx(row);
      var n = host.children.length;
      var to = at + dir;
      if (to < 1 || to > n) return;
      panels[rootId].refocus = to;
      fire(id('rpt_move'), { from: at, to: to, after: dir > 0 });
    });

    showView('builder');
  }

  if (window.Shiny) {
    // The rows, as a diff: {root, n, set: [{i, html}]} swaps exactly the
    // changed nodes, so a toggle repaints one row and an append adds one.
    Shiny.addCustomMessageHandler('blockr-report-rows', function (msg) {
      var p = panels[msg.root];
      if (!p) return;
      var host = p.host;
      var n = msg.n || 0;
      while (host.children.length > n) host.removeChild(host.lastElementChild);
      var tpl = document.createElement('template');
      (msg.set || []).forEach(function (e) {
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
        p.grow(ta);
        if (document.activeElement !== ta) ta.focus();
      }
      // A keyboard move keeps the keyboard on the moved row.
      if (p.refocus) {
        var row = host.children[p.refocus - 1];
        p.refocus = null;
        if (row) BlockrOutline.keyFocus(row);
      }
    });

    Shiny.addCustomMessageHandler('blockr-report-catalog', function (msg) {
      handlers.catalog(msg);
    });
    Shiny.addCustomMessageHandler('blockr-report-picked', function (msg) {
      handlers.picked(msg);
    });

    Shiny.addCustomMessageHandler('blockr-report-download', function (msg) {
      var el = document.getElementById(msg.id);
      if (el) el.click();
    });
  }

  window.BlockrReport = {
    init: function (rootId) {
      if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', function () { init(rootId); });
      } else {
        init(rootId);
      }
    }
  };
})();
