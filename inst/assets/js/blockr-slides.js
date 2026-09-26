// The slides panel's client (R/slides.R builds the markup): the deck's
// rows (open, remove, drag, Alt+Up / Alt+Down), the "Add slide" menu and
// the download menu's rows. One instance per panel, found by its root id;
// the server's messages carry that id.
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
    var list = root.querySelector('.blockr-otl-list');

    var state = { catalog: [], icons: {}, picked: {} };
    panels[rootId] = { root: root, state: state };
    (early[rootId] || []).forEach(function (m) { handlers[m.type](m.msg); });
    delete early[rootId];

    var sync = function () { fire(id('sld_sync')); };
    if (Shiny.shinyapp && Shiny.shinyapp.isConnected && Shiny.shinyapp.isConnected()) {
      sync();
    } else {
      $(document).one('shiny:connected', sync);
    }

    function act(row, a) {
      fire(id('sld_act'), { id: row.getAttribute('data-blk'), act: a });
    }

    root.addEventListener('click', function (e) {
      var t = e.target;

      if (t.closest('.blockr-otl-add')) {
        BlockrOutline.boardMenu(t.closest('.blockr-otl-add'), {
          items: state.catalog,
          icons: state.icons,
          picked: state.picked,
          onPick: function (blk, was) {
            if (was) fire(id('sld_act'), { id: blk, act: 'rm' });
            else fire(id('sld_add'), blk);
          }
        });
        return;
      }

      var btn = t.closest('.blockr-sld-row [data-act]');
      if (btn) {
        act(btn.closest('.blockr-sld-row'), btn.getAttribute('data-act'));
        return;
      }

      // A plain click on the row opens that block's panel: the deck lists
      // blocks, and the obvious question about one is "show me this one".
      var row = t.closest('.blockr-sld-row');
      if (row) fire(id('sld_open'), { id: row.getAttribute('data-blk'), n: Math.random() });
    });

    root.addEventListener('keydown', function (e) {
      var row = e.target;
      if (e.key !== 'Enter' || !row.classList || !row.classList.contains('blockr-sld-row')) return;
      e.preventDefault();
      fire(id('sld_open'), { id: row.getAttribute('data-blk'), n: Math.random() });
    });

    // Keyboard moves: the list re-renders, so the moved row gets the focus
    // back once it is drawn again.
    var refocus = null;
    BlockrOutline.rowKeys(root, '.blockr-sld-row', function (row, dir) {
      refocus = row.getAttribute('data-blk');
      act(row, dir < 0 ? 'up' : 'down');
    });
    $(root).on('shiny:value', function (e) {
      if (!refocus) return;
      var blk = refocus;
      setTimeout(function () {
        var r = root.querySelector('.blockr-sld-row[data-blk="' + blk + '"]');
        if (r) r.focus();
      }, 60);
      refocus = null;
      return e;
    });

    // ---- drag ----------------------------------------------------------
    var dragged = null;

    // The pointer's half of the row decides which gap the drop targets;
    // the caret and the drop share this one rule.
    function dropAfter(e, row) {
      var box = row.getBoundingClientRect();
      return (e.clientY - box.top) > box.height / 2;
    }

    // ONE caret per list, moved to the targeted gap, its position the
    // midpoint between the two rows bounding it.
    function caret() {
      var c = list.querySelector(':scope > .blockr-otl-caret');
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
      var row = e.target.closest && e.target.closest('.blockr-sld-row');
      if (!row) return;
      dragged = row.getAttribute('data-blk');
      row.classList.add('is-dragging');
      if (e.dataTransfer) e.dataTransfer.effectAllowed = 'move';
    });
    root.addEventListener('dragend', function () {
      dragged = null;
      caret().hidden = true;
      root.querySelectorAll('.blockr-sld-row.is-dragging').forEach(function (r) {
        r.classList.remove('is-dragging');
      });
    });
    root.addEventListener('dragover', function (e) {
      if (dragged === null) return;
      var row = e.target.closest && e.target.closest('.blockr-sld-row');
      if (!row) return;
      e.preventDefault();
      moveCaret(row, dropAfter(e, row));
    });
    root.addEventListener('drop', function (e) {
      if (dragged === null) return;
      var row = e.target.closest && e.target.closest('.blockr-sld-row');
      if (!row) return;
      e.preventDefault();
      caret().hidden = true;
      fire(id('sld_move'), {
        id: dragged, target: row.getAttribute('data-blk'), after: dropAfter(e, row)
      });
      dragged = null;
    });

    // ---- the download menu ---------------------------------------------
    // Its rows are moved to <body> while the menu is open, so they are
    // found through the document, and matched to this panel by the menu's
    // trigger.
    var trigger = root.querySelector('.blockr-action-menu__trigger');
    document.addEventListener('click', function (e) {
      var row = e.target.closest && e.target.closest('.blockr-sld-fmt');
      if (!row || !window.Blockr || Blockr.actionMenu.current() !== trigger) return;
      var fmt = row.getAttribute('data-format');
      Shiny.setInputValue(id('sld_format'), fmt);
      fire(id('sld_go'), { format: fmt, n: Math.random() });
    });
  }

  if (window.Shiny) {
    Shiny.addCustomMessageHandler('blockr-slides-catalog', function (msg) {
      handlers.catalog(msg);
    });
    Shiny.addCustomMessageHandler('blockr-slides-picked', function (msg) {
      handlers.picked(msg);
    });
    Shiny.addCustomMessageHandler('blockr-slides-download', function (msg) {
      var el = document.getElementById(msg.id);
      if (el) el.click();
    });
  }

  window.BlockrSlides = {
    init: function (rootId) {
      if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', function () { init(rootId); });
      } else {
        init(rootId);
      }
    }
  };
})();
