// blockr.outline: the design-system controls of the report and slides panels
// and the slide block, bound to Shiny.
//
// The panels are built in R, so their controls arrive as markup: an element
// with the class `blockr-otl-field` and a `data-kind` is one Shiny input, and
// the binding below turns it into the blockr.ui control of that kind
// (Blockr.textCommit, Blockr.Select, the segmented control, the checkbox).
// Text and numbers commit on Enter or blur.
//
// A field reports NULL until it is used or the server sets it. The report's
// panel is drawn before its server knows the restored settings, so a field
// that reported its drawn default would overwrite them. The server sets a
// field with session$sendInputMessage(id, list(value = , options = )), which
// never echoes back.
//
// Also here: the gear tray (a .blockr-gear-btn with data-blockr-tray opens
// the tray of that id, through Blockr.gearTray), the menu that picks
// blocks already on the board (BlockrOutline.boardMenu), and what the
// report's and the deck's block lists share: the current row, the row's
// "…" menu, renaming a row's block in place, keyboard moves.
//
// Needs blockr.ui's controls_dep() (Blockr.*), loaded before this file.
(function () {
  'use strict';

  var CHECK_SVG =
    '<svg width="10" height="10" viewBox="0 0 16 16" fill="currentColor">' +
    '<path d="M13.854 3.646a.5.5 0 0 1 0 .708l-7 7a.5.5 0 0 1-.708 0l-3.5-3.5a.5.5 ' +
    '0 1 1 .708-.708L6.5 10.293l6.646-6.647a.5.5 0 0 1 .708 0"/></svg>';

  function readJSON(s, fallback) {
    try { return s ? JSON.parse(s) : fallback; } catch (e) { return fallback; }
  }

  // ---- one field ---------------------------------------------------------

  // Set the value, as the user did (report = true) or as the server did.
  function setValue(el, value, report) {
    el._otlTouched = true;
    el._otlValue = value;
    if (report && el._otlCallback) el._otlCallback(false);
    changed(el);
  }

  // Tells the panel a field moved, by the user or the server (the report
  // shows the embed checkbox for html only).
  function changed(el) {
    el.dispatchEvent(new CustomEvent('blockr-otl-change', {
      bubbles: true, detail: { value: el._otlValue }
    }));
  }

  var kinds = {

    // A text field that commits on Enter or blur (the Enter button arms
    // while it is dirty; Escape reverts).
    text: {
      init: function (el) {
        var input = el.querySelector('input, textarea');
        el._otlValue = input.value;
        el._otlCommit = Blockr.textCommit(input, {
          onCommit: function (v) { setValue(el, v, true); }
        });
      },
      set: function (el, v) {
        el._otlCommit.sync(v == null ? '' : String(v));
        el._otlValue = v == null ? '' : String(v);
      }
    },

    // A number: the same field, type=number, and an entry that is not a
    // number goes back to the last one.
    number: {
      init: function (el) {
        var input = el.querySelector('input');
        var last = input.value;
        el._otlValue = parseFloat(last);
        el._otlCommit = Blockr.textCommit(input, {
          onCommit: function (v) {
            var n = parseFloat(v);
            var min = input.min === '' ? -Infinity : parseFloat(input.min);
            if (!isFinite(n) || n < min) {
              el._otlCommit.sync(last);
              return;
            }
            last = v;
            setValue(el, n, true);
          }
        });
        el._otlSync = function (v) { last = v; el._otlCommit.sync(v); };
      },
      set: function (el, v) {
        el._otlSync(v == null ? '' : String(v));
        el._otlValue = v == null ? null : parseFloat(v);
      }
    },

    // Several lines: Enter is a new line, so it commits on blur, and on
    // Ctrl or Cmd with Enter. Escape reverts.
    textarea: {
      init: function (el) {
        var ta = el.querySelector('textarea');
        var committed = ta.value;
        el._otlValue = committed;
        var commit = function () {
          if (ta.value === committed) return;
          committed = ta.value;
          setValue(el, committed, true);
        };
        ta.addEventListener('blur', commit);
        ta.addEventListener('keydown', function (e) {
          if (e.key === 'Enter' && (e.metaKey || e.ctrlKey)) {
            e.preventDefault();
            commit();
          } else if (e.key === 'Escape' && ta.value !== committed) {
            e.stopPropagation();
            ta.value = committed;
          }
        });
        el._otlSync = function (v) { committed = v; ta.value = v; };
      },
      set: function (el, v) {
        el._otlSync(v == null ? '' : String(v));
        el._otlValue = v == null ? '' : String(v);
      }
    },

    // Four or more values: Blockr.Select, bordered, in the grid. The
    // options are {value, label}; the list shows the labels ("Word") and
    // the field reports the value ("docx").
    select: {
      init: function (el) {
        var opts = readJSON(el.getAttribute('data-options'), []);
        el._otlValue = el.getAttribute('data-value');
        el._otlSelect = Blockr.Select.single(el, {
          options: selectLabels(el, opts),
          selected: labelOf(el, el._otlValue),
          bordered: true,
          search: opts.length > 8,
          onChange: function (lab) { setValue(el, valueOf(el, lab), true); }
        });
      },
      set: function (el, v, msg) {
        if (msg && msg.options) {
          el._otlSelect.setOptions(selectLabels(el, msg.options), labelOf(el, v));
        } else {
          el._otlSelect.setValue(labelOf(el, v));
        }
        el._otlValue = valueOf(el, el._otlSelect.getValue());
      }
    },

    // Two or three short values: the segmented control. The segments are
    // drawn in R (so an icon-only segment can carry its glyph); a click
    // picks.
    segmented: {
      init: function (el) {
        el._otlValue = el.getAttribute('data-value');
        paintSegments(el, el._otlValue);
        el.addEventListener('click', function (e) {
          var seg = e.target.closest('.blockr-segmented__seg');
          if (!seg || !el.contains(seg)) return;
          var v = seg.getAttribute('data-value');
          if (v === el._otlValue) return;
          paintSegments(el, v);
          setValue(el, v, true);
        });
      },
      set: function (el, v) {
        paintSegments(el, v);
        el._otlValue = v;
      }
    },

    // On or off.
    checkbox: {
      init: function (el) {
        var input = el.querySelector('input');
        el._otlValue = input.checked;
        input.addEventListener('change', function () {
          setValue(el, input.checked, true);
        });
      },
      set: function (el, v) {
        el.querySelector('input').checked = !!v;
        el._otlValue = !!v;
      }
    },

    // Icon tiles (the slide block's layouts). Tiles carry data-value; the
    // controls of one layout only (data-layouts, inside the same
    // .slb-root) show or hide with the pick, client-side, so a switch
    // never rebuilds a field.
    tiles: {
      init: function (el) {
        el._otlValue = el.getAttribute('data-value');
        paintTiles(el, el._otlValue);
        el.addEventListener('click', function (e) {
          var tile = e.target.closest('[data-value]');
          if (!tile || !el.contains(tile)) return;
          var v = tile.getAttribute('data-value');
          if (v === el._otlValue) return;
          paintTiles(el, v);
          setValue(el, v, true);
        });
      },
      set: function (el, v) {
        paintTiles(el, v);
        el._otlValue = v;
      }
    },

    // A name that renames in place (design system, "Renaming in place"): plain
    // text with the hover wash; a double-click, or Enter on it, turns it into
    // a field at its own size. Enter or a click elsewhere commits, Escape
    // restores. An empty name is refused in place.
    name: {
      init: function (el) {
        el._otlValue = el.textContent;
        el.addEventListener('dblclick', function () { startRename(el); });
        el.addEventListener('keydown', function (e) {
          if ((e.key === 'Enter' || e.key === 'F2') && !el._otlEditing) {
            e.preventDefault();
            startRename(el);
          }
        });
      },
      set: function (el, v) {
        el._otlValue = v == null ? '' : String(v);
        if (!el._otlEditing) paintName(el);
      }
    }
  };

  // Select options arrive as [{value, label}] (or a named list from R,
  // {label: value}); the widget is given the labels.
  function selectLabels(el, opts) {
    if (!Array.isArray(opts)) {
      opts = Object.keys(opts).map(function (k) { return { label: k, value: opts[k] }; });
    }
    el._otlOpts = opts;
    return opts.map(function (o) { return o.label || o.value; });
  }
  function labelOf(el, v) {
    var o = (el._otlOpts || []).find(function (x) { return x.value === v; });
    return o ? (o.label || o.value) : v;
  }
  function valueOf(el, lab) {
    var o = (el._otlOpts || []).find(function (x) { return (x.label || x.value) === lab; });
    return o ? o.value : lab;
  }

  function paintSegments(el, v) {
    el.querySelectorAll('.blockr-segmented__seg').forEach(function (s) {
      var on = s.getAttribute('data-value') === v;
      s.classList.toggle('is-selected', on);
      s.setAttribute('aria-checked', on ? 'true' : 'false');
    });
  }

  function paintTiles(el, v) {
    el.querySelectorAll('[data-value]').forEach(function (t) {
      var on = t.getAttribute('data-value') === v;
      t.classList.toggle('is-selected', on);
      t.setAttribute('aria-pressed', on ? 'true' : 'false');
    });
    var root = el.closest('.slb-root') || document;
    root.querySelectorAll('[data-layouts]').forEach(function (f) {
      f.hidden = f.getAttribute('data-layouts').split(' ').indexOf(v) < 0;
    });
  }

  function paintName(el) {
    el.textContent = el._otlValue || '';
    el.classList.toggle('is-empty', !el._otlValue);
    if (!el._otlValue && el.getAttribute('data-placeholder')) {
      el.textContent = el.getAttribute('data-placeholder');
    }
  }

  function startRename(el) {
    if (el._otlEditing) return;
    el._otlEditing = true;
    var old = el._otlValue || '';
    var input = document.createElement('input');
    input.type = 'text';
    input.className = 'blockr-otl-name__input';
    input.value = old;
    input.setAttribute('aria-label', el.getAttribute('aria-label') || 'Name');
    var msg = document.createElement('div');
    msg.className = 'blockr-otl-name__error';
    msg.hidden = true;
    msg.textContent = el.getAttribute('data-empty') || 'A name cannot be empty';
    el.textContent = '';
    el.classList.add('is-editing');
    el.appendChild(input);
    el.appendChild(msg);
    var done = false;
    var finish = function (commit) {
      if (done) return;
      var v = input.value.trim();
      if (commit && !v) {
        // Refused: the field keeps focus and says why.
        input.classList.add('is-invalid');
        msg.hidden = false;
        input.focus();
        return;
      }
      done = true;
      el._otlEditing = false;
      el.classList.remove('is-editing');
      if (commit && v !== old) {
        setValue(el, v, true);
      }
      paintName(el);
      el.focus();
    };
    input.addEventListener('keydown', function (e) {
      e.stopPropagation();
      if (e.key === 'Enter') { e.preventDefault(); finish(true); }
      else if (e.key === 'Escape') { e.preventDefault(); finish(false); }
    });
    input.addEventListener('input', function () {
      input.classList.remove('is-invalid');
      msg.hidden = true;
    });
    // A click elsewhere commits; an empty field then goes back to the old
    // name rather than holding the focus hostage.
    input.addEventListener('blur', function () {
      if (done) return;
      if (!input.value.trim()) finish(false); else finish(true);
    });
    input.focus();
    input.select();
  }

  // ---- the Shiny binding -------------------------------------------------

  if (window.Shiny) {
    var binding = new Shiny.InputBinding();
    $.extend(binding, {
      find: function (scope) {
        return $(scope).find('.blockr-otl-field');
      },
      initialize: function (el) {
        if (el._otlInit) return;
        el._otlInit = true;
        var k = kinds[el.getAttribute('data-kind')];
        if (k) k.init(el);
      },
      getValue: function (el) {
        return el._otlTouched ? el._otlValue : null;
      },
      subscribe: function (el, callback) {
        el._otlCallback = callback;
      },
      unsubscribe: function (el) {
        el._otlCallback = null;
      },
      receiveMessage: function (el, msg) {
        var k = kinds[el.getAttribute('data-kind')];
        if (!k || !msg || !('value' in msg)) return;
        el._otlTouched = true;
        k.set(el, msg.value, msg);
        changed(el);
      }
    });
    Shiny.inputBindings.register(binding, 'blockr.outline.field');
  }

  // ---- the gear tray -----------------------------------------------------

  // Wired on the first click, so a tray rendered at any time works: the
  // first click builds the Blockr.gearTray handle and opens it; from then on
  // the handle's own listener toggles.
  var trays = new WeakMap();
  document.addEventListener('click', function (e) {
    var gear = e.target.closest && e.target.closest('.blockr-gear-btn[data-blockr-tray]');
    if (!gear || trays.has(gear)) return;
    var band = document.getElementById(gear.getAttribute('data-blockr-tray'));
    if (!band) return;
    var h = Blockr.gearTray(band, gear, { label: gear.getAttribute('aria-label') || 'Settings' });
    trays.set(gear, h);
    h.set(true);
  });

  // ---- picking a block already on the board -----------------------------

  // The design system's "Picking a block" menu, for blocks already on the
  // board: one row per block, its mark, its title and its type as meta
  // text. A block already in the list is ticked; a pick adds an unticked
  // one and removes a ticked one.
  //
  // opts: { items: [{id, name, type, color, icon_key}], icons: {key: svg},
  //         picked: {id: true}, onPick: function (id, wasPicked) }
  var open = null;
  function boardMenu(anchor, opts) {
    if (open && open.anchor === anchor) { open.menu.close(); return; }
    var items = (opts.items || []).map(function (b) {
      return {
        label: b.name,
        meta: b.type,
        keywords: b.id,
        checked: !!opts.picked[b.id],
        mark: { icon: opts.icons[b.icon_key] || '', color: b.color },
        onSelect: function () { opts.onPick(b.id, !!opts.picked[b.id]); }
      };
    });
    if (!items.length) {
      items = [{ label: 'No blocks on the board', disabled: true }];
    }
    var m = Blockr.menu(anchor, {
      caption: opts.caption,
      filter: items.length > 8 ? 'Search blocks' : false,
      minWidth: 240,
      items: items,
      onClose: function () { if (open && open.anchor === anchor) open = null; }
    });
    open = { anchor: anchor, menu: m };
  }

  // A list row moves with Alt+Up and Alt+Down: `rows` is the row selector,
  // `move(row, dir)` does the move.
  function rowKeys(root, rows, move) {
    root.addEventListener('keydown', function (e) {
      if (!e.altKey || (e.key !== 'ArrowUp' && e.key !== 'ArrowDown')) return;
      var row = e.target.closest && e.target.closest(rows);
      if (!row || row !== e.target) return;
      e.preventDefault();
      move(row, e.key === 'ArrowUp' ? -1 : 1);
    });
  }

  // ---- block lists: the current row ------------------------------------

  // The block open in the dock is the current row of every block list
  // (design system, "Block lists"). dockViewR announces each activation as
  // a bubbling `dockview:active-panel` event on its dock, with the panel id;
  // a block's panel id is `block_panel-<block id>`. Other panels (these
  // panels themselves, the outline) leave the current block as it was, so a
  // click in the report does not unmark the block the dock shows. The event
  // reports changes only, so this listener is bound when the file loads,
  // before the dock's first activation.
  var PANEL_PREFIX = 'block_panel-';
  var currentBlock = null;

  function paintCurrent(list) {
    list.querySelectorAll('.blockr-otl-row[data-blk]').forEach(function (r) {
      var on = currentBlock !== null && r.getAttribute('data-blk') === currentBlock;
      if (r.classList.contains('is-current') === on) return;
      r.classList.toggle('is-current', on);
      if (on) r.setAttribute('aria-current', 'true');
      else r.removeAttribute('aria-current');
    });
  }

  document.addEventListener('dockview:active-panel', function (e) {
    var id = e.detail && e.detail.id;
    if (typeof id !== 'string' || id.indexOf(PANEL_PREFIX) !== 0) return;
    currentBlock = id.slice(PANEL_PREFIX.length);
    document.querySelectorAll('[data-otl-current-list]').forEach(paintCurrent);
  });

  // Keep `list` painted: now, on every activation, and whenever its rows
  // are drawn again (the report patches rows, the deck re-renders).
  function watchCurrent(list) {
    if (!list || list.hasAttribute('data-otl-current-list')) return;
    list.setAttribute('data-otl-current-list', '');
    paintCurrent(list);
    new MutationObserver(function () { paintCurrent(list); })
      .observe(list, { childList: true, subtree: true });
  }

  // ---- block lists: the row's "…" menu ---------------------------------

  // Opens the row's action menu under its "…" (Blockr.menu, lined up with
  // the trigger's right edge). A second click on the same "…" closes it. The
  // row keeps its tools up while the menu is open.
  var rowMenuOpen = null;
  function rowMenu(btn, items) {
    if (rowMenuOpen && rowMenuOpen.btn === btn) { rowMenuOpen.handle.close(); return; }
    var row = btn.closest('.blockr-otl-row');
    if (row) row.classList.add('is-menu-open');
    btn.setAttribute('aria-expanded', 'true');
    var handle = Blockr.menu(btn, {
      items: items,
      align: 'end',
      onClose: function () {
        if (row) row.classList.remove('is-menu-open');
        btn.setAttribute('aria-expanded', 'false');
        if (rowMenuOpen && rowMenuOpen.btn === btn) rowMenuOpen = null;
      }
    });
    rowMenuOpen = { btn: btn, handle: handle };
  }

  // ---- block lists: renaming a row's block in place ---------------------

  // The name turns into a field at its own size (design system, "Renaming
  // in place"). Enter or a click elsewhere commits, Escape restores; an
  // empty name is refused in place. `onCommit(name)` gets a changed,
  // non-empty name. While the field is up the row does not drag, so the
  // pointer can select text in it.
  function renameRow(nameEl, onCommit) {
    if (!nameEl || nameEl.classList.contains('is-editing')) return;
    var row = nameEl.closest('.blockr-otl-row');
    var old = (nameEl.textContent || '').trim();
    var input = document.createElement('input');
    input.type = 'text';
    input.className = 'blockr-otl-row__input';
    input.value = old;
    input.setAttribute('aria-label', 'Block name');
    var msg = document.createElement('div');
    msg.className = 'blockr-otl-row__error';
    msg.hidden = true;
    msg.textContent = 'A name cannot be empty';
    if (Blockr.tooltip && Blockr.tooltip.hide) Blockr.tooltip.hide();
    nameEl.textContent = '';
    nameEl.classList.add('is-editing');
    nameEl.appendChild(input);
    nameEl.appendChild(msg);
    if (row) {
      row.classList.add('is-renaming');
      row.setAttribute('draggable', 'false');
    }
    var done = false;
    var finish = function (commit) {
      if (done) return;
      var v = input.value.trim();
      if (commit && !v) {
        input.classList.add('is-invalid');
        msg.hidden = false;
        input.focus();
        return;
      }
      done = true;
      nameEl.classList.remove('is-editing');
      nameEl.textContent = commit && v ? v : old;
      if (row) {
        row.classList.remove('is-renaming');
        row.setAttribute('draggable', 'true');
        if (row.isConnected) row.focus();
      }
      if (commit && v && v !== old) onCommit(v);
    };
    input.addEventListener('keydown', function (e) {
      e.stopPropagation();
      if (e.key === 'Enter') { e.preventDefault(); finish(true); }
      else if (e.key === 'Escape') { e.preventDefault(); finish(false); }
    });
    input.addEventListener('input', function () {
      input.classList.remove('is-invalid');
      msg.hidden = true;
    });
    // The field's own clicks are not the row's (open, drag).
    ['click', 'dblclick', 'mousedown'].forEach(function (t) {
      input.addEventListener(t, function (e) { e.stopPropagation(); });
    });
    input.addEventListener('blur', function () {
      if (done) return;
      finish(!!input.value.trim());
    });
    input.focus();
    input.select();
  }

  // Focus a row redrawn after a keyboard move. The redrawn row is a new
  // element focused from script, which the browser does not count as
  // keyboard focus, so the row carries the keyboard look itself until it
  // loses the focus.
  function keyFocus(row) {
    if (!row) return;
    row.focus();
    row.classList.add('is-key-focus');
    row.addEventListener('blur', function off() {
      row.classList.remove('is-key-focus');
      row.removeEventListener('blur', off);
    });
  }

  window.BlockrOutline = {
    boardMenu: boardMenu,
    rowKeys: rowKeys,
    keyFocus: keyFocus,
    rowMenu: rowMenu,
    renameRow: renameRow,
    watchCurrent: watchCurrent,
    currentBlock: function () { return currentBlock; },
    checkSvg: CHECK_SVG
  };
})();
