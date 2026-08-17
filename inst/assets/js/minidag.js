/* Minidag: the BOARD adapter for the list+rail editor (blockr.outline).
 *
 * The drawing and the gestures live in `minidag-rail.js`, which knows nothing
 * about blockr. This file is everything that does: the Shiny channels, the
 * one-instance-per-container registry, block arity, and how a block paints
 * itself in a row.
 *
 * One instance per `.minidag[data-ns]` container. The R side pushes the full
 * board model ('minidag-data') and per-block status badges ('minidag-badge');
 * user gestures come back out of the renderer as `emit(name, payload)` and go
 * on as event-priority Shiny inputs (link_add, link_rm, block_rm,
 * block_rename, block_select, block_append, block_add, stack_add,
 * stack_rename, stack_rm, stack_join, stack_leave). The client never mutates
 * the model itself — every edit round-trips through the board and comes back
 * as a data push.
 *
 * Model: blocks [{id, name, category, color, icon, inputs[], variadic}],
 * links [{id, from, to, input}], stacks [{id, name, color, blocks[]}].
 * A named input slot is FULL when a link with that (to, input) exists;
 * variadic blocks accept unlimited links on the '' slot (blockr.core
 * semantics — mirrored here for instant feedback, enforced again in R).
 */
(() => {
  'use strict';

  const registry = new Map();

  const getInst = (elId) => {
    let inst = registry.get(elId);
    if (!inst) {
      const el = document.getElementById(elId);
      if (!el) return null;
      inst = createInstance(el);
      registry.set(elId, inst);
    }
    return inst;
  };

  const announceAll = () => {
    document.querySelectorAll('.minidag[data-ns]').forEach((el) => {
      const inst = getInst(el.id);
      if (inst && !inst.announced) {
        inst.announced = true;
        Shiny.setInputValue(inst.ns + 'ready', true);
      }
    });
  };

  if (window.Shiny) {
    $(document).on('shiny:connected', announceAll);
    Shiny.addCustomMessageHandler('minidag-data', (msg) => {
      const inst = getInst(msg.el);
      if (inst) inst.setData(msg);
    });
    Shiny.addCustomMessageHandler('minidag-badge', (msg) => {
      const inst = getInst(msg.el);
      if (inst) inst.setBadge(msg);
    });
    // The catalogue is a pure function of the registry, so it arrives once
    // when the client announces itself, not on every board change.
    Shiny.addCustomMessageHandler('minidag-registry', (msg) => {
      const inst = getInst(msg.el);
      if (inst) inst.setRegistry(msg);
    });
    Shiny.addCustomMessageHandler('minidag-clipboard', (msg) => {
      const inst = getInst(msg.el);
      if (inst) inst.setClipboard(msg.json);
    });
  }

  // test/inspection hook
  window._minidag = (elId) => {
    const inst = elId ? registry.get(elId) : registry.values().next().value;
    return inst ? inst.inspect() : null;
  };

  function createInstance(rootEl) {
    const ns = rootEl.dataset.ns;
    const push = (name, payload) =>
      Shiny.setInputValue(ns + name, payload, { priority: 'event' });

    // the renderer hands the model back on every query, so arity is always
    // read off the board as it stands, never off a stale copy
    let blocks = [], links = [], views = [], stacks = [], extensions = [];

    // The one held gesture in the menu: clearing the minidag's OWN tick on the
    // view you are looking at removes the panel the click happened in, and the
    // way back is blockr.dock's per-view "+ Add panel" picker rather than
    // anything in here. So that box arms on the first click and commits on the
    // second. Everything else is one click.
    let armedTick = null;             // '<extension id>@<view id>' while armed

    const blockOf = (id) => blocks.find((b) => b.id === id);
    const linksInto = (id) => links.filter((l) => l.to === id);

    // free input slots of a block: unoccupied named slots, plus the ''
    // (variadic) slot which never fills up
    const freeSlots = (b) => {
      const occ = new Set(linksInto(b.id).map((l) => l.input));
      const free = (b.inputs || []).filter((s) => !occ.has(s));
      if (b.variadic) free.push('');
      return free;
    };

    // The glyph comes from the catalogue, keyed by the block's type, because
    // an icon belongs to a type and not to a board: it arrives once instead
    // of riding every board change as a base64 data URI. The tile is tinted
    // from the block's own colour and the SVG uses `currentColor`, so it
    // renders white on the solid square -- the same pairing the picker rows
    // use. A type the catalogue does not carry falls back to the letter.
    const iconFor = (type) => {
      const m = registry.byType && registry.byType.get(type);
      return m && m.icon ? m.icon : null;
    };

    const kindIcon = (b) => {
      const k = document.createElement('span');
      k.className = 'md-kind';
      k.style.background = b.color || '#999';
      const svg = iconFor(b.type);
      if (svg) {
        k.innerHTML = svg;
      } else {
        k.textContent = (b.name || '?').slice(0, 1).toUpperCase();
      }
      k.title = b.category || '';
      return k;
    };

    // input-slot pips: an open circle per FREE named slot, an ∞ pip for
    // variadic blocks; data blocks (no inputs) show nothing.
    //
    // A pip only when it has news. A SATISFIED slot draws nothing: the rail
    // already draws what feeds it and the connections popover has the detail,
    // so a filled pip only restated the icon's colour on every row of a board
    // of one-input transforms -- and did it in the position the eye reads as
    // status. A free slot is the news ("this block still wants an input"), and
    // it is legible at rest, which it never was before.
    const portsStrip = (b) => {
      const wrap = document.createElement('span');
      wrap.className = 'md-ports';
      const occ = new Map();
      linksInto(b.id).forEach((l) => occ.set(l.input, l.from));
      (b.inputs || []).forEach((slot) => {
        if (occ.has(slot)) return;
        const pip = document.createElement('span');
        pip.className = 'md-pip';
        pip.title = slot + ' — free';
        wrap.appendChild(pip);
      });
      if (b.variadic) {
        const n = linksInto(b.id).filter((l) =>
          !(b.inputs || []).includes(l.input)).length;
        // A variadic block never fills up, so the ∞ stays: it says "more may
        // land here", which the absence of a pip could not. Untinted now --
        // the block-coloured version competed with the status dot beside it.
        const pip = document.createElement('span');
        pip.className = 'md-pip md-pip-inf';
        pip.textContent = '∞';
        pip.title = n ? n + ' inputs (unlimited)' : 'unlimited inputs';
        wrap.appendChild(pip);
      }
      return wrap;
    };

    const rail = minidagRail.create(rootEl, {
      // Renderer options go under `opts`; the rest of this object is the adapter
      // contract (emit, nodeLead, ...).
      //
      // The parentless add lives at the END of the flow, not in the search row:
      // every other add gesture is on the left and about a parent (drag a dot to
      // a row, drag it to the gutter, the row's own `+`), and this is the one
      // that is about no parent -- so it belongs where the block will appear,
      // which is after the last row.
      opts: { addButton: false, addRow: true },
      // Adding and appending are the same operation; only the origin
      // differs, so they open the same picker rather than two sidebars.
      // `block_append` carries the release coordinates, which is what the
      // renderer has always sent them for.
      emit: (name, payload) => {
        // Until the catalogue lands the picker cannot open, and swallowing
        // the gesture would be worse than the old behaviour -- so fall
        // through to the board's own browser instead.
        if (name === 'block_append' && registry.append.length) {
          openPicker(payload.from, null, { x: payload.x, y: payload.y });
          return;
        }
        if (name === 'block_add') {
          requestAdd(rootEl.querySelector('.md-addrow'));
          return;
        }
        push(name, payload);
      },

      // a fragment, so icon and pips stay DIRECT children of the row: the
      // chip is a flex line and a wrapper span would collapse them into one
      // item with its own gap
      nodeLead: (b) => {
        const lead = document.createDocumentFragment();
        lead.appendChild(kindIcon(b));
        // only when it has pips in it: an empty strip still costs the row's
        // 7px flex gap, which would push every fully-wired row's status dot
        // and name off the alignment the unwired ones keep
        const ports = portsStrip(b);
        if (ports.childElementCount) lead.appendChild(ports);
        return lead;
      },

      // a board constrains the CONSUMER: the edge occupies one of `to`'s
      // free input slots, and an empty answer is what refuses the drop
      slotsFor: (from, to) => freeSlots(to),

      // an auto-named variadic link is not a slot anyone chose, so the
      // connections popover keeps quiet about it
      showSlot: (l) => l.input !== '' &&
        ((blockOf(l.to) || {}).inputs || []).includes(l.input),

      nodeAside: (b) => {
        const wrap = membershipEl([b.id]) || el('md-views');
        if (!registry.append.length) return wrap;
        const plus = el('md-rowadd', 'button');
        plus.type = 'button';
        plus.textContent = '+';
        plus.title = 'Append a block after ' + b.name;
        plus.addEventListener('click', (ev) => {
          ev.stopPropagation();
          openPicker(b.id, ev.currentTarget.closest('.md-chip'), null);
        });
        wrap.appendChild(plus);
        return wrap;
      },
      stackAside: (s, collapsed) => membershipEl(s.blocks, collapsed)
    });

    /* ---- the membership column ------------------------------------------
     *
     * Reporting, and only reporting: the slot past the spring names the views a
     * row is shown on, or says "all views" when that is every one of them and
     * "no view" when it is none. Editing is the row menu's job -- there is
     * exactly one way to write this, which is what retired the focused-view
     * toggles and the view list they lived in.
     *
     * `ids` is a set because a stack row stands for its members. `kind` is
     * 'blocks' or 'extensions' and picks which membership list of the view the ids are
     * looked up in; everything else is shared, because a extension row is a row.
     */

    const viewOf = (id) => views.find((v) => v.id === id);
    const memberIds = (v, kind) => v[kind === 'extensions' ? 'extensions' : 'blocks'];
    const viewsOf = (id, kind) =>
      views.filter((v) => memberIds(v, kind).includes(id));
    const activeView = () => views.find((v) => v.active) || null;

    const el = (cls, tag) => {
      const e = document.createElement(tag || 'span');
      e.className = cls;
      return e;
    };

    const membershipEl = (ids, collapsed, kind) => {
      if (!views.length) return null;

      kind = kind || 'blocks';
      const wrap = el('md-views');

      const mine = views.filter((v) =>
        ids.some((id) => memberIds(v, kind).includes(id)));

      // On every view, and on none, are the two answers a list of names reads
      // worst: six names is not a fact anybody reads, and an empty slot is
      // indistinguishable from a slot that failed to render.
      const whole = ids.length && views.every((v) =>
        ids.every((id) => memberIds(v, kind).includes(id)));

      if (whole) {
        const t = el('md-vtag md-vall-tag');
        t.textContent = 'all views';
        t.title = views.map((v) => v.name).join(', ');
        wrap.appendChild(t);
        return wrap;
      }

      // Shown nowhere is the ORDINARY case, not a state calling for an action:
      // on a real board half the rows are mutates, joins and reads that nobody
      // ever puts on a page. So the slot stays empty and says nothing. Right-click
      // is there if you want to place it.
      if (!mine.length) {
        return wrap;
      }

      const tag = el('md-vtag');
      tag.textContent = mine[0].name;
      tag.title = mine.map((v) => v.name).join(', ');
      wrap.appendChild(tag);

      if (mine.length > 1) {
        const more = el('md-vtag md-vmore');
        more.textContent = '+' + (mine.length - 1);
        more.title = mine.slice(1).map((v) => v.name).join(', ');
        wrap.appendChild(more);
      }

      // a collapsed stack hides the rows that would have shown the split
      if (collapsed && ids.length > 1) {
        const inAny = ids.filter((id) =>
          views.some((v) => memberIds(v, kind).includes(id))).length;
        if (inAny < ids.length) {
          const n = el('md-vtag md-vmore');
          n.textContent = inAny + '/' + ids.length;
          wrap.appendChild(n);
        }
      }

      return wrap;
    };

    // The parentless add is drawn by the renderer as the last row of the list
    // (`opts.addRow`); this is what it opens. Until the catalogue lands the
    // picker cannot open, and swallowing the gesture would be worse than the old
    // behaviour -- so fall through to the board's own browser instead.
    const requestAdd = (anchor) => {
      if (registry.add.length) {
        openPicker(null, anchor || rootEl.querySelector('.md-addrow'), null);
      } else {
        push('block_add', true);
      }
    };

    /* ---- the extensions group, at the foot of the flow ------------------------
     *
     * Extensions as rows. An extension is not in the DAG, so it has no place in
     * an order derived from it and no lane on the rail; it gets a group after
     * the last block instead, carrying the same membership column, which is the
     * whole point -- "which view is this on?" gets one answer shape for every
     * panel the board has.
     *
     * The catalogue comes from the board (`minidag_extensions()`), not from view
     * membership, so an extension on NO view still has a row. That is the state
     * per-view membership alone can never report, and the one you need a row to
     * get out of.
     */
    const extRow = (t) => {
      const row = el('md-chip md-ext', 'div');
      row.dataset.ext = t.id;

      const k = el('md-kind md-extkind');
      k.textContent = (t.name || '?').slice(0, 1).toUpperCase();
      row.appendChild(k);

      const nm = el('md-name');
      nm.textContent = t.name;
      nm.title = t.name + (t.self ? ' \u00b7 this panel' : '');
      row.appendChild(nm);

      const mem = membershipEl([t.id], false, 'extensions');
      if (mem) row.appendChild(mem);

      return row;
    };

    const paintExts = () => {
      let box = rootEl.querySelector('.md-exts');

      // Nothing to say with no extensions, and nothing to say with no views:
      // membership needs somewhere to be a member of.
      if (!extensions.length || !views.length) {
        if (box) box.remove();
        return;
      }

      if (!box) {
        box = el('md-exts', 'div');
        rootEl.appendChild(box);
      }
      box.innerHTML = '';

      const head = el('md-extshead', 'div');
      head.textContent = 'Extensions';
      const n = el('md-extscount');
      n.textContent = String(extensions.length);
      head.appendChild(n);
      box.appendChild(head);

      extensions.forEach((t) => box.appendChild(extRow(t)));
    };

    /* ---- the row menu ----------------------------------------------------
     *
     * The one place membership is written. It carries the whole answer: three
     * presets, then a tick per view, then the row operations that have no
     * discoverable gesture. Connecting and appending are NOT here -- the rail
     * dot and the row's `+` are better affordances than a dialog, and a menu
     * entry would teach the wrong gesture for the thing the rail is best at.
     *
     * It acts on the SELECTION, the way a file manager does: right-clicking a
     * selected row speaks for all of them, and right-clicking an unselected one
     * collapses the selection to it first, so the menu never speaks for rows
     * carrying no mark.
     *
     * It lives on `document.body`, fixed to the viewport. It cannot be a child
     * of the container: `.dockview-panel` scrolls (`overflow: auto`) and
     * `.blockr-view-container` clips (`overflow: hidden`), so a menu taller than
     * the panel would be cut off or would make the panel scroll. `.md-droptip`
     * already does this, for the same reason.
     */

    // A preset is a STATE of the ticks below it, not a separate command, which
    // is why the three read as radios: whichever one currently holds is lit, and
    // ticking a box by hand lights none of them.
    const presetOf = (ids, kind) => {
      if (!ids.length || !views.length) return null;
      const on = (v) => ids.every((id) => memberIds(v, kind).includes(id));
      const none = (v) => !ids.some((id) => memberIds(v, kind).includes(id));
      if (views.every(on)) return 'all';
      if (views.every(none)) return 'none';
      const act = activeView();
      if (act && on(act) && views.every((v) => v === act || none(v))) {
        return 'only';
      }
      return null;
    };

    let menuEl = null, menuSpec = null, menuOpenedAt = 0;

    const closeMenu = () => {
      if (menuEl) {
        menuEl.remove();
        menuEl = null;
      }
      menuSpec = null;
      armedTick = null;
    };

    // A tick round-trips through the board, so every tick provokes a push. If a
    // push closed the menu, "ticking keeps it open" would be impossible -- so the
    // push REPAINTS it, and the boxes, counts and lit preset follow the state
    // that came back. It closes only when what it is about is gone: a block
    // removed, an extension unmounted, the last view dropped.
    const refreshMenu = () => {
      if (!menuEl || !menuSpec) {
        return;
      }
      const alive = menuSpec.blocks.every((id) => !!blockOf(id)) &&
        menuSpec.extensions.every((id) => extensions.some((t) => t.id === id));
      if (!alive || views.length < 2 && menuSpec.kind === 'ext') {
        closeMenu();
        return;
      }
      render(menuSpec);
    };

    const write = (spec, mode, view) => {
      push('membership', {
        blocks: spec.blocks, extensions: spec.extensions, mode: mode, view: view || null
      });
    };

    const menuBtn = (label, key, cls, fn) => {
      const b = el('md-ctxitem' + (cls ? ' ' + cls : ''), 'button');
      b.type = 'button';
      const l = el('md-ctxlabel');
      l.textContent = label;
      b.appendChild(l);
      if (key) {
        const k = el('md-ctxkey');
        k.textContent = key;
        b.appendChild(k);
      }
      if (fn) {
        b.addEventListener('click', (ev) => {
          ev.stopPropagation();
          fn();
        });
      } else {
        b.disabled = true;
      }
      return b;
    };

    // The segmented preset row. One row rather than three, so four views show at
    // rest in the same height. It cannot carry the active view's NAME (a third of
    // the menu's width will not hold it), so it says "Current" and the marked row
    // in the list below is what names it.
    const presetRow = (spec, current) => {
      const seg = el('md-ctxseg', 'div');
      const act = activeView();

      const one = (label, key, mode, view, title) => {
        const b = el('md-ctxsegbtn' + (current === key ? ' on' : ''), 'button');
        b.type = 'button';
        b.textContent = label;
        b.title = title;
        b.addEventListener('click', (ev) => {
          ev.stopPropagation();
          closeMenu();
          write(spec, mode, view);
        });
        return b;
      };

      seg.appendChild(one('All', 'all', 'all', null,
        'Show on every view (' + views.length + ')'));
      seg.appendChild(one('None', 'none', 'none', null,
        'Show on no view — it stays on the board'));
      if (act) {
        seg.appendChild(one('Current', 'only', 'only', act.id,
          'Show on ' + act.name + ' only'));
      }
      return seg;
    };

    const tickRow = (spec, v) => {
      const kind = spec.extensions.length ? 'extensions' : 'blocks';
      const ids = spec.blocks.concat(spec.extensions);
      const on = ids.filter((id) => memberIds(v, kind).includes(id)).length;
      const state = on === 0 ? 'none' : on === ids.length ? 'all' : 'some';

      // The one held gesture: clearing our own tick on the view we are shown in
      // removes the panel the click happened in.
      const mine = spec.extensions.length === 1 &&
        (extensions.find((t) => t.id === spec.extensions[0]) || {}).self;
      const armKey = spec.extensions[0] + '@' + v.id;
      const armed = mine && armedTick === armKey;

      const row = el('md-ctxtick' + (armed ? ' md-armed' : ''), 'div');

      row.appendChild(el('md-ctxbox' + (state === 'all' ? ' on'
        : state === 'some' ? ' some' : '')));

      const nm = el('md-ctxviewname');
      nm.textContent = armed ? 'Remove from ' + v.name + '?' : v.name;
      row.appendChild(nm);

      const n = el('md-ctxviewn');
      n.textContent = v.n;
      n.title = v.n + ' panels on ' + v.name;
      row.appendChild(n);

      if (v.active) {
        const dot = el('md-ctxactive');
        dot.title = 'The current view';
        row.appendChild(dot);
      }

      // "send it to this one": the box adds, the word clears every other view
      const only = el('md-ctxonly');
      only.textContent = 'only';
      only.title = 'Show on ' + v.name + ' and no other view';
      only.addEventListener('click', (ev) => {
        ev.stopPropagation();
        closeMenu();
        write(spec, 'only', v.id);
      });
      row.appendChild(only);

      row.addEventListener('click', (ev) => {
        ev.stopPropagation();
        const clearing = state !== 'none';

        if (mine && clearing && v.active && !armed) {
          armedTick = armKey;
          render(spec);          // repaint in place: the menu stays open
          return;
        }

        armedTick = null;
        write(spec, clearing ? 'rm' : 'add', v.id);
        // ticking is usually done more than once, so the menu stays open and the
        // board push repaints it; the presets and counts follow.
      });

      return row;
    };

    const render = (spec) => {
      const ids = spec.blocks.concat(spec.extensions);
      const kind = spec.extensions.length ? 'extensions' : 'blocks';
      const current = presetOf(ids, kind);
      const box = menuEl;

      box.innerHTML = '';

      const head = el('md-ctxhead', 'div');
      head.textContent = spec.label;
      const hn = el('md-ctxheadn');
      hn.textContent = views.filter((v) =>
        ids.some((id) => memberIds(v, kind).includes(id))).length
        + '/' + views.length;
      head.appendChild(hn);
      box.appendChild(head);

      box.appendChild(presetRow(spec, current));

      const cap = el('md-ctxcap', 'div');
      cap.textContent = 'Views';
      const cn = el('md-ctxcapn');
      cn.textContent = views.length;
      cap.appendChild(cn);
      box.appendChild(cap);

      const list = el('md-ctxticks', 'div');
      views.forEach((v) => list.appendChild(tickRow(spec, v)));
      box.appendChild(list);

      // The row operations, and only the ones with no gesture of their own.
      if (spec.kind !== 'ext') {
        box.appendChild(el('md-ctxsep', 'div'));

        if (spec.kind === 'block' && spec.blocks.length === 1) {
          box.appendChild(menuBtn('Rename', 'dbl-click', '', () => {
            closeMenu();
            rail.editName(spec.blocks[0]);
          }));
        }
        if (spec.kind === 'stack') {
          box.appendChild(menuBtn('Rename group', 'dbl-click', '', () => {
            closeMenu();
            rail.editName('stack:' + spec.stack);
          }));
        }

        box.appendChild(menuBtn('Copy', CLIP_KEY + 'C', '', () => {
          closeMenu();
          document.execCommand('copy');
        }));
        box.appendChild(menuBtn('Cut', CLIP_KEY + 'X', '', () => {
          closeMenu();
          document.execCommand('cut');
        }));

        box.appendChild(el('md-ctxsep', 'div'));
        box.appendChild(menuBtn(
          spec.blocks.length > 1
            ? 'Remove ' + spec.blocks.length + ' blocks'
            : 'Remove block',
          '', 'md-ctxdanger', () => {
            closeMenu();
            spec.blocks.forEach((id) => push('block_rm', { id: id }));
          }
        ));
      }
    };

    const CLIP_KEY = /Mac|iP/.test(navigator.platform || '') ? '⌘' : 'Ctrl+';

    // Reads the clipboard the way blockr.dag's Paste entry does. A menu cannot
    // know in advance whether the clipboard holds a subboard (reading it is
    // async and permissioned), so the item is always offered and a foreign
    // clipboard is simply ignored -- the same contract as the paste keystroke.
    const pasteFromClipboard = () => {
      if (!navigator.clipboard || !navigator.clipboard.readText) return;
      navigator.clipboard.readText().then((text) => {
        if (!text) return;
        let data = null;
        try { data = JSON.parse(text); } catch (e) { return; }
        if (!data || data.object !== 'subboard') return;
        push('block_paste', { json: text });
      }).catch(() => {});
    };

    const openBoardMenu = (ev) => {
      closeMenu();

      menuEl = el('md-ctxmenu md-ctxboard', 'div');
      menuSpec = null;                  // nothing to repaint on a board push

      const head = el('md-ctxhead', 'div');
      head.textContent = 'Board';
      menuEl.appendChild(head);

      menuEl.appendChild(menuBtn('Add a block', '', '', () => {
        closeMenu();
        requestAdd(null);
      }));
      menuEl.appendChild(menuBtn('Paste', CLIP_KEY + 'V', '', () => {
        closeMenu();
        pasteFromClipboard();
      }));

      document.body.appendChild(menuEl);
      placeMenu(ev);
    };

    // Fixed to the viewport, then clamped: flip up when it would run off the
    // bottom, and pull left when it would run off the right.
    const placeMenu = (ev) => {
      const box = menuEl.getBoundingClientRect();
      let left = ev.clientX;
      let top = ev.clientY;
      if (left + box.width > window.innerWidth - 6) {
        left = Math.max(6, window.innerWidth - box.width - 6);
      }
      if (top + box.height > window.innerHeight - 6) {
        top = Math.max(6, ev.clientY - box.height);
      }
      menuEl.style.left = left + 'px';
      menuEl.style.top = top + 'px';
      menuEl.style.visibility = '';
      menuOpenedAt = performance.now();
    };

    const openMenu = (spec, ev) => {
      closeMenu();

      if (!spec.blocks.length && !spec.extensions.length) return;
      // Nothing to say about placement on a board with one view, and for an extension
      // row placement is all the menu has -- so it does not open at all there.
      if (views.length < 2 && spec.kind === 'ext') return;

      menuEl = el('md-ctxmenu', 'div');
      menuSpec = spec;
      menuEl.style.visibility = 'hidden';
      document.body.appendChild(menuEl);
      render(spec);

      placeMenu(ev);
    };

    // One delegated listener for every row shape the minidag draws. The rail owns
    // block rows and stack heads (inside `.md-deck`), this file owns extension rows;
    // the menu does not care which, because membership does not.
    rootEl.addEventListener('contextmenu', (ev) => {
      const ext = ev.target.closest('.md-ext[data-ext]');
      const head = ev.target.closest('.md-stackhead[data-stack]');
      const row = ev.target.closest('.md-chip[data-id]');

      ev.preventDefault();

      // Empty space is a target of its own: the two operations that are about
      // the BOARD rather than about a row. Paste has no other possible home --
      // it is not about a row, so it cannot be in a row menu.
      if (!ext && !head && !row) {
        openBoardMenu(ev);
        return;
      }

      if (ext) {
        const t = extensions.find((x) => x.id === ext.dataset.ext);
        openMenu({
          kind: 'ext', blocks: [], extensions: [ext.dataset.ext],
          label: (t || {}).name || ext.dataset.ext
        }, ev);
        return;
      }

      if (head) {
        const stack = stacks.find((x) => x.id === head.dataset.stack);
        const mem = stack ? asArr(stack.blocks) : [];
        if (!mem.length) return;
        rail.selectOnly(mem);
        openMenu({
          kind: 'stack', blocks: mem, extensions: [], stack: stack.id,
          label: (stack.name || 'Group') + ' · ' + mem.length + ' blocks'
        }, ev);
        return;
      }

      const id = row.dataset.id;
      let sel = rail.selection();

      if (!sel.includes(id)) {
        rail.selectOnly([id]);
        sel = [id];
      }

      openMenu({
        kind: 'block', blocks: sel, extensions: [],
        label: sel.length > 1
          ? sel.length + ' rows selected'
          : (blockOf(id) || {}).name || id
      }, ev);
    });

    // Dismissal: a click anywhere outside, Escape, or a scroll -- the menu is
    // fixed to the viewport, so a panel scrolling under it would leave it
    // pointing at a row that has moved.
    document.addEventListener('click', (ev) => {
      if (menuEl && !menuEl.contains(ev.target)) closeMenu();
    });
    document.addEventListener('keydown', (ev) => {
      if (ev.key === 'Escape') closeMenu();
    });
    // ...but NOT the scroll that opening it caused. Right-clicking a row near the
    // panel's edge scrolls it into view first, and a smooth scroll keeps firing
    // for a few frames after the contextmenu -- so an unguarded listener closed
    // the menu the same gesture had just opened. Anything later than the settling
    // window is a real scroll and does dismiss it.
    window.addEventListener('scroll', () => {
      if (menuEl && performance.now() - menuOpenedAt > 260) closeMenu();
    }, true);

    // Chrome the adapter owns, as opposed to the rows the renderer draws. Only
    // the extensions group is left: the view list, its trigger and the focused-view
    // dimming all went when the menu took over editing, and with them the grid
    // layout mode the list needed.
    const paintChrome = () => {
      paintExts();
    };

    /* ---- the inline block picker ----------------------------------------
     *
     * One surface for adding and appending, because they are one operation:
     * blockr.dock's `target_mode()` returns "add" when there is no target,
     * and the only downstream difference is that an append also makes a
     * link. So the origin decides two things and nothing else -- whether a
     * link is created, and which pool is offered (appending needs a block
     * that can receive a link).
     *
     * Two states, one component. At REST it browses: every type, grouped by
     * category, with its description. That is the catalogue, which is why
     * the minidag needs no second copy of it in a pane. As soon as you type it
     * RECALLS: flat, ranked, keyboard-driven, uncapped -- a silent
     * truncation would hide types from the only place they are listed.
     *
     * It opens AT the insertion point rather than as a page overlay, so the
     * rail and the origin row stay on screen while you choose.
     */

    let registry = { add: [], append: [] };
    let picker = null, pickMatches = [], pickCursor = -1;

    const closePicker = () => {
      if (picker) picker.remove();
      picker = null;
      pickMatches = [];
      pickCursor = -1;
    };

    const commitPick = (meta, originId) => {
      closePicker();
      push('block_insert', { type: meta.type, from: originId || null });
    };

    const openPicker = (originId, anchor, at) => {

      closePicker();

      const pool = originId ? registry.append : registry.add;
      if (!pool.length) return;

      const box = el('md-picker', 'div');

      // No context header. The picker opens ON the row whose `+` you pressed,
      // or where you released the drag, so the origin is stated by position;
      // a line above the list would only restate what you can see. It is also
      // the one element here that had no equivalent in the block browser.
      const inp = document.createElement('input');
      inp.className = 'md-pick-search';
      inp.type = 'search';
      inp.placeholder = 'Search block types…';
      box.appendChild(inp);

      const list = el('md-pick-list', 'div');
      box.appendChild(list);

      // The block browser's resting row: tinted tile, name, package badge,
      // and the description as `title` only. Its own comment says the
      // description band "stays hidden until the card is expanded, keeping
      // the resting list dense" -- putting all 60 on screen was the clutter.
      // `-1` means "no row is current", and it is also what `pickCursor`
      // holds while browsing -- so a bare `i === pickCursor` marked every
      // row current, giving all 60 the hover border and wash at rest. That
      // is what turned a flush list into a column of boxes.
      const isCur = (i) => i >= 0 && i === pickCursor;

      const rowFor = (m, i) => {
        const r = el('md-pick-row' + (isCur(i) ? ' cur' : ''), 'div');
        r.dataset.cat = m.category || 'other';
        const ic = el('md-kind');
        if (m.icon) {
          ic.innerHTML = m.icon;          // registry glyph, as the browser
        } else {
          ic.textContent = (m.name || '?').slice(0, 1).toUpperCase();
        }
        r.appendChild(ic);
        const nm = el('md-pick-name');
        nm.textContent = m.name;
        r.appendChild(nm);
        const pk = el('md-pick-pkg');
        pk.textContent = m.package;
        r.appendChild(pk);
        r.title = m.description || m.name;
        r.addEventListener('click', () => commitPick(m, originId));
        return r;
      };

      const paint = () => {

        const q = inp.value.trim().toLowerCase();
        list.innerHTML = '';
        list.classList.toggle('browse', !q);

        if (!q) {
          pickMatches = pool;
          pickCursor = -1;            // nothing preselected while browsing
          const groups = new Map();
          pool.forEach((m) => {
            const k = m.category || 'other';
            if (!groups.has(k)) groups.set(k, []);
            groups.get(k).push(m);
          });
          groups.forEach((items, cat) => {
            // the category name alone, as `category_section()` renders it
            // (`tags$h3(category)`) -- the count was one more thing to read
            const h = el('md-pick-group', 'div');
            h.textContent = cat;
            list.appendChild(h);
            items.forEach((m) => list.appendChild(rowFor(m, -1)));
          });
          return;
        }

        // name matches first, then the ones that only match by package or
        // category -- typing "dplyr" should find that package's blocks
        const hit = (m) => (m.name || '').toLowerCase().includes(q);
        const near = (m) => (m.package || '').toLowerCase().includes(q) ||
          (m.category || '').toLowerCase().includes(q) ||
          (m.description || '').toLowerCase().includes(q);
        pickMatches = pool.filter(hit).concat(
          pool.filter((m) => !hit(m) && near(m))
        );
        pickCursor = Math.max(0, Math.min(pickCursor, pickMatches.length - 1));
        pickMatches.forEach((m, i) => list.appendChild(rowFor(m, i)));

        if (!pickMatches.length) {
          const e = el('md-pick-group', 'div');
          e.textContent = 'no block type matches';
          list.appendChild(e);
        }
      };

      inp.addEventListener('input', () => { pickCursor = 0; paint(); });
      inp.addEventListener('keydown', (ev) => {
        if (ev.key === 'ArrowDown') {
          ev.preventDefault();
          pickCursor = Math.min(pickCursor + 1, pickMatches.length - 1);
          paint();
          const cur = list.querySelector('.cur');
          if (cur) cur.scrollIntoView({ block: 'nearest' });
        }
        if (ev.key === 'ArrowUp') {
          ev.preventDefault();
          pickCursor = Math.max(0, pickCursor - 1);
          paint();
          const cur = list.querySelector('.cur');
          if (cur) cur.scrollIntoView({ block: 'nearest' });
        }
        // only when something is genuinely selected: browsing preselects
        // nothing, so Enter there must not add whatever happens to be first
        if (ev.key === 'Enter' && pickCursor >= 0 && pickMatches[pickCursor]) {
          commitPick(pickMatches[pickCursor], originId);
        }
        if (ev.key === 'Escape') { ev.stopPropagation(); closePicker(); }
      });

      rootEl.appendChild(box);
      picker = box;
      paint();

      // Anchor at the insertion point. `at` is the drag's release position
      // (the renderer sends x/y on `block_append` for exactly this); a
      // trigger with no coordinates anchors under its own element.
      const rootBox = rootEl.getBoundingClientRect();
      let left, top;
      if (at) {
        left = at.x;
        top = at.y + 8;
      } else {
        const r = anchor.getBoundingClientRect();
        left = r.left - rootBox.left;
        top = r.bottom - rootBox.top + 6;
      }
      const w = box.offsetWidth, h = box.offsetHeight;
      // flip up rather than run off the bottom, which is the common case
      // when appending from the last row
      if (top + h > rootEl.clientHeight && top - h - 12 > 0) top = top - h - 20;
      box.style.left = Math.max(4, Math.min(left, rootEl.clientWidth - w - 4)) + 'px';
      box.style.top = Math.max(4, top) + 'px';

      inp.focus();
    };

    // Click outside closes it. Capture phase, so a click that also lands on
    // a row does not reopen it in the same gesture.
    document.addEventListener('mousedown', (ev) => {
      if (picker && !picker.contains(ev.target)) closePicker();
    }, true);

    /* ---- clipboard -------------------------------------------------
     *
     * Built on the native `copy` / `cut` / `paste` events, not on
     * `navigator.clipboard`. That API needs a permission Chrome prompts for
     * and Safari largely refuses, and reading from it is async -- so a
     * handler that awaits the read has already lost the user gesture by the
     * time it would call `preventDefault()`. The events carry
     * `clipboardData` synchronously and need no permission, because the user
     * pressing the key IS the authorisation.
     *
     * That leaves one problem: on `copy` the payload has to be handed over
     * synchronously, but only the server can build it. So the payload is
     * prepared AHEAD of the keystroke -- every time the selection changes,
     * the server sends the serialised subboard and it is cached here. By the
     * time anyone presses the key it is already sitting in `clipCache`.
     *
     * Wire-compatible with blockr.dag: the same `{object: "subboard"}`
     * envelope, so a selection copied on the canvas pastes here and back.
     *
     * Scoped to the minidag last clicked in, so a board mounting both this and
     * the DAG canvas does not act twice on one keystroke.
     */

    let clipCache = null;     // serialised subboard for the current selection
    let clipIds = [];         // what it was built from
    let lastSel = '';
    let lastInside = false;

    document.addEventListener('mousedown', (ev) => {
      lastInside = rootEl.contains(ev.target);
    }, true);

    // The renderer owns the selection and does not announce changes, so the
    // adapter reads it back after any gesture that could have altered it.
    const syncSelection = () => {
      const ids = rail.inspect().selection;
      const key = ids.join(',');
      if (key === lastSel) return;
      lastSel = key;
      clipCache = null;
      clipIds = ids;
      if (ids.length) push('block_selection', { ids });
    };

    rootEl.addEventListener('mouseup', () => setTimeout(syncSelection, 0));
    rootEl.addEventListener('keyup', () => setTimeout(syncSelection, 0));

    const setClipboard = (json) => { clipCache = json; };

    const ownsGesture = () => {
      if (!lastInside) return false;
      const a = document.activeElement;
      if (a && (a.tagName === 'INPUT' || a.tagName === 'TEXTAREA' ||
                a.tagName === 'SELECT' || a.isContentEditable)) {
        return false;
      }
      // real prose selected: leave the clipboard to the browser so an error
      // message or a column name copies as itself
      const sel = window.getSelection && window.getSelection();
      return !(sel && sel.toString().length > 0);
    };

    const onCopy = (cut) => (ev) => {
      if (!ownsGesture() || !clipIds.length) return;
      if (!clipCache) return;              // payload not back yet: do nothing
      ev.preventDefault();
      ev.clipboardData.setData('text/plain', clipCache);
      if (cut) push('block_cut', { ids: clipIds });
    };

    document.addEventListener('copy', onCopy(false));
    document.addEventListener('cut', onCopy(true));

    document.addEventListener('paste', (ev) => {
      if (!ownsGesture()) return;
      const text = ev.clipboardData && ev.clipboardData.getData('text/plain');
      if (!text) return;
      let data = null;
      try { data = JSON.parse(text); } catch (e) { return; }
      if (!data || data.object !== 'subboard') return;   // not ours: let it be
      ev.preventDefault();
      push('block_paste', { json: text });
    });

    const asArr = (x) => x == null ? [] : (Array.isArray(x) ? x : [x]);

    const setData = (msg) => {
      blocks = asArr(msg.blocks).map((b) => ({
        id: b.id, name: b.name, type: b.type || '',
        category: b.category || '', color: b.color,
        inputs: asArr(b.inputs), variadic: !!b.variadic
      }));
      links = asArr(msg.links).map((l) => ({
        id: l.id, from: l.from, to: l.to,
        input: l.input == null ? '' : l.input
      }));
      stacks = asArr(msg.stacks);
      views = asArr(msg.views).map((v) => ({
        id: v.id, name: v.name, active: !!v.active, n: +v.n || 0,
        blocks: asArr(v.blocks), extensions: asArr(v.extensions)
      }));
      extensions = asArr(msg.extensions).map((t) => ({
        id: t.id, name: t.name || t.id, self: !!t.self
      }));
      // An armed tick is a held gesture: arming pushes nothing, so any push
      // arriving between the two clicks came from somewhere else and the safe
      // reading is to disarm.
      armedTick = null;
      rail.setData({ blocks, links, stacks });
      paintChrome();
      refreshMenu();
    };

    const setRegistry = (msg) => {
      registry = {
        add: asArr(msg.add).map((m) => Object.assign({}, m, {
          inputs: asArr(m.inputs)
        })),
        append: asArr(msg.append).map((m) => Object.assign({}, m, {
          inputs: asArr(m.inputs)
        }))
      };
      registry.byType = new Map(registry.add.map((m) => [m.type, m]));
      // The catalogue and the first board model race -- both are sent when
      // the client announces itself -- so if the model won, the rows are
      // already drawn with letter tiles. Redraw once the glyphs exist.
      if (blocks.length) rail.render();
    };

    return {
      ns,
      announced: false,
      setData,
      setRegistry,
      setClipboard,
      setBadge: rail.setBadge,
      inspect: rail.inspect
    };
  }
})();
