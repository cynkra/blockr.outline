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
 * stack_rename, stack_rm). The client never mutates the model itself — every
 * edit round-trips through the board and comes back as a data push.
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
    let blocks = [], links = [], views = [], stacks = [];

    // Which view the deck is FOCUSED on, or null for "all views". This is
    // pure client state and deliberately not board state: it says what you
    // are looking at, not what the board is. It does not serialize, it does
    // not travel to another session, and it is not the board's ACTIVE view
    // (which decides where a revealed panel lands and does live on the
    // board). Focusing a view here never switches the dock.
    let focus = null;
    let listOpen = false;

    // Rename plumbing: `editingView` holds the view whose name is being
    // typed, so a server push does not replace the element under the caret;
    // `pendingList` remembers that a repaint was owed while that was true.
    let editingView = null, pendingList = false;
    let clickTimer = null;
    const DBL_MS = 240;

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

    const kindIcon = (b) => {
      const k = document.createElement('span');
      k.className = 'md-kind';
      if (b.icon) {
        const img = document.createElement('img');
        img.src = b.icon;
        img.alt = b.category || 'block';
        k.appendChild(img);
      } else {
        k.style.background = b.color || '#999';
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
      emit: push,

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

      nodeAside: (b) => membershipEl([b.id]),
      stackAside: (s, collapsed) => membershipEl(s.blocks, collapsed)
    });

    /* ---- view membership ------------------------------------------------
     *
     * One control, two readings, decided by whether a view is focused.
     * Unfocused it REPORTS ("this block is shown on Laboratory"); focused it
     * EDITS (a toggle for that one view). Both live past the spring so they
     * read as a column down the deck.
     *
     * `ids` is a set because a stack row stands for its members: one block
     * gives a plain toggle, several give a tri-state.
     */

    const viewOf = (id) => views.find((v) => v.id === id);
    const viewsOf = (blockId) => views.filter((v) => v.blocks.includes(blockId));

    const el = (cls, tag) => {
      const e = document.createElement(tag || 'span');
      e.className = cls;
      return e;
    };

    const membershipEl = (ids, collapsed) => {
      if (!views.length) return null;

      const wrap = el('md-views');

      if (focus) {
        const v = viewOf(focus);
        if (!v) return null;
        const inView = ids.filter((id) => v.blocks.includes(id)).length;
        const state = inView === 0 ? 'none'
          : inView === ids.length ? 'all' : 'some';

        const t = el('md-vtoggle' + (state === 'all' ? ' on'
          : state === 'some' ? ' some' : ''), 'button');
        t.type = 'button';
        t.title = ids.length === 1
          ? (state === 'all' ? 'Shown on ' + v.name + ' — click to remove'
            : 'Not on ' + v.name + ' — click to add')
          : inView + ' of ' + ids.length + ' shown on ' + v.name
            + (state === 'all' ? ' — click to remove all'
              : ' — click to add all');
        t.addEventListener('click', (ev) => {
          ev.stopPropagation();
          // a partial or empty set fills, a full one clears
          push('view_toggle', { view: v.id, blocks: ids, add: state !== 'all' });
        });
        wrap.appendChild(t);

        // a collapsed stack hides the rows that would have shown the split,
        // so the count has to be on the stack row itself
        if (state === 'some' && collapsed) {
          const n = el('md-vtag md-vmore');
          n.textContent = inView + '/' + ids.length;
          wrap.appendChild(n);
        }
        return wrap;
      }

      const mine = ids.length === 1
        ? viewsOf(ids[0])
        : views.filter((v) => ids.some((id) => v.blocks.includes(id)));

      if (!mine.length) return wrap;   // shown nowhere: an empty, silent slot

      const tag = el('md-vtag');
      tag.textContent = mine.length === 1 ? mine[0].name
        : (ids.length === 1 ? mine[0].name : mine.length + ' views');
      tag.title = mine.map((v) => v.name).join(', ');
      tag.addEventListener('click', (ev) => {
        ev.stopPropagation();
        setFocus(mine[0].id);
      });
      wrap.appendChild(tag);

      if (ids.length === 1 && mine.length > 1) {
        const more = el('md-vtag md-vmore');
        more.textContent = '+' + (mine.length - 1);
        more.title = mine.slice(1).map((v) => v.name).join(', ');
        wrap.appendChild(more);
      }
      return wrap;
    };

    /* ---- the view list --------------------------------------------------
     *
     * There is no mode switch: the mode IS which row of this list is
     * selected. "All views" reports membership, a view focuses it. A second
     * control that could disagree with the list would be one too many.
     *
     * The list is a panel you open, not furniture -- the deck often sits in
     * a narrow dock panel, so it is off by default and its trigger lives in
     * the renderer's own search row.
     */

    const PANEL_ICON = '<svg viewBox="0 0 24 24" fill="none" stroke="curren'
      + 'tColor" stroke-width="1.9"><rect x="3" y="4" width="18" height="16"'
      + ' rx="2"/><path d="M9 4v16"/></svg>';

    const setFocus = (id) => {
      focus = id;
      if (id) listOpen = true;
      // While a view is focused the deck is already using opacity to say
      // "not in this view"; the rail's hover dim would say "not connected to
      // the row under your pointer" in the same currency, over the same rows.
      rail.setHoverFocus(!id);
      rail.render();
      paintChrome();
    };

    const viewRow = (v) => {
      const row = el('md-vrow' + (focus === v.id ? ' on' : ''), 'div');
      row.dataset.view = v.id;

      const nm = el('md-vname');
      nm.textContent = v.name;
      nm.title = 'Click to focus · double-click to rename';
      nm.addEventListener('dblclick', (ev) => {
        ev.stopPropagation();
        // the two clicks of this double-click each queued a focus toggle;
        // let them go, or the list is rebuilt out from under the caret
        clearTimeout(clickTimer);
        editingView = v.id;
        nm.contentEditable = 'true';
        nm.focus();
        document.getSelection().selectAllChildren(nm);
      });
      nm.addEventListener('blur', () => {
        if (nm.contentEditable !== 'true') return;
        nm.contentEditable = 'false';
        editingView = null;
        const val = nm.textContent.trim();
        if (val && val !== v.name) push('view_rename', { id: v.id, name: val });
        nm.textContent = v.name;
        if (pendingList) paintList();
      });
      nm.addEventListener('keydown', (ev) => {
        if (ev.key === 'Enter') { ev.preventDefault(); nm.blur(); }
        if (ev.key === 'Escape') { nm.textContent = v.name; nm.blur(); }
      });
      row.appendChild(nm);

      const count = el('md-vcount');
      count.textContent = String(v.blocks.length);
      row.appendChild(count);

      // The board's active view decides where a revealed block lands, so it
      // is worth seeing. It is not the same as focus and must not look like
      // it: a dot, not a selection.
      if (v.active) {
        const dot = el('md-vactive');
        dot.title = 'Active view — revealing a block puts it here';
        row.appendChild(dot);
      }

      const rm = el('md-vrm', 'button');
      rm.type = 'button';
      rm.textContent = '×';
      rm.title = views.length > 1 ? 'Remove view' : 'The last view cannot go';
      rm.disabled = views.length < 2;
      rm.addEventListener('click', (ev) => {
        ev.stopPropagation();
        push('view_rm', { id: v.id });
      });
      row.appendChild(rm);

      // Focusing rebuilds this list, so it has to wait long enough to know
      // this is not the first half of a double-click -- otherwise renaming is
      // impossible: the caret lands in an element that has already been
      // replaced. The renderer solves the same problem for block names by
      // deferring renders while `editing()`; here the cheaper fix is to not
      // start the rebuild until the gesture is known.
      row.addEventListener('click', (ev) => {
        if (ev.target.isContentEditable) return;
        clearTimeout(clickTimer);
        clickTimer = setTimeout(
          () => setFocus(focus === v.id ? null : v.id), DBL_MS
        );
      });
      return row;
    };

    const paintList = () => {
      // A board push while a name is being typed would swap the element the
      // caret sits in, ending the edit and losing the keystrokes. Defer.
      if (editingView) {
        pendingList = true;
        return;
      }
      pendingList = false;

      let panel = rootEl.querySelector('.md-viewlist');
      if (!listOpen || !views.length) {
        if (panel) panel.remove();
        return;
      }
      if (!panel) {
        panel = el('md-viewlist', 'div');
        rootEl.insertBefore(panel, rootEl.firstChild);
      }
      panel.innerHTML = '';

      const head = el('md-vhead', 'div');
      head.textContent = 'Views';
      panel.appendChild(head);

      const all = el('md-vrow md-vall' + (focus ? '' : ' on'), 'div');
      const allNm = el('md-vname');
      allNm.textContent = 'All views';
      all.appendChild(allNm);
      const allN = el('md-vcount');
      allN.textContent = String(views.length);
      all.appendChild(allN);
      all.addEventListener('click', () => setFocus(null));
      panel.appendChild(all);

      views.forEach((v) => panel.appendChild(viewRow(v)));

      const add = el('md-vadd', 'button');
      add.type = 'button';
      add.textContent = '+ New view';
      add.addEventListener('click', () => push('view_add', { name: 'New view' }));
      panel.appendChild(add);
    };

    const paintTrigger = () => {
      const row = rootEl.querySelector('.md-search-row');
      if (!row) return;
      let btn = row.querySelector('.md-viewbtn');
      if (!views.length) {
        if (btn) btn.remove();
        return;
      }
      if (!btn) {
        btn = el('md-viewbtn', 'button');
        btn.type = 'button';
        btn.innerHTML = PANEL_ICON;
        btn.addEventListener('click', () => {
          listOpen = !listOpen;
          if (!listOpen) focus = null;
          rail.render();
          paintChrome();
        });
        row.appendChild(btn);
      }
      btn.classList.toggle('on', listOpen);
      btn.title = listOpen ? 'Hide views' : 'Views';
    };

    const paintChrome = () => {
      paintTrigger();
      paintList();
      // NOT `md-focused` -- the renderer already owns that class for search
      // dimming, and reusing it here would dim the deck on every view focus
      rootEl.classList.toggle('md-listopen', listOpen && views.length > 0);
      rootEl.classList.toggle('md-viewfocus', !!focus);
    };

    const asArr = (x) => x == null ? [] : (Array.isArray(x) ? x : [x]);

    const setData = (msg) => {
      blocks = asArr(msg.blocks).map((b) => ({
        id: b.id, name: b.name, category: b.category || '', color: b.color,
        icon: b.icon || null, inputs: asArr(b.inputs), variadic: !!b.variadic
      }));
      links = asArr(msg.links).map((l) => ({
        id: l.id, from: l.from, to: l.to,
        input: l.input == null ? '' : l.input
      }));
      stacks = asArr(msg.stacks);
      views = asArr(msg.views).map((v) => ({
        id: v.id, name: v.name, active: !!v.active, blocks: asArr(v.blocks)
      }));
      // A focused view that was just removed leaves focus dangling; fall
      // back to the reporting mode rather than to a view nobody picked.
      if (focus && !viewOf(focus)) focus = null;
      rail.setData({ blocks, links, stacks });
      paintChrome();
    };

    return {
      ns,
      announced: false,
      setData,
      setBadge: rail.setBadge,
      inspect: rail.inspect
    };
  }
})();
