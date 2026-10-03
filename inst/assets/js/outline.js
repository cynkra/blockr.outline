/* Outline: the BOARD adapter for the list+rail editor (blockr.outline).
 *
 * The drawing and the gestures live in `outline-rail.js`, which knows nothing
 * about blockr. This file is everything that does: the Shiny channels, the
 * one-instance-per-container registry, block arity, and how a block paints
 * itself in a row.
 *
 * One instance per `.outline[data-ns]` container. The R side pushes the full
 * board model ('outline-data') and per-block status badges ('outline-badge');
 * user gestures come back out of the renderer as `emit(name, payload)` and go
 * on as event-priority Shiny inputs (link_add, link_rm, link_mod,
 * link_insert, block_rm, block_rename, block_select, block_append, block_add,
 * stack_add, stack_rename, stack_rm, stack_join, stack_leave). The client
 * never mutates the model itself: every edit round-trips through the board
 * and comes back as a data push.
 *
 * Model: blocks [{id, name, category, color, icon, inputs[], variadic, drops}],
 * links [{id, from, to, input}], stacks [{id, name, color, blocks[]}].
 * A named input slot is FULL when a link with that (to, input) exists;
 * variadic blocks accept unlimited links on the '' slot (blockr.core
 * semantics — mirrored here for instant feedback, enforced again in R).
 */
/* Board arity, as pure functions of the model so `node --test` can hold
 * them to blockr.core's rules (tests/js/outline-board.test.js). A block's
 * named inputs fill one link each; a variadic block takes any number, on
 * inputs it names itself, and never fills up. */
(function (root, factory) {
  'use strict';
  const api = factory();
  if (typeof module === 'object' && module.exports) module.exports = api;
  if (root) root.outlineBoard = api;
})(typeof globalThis !== 'undefined' ? globalThis : this, function () {
  'use strict';

  const linksInto = (links, id) => links.filter((l) => l.to === id);

  // The inputs a new link into `b` could take: its free named inputs, and
  // '' (a new one) when it takes any number.
  const freeSlots = (b, links) => {
    const occ = new Set(linksInto(links, b.id).map((l) => l.input));
    const free = (b.inputs || []).filter((s) => !occ.has(s));
    if (b.variadic) free.push('');
    return free;
  };

  // What the row's input markers show: the named inputs nothing is linked
  // into, and whether the block takes any number. A linked input draws
  // nothing; the rail shows it.
  const inputMarkers = (b, links) => {
    const occ = new Set(linksInto(links, b.id).map((l) => l.input));
    const inputs = (b.inputs || []).filter((s) => !occ.has(s));
    return inputs.length || b.variadic
      ? { inputs: inputs, variadic: !!b.variadic } : null;
  };

  // What a link's menu can do to its input. Into a block that takes any
  // number of inputs the input is a name, so it can be renamed (to one no
  // other link into the block uses); into a block with fixed inputs the
  // link can move to another free one. Neither when there is nothing to do.
  const linkEdit = (l, blocks, links) => {
    const to = blocks.find((b) => b.id === l.to);
    if (!to) return null;
    const others = linksInto(links, to.id).filter((x) => x.id !== l.id);
    if (to.variadic) {
      return { rename: true, taken: others.map((x) => x.input) };
    }
    const used = new Set(others.map((x) => x.input));
    const move = (to.inputs || []).filter((s) => s !== l.input && !used.has(s));
    return move.length ? { move: move } : null;
  };

  // The links into `id`, for "Insert a block before".
  const inputLinks = (id, links) => linksInto(links, id);

  return { freeSlots, inputMarkers, linkEdit, inputLinks };
});

(() => {
  'use strict';

  // In node (the tests require this file for `outlineBoard`) there is no
  // page to adapt to.
  if (typeof document === 'undefined') return;

  const registry = new Map();

  // The design system's pieces, through the rail (see outline-rail.js):
  // `ui.icon('plus')`, `ui.tip(el, text)`, `toolBtn(cls, svg, label)`.
  const ui = {
    icon: (name) => outlineRail.icon(name),
    tip: (el, text, opts) => outlineRail.tip(el, text, opts)
  };
  const toolBtn = (cls, svg, label) => outlineRail.toolBtn(cls, svg, label);

  const getInst = (elId) => {
    let inst = registry.get(elId);
    // A dock panel that was closed and reopened puts a NEW element under the
    // same id, and the cached instance still points at the detached one.
    // Rebuild against what is on screen.
    if (inst && inst.el && !document.body.contains(inst.el)) {
      registry.delete(elId);
      inst = null;
    }
    if (!inst) {
      const el = document.getElementById(elId);
      if (!el) return null;
      inst = createInstance(el);
      registry.set(elId, inst);
      if (currentBlock) inst.setCurrent(currentBlock);
      // Announce a mount that happens after the socket is up. The server
      // skips a push whose model equals the last one it sent, so a panel
      // remounted mid-session has to ask, or it stays empty until the next
      // real board edit. At boot `announceAll` is the one that fires and
      // this is a no-op, since the socket is not connected yet.
      const app = window.Shiny && Shiny.shinyapp;
      if (app && typeof app.isConnected === 'function' && app.isConnected()) {
        inst.announced = true;
        Shiny.setInputValue(inst.ns + 'ready', true, { priority: 'event' });
      }
    }
    return inst;
  };

  const announceAll = () => {
    document.querySelectorAll('.outline[data-ns]').forEach((el) => {
      const inst = getInst(el.id);
      if (inst && !inst.announced) {
        inst.announced = true;
        Shiny.setInputValue(inst.ns + 'ready', true);
      }
    });
  };

  if (window.Shiny) {
    $(document).on('shiny:connected', announceAll);
    Shiny.addCustomMessageHandler('outline-data', (msg) => {
      const inst = getInst(msg.el);
      if (inst) inst.setData(msg);
    });
    Shiny.addCustomMessageHandler('outline-badge', (msg) => {
      const inst = getInst(msg.el);
      if (inst) inst.setBadge(msg);
    });
    // The catalogue is a pure function of the registry, so it arrives once
    // when the client announces itself, not on every board change.
    Shiny.addCustomMessageHandler('outline-registry', (msg) => {
      const inst = getInst(msg.el);
      if (inst) inst.setRegistry(msg);
    });
    Shiny.addCustomMessageHandler('outline-clipboard', (msg) => {
      const inst = getInst(msg.el);
      if (inst) inst.setClipboard(msg.json);
    });
  }

  /* The current row is the block open in the dock (design system, "Block
   * lists"). dockViewR fires a bubbling `dockview:active-panel` event on its
   * dock with the panel id, and a block's panel id is `block_panel-<id>`.
   * Other panels (the outline itself, the report) leave the current block as
   * it was, so a click in them does not unmark the block the dock shows. The
   * event reports changes only, so this is bound when the file loads, before
   * the dock's first activation, and an outline mounted later is handed the
   * block from here. */
  const PANEL_PREFIX = 'block_panel-';
  let currentBlock = null;
  document.addEventListener('dockview:active-panel', (e) => {
    const id = e.detail && e.detail.id;
    if (typeof id !== 'string' || id.indexOf(PANEL_PREFIX) !== 0) return;
    currentBlock = id.slice(PANEL_PREFIX.length);
    registry.forEach((inst) => inst.setCurrent(currentBlock));
  });

  // test/inspection hook
  window._outline = (elId) => {
    const inst = elId ? registry.get(elId) : registry.values().next().value;
    return inst ? inst.inspect() : null;
  };

  function createInstance(rootEl) {
    const ns = rootEl.dataset.ns;
    const pushRaw = (name, payload) =>
      Shiny.setInputValue(ns + name, payload, { priority: 'event' });

    /* Join-at-birth: while the rail has a stack focused, a block created
     * from that view joins the focused stack -- otherwise it would land
     * outside the frame and vanish from the view the instant it landed
     * (the board's own rule is that append never touches stacks; membership
     * moves by dragging a row into the frame).
     *
     * All three creation messages pass through here, so this is the one
     * choke point: arm on the way out, and when the next model arrives with
     * blocks that were not there before, send the join. Armed state expires
     * with the focus (see `stack_focus` in `emit`), on the first model that
     * brings new blocks, or after 90s -- a block browser left open and
     * abandoned should not tag along on tomorrow's add.
     */
    let pendingJoin = null;      // { stack, at } while an add is in flight
    let lastBlockIds = null;     // ids of the previous model, for the diff

    const push = (name, payload) => {
      if (name === 'block_insert' || name === 'block_add' ||
          name === 'block_append' || name === 'link_insert') {
        const f = rail.stackFocus();
        pendingJoin = f ? { stack: f, at: Date.now() } : null;
      }
      pushRaw(name, payload);
    };

    // the renderer hands the model back on every query, so arity is always
    // read off the board as it stands, never off a stale copy
    let blocks = [], links = [], views = [], stacks = [], extensions = [];

    // The one held gesture in the menu: clearing the outline's OWN tick on the
    // view you are looking at removes the panel the click happened in, and the
    // way back is blockr.dock's per-view "+ Add panel" picker rather than
    // anything in here. So that box arms on the first click and commits on the
    // second. Everything else is one click.
    let armedTick = null;             // '<extension id>@<view id>' while armed

    const blockOf = (id) => blocks.find((b) => b.id === id);
    const linksInto = (id) => outlineBoard.inputLinks(id, links);
    const freeSlots = (b) => outlineBoard.freeSlots(b, links);

    // The glyph comes from the catalogue, keyed by the block's type, because
    // an icon belongs to a type and not to a board: it arrives once instead
    // of riding every board change as a base64 data URI. The SVG uses
    // `currentColor`, so it takes the category colour the mark sets -- the
    // same pairing the picker's rows use. A type the catalogue does not
    // carry falls back to the letter.
    const iconFor = (type) => {
      const m = registry.byType && registry.byType.get(type);
      return m && m.icon ? m.icon : null;
    };
    const typeName = (b) => {
      const m = registry.byType && registry.byType.get(b.type);
      return m && m.name ? m.name : b.type;
    };

    // The block's mark, as the design system draws it in a list: the glyph
    // in the category colour on an 18% tint of it (outline.css mixes the
    // tint, so it follows the dark scheme). The category colour is
    // `blk_color()`'s, carried on the model.
    const kindIcon = (b) => {
      const k = document.createElement('span');
      k.className = 'md-kind';
      if (b.color) k.style.setProperty('--blockr-outline-mark', b.color);
      const svg = iconFor(b.type);
      if (svg) {
        k.innerHTML = svg;
      } else {
        k.textContent = (b.name || '?').slice(0, 1).toUpperCase();
      }
      return k;
    };

    const rail = outlineRail.create(rootEl, {
      // Renderer options go under `opts`; the rest of this object is the adapter
      // contract (emit, nodeLead, ...).
      //
      // The parentless add lives at the END of the flow, not in the search row:
      // every other add gesture is on the left and about a parent (drag a dot to
      // a row, drag it to the gutter, the row's own `+`), and this is the one
      // that is about no parent -- so it belongs where the block will appear,
      // which is after the last row.
      //
      // `rowMenu`: the rows' "…" (see `nodeTools`) carries every row tool,
      // so a narrow panel shows only it.
      opts: { addButton: false, addRow: true, rowMenu: true },
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
          // the renderer names the button that was pressed, so the picker
          // opens on it wherever it is -- mid-panel on an empty board, at
          // the foot of the list on a full one
          requestAdd(payload && payload.el);
          return;
        }
        // Leaving or moving the focus disarms join-at-birth: the add was
        // meant for the view it was started in. Forwarded anyway -- R does
        // not observe it today, but a deep link would enter here.
        if (name === 'stack_focus') {
          pendingJoin = null;
        }
        push(name, payload);
      },

      // The row is the design system's block list row: the mark, the name,
      // the row's end. Linked inputs are on the rail; the free ones are
      // markers after the name (`inputMarkers`).
      nodeLead: (b) => kindIcon(b),

      // a board constrains the CONSUMER: the edge occupies one of `to`'s
      // free input slots, and an empty answer is what refuses the drop
      slotsFor: (from, to) => freeSlots(to),

      // an auto-named variadic link is not a slot anyone chose, so the
      // edge labels keep quiet about it
      showSlot: (l) => l.input !== '' &&
        ((blockOf(l.to) || {}).inputs || []).includes(l.input),

      // Links (design system, "Links in the outline"). Every input has a
      // name on a board, a variadic one too, and the link's tooltip and
      // menu say which.
      linkInput: (l) => l.input,
      // blockr.dock's insert action: its "+" menu, captioned for the link
      linkInsert: (l) => push('link_insert', { id: l.id }),
      linkEdit: (l) => outlineBoard.linkEdit(l, blocks, links),
      inputMarkers: (b) => outlineBoard.inputMarkers(b, links),
      // a block in a menu that lists the board's blocks: its mark, and its
      // type as meta text (the catalogue's name for it)
      nodeItem: (b) => ({
        mark: { icon: iconFor(b.type) || '', color: b.color || '' },
        meta: typeName(b)
      }),

      // The row's one tool, shown on hover or keyboard focus: the "…" that
      // opens the row menu, which carries every action of the row (Append
      // a block, Rename, ..., Remove last).
      nodeTools: (b) => moreBtn(() => ({ ids: [b.id], kind: 'blocks' })),
      stackTools: (stack) => moreBtn(() => ({ stack: stack.id })),

      // Past the spring: the views the row is on.
      nodeAside: (b) => membershipEl([b.id]),
      // A stack has no view membership of its own: `dock_view` members are block
      // panels, so anything shown here could only be a UNION over the members --
      // "Overview +1" on a two-block stack meaning one member is on each. That
      // reads as a fact about the stack and is not one. Expanded, the member rows
      // already say it exactly; collapsed, the header's right-click menu shows
      // the per-view checklist with a tri-state box, which is the honest form of
      // the same summary.
      stackAside: () => null
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

    const membershipEl = (ids, kind) => {
      // With one view "on every view" is true of every row on the board, so
      // the column would say the same word on each row and take the width
      // the names need. It is only a fact worth a column once there is a
      // second view to be absent from.
      if (views.length < 2) return null;

      kind = kind || 'blocks';
      const wrap = el('md-views');
      const cur = activeView();

      const mine = views.filter((v) =>
        ids.some((id) => memberIds(v, kind).includes(id)));

      // On every view, and on none, are the two answers a list of names reads
      // worst: six names is not a fact anybody reads, and an empty slot is
      // indistinguishable from a slot that failed to render.
      const whole = ids.length && views.every((v) =>
        ids.every((id) => memberIds(v, kind).includes(id)));

      // Meta text (design system, "Block lists"): 13px text-muted, state the
      // reader needs without opening anything. It is not a control; the row
      // menu is where membership is written.
      const textEl = (text) => {
        const t = el('md-vname');
        t.textContent = text;
        ui.tip(t, text, { overflow: true });
        return t;
      };

      if (whole) {
        wrap.appendChild(textEl('all views'));
        return wrap;
      }

      // Shown nowhere is the ORDINARY case, not a state calling for an action:
      // on a real board half the rows are mutates, joins and reads that nobody
      // ever puts on a page. So the slot stays empty and says nothing. Clicking
      // the row is what puts it on the view you are in.
      if (!mine.length) {
        return wrap;
      }

      // The current view first when the row is on it: while you are looking at
      // Detail, "Detail" is the fact you need, and "Overview +1" makes you count.
      const onCur = cur && mine.some((v) => v.id === cur.id);
      const rest = mine.filter((v) => !onCur || v.id !== cur.id);
      const first = onCur ? cur : mine[0];
      const others = onCur ? rest : mine.slice(1);

      wrap.appendChild(textEl(first.name));

      // "+N" in text-accent stands in for the views the first one leaves out:
      // the hidden views in its tooltip, one per line, and a click opens the
      // row menu, whose checklist is the full surface for them.
      if (others.length) {
        const more = el('md-vmore', 'button');
        more.type = 'button';
        more.textContent = '+' + others.length;
        more.setAttribute('aria-label', 'Also on ' +
          others.map((v) => v.name).join(', '));
        ui.tip(more, others.map((v) => v.name));
        more.addEventListener('click', (ev) => {
          ev.stopPropagation();
          openRowMenuFor(ev.currentTarget, ids, kind);
        });
        wrap.appendChild(more);
      }

      return wrap;
    };

    /* ---- the extensions group, at the foot of the flow ------------------------
     *
     * Extensions as rows. An extension is not in the DAG, so it has no place in
     * an order derived from it and no lane on the rail; it gets a group after
     * the last block instead, carrying the same membership column, which is the
     * whole point -- "which view is this on?" gets one answer shape for every
     * panel the board has.
     *
     * The catalogue comes from the board (`outline_extensions()`), not from view
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
      ui.tip(nm, t.name, { overflow: true });
      row.appendChild(nm);

      row.appendChild(el('md-spring'));

      // The row's end, as on a block row: the views as meta text at rest,
      // the "…" on hover or keyboard focus. The menu is about views only, so
      // it is there only where there is a second view.
      const end = el('md-end');
      const tools = el('md-rowtools');
      if (views.length >= 2) {
        const more = toolBtn('md-more', MORE_ICON, 'Actions');
        more.addEventListener('click', (ev) => {
          ev.stopPropagation();
          openRowMenuFor(more, [t.id], 'extensions');
        });
        tools.appendChild(more);
      }
      end.appendChild(tools);
      const mem = membershipEl([t.id], 'extensions');
      if (mem) end.appendChild(mem);
      row.appendChild(end);

      row.tabIndex = 0;
      row.addEventListener('keydown', (ev) => {
        if (ev.key === 'Enter' && ev.target === row) {
          ev.preventDefault();
          row.click();
        }
      });

      // Same gesture as a block row, and the same answer: show it HERE. An
      // extension on no view has no other way back onto a page, and one on
      // another view should not take you off the one you are working on -- so
      // the click mounts it on the current view when the view does not hold
      // it, and focuses it when it does. Its own row is the exception: the
      // panel you are clicking in is already in front of you.
      if (!t.self) {
        row.addEventListener('click', () => push('ext_select', { id: t.id }));
      } else {
        row.classList.add('md-self');
      }

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

      // a section title, with the count as muted text after it
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
     * presets, then a tick per view, then the row operations: rename, the
     * link gestures (insert before, append, connect to), so every one of
     * them has a keyboard path, then copy, cut and remove.
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

    // Whether every id is on every view / on none of them. What the two caption
    // links act on, and what greys them out when they would be a no-op.
    const onEvery = (ids, kind) =>
      !!ids.length && !!views.length &&
      views.every((v) => ids.every((id) => memberIds(v, kind).includes(id)));
    const onNone = (ids, kind) =>
      !!views.length &&
      views.every((v) => !ids.some((id) => memberIds(v, kind).includes(id)));

    let menuEl = null, menuSpec = null, menuOpenedAt = 0;
    let menuPlaced = null, menuAnchor = null, menuRow = null;

    const closeMenu = () => {
      if (menuRow) { menuRow.classList.remove('md-menu-open'); menuRow = null; }
      if (menuPlaced) { menuPlaced.stop(); menuPlaced = null; }
      if (menuAnchor) { menuAnchor.remove(); menuAnchor = null; }
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
      renderMenu(menuSpec);
    };

    const write = (spec, mode, view) => {
      push('membership', {
        blocks: spec.blocks, extensions: spec.extensions, mode: mode, view: view || null
      });
    };

    // A row of the design system's menu (blockr.ui's `.blockr-menu__*`
    // classes, which style a menu built elsewhere the same way): the label,
    // then the keystroke that does the same as meta text. A destructive row
    // carries the bin and turns red only under the pointer.
    const menuBtn = (label, key, opts, fn) => {
      opts = opts || {};
      const b = el('blockr-menu__item' +
        (opts.danger ? ' blockr-menu__item--danger' : ''), 'button');
      b.type = 'button';
      b.setAttribute('role', 'menuitem');
      if (opts.icon) {
        const ic = el('blockr-menu__icon');
        ic.innerHTML = ui.icon(opts.icon);
        b.appendChild(ic);
      }
      const l = el('blockr-menu__label');
      l.textContent = label;
      b.appendChild(l);
      if (key) {
        const k = el('blockr-menu__meta');
        k.textContent = key;
        b.appendChild(k);
      }
      b.addEventListener('click', (ev) => {
        ev.stopPropagation();
        fn();
      });
      return b;
    };

    // The two bulk edits, as words at the right of the group title that
    // already labels what they act on: quiet text buttons, a shortcut for
    // ticking rather than commands of the rank of "Remove block".
    const capActions = (spec, ids, kind) => {
      const wrap = el('md-ctxacts');

      const one = (label, mode, off) => {
        const b = el('md-ctxact', 'button');
        b.type = 'button';
        b.textContent = label;
        b.disabled = off;
        if (!off) {
          b.addEventListener('click', (ev) => {
            ev.stopPropagation();
            closeMenu();
            write(spec, mode, null);
          });
        }
        return b;
      };

      wrap.appendChild(one('None', 'none', onNone(ids, kind)));
      wrap.appendChild(one('All', 'all', onEvery(ids, kind)));
      return wrap;
    };

    // The half-way mark for a selection or a stack that is only partly on a
    // view: a short stroke in the tick's slot, drawn like the tick.
    const PARTIAL = '<svg width="14" height="14" viewBox="0 0 16 16" fill="none" ' +
      'stroke="currentColor" stroke-width="1.5" stroke-linecap="round">' +
      '<path d="M4 8h8"></path></svg>';

    // A view as a multi pick: the tick in a slot every row keeps, the name
    // (weight 600 on the view you are on, the menu's "current" mark), and the
    // number of panels on it as meta text.
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

      const row = el('blockr-menu__item md-ctxtick' +
        (v.active ? ' md-ctxcur' : '') +
        (armed ? ' md-armed' : ''), 'button');
      row.type = 'button';
      row.setAttribute('role', 'menuitemcheckbox');
      row.setAttribute('aria-checked',
        state === 'all' ? 'true' : state === 'some' ? 'mixed' : 'false');

      const box = el('md-ctxbox');
      box.innerHTML = state === 'all' ? ui.icon('check')
        : state === 'some' ? PARTIAL : '';
      row.appendChild(box);

      const nm = el('blockr-menu__label');
      nm.textContent = armed ? 'Remove from ' + v.name + '?' : v.name;
      row.appendChild(nm);

      const n = el('blockr-menu__meta');
      n.textContent = v.n;
      row.appendChild(n);

      row.addEventListener('click', (ev) => {
        ev.stopPropagation();
        const clearing = state !== 'none';

        if (mine && clearing && v.active && !armed) {
          armedTick = armKey;
          renderMenu(spec);      // repaint in place: the menu stays open
          return;
        }

        armedTick = null;
        write(spec, clearing ? 'rm' : 'add', v.id);
        // ticking is usually done more than once, so the menu stays open and the
        // board push repaints it; the presets and counts follow.
      });

      return row;
    };

    const renderMenu = (spec) => {
      const ids = spec.blocks.concat(spec.extensions);
      const kind = spec.extensions.length ? 'extensions' : 'blocks';
      const box = menuEl;

      box.innerHTML = '';

      // The head: what the menu is about, and how many views hold it.
      const head = el('blockr-menu__head', 'div');
      const title = el('blockr-menu__head-title', 'div');
      title.textContent = spec.label;
      head.appendChild(title);
      const on = views.filter((v) =>
        ids.some((id) => memberIds(v, kind).includes(id))).length;
      const text = el('blockr-menu__head-text', 'div');
      text.textContent = 'On ' + on + ' of ' + views.length +
        (views.length === 1 ? ' view' : ' views');
      head.appendChild(text);
      box.appendChild(head);

      const cap = el('blockr-menu__title md-ctxcap', 'div');
      const capText = el('md-ctxcaptext');
      capText.textContent = 'Views';
      cap.appendChild(capText);
      cap.appendChild(capActions(spec, ids, kind));
      box.appendChild(cap);

      const list = el('md-ctxticks', 'div');
      views.forEach((v) => list.appendChild(tickRow(spec, v)));
      box.appendChild(list);

      // The row operations, and only the ones with no gesture of their own.
      if (spec.kind !== 'ext') {
        box.appendChild(el('blockr-menu__divider', 'div'));

        if (spec.kind === 'block' && spec.blocks.length === 1) {
          const id = spec.blocks[0];
          box.appendChild(menuBtn('Rename', '', null, () => {
            closeMenu();
            rail.editName(id);
          }));
          // The link gestures, so each has a keyboard path (design system,
          // "Links in the outline"): into the block's input link, after it,
          // and from it into another block.
          if (linksInto(id).length) {
            box.appendChild(menuBtn('Insert a block before', '', null, () => {
              closeMenu();
              insertBefore(id);
            }));
          }
          // appending after this row: the rail's drag does the same
          if (registry.append.length) {
            box.appendChild(menuBtn('Append a block', '', null, () => {
              closeMenu();
              openPicker(id, rowOf(id), null);
            }));
          }
          box.appendChild(menuBtn('Connect to\u2026', '', null, () => {
            closeMenu();
            rail.connectFrom(id, rowOf(id));
          }));
        }
        if (spec.kind === 'stack') {
          const sid = spec.stack;
          box.appendChild(menuBtn('Rename group', '', null, () => {
            closeMenu();
            rail.editName('stack:' + sid);
          }));
          // the stack's focus and dissolve, which have no tool on the row
          if (rail.stackFocus() !== sid) {
            box.appendChild(menuBtn('Show only this group', '', null, () => {
              closeMenu();
              rail.focusStack(sid);
            }));
          } else {
            box.appendChild(menuBtn('Show the whole board', '', null, () => {
              closeMenu();
              rail.focusStack(null);
            }));
          }
          box.appendChild(menuBtn('Dissolve group', '', null, () => {
            closeMenu();
            push('stack_rm', { id: sid });
          }));
        }

        box.appendChild(menuBtn('Copy', Blockr.keys('Mod+C'), null, () => {
          closeMenu();
          document.execCommand('copy');
        }));
        box.appendChild(menuBtn('Cut', Blockr.keys('Mod+X'), null, () => {
          closeMenu();
          document.execCommand('cut');
        }));

        box.appendChild(el('blockr-menu__divider', 'div'));
        // A block with links loses them when it goes, and the row says so.
        const one = spec.blocks.length === 1 ? blockOf(spec.blocks[0]) : null;
        box.appendChild(menuBtn(
          spec.blocks.length > 1
            ? 'Remove ' + spec.blocks.length + ' blocks'
            : 'Remove block',
          one && one.drops ? 'its links are dropped' : '',
          { danger: true, icon: 'trash' }, () => {
            closeMenu();
            push('block_rm', { ids: spec.blocks });
          }
        ));
      }
    };

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

    // Empty space: the board's two operations, as a plain menu of actions
    // (`Blockr.menu`) at the pointer.
    const openBoardMenu = (ev) => {
      closeMenu();
      const B = window.Blockr;
      if (!B || !B.menu) return;
      const at = deckPoint(ev);
      const a = outlineRail.pointAnchor(ev.clientX, ev.clientY);
      B.menu(a, {
        caption: 'Board',
        items: [
          // where the menu was opened: the gesture had a position, and the
          // picker is about to insert something there
          { label: 'Add a block', onSelect: () => requestAdd(null, at) },
          { label: 'Paste', meta: Blockr.keys('Mod+V'), onSelect: pasteFromClipboard }
        ],
        onClose: () => a.remove()
      });
    };

    // Open the row menu hanging from `anchor` (a point for a right-click, the
    // "+N" tag for a click on it), placed as every menu is: `Blockr.place`,
    // under the anchor, flipped above when there is no room.
    const openMenu = (spec, anchor, temp) => {
      closeMenu();

      if (!spec.blocks.length && !spec.extensions.length) {
        if (temp) temp.remove();
        return;
      }
      // Nothing to say about placement on a board with one view, and for an extension
      // row placement is all the menu has -- so it does not open at all there.
      if (views.length < 2 && spec.kind === 'ext') {
        if (temp) temp.remove();
        return;
      }

      menuEl = el('blockr-menu md-ctxmenu', 'div');
      menuEl.setAttribute('role', 'menu');
      menuSpec = spec;
      menuAnchor = temp || null;
      // A tool that shows only while its row is hovered stays while the
      // menu it opened is open, so the menu keeps its anchor.
      menuRow = anchor.closest ? anchor.closest('.md-chip, .md-stackhead') : null;
      if (menuRow) menuRow.classList.add('md-menu-open');
      document.body.appendChild(menuEl);
      renderMenu(spec);

      menuPlaced = window.Blockr && Blockr.place
        ? Blockr.place(menuEl, anchor, { width: { min: 220, max: 320 } })
        : null;
      menuOpenedAt = performance.now();
    };

    const rowOf = (id) => rootEl.querySelector(
      '.md-chip[data-id="' + CSS.escape(id) + '"]');

    // "Insert a block before": into the block's input link, or, with
    // several, into the one picked from a list of them.
    const insertBefore = (id) => {
      const ins = linksInto(id);
      if (ins.length === 1) {
        push('link_insert', { id: ins[0].id });
        return;
      }
      const row = rowOf(id);
      if (!ins.length || !row || !window.Blockr || !Blockr.menu) return;
      Blockr.menu(row, {
        caption: 'Insert a block before ' + ((blockOf(id) || {}).name || id),
        items: ins.map((l) => {
          const from = blockOf(l.from);
          return {
            label: from ? from.name : l.from,
            meta: 'into ' + l.input,
            onSelect: () => push('link_insert', { id: l.id })
          };
        })
      });
    };

    const blockSpec = (ids) => ({
      kind: 'block', blocks: ids, extensions: [],
      label: ids.length > 1
        ? ids.length + ' rows selected'
        : (blockOf(ids[0]) || {}).name || ids[0]
    });

    const extSpec = (id) => ({
      kind: 'ext', blocks: [], extensions: [id],
      label: (extensions.find((x) => x.id === id) || {}).name || id
    });

    const stackSpec = (id) => {
      const stack = stacks.find((x) => x.id === id);
      const mem = stack ? asArr(stack.blocks) : [];
      return {
        kind: 'stack', blocks: mem, extensions: [], stack: id,
        label: ((stack || {}).name || 'Group') + ' · ' + mem.length + ' blocks'
      };
    };

    // The "+N" tag's click and the "…" tool: the same menu as a right-click
    // on the row.
    const openRowMenuFor = (anchor, ids, kind) => {
      if (kind === 'extensions') {
        openMenu(extSpec(ids[0]), anchor);
        return;
      }
      rail.selectOnly(ids);
      openMenu(blockSpec(ids), anchor);
    };

    // "…": the row's one tool, which opens its menu.
    const MORE_ICON = '<svg width="14" height="14" viewBox="0 0 16 16" ' +
      'fill="currentColor"><circle cx="3.5" cy="8" r="1.25"></circle>' +
      '<circle cx="8" cy="8" r="1.25"></circle>' +
      '<circle cx="12.5" cy="8" r="1.25"></circle></svg>';
    const moreBtn = (what) => {
      const b = toolBtn('md-more', MORE_ICON, 'Actions');
      b.addEventListener('click', (ev) => {
        ev.stopPropagation();
        const w = what();
        if (w.stack) {
          const spec = stackSpec(w.stack);
          if (!spec.blocks.length) return;
          rail.selectOnly(spec.blocks);
          openMenu(spec, b);
          return;
        }
        openRowMenuFor(b, w.ids, w.kind);
      });
      return b;
    };

    // One delegated listener for every row shape the outline draws. The rail owns
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

      const at = () => outlineRail.pointAnchor(ev.clientX, ev.clientY);

      if (ext) {
        const a = at();
        openMenu(extSpec(ext.dataset.ext), a, a);
        return;
      }

      if (head) {
        const spec = stackSpec(head.dataset.stack);
        if (!spec.blocks.length) return;
        rail.selectOnly(spec.blocks);
        const a = at();
        openMenu(spec, a, a);
        return;
      }

      const id = row.dataset.id;
      let sel = rail.selection();

      if (!sel.includes(id)) {
        rail.selectOnly([id]);
        sel = [id];
      }

      const a = at();
      openMenu(blockSpec(sel), a, a);
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
    // ...and NOT the menu's own view list, either. The listener is in the
    // CAPTURE phase (a scroll event does not bubble, so a bubbling listener on
    // window would never see a panel scroll at all) -- which means it also sees
    // the scroll of every element INSIDE the menu. On an 11-view board that made
    // the tick list unscrollable: the first notch dismissed the menu it was
    // scrolling.
    window.addEventListener('scroll', (ev) => {
      if (!menuEl) return;
      if (ev.target instanceof Node && menuEl.contains(ev.target)) return;
      if (performance.now() - menuOpenedAt > 260) closeMenu();
    }, true);

    // Chrome the adapter owns, as opposed to the rows the renderer draws. Only
    // the extensions group is left: the view list, its trigger and the focused-view
    // dimming all went when the menu took over editing, and with them the grid
    // layout mode the list needed.
    const paintChrome = () => {
      paintExts();
    };

    /* ---- the block picker: the "+" menu -----------------------------------
     *
     * One menu for adding and appending, because they are one operation:
     * blockr.dock's `target_mode()` returns "add" when there is no target,
     * and the only downstream difference is that an append also makes a
     * link. So the origin decides two things and nothing else -- whether a
     * link is created, and which pool is offered (appending needs a block
     * that can receive a link).
     *
     * It is the design system's "Picking a block": `Blockr.menu` with a
     * caption saying what the pick does, a filter box, category titles, one
     * row per block type with its mark, its name and the package as a
     * badge, and no description. The rows are built the way blockr.dock's
     * own "+" menu builds them (`add_block_menu_items()`, not exported), so
     * the two read the same. It opens at the control that was pressed, or
     * at the point a drag was released, so the rail and the origin row stay
     * on screen while you choose.
     */

    let registry = { add: [], append: [] };

    const upperFirst = (x) => x ? x.charAt(0).toUpperCase() + x.slice(1) : x;

    const commitPick = (meta, originId) => {
      // An insert from inside a focused stack view names the stack, so R
      // can add the block AND its membership in one board update -- landing
      // loose and joining a roundtrip later left the block outside the view
      // for a beat and cost a second full model push on a large board. The
      // adapter's pendingJoin (see `push`) stays as the fallback for add
      // paths that cannot carry the hint.
      push('block_insert', {
        type: meta.type,
        from: originId || null,
        stack: rail.stackFocus() || null
      });
    };

    // What the renderer's "Add a block" row and the board menu's "Add a block"
    // both call: a block with no origin, and so no link. Until the catalogue
    // lands the picker cannot open, and swallowing the gesture would be worse
    // than the old behaviour -- so fall through to the board's own browser.
    // A pointer event in the coordinates `at` is stated in: the renderer
    // measures its drags against the deck, so anything the adapter hands to
    // `openPicker()` has to arrive in the same frame.
    const deckPoint = (ev) => {
      const deck = rootEl.querySelector('.md-deck');
      if (!deck) return null;
      const box = deck.getBoundingClientRect();
      return { x: ev.clientX - box.left, y: ev.clientY - box.top };
    };

    // Whichever control asked for a block: the "Add a block" row on a board
    // that has rows, the empty state's slot on one that has none.
    const addTrigger = () =>
      rootEl.querySelector('.md-addrow') ||
      rootEl.querySelector('.md-empty-add') ||
      rootEl.querySelector('.md-add');

    const requestAdd = (anchor, at) => {
      if (registry.add.length) {
        openPicker(null, at ? null : anchor || addTrigger(), at || null);
      } else {
        push('block_add', true);
      }
    };

    // The rows, grouped under category titles in the order the catalogue
    // lists them.
    const pickerItems = (pool, originId) => {
      const groups = new Map();
      pool.forEach((m) => {
        const k = m.category || 'other';
        if (!groups.has(k)) groups.set(k, []);
        groups.get(k).push(m);
      });
      const items = [];
      groups.forEach((metas, cat) => {
        items.push({ title: upperFirst(cat) });
        metas.forEach((m) => items.push({
          label: m.name,
          badge: m.package,
          // the type id, not the description: a description mentions other
          // blocks' words and would match half the list
          keywords: m.type + ' ' + cat,
          mark: { icon: m.icon || '', color: m.color || '' },
          onSelect: () => commitPick(m, originId)
        }));
      });
      return items;
    };

    const openPicker = (originId, anchor, at) => {

      const pool = originId ? registry.append : registry.add;
      if (!pool.length || !window.Blockr || !Blockr.menu) return;

      // `at` is the drag's release position in deck coordinates (the
      // renderer sends x/y on `block_append` for exactly this); a trigger
      // hangs the menu under itself.
      let temp = null;
      if (!anchor || !anchor.isConnected) {
        const deck = rootEl.querySelector('.md-deck') || rootEl;
        const box = deck.getBoundingClientRect();
        temp = outlineRail.pointAnchor(
          box.left + (at ? at.x : 8), box.top + (at ? at.y : 8));
        anchor = temp;
      }

      const origin = originId ? blockOf(originId) : null;
      const fs = stacks.find((s) => s.id === rail.stackFocus());

      Blockr.menu(anchor, {
        caption: origin ? 'Append to ' + origin.name
          : fs ? 'Add a block to ' + fs.name : 'Add a block',
        filter: 'Search blocks',
        minWidth: 300,
        items: pickerItems(pool, originId),
        onClose: () => { if (temp) temp.remove(); }
      });
    };

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
     * Scoped to the outline last clicked in, so a board mounting both this and
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
        inputs: asArr(b.inputs), variadic: !!b.variadic,
        // removing it drops its links
        drops: !!b.drops
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

      // Join-at-birth lands here: the model that brings the created block is
      // the first place its id exists. Joined only if it arrived loose --
      // a paste of a whole stack brings its own membership and keeps it.
      const ids = new Set(blocks.map((b) => b.id));
      if (pendingJoin && lastBlockIds) {
        if (Date.now() - pendingJoin.at > 90000) {
          pendingJoin = null;
        } else {
          const fresh = blocks.map((b) => b.id)
            .filter((id) => !lastBlockIds.has(id));
          if (fresh.length) {
            const stacked = new Set(stacks.flatMap((s) => asArr(s.blocks)));
            const join = fresh.filter((id) => !stacked.has(id));
            if (join.length && stacks.some((s) => s.id === pendingJoin.stack)) {
              pushRaw('stack_join', {
                blocks: join, stack: pendingJoin.stack
              });
            }
            pendingJoin = null;
          }
        }
      }
      lastBlockIds = ids;
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
      el: rootEl,
      announced: false,
      setData,
      setRegistry,
      setClipboard,
      setBadge: rail.setBadge,
      setCurrent: rail.setCurrent,
      inspect: rail.inspect
    };
  }
})();
