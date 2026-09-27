/* Outline rail: the list+rail editor, with no idea what a board is.
 *
 * The drawing, the gestures and the geometry that `outline-layout.js` does
 * not own. Everything that knows about blockr boards -- Shiny, the dock
 * extension protocol, block arity, block icons -- lives in an ADAPTER that
 * the host passes in. `outline.js` is the board adapter; blockr.process has
 * a second one whose nodes are process steps.
 *
 * Model (the adapter pushes it in via `setData`, same field names as
 * outline-layout.js): nodes are `blocks` [{id, name, ...}], edges are `links`
 * [{id, from, to, input}], `stacks` [{id, name, color, blocks[]}]. The
 * renderer never mutates it: a gesture calls `adapter.emit(name, payload)`
 * and the host is expected to push a new model back.
 *
 * adapter = {
 *   emit(name, payload)          gestures out: link_add, link_rm, link_mod,
 *                                block_rm,
 *                                block_rename, block_select, block_append,
 *                                block_add, stack_add, stack_rename, stack_rm,
 *                                stack_join, stack_leave, stack_focus
 *                                ({id} on entering, {id: null} on leaving --
 *                                informational, the view change is internal)
 *   nodeLead(node) -> Element    row content before the name (icon, ports)
 *   nodeTrail(node) -> Element   row content after the name (chips, fields)
 *   nodeAside(node) -> Element   row content PAST the spring, so it right-
 *                                aligns into a column down the outline (the
 *                                board paints view membership here)
 *   stackAside(stack) -> Element the same, for a stack header and for the
 *                                collapsed stack row. Stack rows are built
 *                                entirely by the renderer, so this is their
 *                                only adapter hook.
 *   nodeTools(node) -> Element   optional: tools of the row, shown with its
 *                                remove button on hover; give them
 *                                `md-reveal`. With `opts.rowMenu` the host
 *                                gives ONE, the "…" that opens the row's
 *                                action menu (Remove included), and the
 *                                renderer draws no remove or focus tools of
 *                                its own (design system, "Block lists")
 *   stackTools(stack, collapsed) the same, for a stack header and the
 *                                collapsed stack row
 *   slotsFor(from, to) -> []     which slots this edge could occupy; EMPTY
 *                                means "refuse the connection". Boards ask
 *                                the consumer (free named inputs, '' when
 *                                variadic); a process asks the producer
 *                                (which outcome does this branch leave on).
 *   slotPrompt(from, to)         caption of the slot picker
 *   showSlot(link) -> bool       whether that slot is worth naming
 *
 *   Links (design system, "Links in the outline"). A link lights up under
 *   the pointer and a click opens its menu, which always has Remove link.
 *   Each hook below is optional; a host without it gets no such row.
 *   linkInput(link) -> string    the input a link goes into, as the tooltip
 *                                ("into data") and the menu head name it;
 *                                default: the slot where `showSlot` says so
 *   linkInsert(link)             "Insert a block here" in the link menu
 *   linkEdit(link) -> {rename: true, taken: []} | {move: []} | null
 *                                "Rename input" (the name becomes a field in
 *                                the menu; `taken` are refused in place) or
 *                                "Move to input" (a Select.menu of `move`);
 *                                the rail emits `link_mod` {id, input}
 *   inputMarkers(node) -> {inputs: [], variadic: bool} | null
 *                                markers after the name: an open circle per
 *                                free named input, "∞" when it takes any
 *                                number; a click lists the blocks that can
 *                                feed it and emits `link_add`
 *   nodeItem(node) -> {mark, meta}
 *                                how a node is drawn as a row of a menu that
 *                                lists blocks (`Blockr.menu` item fields)
 *   opts { search, searchMin, stacks, remove, status, allowCycles, nameEdit,
 *          edgeLabels, labelPad, searchPlaceholder, searchEmptyText, emptyText,
 *          emptyAddText, metrics, stackNoun, stackUnit, stackIcon, stackAddText,
 *          stackRmTitle, focusView, crumbRootText, rowMenu }
 *
 * The page needs blockr.ui's controls (`blockr.ui::controls_dep()`, which
 * `outline_rail_dep()` brings along): the rows draw its chevron and icons,
 * name their tools with `Blockr.tooltip`, and open their menus with
 * `Blockr.menu`.
 *
 * The stack wording is an option because a stack is only a stack on a board.
 * The process editor pushes the same object through as a multi-instance
 * sub-process, where the frame around a run of rows means "these repeat".
 *
 * Loop-backs (`opts.allowCycles`) and edge labels (`opts.edgeLabels`) are off
 * for a board, which has neither, and on for a process, which is defined by
 * both: an arrow climbing back to the QS check, labelled with the outcome it
 * left on.
 * }
 */
(function (root, factory) {
  'use strict';
  var api = factory();
  if (typeof module === 'object' && module.exports) {
    module.exports = api;
  }
  if (root) {
    root.outlineRail = api;
  }
})(typeof globalThis !== 'undefined' ? globalThis : this, function () {
  'use strict';

  const DEFAULT_METRICS = {
    // The design system's block list row (design system, "Block lists"):
    // 34px, no space between rows.
    LANE_W: 16, ROW_H: 34, GAP: 0, RAIL_L: 10, RAIL_R: 8, DOT_R: 4,
    // A stack is a tinted band around its header and members: STACK_PAD
    // inside it on every side, STACK_GAP of air between it and whatever is
    // above or below it. Row positions are worked out from these numbers
    // (`rowTops`), and the list sets the same numbers as margins and padding,
    // so the rail's dots sit on the row centres without measuring the DOM
    // (which reads 0 in a dock tab that is not showing).
    STACK_PAD: 2, STACK_GAP: 4
  };
  const LANE_COLORS = ['#9ca3af', '#2563eb', '#0d9488', '#7c3aed', '#b45309', '#be185d'];
  const STATUS_RANK = { failed: 3, waiting: 2, unset: 1 };

  const SVG = 'http://www.w3.org/2000/svg';
  const svgEl = (tag) => document.createElementNS(SVG, tag);

  const STACK_ICON = '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linejoin="round"><path d="m12 3 9 5-9 5-9-5 9-5z"/><path d="m3 13 9 5 9-5"/></svg>';
  const SEARCH_ICON = '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round"><circle cx="11" cy="11" r="7"/><path d="m21 21-4.3-4.3"/></svg>';
  const FOCUS_ICON = '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><path d="M3 8V5a2 2 0 0 1 2-2h3"/><path d="M16 3h3a2 2 0 0 1 2 2v3"/><path d="M21 16v3a2 2 0 0 1-2 2h-3"/><path d="M8 21H5a2 2 0 0 1-2-2v-3"/><circle cx="12" cy="12" r="2.6" fill="currentColor" stroke="none"/></svg>';
  const WARN_ICON = '<svg viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.4" stroke-linecap="round" stroke-linejoin="round"><path d="M8 2.5 14.5 13.5h-13z"/><path d="M8 6.5v3"/><circle cx="8" cy="11.6" r="0.4" fill="currentColor"/></svg>';

  const escapeHtml = (s) => String(s).replace(/[&<>"']/g, (c) => ({
    '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;'
  }[c]));

  /* ---- the design system (blockr.ui), read at call time ----------------
   *
   * The chevron, the icons, the tooltip, the menu and the placement routine
   * are blockr.ui's (`Blockr.*`, loaded by `outline_rail_dep()` through
   * `blockr.ui::controls_dep()`). Read when used rather than when this file
   * loads, so the load order of two scripts on one page does not matter,
   * and so `node --test` can still require the module.
   */
  const ui = () => (typeof window !== 'undefined' && window.Blockr) || null;

  const icon = (name) => {
    const B = ui();
    return (B && B.icons && B.icons[name]) || '';
  };

  // The one tooltip (the light card). An icon-only control gets one; a label
  // gets one only while it is cut off (`overflow`). `aria-label` rides along
  // on controls so a screen reader hears the same words.
  const tip = (el, text, opts) => {
    const B = ui();
    if (B && B.tooltip) B.tooltip.set(el, text, opts);
  };

  // A 26px tool: bare, the icon in text-muted, a hover wash. Every one of
  // them is icon-only, so every one carries its name as a tooltip.
  const toolBtn = (cls, svg, label) => {
    const b = document.createElement('button');
    b.type = 'button';
    b.className = 'md-tool' + (cls ? ' ' + cls : '');
    b.innerHTML = svg;
    b.setAttribute('aria-label', label);
    tip(b, label);
    return b;
  };

  // A fold's chevron: down while open, right while closed (rotated by CSS).
  const chevBtn = (cls, open, label) => {
    const b = toolBtn('md-chev' + (open ? '' : ' md-closed') +
      (cls ? ' ' + cls : ''), icon('chevron'), label);
    b.setAttribute('aria-expanded', open ? 'true' : 'false');
    return b;
  };

  // A zero-size box to hang a menu from where there is no element to hang it
  // from: the point a drag was released, the point of a right-click.
  const pointAnchor = (x, y) => {
    const a = document.createElement('div');
    a.className = 'md-anchor';
    a.style.cssText = 'position:fixed;width:0;height:0;left:' + x +
      'px;top:' + y + 'px;';
    document.body.appendChild(a);
    return a;
  };

  // Kept for hosts that read it off the module; the renderer itself mixes
  // tints in the stylesheet (`color-mix()`), so they follow the dark scheme.
  const hexA = (hex, a) => {
    const h = hex.replace('#', '');
    const r = parseInt(h.slice(0, 2), 16), g = parseInt(h.slice(2, 4), 16),
      b = parseInt(h.slice(4, 6), 16);
    return 'rgba(' + r + ',' + g + ',' + b + ',' + a + ')';
  };

  /* Where each drawn row sits, top edge, in deck coordinates, for the rows
   * `outlineLayout.displayRows()` returns. A stack header and a folded stack
   * open a band: STACK_GAP of air (unless it is the first row), then
   * STACK_PAD. The band closes after its last member with STACK_PAD, and
   * whatever follows a band gets STACK_GAP of air. `gaps[i]` is the margin
   * row i (or the band it opens) gets in the list, so the DOM is laid out
   * from the same numbers the rail draws its dots at. Pure, so `node --test`
   * can hold it to the rows. */
  const opensBand = (r) => r.t === 'header' || r.t === 'stack';
  const bandOf = (r) => opensBand(r) ? r.stack.id
    : r.inStack ? r.inStack.id : null;
  const rowTopsFor = (rows, M) => {
    const { ROW_H, GAP, STACK_PAD, STACK_GAP } = Object.assign({}, DEFAULT_METRICS, M || {});
    const tops = [], gaps = [];
    let y = 0, afterBand = false;
    rows.forEach((r, i) => {
      const opens = opensBand(r);
      const g = !i ? 0 : opens || afterBand ? STACK_GAP : GAP;
      gaps.push(g);
      y += g + (opens ? STACK_PAD : 0);
      tops.push(y);
      y += ROW_H;
      const next = rows[i + 1];
      const band = bandOf(r);
      const closes = band !== null &&
        !(next && !opensBand(next) && bandOf(next) === band);
      if (closes) y += STACK_PAD;
      afterBand = closes;
    });
    return { tops: tops, gaps: gaps, height: y };
  };

  /* ---- links: who can connect to whom -----------------------------------
   *
   * Pure, so `node --test` can hold them to the rules. `slotsFor(from, to)`
   * is the adapter's: the inputs of `to` a link from `from` could take, ''
   * for a new one on a block that takes any number.
   */

  // Every block reachable from `id` along the links, downwards (its
  // descendants) or upwards (its ancestors). `id` itself is not included.
  const linkReach = (links, id, down) => {
    const seen = new Set();
    const q = [id];
    while (q.length) {
      const x = q.shift();
      links.forEach((l) => {
        const a = down ? l.from : l.to, b = down ? l.to : l.from;
        if (a === x && !seen.has(b)) { seen.add(b); q.push(b); }
      });
    }
    return seen;
  };

  // The blocks that can feed input `input` of `to` ('' for a new input on a
  // block that takes any number): not `to` itself, nothing downstream of it
  // (that would close a cycle), and on a new input, nothing already feeding
  // it.
  const feedersFor = (blocks, links, to, input, slotsFor) => {
    const down = linkReach(links, to.id, true);
    const into = new Set(links.filter((l) => l.to === to.id).map((l) => l.from));
    return blocks.filter((b) => b.id !== to.id && !down.has(b.id) &&
      !(input === '' && into.has(b.id)) &&
      slotsFor(b, to).indexOf(input) >= 0);
  };

  // The blocks `from` can connect to ("Connect to…"): a free input or a new
  // one, not `from` itself, nothing upstream of it. A block that takes any
  // number of inputs and already reads `from` is left out: a second link
  // would hand it the same data twice.
  const targetsFor = (blocks, links, from, slotsFor) => {
    const up = linkReach(links, from.id, false);
    return blocks.filter((b) => {
      if (b.id === from.id || up.has(b.id)) return false;
      const free = slotsFor(from, b);
      if (!free.length) return false;
      const onlyNew = free.every((s) => s === '');
      return !(onlyNew && links.some((l) => l.from === from.id && l.to === b.id));
    });
  };

  // Why an input name is refused ('' when it is not): the design system's
  // rename rules, an empty name or one another input of the block has.
  const inputNameError = (name, taken) => {
    const v = String(name == null ? '' : name).trim();
    if (!v) return 'An input needs a name';
    if ((taken || []).indexOf(v) >= 0) return 'Another input is called ' + v;
    return '';
  };

  let uidCounter = 0;

  function create(rootEl, adapter) {

    const uid = 'md' + uidCounter++;
    const emit = (name, payload) => adapter.emit(name, payload);
    const opts = Object.assign({
      search: true, addButton: true, addRow: false, stacks: true,
      remove: true, status: true,
      allowCycles: false, nameEdit: 'dblclick',
      edgeLabels: false, labelPad: 34,
      // The design system's search field shows only above 8 entries, as a
      // menu shows its filter box: a list that fits on screen is read, not
      // searched. A host that wants it always can pass 0.
      searchMin: 8,
      searchPlaceholder: 'Search…',
      searchEmptyText: 'No block matches',
      emptyText: 'No blocks yet.',
      emptyAddText: 'Add block',
      addRowText: 'Add block',
      stackNoun: 'Stack', stackUnit: 'blocks', stackIcon: STACK_ICON,
      stackAddText: 'Stack them', stackRmTitle: 'Dissolve stack (blocks stay)',
      // The stack-focus view: a focus button on every stack row, and the
      // list narrowed to that stack plus whatever touches it. The whole
      // board's name in the breadcrumb is an option because only a board
      // calls itself one.
      focusView: true, crumbRootText: 'Board',
      rowMenu: false
    }, adapter.opts || {});
    const M = Object.assign({}, DEFAULT_METRICS, opts.metrics || {});
    const { LANE_W, ROW_H, GAP, RAIL_L, RAIL_R, DOT_R } = M;
    const { STACK_PAD, STACK_GAP } = M;
    // The length a lane change is drawn over, and the step of the tail
    // under the list. Rows are not evenly spaced any more (a stack's band
    // adds its padding and air), so where a row IS comes from `rowTops`.
    const PITCH = ROW_H + GAP;

    const slotsFor = adapter.slotsFor;
    const nodeLead = adapter.nodeLead || (() => null);
    const nodeTrail = adapter.nodeTrail || (() => null);
    const nodeAside = adapter.nodeAside || (() => null);
    const stackAside = adapter.stackAside || (() => null);
    const nodeTools = adapter.nodeTools || (() => null);
    const stackTools = adapter.stackTools || (() => null);
    const slotPrompt = adapter.slotPrompt ||
      ((from, to) => 'Into which input of ' + to.name + '?');
    // whether the connections popover names the slot an edge occupies
    const showSlot = adapter.showSlot || ((l) => l.input !== '');
    // the link hooks (see the header); absent ones leave their rows out
    const linkInput = adapter.linkInput || ((l) => showSlot(l) ? l.input : '');
    const linkInsert = adapter.linkInsert || null;
    const linkEdit = adapter.linkEdit || (() => null);
    const inputMarkers = adapter.inputMarkers || null;
    const nodeItem = adapter.nodeItem || (() => ({}));

    /* ---- skeleton ---- */

    rootEl.innerHTML = '';
    // outline.css is scoped to `.outline`, and the renderer owns the skeleton
    // that stylesheet describes -- so it puts the class on rather than making
    // every host remember to (the board's container already has it).
    rootEl.classList.add('outline');
    // A host whose row tools include a menu of the row's other tools (the
    // board's "…", see `nodeTools`): on a narrow panel only that one shows.
    rootEl.classList.toggle('md-rowmenu', !!opts.rowMenu);
    // The row height is a metric, so CSS reads it from here rather than
    // hard-coding 28px: a process step row is taller than a block row.
    rootEl.style.setProperty('--blockr-outline-row-h', ROW_H + 'px');
    rootEl.style.setProperty('--blockr-outline-stack-pad', STACK_PAD + 'px');

    // Everything that is ABOUT the list rather than in it -- search, and the
    // selection actions -- rides in one sticky header. On a 92-row board the
    // action bar used to be the last child, pinned to the bottom edge of a
    // panel you had scrolled far away from: you ctrl-clicked two rows at the
    // top and the "Stack them" it offered was a screen below, if the panel
    // scrolled at all. Both now stay under the search box as the list moves.
    const headEl = document.createElement('div');
    headEl.className = 'md-head';
    rootEl.appendChild(headEl);

    // The focused stack, as a tag with an x, while a stack is focused. Above
    // the search row, so the search reads as scoped BY it: inside a focused
    // stack the query looks at its members only.
    let crumbEl = null;
    if (opts.stacks && opts.focusView) {
      crumbEl = document.createElement('div');
      crumbEl.className = 'md-crumb';
      crumbEl.hidden = true;
      headEl.appendChild(crumbEl);
    }

    // The search field, the fold-all tool and (for a host that wants one) an
    // add tool share one row. The row itself goes when none of them is
    // showing, which on a small board without stacks is the usual case.
    let searchRow = null, searchField = null, searchEl = null, hitsEl = null,
      clearEl = null, foldBtn = null, addBtn = null;
    if (opts.search || opts.stacks || opts.addButton) {
      searchRow = document.createElement('div');
      searchRow.className = 'md-search-row';
      headEl.appendChild(searchRow);
    }
    if (opts.search) {
      // The design system's search field: the magnifier inside on the left,
      // the hit count and a clear button inside on the right while there is
      // a query.
      searchField = document.createElement('div');
      searchField.className = 'md-search-field';
      searchField.innerHTML = '<span class="md-search-icon">' + SEARCH_ICON + '</span>';
      searchEl = document.createElement('input');
      searchEl.className = 'md-search';
      searchEl.type = 'text';
      searchEl.autocomplete = 'off';
      searchEl.placeholder = opts.searchPlaceholder;
      searchEl.setAttribute('aria-label', 'Search');
      searchField.appendChild(searchEl);
      hitsEl = document.createElement('span');
      hitsEl.className = 'md-hits';
      searchField.appendChild(hitsEl);
      clearEl = toolBtn('md-search-clear', icon('x'), 'Clear the search');
      clearEl.hidden = true;
      clearEl.addEventListener('click', () => {
        searchEl.value = '';
        applySearch();
        searchEl.focus();
      });
      searchField.appendChild(clearEl);
      searchRow.appendChild(searchField);
    }
    // Fold or unfold EVERY stack at once: collapsed-all reads as a table
    // of contents of the board. Lives beside the search because both are
    // about the whole list; hidden inside a focused view, where the
    // folding is the view's own doing. `updateFold` (called from render)
    // keeps its tooltip and pressed state honest.
    if (opts.stacks) {
      foldBtn = toolBtn('md-fold', opts.stackIcon, 'Collapse all');
      foldBtn.addEventListener('click', () => {
        const all = stacks.length &&
          stacks.every((s) => collapsed.has(s.id));
        if (all) collapsed.clear();
        else stacks.forEach((s) => collapsed.add(s.id));
        render();
      });
      searchRow.appendChild(foldBtn);
    }
    if (opts.addButton) {
      addBtn = toolBtn('md-add', icon('plus'), 'Add a block');
      // the button itself travels with the gesture: an adapter that
      // answers with a picker opens it ON the control that was pressed
      addBtn.addEventListener('click', () => emit('block_add', { el: addBtn }));
      searchRow.appendChild(addBtn);
    }

    let barEl = null, selcountEl = null, mkstackBtn = null;
    if (opts.stacks) {
      barEl = document.createElement('div');
      barEl.className = 'md-actionbar';
      selcountEl = document.createElement('span');
      selcountEl.className = 'md-selcount';
      barEl.appendChild(selcountEl);
      mkstackBtn = document.createElement('button');
      mkstackBtn.type = 'button';
      mkstackBtn.className = 'md-btn md-btn--main';
      mkstackBtn.textContent = opts.stackAddText;
      barEl.appendChild(mkstackBtn);
      const clearselBtn = toolBtn('md-selclear', icon('x'), 'Clear the selection');
      barEl.appendChild(clearselBtn);
      headEl.appendChild(barEl);

      clearselBtn.addEventListener('click', () => {
        selection.clear();
        // and the anchor with it: "Clear" means start over, so the next
        // shift-click must not sweep from a row cleared three actions ago
        selAnchor = null;
        // Selection is painted, not built: rings off + bar update, same as
        // every other selection change. This used to call render() -- a
        // full 92-row rebuild to remove some CSS classes.
        deckEl.querySelectorAll('.md-chip.sel').forEach(
          (x) => x.classList.remove('sel')
        );
        updateBar();
      });

      mkstackBtn.addEventListener('click', () => {
        const members = [...selection];
        // Already stacked is not a refusal. The new group is where they end
        // up, and they leave the one they were in on the way in -- the same
        // move as dragging them out of the frame and grouping them, in one
        // gesture. Regrouping part of a stack is how a stack gets split, and
        // it was the one membership edit with no gesture at all.
        const trial = stacks.map((s) => Object.assign({}, s, {
          blocks: s.blocks.filter((id) => !members.includes(id))
        })).filter((s) => s.blocks.length).concat(
          [{ id: '_trial', name: '', blocks: members }]
        );
        if (G.superOrder(model(), trial).hadCycle) {
          barEl.classList.add('err');
          selcountEl.textContent = 'That grouping would tangle the flow';
          return;
        }
        selection.clear();
        selAnchor = null;
        emit('stack_add', { blocks: members });
      });
    }

    const deckEl = document.createElement('div');
    deckEl.className = 'md-deck';
    rootEl.appendChild(deckEl);

    /* ---- state ---- */

    let blocks = [], links = [], stacks = [];
    const statuses = new Map();   // node id -> {color, label, status} | null
    const collapsed = new Set();  // stack ids, client-side only
    let stackFocus = null;        // stack id, client-side only (like collapsed)
    let selection = new Set();
    // the row a shift-click measures its range from: the last row clicked at
    // all, plain or cmd, which is what every list in every file manager does
    let selAnchor = null;
    let lastPos = new Map();
    let closePicker = null, dragging = false, pendingRender = false;
    // the row that takes Tab: a block id, or `stack:<id>` for a stack's row
    let kbKey = null, kbMoved = false;
    // The current row: the block open in the dock, as the host reports it
    // (`setCurrent`). Not the selection, which is what a click or a
    // ctrl-click gathers for "Stack them" and the clipboard.
    let current = null;
    const rowKey = (el) => el.dataset.id ||
      (el.dataset.stack ? 'stack:' + el.dataset.stack : null);

    const blockOf = (id) => blocks.find((b) => b.id === id);
    const stackOf = (id) => stacks.find((s) => s.blocks.includes(id)) || null;
    const parentsOf = (id) => links.filter((l) => l.to === id).map((l) => l.from);
    const childrenOf = (id) => links.filter((l) => l.from === id).map((l) => l.to);

    const upstream = (id) => {
      const seen = new Set(); const q = [id];
      while (q.length) {
        const x = q.shift();
        parentsOf(x).forEach((p) => { if (!seen.has(p)) { seen.add(p); q.push(p); } });
      }
      return seen;
    };
    const downstream = (id) => {
      const seen = new Set(); const q = [id];
      while (q.length) {
        const x = q.shift();
        childrenOf(x).forEach((c) => { if (!seen.has(c)) { seen.add(c); q.push(c); } });
      }
      return seen;
    };

    /* ---- ordering + lanes: the pure geometry lives in outline-layout.js ---- */

    // `outline-layout.js` owns row order and lane assignment so `node --test`
    // can hold it to its invariants (tests/js/). Everything below draws.
    const G = (typeof globalThis !== 'undefined' ? globalThis : window).outlineLayout;

    const model = () => ({ blocks, links, stacks, collapsed, lastPos });

    /* ---- search: the rows a query leaves standing ----------------------
     *
     * A search NARROWS the list rather than dimming it. Dimming is fine on a
     * board you can see all of and useless on one you cannot: the CDEX board
     * is 92 rows, so "19 / 92" used to leave you scrolling four screens of
     * pale grey looking for the hits, none of which were on the first one.
     *
     * What survives is the hits AND everything upstream of them, because a
     * block shown without the blocks feeding it is not a smaller picture of
     * the board, it is a wrong one: the rail would draw a chain that starts
     * nowhere. Ancestors only, never descendants -- on a board where one
     * click-aggregator collects from every chart, pulling descendants in
     * takes any query straight back to all 92 rows. Measured on CDEX:
     * ancestors alone leave 17 to 49 rows depending on the query, which is
     * the 3-4x that makes the list readable; with descendants, all of them.
     *
     * `searchKeep` is null when no query is active, and the drawing path is
     * the ONLY thing that reads it -- `model()` stays whole, so the stack
     * and cycle checks below still reason about the real board.
     */
    let searchKeep = null, searchHits = null;

    const drawModel = () => {
      // Focus first: the focused view is a different narrowing with its own
      // rules (neighbours stay as doorways), and the search inside it is
      // handed over as the member filter. `focusModel` reads the model
      // whole, so the stack and cycle checks below keep reasoning about the
      // real board -- same contract as `searchKeep`.
      if (stackFocus) {
        const m = G.focusModel(model(), stackFocus, searchKeep);
        if (m) return m;
      }
      if (!searchKeep) return model();
      const keep = searchKeep;
      return {
        blocks: blocks.filter((b) => keep.has(b.id)),
        links: links.filter((l) => keep.has(l.from) && keep.has(l.to)),
        // a group keeps the members that survived; one with none left is not
        // drawn, and `collapsed` can name a stack that is no longer there
        // without harm
        stacks: stacks
          .map((s) => Object.assign({}, s, {
            blocks: s.blocks.filter((id) => keep.has(id))
          }))
          .filter((s) => s.blocks.length),
        collapsed: collapsed,
        lastPos: lastPos
      };
    };

    /* ---- stack focus: the deck narrowed to one stack ------------------
     *
     * Client-side state like `collapsed`: a way of LOOKING at the board, not
     * a fact about it. Entering clears the search (the query belonged to the
     * view being left), and the search box then scopes to the focused
     * stack's members. `stack_focus` is emitted on every change so a host
     * can react (the board adapter uses it to expire its pending
     * join-at-birth); it is not required to.
     */

    const focusedStack = () =>
      stackFocus ? stacks.find((s) => s.id === stackFocus) || null : null;

    const resetSearch = () => {
      if (searchEl) searchEl.value = '';
      searchKeep = null;
      searchHits = null;
      if (hitsEl) hitsEl.textContent = '';
      if (clearEl) clearEl.hidden = true;
    };

    const setStackFocus = (id) => {
      if (stackFocus === id) return;
      // Folding EVERY stack is a one-time overview: it exists to pick a
      // stack from the table of contents, so diving into one consumes it --
      // leaving the focus lands on the full list, not back on the fold.
      // Individually collapsed stacks (a deliberate chevron each) survive.
      if (stacks.length && stacks.every((s) => collapsed.has(s.id))) {
        collapsed.clear();
      }
      // A focus is a view change, not an action on a selection -- and the
      // double-click that enters it BEGINS with a plain click, which on a
      // stack header means "select the whole group". Without this you
      // arrived in the focused view with every member ringed and the
      // action bar up, which read as the view having done something.
      if (selection.size) {
        selection.clear();
        selAnchor = null;
      }
      stackFocus = id;
      resetSearch();
      emit('stack_focus', { id: id });
      render();
    };

    const clearStackFocus = () => {
      if (!stackFocus) return;
      stackFocus = null;
      resetSearch();
      emit('stack_focus', { id: null });
      render();
    };

    const updateFold = () => {
      if (!foldBtn) return;
      foldBtn.hidden = !stacks.length || !!stackFocus;
      const all = stacks.length && stacks.every((x) => collapsed.has(x.id));
      // pressed while everything is folded: the accent tint, as every
      // pressed icon button
      foldBtn.classList.toggle('active', !!all);
      foldBtn.setAttribute('aria-pressed', all ? 'true' : 'false');
      const label = all ? 'Expand all' : 'Collapse all';
      foldBtn.setAttribute('aria-label', label);
      tip(foldBtn, label);
    };

    // What the head row holds right now. The search field is there above
    // `searchMin` entries (the members, inside a focused stack) or while a
    // query is typed; the row goes when nothing in it shows, and the
    // selection bar, which normally lies over it, then takes its own line.
    const updateHead = () => {
      if (searchField) {
        const fs = focusedStack();
        const n = fs ? fs.blocks.length : blocks.length;
        searchField.hidden = n <= opts.searchMin && !searchEl.value;
      }
      if (searchRow) {
        searchRow.hidden = (!searchField || searchField.hidden) &&
          (!foldBtn || foldBtn.hidden) && !addBtn;
      }
      headEl.classList.toggle('md-head--bare', !searchRow || searchRow.hidden);
    };

    const updateCrumb = () => {
      const s = focusedStack();
      rootEl.classList.toggle('md-focused', !!s);
      updateFold();
      updateHead();
      if (!crumbEl) return;
      crumbEl.hidden = !s;
      crumbEl.innerHTML = '';
      if (!s) return;
      // One tag, one ×. This started life as a "Board › <stack>" breadcrumb,
      // but nothing else in the app navigates by breadcrumb, so it read as a
      // new idiom to learn. A tag is one the design system already has: a
      // value in force, and × takes it off.
      const cur = document.createElement('span');
      cur.className = 'md-crumb-cur';
      if (s.color) {
        cur.classList.add('md-tinted');
        cur.style.setProperty('--blockr-outline-stack', s.color);
      }
      const cap = document.createElement('span');
      cap.className = 'md-cap';
      cap.innerHTML = opts.stackIcon;
      if (s.color) cap.style.setProperty('--blockr-outline-mark', s.color);
      cur.appendChild(cap);
      const nm = document.createElement('span');
      nm.className = 'md-crumb-name';
      nm.textContent = s.name;
      tip(nm, s.name, { overflow: true });   // readable even when cut off
      cur.appendChild(nm);
      const n = document.createElement('span');
      n.className = 'md-badge';
      n.textContent = s.blocks.length + ' ' + opts.stackUnit;
      cur.appendChild(n);
      const x = toolBtn('md-crumb-x', icon('remove'),
        'Back to the whole ' + opts.crumbRootText.toLowerCase());
      x.addEventListener('click', clearStackFocus);
      cur.appendChild(x);
      crumbEl.appendChild(cur);
    };

    const displayRows = () => G.displayRows(drawModel());
    const innerOrder = (s) => G.innerOrder(model(), s);
    const railIdOf = (id) => G.railIdOf(model(), id);
    const railModel = (rows) => G.railModel(drawModel(), rows);
    const layout = (entries, rl, rowOf, back) => G.layout(entries, rl, rowOf, back);

    const laneX = (l) => RAIL_L + l * LANE_W;

    const rowTops = (rows) => rowTopsFor(rows, M);

    // The last drawn layout: the drags read it to tell which row the
    // pointer is over.
    let geo = { rows: [], tops: [], gaps: [], height: 0 };
    const dotY = (r) => geo.tops[r] + ROW_H / 2;
    // The row under a deck y, or -1. Each row owns half the air on either
    // side of it, so the gaps between rows and bands are never dead.
    const rowAtY = (y) => {
      const t = geo.tops, n = t.length;
      if (!n || y < -8) return -1;
      for (let i = 0; i < n; i++) {
        const end = i + 1 < n ? (t[i] + ROW_H + t[i + 1]) / 2 : geo.height + 12;
        if (y < end) return i;
      }
      return -1;
    };

    /* ---- rail id -> concrete endpoints (slot-aware) ---- */

    // drag source: a collapsed stack wires from its last block
    const sinkOf = (railId) => {
      if (!railId.startsWith('stack:')) return railId;
      const s = stacks.find((x) => x.id === railId.slice(6));
      const inner = innerOrder(s);
      return inner[inner.length - 1];
    };

    // drop target: the first member (topological) this edge could occupy
    const targetBlock = (fromRail, railId) => {
      const from = blockOf(sinkOf(fromRail));
      const fits = (b) => b && from && slotsFor(from, b).length;
      if (!railId.startsWith('stack:')) {
        const b = blockOf(railId);
        return fits(b) ? b : null;
      }
      const s = stacks.find((x) => x.id === railId.slice(6));
      for (const id of innerOrder(s)) {
        const b = blockOf(id);
        if (fits(b)) return b;
      }
      return null;
    };

    // 'ok' | 'full' | 'cycle' | 'self' — why a drop on `toRail` would (not) work
    const dropVerdict = (fromRail, toRail) => {
      const fromId = sinkOf(fromRail);
      const from = blockOf(fromId);
      const b = toRail.startsWith('stack:')
        ? targetBlock(fromRail, toRail)
        : blockOf(toRail);
      if (!b || !from) return 'full';
      if (fromId === b.id) return 'self';
      if (!slotsFor(from, b).length) return 'full';
      if (!opts.allowCycles && upstream(fromId).has(b.id)) return 'cycle';
      return 'ok';
    };

    const doConnect = (fromRail, toRail, px, py) => {
      const fromId = sinkOf(fromRail);
      const from = blockOf(fromId);
      const b = targetBlock(fromRail, toRail);
      if (!b || dropVerdict(fromRail, toRail) !== 'ok') return false;
      const free = slotsFor(from, b);
      if (free.length === 1) {
        emit('link_add', { from: fromId, to: b.id, input: free[0] });
        return true;
      }
      openSlotPicker(px, py, from, b, free, (slot) =>
        emit('link_add', { from: fromId, to: b.id, input: slot }));
      return true;
    };

    /* ---- render ---- */

    const editing = () => !!rootEl.querySelector('[contenteditable="true"]') ||
      (document.activeElement && deckEl.contains(document.activeElement) &&
        document.activeElement.matches('input, textarea, select'));

    const render = () => {
      if (dragging || editing()) { pendingRender = true; return; }
      pendingRender = false;
      if (closePicker) closePicker();
      // The relatedness focus is painted on rows this rebuild is about to
      // destroy, so it has to be dropped -- but it is HOVER state, and the
      // pointer has not moved. Clearing it and leaving it cleared is what
      // made every board change flash the dim off and then, one mousemove
      // later, back on: a click that reveals a block pushes a fresh model,
      // and on a 92-row board the gap is long enough to read as a glitch.
      // So remember it here and repaint it below, once the rows exist again.
      const refocus = focusId;
      clearFocus();
      // Keyboard focus on a row survives the rebuild: the row is found again
      // by its key and focused (a keyboard move pushes a new model, and the
      // row it moved has to keep the keyboard).
      const act = document.activeElement;
      const actRow = act && deckEl.contains(act)
        ? act.closest('.md-chip, .md-stackhead') : null;
      const refocusRow = !!actRow;
      if (actRow) kbKey = rowKey(actRow);
      deckEl.innerHTML = '';
      updateCrumb();

      // A query that matches nothing is not an empty board: offering "+ Add a
      // block" there would answer a question nobody asked, and hide the one
      // fact that matters -- the board still has 92 rows, this query reaches
      // none of them.
      // Both empty states are the design system's one line of italic muted
      // text where the rows would be, with the way out as a slot in it.
      // The design system's empty state (`.blockr-empty`); `md-empty` keeps
      // its look where an older copy of blockr.ui's block stylesheet wins
      // the dependency name.
      const emptyLine = (text, slotText, onSlot) => {
        const p = document.createElement('p');
        p.className = 'blockr-empty blockr-empty--panel md-empty';
        p.appendChild(document.createTextNode(text + (slotText ? ' ' : '')));
        if (slotText) {
          const b = document.createElement('button');
          b.type = 'button';
          b.className = 'md-empty-add blockr-slot md-slot';
          // a host may still write the old "+ Add ..." wording; the slot is
          // already the offer, so the plus goes
          b.textContent = String(slotText).replace(/^\+\s*/, '');
          b.addEventListener('click', () => onSlot(b));
          p.appendChild(b);
        }
        deckEl.appendChild(p);
      };

      if (searchKeep && !searchKeep.size) {
        const q = searchEl ? searchEl.value.trim() : '';
        emptyLine(opts.searchEmptyText + (q ? ' “' + q + '”.' : '.'),
          'Clear the search', () => {
            searchEl.value = '';
            applySearch();
            searchEl.focus();
          });
        updateBar();
        return;
      }

      // An empty list is one line. Where the list ends with the add button
      // (`addRow`), the button is still there under it and the line has no
      // slot of its own.
      if (!blocks.length) {
        if (opts.addRow) {
          emptyLine(opts.emptyText);
          deckEl.appendChild(addRowBtn(0));
        } else {
          emptyLine(opts.emptyText, opts.emptyAddText,
            (b) => emit('block_add', { el: b }));
        }
        updateBar();
        return;
      }

      const rows = displayRows();
      lastPos = new Map();
      rows.forEach((r, i) => {
        if (r.t === 'node') lastPos.set('n:' + r.node.id, i);
        else lastPos.set('s:' + r.stack.id, i);
      });
      geo = Object.assign({ rows: rows }, rowTops(rows));
      const { entries, rowOf, rl, back } = railModel(rows);
      const { laneOf, edges, backs, nLanes } = layout(entries, rl, rowOf, back);
      const labelPad = opts.edgeLabels ? opts.labelPad : 0;
      const railW = RAIL_L + nLanes * LANE_W + RAIL_R + labelPad;
      const H = geo.height;
      // a row drags only where there is somewhere to drop it (a stack)
      rootEl.classList.toggle('md-can-drag', !!(opts.stacks && stacks.length));

      const svg = svgEl('svg');
      svg.setAttribute('class', 'md-rail');
      svg.setAttribute('width', railW);
      svg.setAttribute('height', H);

      const arrowId = uid + '-arrow';
      if (backs.length) {
        const defs = svgEl('defs');
        defs.innerHTML =
          '<marker id="' + arrowId + '" viewBox="0 0 8 8" refX="7" refY="4" ' +
          'markerWidth="5" markerHeight="5" orient="auto">' +
          '<path d="M0,0 L8,4 L0,8 z" class="md-arrow"/></marker>';
        svg.appendChild(defs);
      }

      // Siblings share their producer's bus, so their drawn paths overlap
      // above the first consumer. Hovering has to stay unambiguous: each edge
      // owns only the stretch of bus BELOW the previous consumer -- that band
      // is where the hover lights it and where a click opens its menu (the
      // siblings still running down the same stretch are listed first).
      const hitFrom = new Map();
      const vRun = new Map();
      const byFrom = new Map();
      edges.forEach((e) => {
        const arr = byFrom.get(e.from) || [];
        arr.push(e);
        byFrom.set(e.from, arr);
      });
      byFrom.forEach((arr, from) => {
        arr.sort((a, b) => (rowOf.get(a.to) ?? 0) - (rowOf.get(b.to) ?? 0));
        arr.forEach((e, i) => hitFrom.set(
          e.from + '>' + e.to,
          i === 0 ? rowOf.get(from) : rowOf.get(arr[i - 1].to)
        ));
      });

      edges.forEach((e) => {
        const rF = rowOf.get(e.from), rT = rowOf.get(e.to);
        const xF = laneX(laneOf.get(e.from)), xT = laneX(laneOf.get(e.to));
        const xE = laneX(e.lane);
        const yF = dotY(rF), yT = dotY(rT);
        let d = 'M' + xF + ',' + yF;
        let y = yF;
        if (xE !== xF) {
          const y2 = yF + PITCH;
          d += ' C' + xF + ',' + (yF + PITCH * 0.8) + ' ' + xE + ',' + (y2 - PITCH * 0.8) + ' ' + xE + ',' + y2;
          y = y2;
        }
        const yIn = xE !== xT ? yT - PITCH : yT;
        // the stretch this edge runs straight down its lane: where siblings
        // out of the same producer can lie on top of it
        vRun.set(e, { x: xE, y0: y, y1: Math.max(y, yIn) });
        if (yIn > y) { d += ' L' + xE + ',' + yIn; y = yIn; }
        if (xE !== xT) {
          d += ' C' + xE + ',' + (y + PITCH * 0.8) + ' ' + xT + ',' + (yT - PITCH * 0.8) + ' ' + xT + ',' + yT;
        }
        const p = svgEl('path');
        p.setAttribute('d', d);
        p.setAttribute('fill', 'none');
        p.setAttribute('stroke', LANE_COLORS[e.lane % LANE_COLORS.length]);
        p.setAttribute('stroke-width', '2');
        p.setAttribute('class', 'md-edge');
        p.dataset.from = e.from;
        p.dataset.to = e.to;
        svg.appendChild(p);
        const rH = hitFrom.get(e.from + '>' + e.to) ?? rF;
        const yH = Math.max(dotY(rH), yF);
        const yIn2 = xE !== xT ? yT - PITCH : yT;
        let dh = 'M' + xE + ',' + Math.min(yH, yIn2);
        if (yIn2 > yH) dh += ' L' + xE + ',' + yIn2;
        if (xE !== xT) {
          dh += ' C' + xE + ',' + (yIn2 + PITCH * 0.8) + ' ' + xT + ',' +
            (yT - PITCH * 0.8) + ' ' + xT + ',' + yT;
        }
        const hit = svgEl('path');
        hit.setAttribute('d', dh);
        hit.setAttribute('fill', 'none');
        hit.setAttribute('stroke', 'transparent');
        hit.setAttribute('stroke-width', '12');
        hit.setAttribute('class', 'md-edge-hit');
        wireEdge(e, p, hit);
        svg.appendChild(hit);
      });
      edgeGeo = { edges: edges, vRun: vRun };

      // Loop-backs climb the right-hand gutter, dashed and arrowed: they run
      // against the reading direction, so they are drawn as the exception
      // they are rather than as another line in the flow.
      backs.forEach((e) => {
        const xF = laneX(laneOf.get(e.from)), xT = laneX(laneOf.get(e.to));
        const yF = dotY(rowOf.get(e.from)), yT = dotY(rowOf.get(e.to));
        const xB = laneX(e.lane);
        const p = svgEl('path');
        p.setAttribute('d',
          'M' + xF + ',' + yF +
          ' C' + (xF + LANE_W) + ',' + yF + ' ' + xB + ',' + (yF - PITCH * 0.25) +
          ' ' + xB + ',' + (yF - PITCH * 0.5) +
          ' L' + xB + ',' + (yT + PITCH * 0.5) +
          ' C' + xB + ',' + (yT + PITCH * 0.25) + ' ' + (xT + LANE_W) + ',' + yT +
          ' ' + (xT + DOT_R + 2) + ',' + yT);
        p.setAttribute('fill', 'none');
        p.setAttribute('stroke', LANE_COLORS[e.lane % LANE_COLORS.length]);
        p.setAttribute('stroke-width', '1.6');
        p.setAttribute('stroke-dasharray', '3 3');
        p.setAttribute('marker-end', 'url(#' + arrowId + ')');
        // `up` is not a loop: it is a plain dependency the ROW ORDER could
        // not honour, and the only thing that forces that on a board is a
        // stack whose frame has to jump over a block feeding it. The stack
        // header's "between" button is where that is explained and fixed.
        p.setAttribute('class', 'md-edge md-edge-back' + (e.up ? ' md-edge-up' : ''));
        p.dataset.from = e.from;
        p.dataset.to = e.to;
        svg.appendChild(p);
        const hit = svgEl('path');
        hit.setAttribute('d', p.getAttribute('d'));
        hit.setAttribute('fill', 'none');
        hit.setAttribute('stroke', 'transparent');
        hit.setAttribute('stroke-width', '12');
        hit.setAttribute('class', 'md-edge-hit');
        wireEdge(e, p, hit, true);
        svg.appendChild(hit);
      });

      // What a link is CALLED, in the gutter beside the row it feeds. On a
      // board that is the input slot; in a process it is the branch the work
      // left on ("false"), which is the whole reason a reader can tell a
      // rework arm from the happy path. Anchored at the consumer, so a
      // producer fanning out four ways does not stack four labels on one row.
      if (opts.edgeLabels) {
        const perRow = new Map();
        edges.concat(backs).forEach((e) => {
          const l = linksBehind(e.from, e.to)[0];
          if (!l || !showSlot(l)) return;
          const r = rowOf.get(e.to);
          const n = perRow.get(r) || 0;
          perRow.set(r, n + 1);
          const t = svgEl('text');
          t.setAttribute('x', railW - RAIL_R);
          // one line per label at the canvas type size (11px)
          t.setAttribute('y', dotY(r) - 5 - n * 11);
          t.setAttribute('text-anchor', 'end');
          t.setAttribute('class', 'md-edge-label');
          t.setAttribute('fill', LANE_COLORS[e.lane % LANE_COLORS.length]);
          t.textContent = l.input;
          svg.appendChild(t);
        });
      }

      entries.forEach((e) => {
        const isStack = e.id.startsWith('stack:');
        const stackCol = isStack
          ? (stacks.find((s) => s.id === e.id.slice(6)) || {}).color
          : null;
        const laneCol = LANE_COLORS[laneOf.get(e.id) % LANE_COLORS.length];
        const baseR = isStack ? DOT_R + 1 : DOT_R;
        // The lane or stack colour is data and goes on as an attribute; the
        // surface the dot is punched out of is a token, so the stylesheet
        // sets it (a CSS rule outranks a presentation attribute).
        const c = svgEl('circle');
        c.setAttribute('cx', laneX(laneOf.get(e.id)));
        c.setAttribute('cy', dotY(e.row));
        c.setAttribute('r', baseR);
        if (isStack) c.setAttribute('fill', stackCol || laneCol);
        else c.setAttribute('stroke', laneCol);
        c.setAttribute('stroke-width', '2');
        c.setAttribute('class', 'md-dot' + (isStack ? ' md-dot--stack' : ''));
        c.dataset.rail = e.id;
        svg.appendChild(c);
        const hc = svgEl('circle');
        hc.setAttribute('cx', laneX(laneOf.get(e.id)));
        hc.setAttribute('cy', dotY(e.row));
        hc.setAttribute('r', '11');
        hc.setAttribute('fill', 'transparent');
        hc.setAttribute('pointer-events', 'all');
        hc.setAttribute('class', 'md-dot-hit');
        // the dot is a control with no words on it, so it names its gestures
        tip(hc, 'Drag to connect or append; click for connections');
        hc.addEventListener('mouseenter', () => c.setAttribute('r', String(baseR + 1.5)));
        hc.addEventListener('mouseleave', () => c.setAttribute('r', String(baseR)));
        hc.addEventListener('mousedown', (ev) => {
          // the row is looked up when the menu opens: a render in between
          // replaces it
          startDrag(ev, e.id, hc, () => {
            const anchor = deckEl.querySelector(
              '.md-chip[data-id="' + CSS.escape(e.id) + '"]');
            if (anchor) openConn(anchor, e.id);
          });
        });
        svg.appendChild(hc);
      });
      deckEl.appendChild(svg);

      // The rows. A stack is a band (`.md-stackframe`) holding its header and
      // its members, or its one folded row: the stack's colour at 7% on the
      // surface, no border (design system, "Block lists"). Every margin comes
      // from `rowTops`, so the rows land where the rail put their dots.
      const list = document.createElement('div');
      list.className = 'md-rows';
      list.style.marginLeft = railW + 'px';
      let band = null;
      rows.forEach((r, i) => {
        let rowEl;
        if (opensBand(r)) {
          band = document.createElement('div');
          band.className = 'md-stackframe' +
            (r.t === 'stack' ? ' md-folded' : '');
          band.dataset.stack = r.stack.id;
          // the colour is data and comes in as a custom property; the tint
          // is mixed in the stylesheet, onto the surface, so it follows the
          // dark scheme
          if (r.stack.color) {
            band.classList.add('md-tinted');
            band.style.setProperty('--blockr-outline-stack', r.stack.color);
          }
          band.style.marginTop = geo.gaps[i] + 'px';
          list.appendChild(band);
          rowEl = r.t === 'stack' ? stackChip(r.stack) : stackHead(r.stack);
          band.appendChild(rowEl);
          return;
        }
        rowEl = chip(r.node, r.inStack);
        rowEl.style.marginTop = geo.gaps[i] + 'px';
        if (band && r.inStack && band.dataset.stack === r.inStack.id) {
          band.appendChild(rowEl);
        } else {
          band = null;
          list.appendChild(rowEl);
        }
      });
      deckEl.appendChild(list);
      // One row takes Tab (a roving tabindex): the one keyboard focus was
      // on before this rebuild, else the current row, else the first.
      const focusables = [...list.querySelectorAll('.md-chip, .md-stackhead')];
      focusables.forEach((x) => { x.tabIndex = -1; });
      const back2 = kbKey && focusables.find((x) => rowKey(x) === kbKey);
      paintCurrent();
      const home = back2 || list.querySelector('.md-current') || focusables[0];
      if (home) home.tabIndex = 0;
      if (back2 && refocusRow) {
        back2.focus({ preventScroll: true });
        if (kbMoved) back2.scrollIntoView({ block: 'nearest' });
      }
      kbMoved = false;

      // "Add a block" as the last row of the list rather than as a button in the
      // search row: every other add gesture is about a parent (drag a dot to a
      // row, drag it to the gutter, the row's own `+`), and a block with no input
      // sorts at the END of a topological list -- so it is offered where it will
      // appear.
      //
      // Inside the deck, sharing the list's left margin (the rail is as wide as
      // the board has lanes) and sitting BEFORE the tail spacer, so it is neither
      // wider than the rows nor separated from them by the drop-zone slack. The
      // empty state carries the same offer, so this is only drawn when there are
      // rows.
      if (opts.addRow) deckEl.appendChild(addRowBtn(railW));

      // One empty row's worth of canvas under the list. On a long board (the
      // CDEX one is 92 rows) the list fills the panel exactly, so scrolled to
      // the end there was nowhere left to release a drag for "append" -- and
      // nowhere for the ghost row to show.
      const tail = document.createElement('div');
      tail.className = 'md-tail';
      tail.style.height = PITCH + 'px';
      deckEl.appendChild(tail);

      const wire = svgEl('svg');
      wire.setAttribute('class', 'md-wire');
      deckEl.appendChild(wire);

      updateBar();
      paintHits();
      renderBadges();

      // Only if the row is still there: a block removed by the very update
      // that triggered this render would light nothing, leaving the whole
      // outline dimmed with no explanation.
      if (refocus && deckEl.querySelector(
        '.md-chip[data-id="' + CSS.escape(refocus) + '"], ' +
        '.md-stackhead[data-stack="' + CSS.escape(refocus.replace(/^stack:/, '')) + '"]'
      )) {
        paintFocus(refocus);
      }
    };

    // Shift-click: every row from the anchor to this one joins the selection.
    // The order is read off the DOM, so it is the order ON SCREEN -- which is
    // what someone dragging their eye down the list means -- not the
    // topological order behind it. Rows already in a stack are skipped rather
    // than aborting the range: a stack in the middle of a sweep should not
    // cost you the rows on the far side of it. Additive, never subtractive,
    // so two sweeps compose.
    const selectRange = (fromId, toId) => {
      const ids = [...deckEl.querySelectorAll('.md-chip[data-id]')]
        .map((el) => el.dataset.id)
        .filter((id) => !id.startsWith('stack:'));   // collapsed stack rows
      const a = ids.indexOf(fromId), z = ids.indexOf(toId);
      if (a < 0 || z < 0) return;
      const lo = Math.min(a, z), hi = Math.max(a, z);
      ids.slice(lo, hi + 1).forEach((id) => {
        selection.add(id);
        const row = deckEl.querySelector('.md-chip[data-id="' + CSS.escape(id) + '"]');
        if (row) row.classList.add('sel');
      });
      selAnchor = toId;
      updateBar();
    };

    /* ---- row builders ---- */

    // The end of the list (design system, "Block lists"): a quiet button
    // named for what lands in it, 30px, its plus under the marks. It opens
    // the "+" menu the host answers `block_add` with. `left` is the rail's
    // width, so it lines up with the rows.
    const addRowBtn = (left) => {
      const addRow = document.createElement('button');
      addRow.type = 'button';
      addRow.className = 'md-addrow';
      // the row's 6px padding and half the 24px mark, less half the 12px
      // plus and the button's 8px padding: the plus under the marks' centre
      const inset = left + 4;
      addRow.style.marginLeft = inset + 'px';
      addRow.style.maxWidth = 'calc(100% - ' + inset + 'px)';
      const plus = document.createElement('span');
      plus.className = 'md-addrow-icon';
      plus.innerHTML = icon('plus');
      addRow.appendChild(plus);
      const lbl = document.createElement('span');
      lbl.className = 'md-addrow-label';
      // While focused, a new block joins the focused stack at creation
      // (the host adapter enforces it) -- said here, where the block will
      // appear, because a block that landed OUTSIDE the stack would vanish
      // from this view the instant it landed.
      const fs = focusedStack();
      lbl.textContent = opts.addRowText + (fs ? ' · joins ' + fs.name : '');
      addRow.appendChild(lbl);
      addRow.addEventListener('click', () => emit('block_add', { el: addRow }));
      return addRow;
    };

    // The right end of a row: its tools, then whatever else keeps the right
    // edge (the adapter's aside, a stack's chevron). The tools are in the
    // row, never over the name: at rest they take no space, and on hover or
    // keyboard focus they take the place of the aside (outline.css).
    const rowEnd = (row) => {
      const end = document.createElement('span');
      end.className = 'md-end';
      const tools = document.createElement('span');
      tools.className = 'md-rowtools';
      end.appendChild(tools);
      row.appendChild(end);
      return { el: end, tools: tools };
    };

    // The renderer owns the row shell (identity, selection, drop feedback,
    // rename, status, remove); the adapter paints what is inside it.
    const chip = (b, inStack) => {
      const el = document.createElement('div');
      el.className = 'md-chip' + (inStack ? ' instack' : '') +
        (selection.has(b.id) ? ' sel' : '');
      el.dataset.id = b.id;

      const lead = nodeLead(b);
      if (lead) el.appendChild(lead);

      // Status belongs at the START of the row, right behind the icon: a
      // coloured dot there is where the eye lands, so it must mean eval status
      // and nothing else. It used to sit past `md-spring` at the far right --
      // present, correct, and never seen -- while the adapter's slot pips held
      // this position in the block's own colour. It goes after the lead rather
      // than inside it because `nodeLead` hands back one fragment the renderer
      // does not take apart; adapters that draw pips should draw them only
      // when they carry news, so the dot reads as the row's first mark.
      if (opts.status) {
        const status = document.createElement('span');
        status.className = 'md-status';
        status.dataset.for = b.id;
        el.appendChild(status);
      }

      const name = nameEl(b, (nm) => emit('block_rename', { id: b.id, name: nm }));
      el.appendChild(name);

      // the inputs with nothing linked into them, after the name
      const pips = markersEl(b);
      if (pips) el.appendChild(pips);

      const trail = nodeTrail(b);
      if (trail) el.appendChild(trail);

      const spring = document.createElement('span');
      spring.className = 'md-spring';
      el.appendChild(spring);

      const end = rowEnd(el);

      // The row's tools, shown on hover or keyboard focus of the row: the
      // adapter's, and remove. A host with a row menu (`rowMenu`, the
      // board) hands over one "…" that carries every action, Remove
      // included, so the renderer adds none (design system, "Block lists":
      // one tool, no × on the row).
      const extra = nodeTools(b);
      if (extra) end.tools.appendChild(extra);
      if (opts.remove && !opts.rowMenu) {
        const rm = toolBtn('md-rm md-reveal', icon('x'), 'Remove block');
        rm.addEventListener('click', (e) => {
          e.stopPropagation();
          emit('block_rm', { id: b.id });
        });
        end.tools.appendChild(rm);
      }

      // Past the spring, so it right-aligns: `nodeTrail` sits beside the name
      // and is the wrong place for anything that wants to read as a column
      // down the outline. Kept as a separate hook rather than moving `nodeTrail`,
      // which blockr.process's adapter uses for exactly the beside-the-name
      // job its name promises.
      const aside = nodeAside(b);
      if (aside) end.el.appendChild(aside);

      // Only where there is somewhere to drop: on a board with no stacks the
      // gesture has no target, and swallowing mousedown would cost the row
      // its text selection for nothing.
      el.addEventListener('mousedown', (e) => {
        if (e.button !== 0 || !opts.stacks || !stacks.length) return;
        if (e.target.closest('button, input, select, textarea')) return;
        if (e.target.isContentEditable) return;
        startRowDrag(e, b, el);
      });

      el.addEventListener('click', (e) => {
        if (e.target.closest('button, input, select, textarea')) return;
        if (name.isContentEditable) return;

        if (opts.stacks && e.shiftKey && selAnchor && selAnchor !== b.id) {
          selectRange(selAnchor, b.id);
          return;
        }
        if (opts.stacks && (e.metaKey || e.ctrlKey || e.shiftKey)) {
          // A stacked block used to refuse selection outright, because the
          // only thing selection did was make a stack and a block cannot be
          // in two. Selection now also feeds copy, so the refusal belonged on
          // "Stack them", not here -- and while it sat here a stack could not
          // be copied at all.
          if (selection.has(b.id)) selection.delete(b.id); else selection.add(b.id);
          el.classList.toggle('sel');
          selAnchor = b.id;
          updateBar();
          return;
        }
        // A plain click IS a selection of one -- the row you clicked replaces
        // whatever was selected. Requiring a modifier for the first block and
        // a modifier for every one after it made the opening gesture of every
        // multi-select a keystroke nobody asked for, and left a plain click
        // with no visible consequence in the list at all. Revealing the panel
        // still happens; the two are not in competition, one is what you
        // looked at and the other is what you are about to act on.
        selAnchor = b.id;

        if (opts.stacks) {
          selection.clear();
          deckEl.querySelectorAll('.md-chip.sel').forEach(
            (x) => x.classList.remove('sel')
          );
          // a stacked block still refuses selection (dissolve first), so the
          // click clears and selects nothing rather than lying about it
          selection.add(b.id);
          el.classList.add('sel');
          updateBar();
        }

        emit('block_select', { id: b.id });
      });

      return el;
    };

    // `nameEdit: 'always'` gives an input instead of dblclick-to-rename: on a
    // board a name is an occasional correction, in a process editor naming
    // the step is the work.
    const nameEl = (obj, commit) => {
      if (opts.nameEdit === 'always') {
        const inp = document.createElement('input');
        inp.type = 'text';
        inp.className = 'md-name md-name-input';
        inp.value = obj.name;
        inp.placeholder = 'Name…';
        inp.addEventListener('input', () => {
          obj.name = inp.value;
          commit(inp.value);
        });
        inp.addEventListener('blur', () => { if (pendingRender) render(); });
        return inp;
      }
      const name = document.createElement('span');
      name.className = 'md-name';
      name.textContent = obj.name;
      renameable(name, obj, commit, 'block');
      return name;
    };

    /* Renaming in place (design system, "Renaming in place"): a double-click
     * on the name, or Rename in the row menu (`editName`), turns the text
     * into a field at its own size and weight. Enter commits, Escape
     * restores, a click elsewhere commits. An empty name is refused in place:
     * the danger edge and one line under the field saying why, and the field
     * stays open. At rest the name carries a tooltip only while it is cut
     * off. */
    const renameable = (name, obj, commit, noun) => {
      name.classList.add('md-name--edit');
      // no tooltip while the name is a field
      tip(name, () => name.isContentEditable ? '' : obj.name, { overflow: true });
      let errEl = null;
      const setBad = (bad) => {
        name.classList.toggle('md-name--bad', bad);
        if (bad && !errEl) {
          errEl = document.createElement('span');
          errEl.className = 'md-name-error';
          errEl.setAttribute('role', 'alert');
          errEl.textContent = 'A ' + noun + ' needs a name';
          const row = name.parentElement;
          if (row) {
            row.appendChild(errEl);
            // under the name, pulled left as far as it has to be to stay in
            // the row (a narrow panel clips what runs past it)
            errEl.style.left = Math.max(0, Math.min(name.offsetLeft,
              row.clientWidth - errEl.offsetWidth)) + 'px';
          }
        }
        if (!bad && errEl) { errEl.remove(); errEl = null; }
      };
      const start = () => {
        // the row's tools stand back while it is renamed
        if (name.parentElement) name.parentElement.classList.add('md-renaming');
        name.contentEditable = 'true';
        name.spellcheck = false;
        name.focus();
        document.getSelection().selectAllChildren(name);
      };
      name._mdEdit = start;
      name.addEventListener('dblclick', start);
      name.addEventListener('input', () => setBad(false));
      name.addEventListener('blur', () => {
        if (name.contentEditable !== 'true') return;
        name.contentEditable = 'false';
        if (name.parentElement) name.parentElement.classList.remove('md-renaming');
        setBad(false);
        const nm = name.textContent.trim();
        // a click elsewhere commits; an empty name cannot be committed, so
        // leaving the field with one restores the old name
        if (nm && nm !== obj.name) {
          obj.name = nm;
          commit(nm);
        }
        name.textContent = obj.name;
        if (pendingRender) render();
      });
      name.addEventListener('keydown', (e) => {
        if (e.key === 'Enter') {
          e.preventDefault();
          if (!name.textContent.trim()) { setBad(true); return; }
          name.blur();
        }
        if (e.key === 'Escape') {
          e.stopPropagation();
          name.textContent = obj.name;
          name.blur();
        }
      });
    };

    const stackChip = (stack) => {
      const el = document.createElement('div');
      el.className = 'md-chip md-stackchip';
      el.dataset.id = 'stack:' + stack.id;

      // the stack's mark: its icon on a tint of the stack's colour, drawn as
      // a block's mark is
      const k = document.createElement('span');
      k.className = 'md-kind md-stackkind';
      k.innerHTML = opts.stackIcon;
      if (stack.color) k.style.setProperty('--blockr-outline-mark', stack.color);
      k.setAttribute('aria-label', opts.stackNoun);
      el.appendChild(k);

      // Same position as on a block row (see chip()): the collapsed stack's
      // worst member status, read where a status dot always lives.
      const status = document.createElement('span');
      status.className = 'md-status';
      status.dataset.stack = stack.id;
      el.appendChild(status);

      // renamed in place like the expanded header's name
      const name = document.createElement('span');
      name.className = 'md-name';
      name.textContent = stack.name;
      renameable(name, stack,
        (nm) => emit('stack_rename', { id: stack.id, name: nm }),
        opts.stackNoun.toLowerCase());
      el.appendChild(name);

      el.appendChild(countEl(stack));

      const spring = document.createElement('span');
      spring.className = 'md-spring';
      el.appendChild(spring);

      const end = rowEnd(el);

      const extraS = stackTools(stack, true);
      if (extraS) end.tools.appendChild(extraS);

      const aside = stackAside(stack, true);
      if (aside) end.el.appendChild(aside);

      // Inside a focused view every other stack's row is a DOORWAY: the
      // collapse behind it is synthetic (drawModel folded it, `collapsed`
      // was never touched), so expanding it in place would contradict the
      // view. The whole row hops the focus there instead -- which is how a
      // big board gets walked stack by stack.
      const doorway = opts.focusView && stackFocus && stack.id !== stackFocus;

      // with a row menu, "Show only this group" is in the "…"
      if (opts.focusView && !doorway && !opts.rowMenu) {
        const fb = toolBtn('md-focusbtn', FOCUS_ICON, 'Show only ' + stack.name);
        fb.setAttribute('aria-pressed', 'false');
        fb.addEventListener('click', (e) => {
          e.stopPropagation();
          setStackFocus(stack.id);
        });
        end.tools.appendChild(fb);
      }

      // In a doorway the arrow is a "go to": it points right and stays
      // there. Otherwise it is the fold, closed, at the end of the row.
      const chev = doorway
        ? toolBtn('md-chev md-closed md-goto', icon('chevron'), 'Show only ' + stack.name)
        : chevBtn('', false, 'Expand ' + opts.stackNoun.toLowerCase());
      chev.addEventListener('click', (e) => {
        e.stopPropagation();
        if (doorway) { setStackFocus(stack.id); return; }
        collapsed.delete(stack.id);
        render();
      });
      end.el.appendChild(chev);

      if (doorway) {
        el.classList.add('md-hop');
      }

      el.addEventListener('click', (e) => {
        if (e.target.closest('button') || e.target.isContentEditable) return;
        if (doorway) {
          setStackFocus(stack.id);
          return;
        }
        if (!opts.stacks) {
          openConn(el, 'stack:' + stack.id);
          return;
        }
        selectStack(stack, e.metaKey || e.ctrlKey || e.shiftKey);
      });

      // Same drill-in double-click as the expanded header: on the row, not
      // on the name, which renames.
      if (opts.focusView && !doorway) {
        el.addEventListener('dblclick', (e) => {
          if (e.target.closest('button, .md-name') || e.target.isContentEditable) return;
          setStackFocus(stack.id);
        });
      }

      return el;
    };

    // A stack's member count: 13px text-muted, a plain number.
    const countEl = (stack) => {
      const n = document.createElement('span');
      n.className = 'md-count';
      n.textContent = String(stack.blocks.length);
      n.setAttribute('aria-label', stack.blocks.length + ' ' + opts.stackUnit);
      return n;
    };

    const stackHead = (stack) => {
      const el = document.createElement('div');
      el.className = 'md-stackhead';
      el.dataset.stack = stack.id;

      // the stack's 24px mark, in the stack's colour
      const cap = document.createElement('span');
      cap.className = 'md-cap md-stackkind';
      cap.innerHTML = opts.stackIcon;
      if (stack.color) cap.style.setProperty('--blockr-outline-mark', stack.color);
      el.appendChild(cap);

      const name = document.createElement('span');
      name.className = 'md-name';
      name.textContent = stack.name;
      renameable(name, stack,
        (nm) => emit('stack_rename', { id: stack.id, name: nm }),
        opts.stackNoun.toLowerCase());
      el.appendChild(name);

      el.appendChild(countEl(stack));

      // A stack the flow runs OUT of and back INTO cannot be drawn as one
      // clean run of rows, and the symptom -- an arrow climbing the gutter,
      // rows in an order that looks wrong -- gives no hint of the cause. So
      // name it here, on the group responsible, with the fix one click away:
      // the blocks in the way are exactly the ones that make it convex again.
      // The explanation and the fix live in a menu the button opens: the
      // explanation is its head, the fix its one action (disabled, with the
      // reason, when the blocks in the way belong to another stack).
      const holes = G.stackHoles(model(), stack);
      if (holes.length) {
        const nameOfBlk = (id) => (blockOf(id) || { name: id }).name;
        const free = holes.filter((id) => !stackOf(id));
        const noun = opts.stackNoun.toLowerCase();
        const warn = document.createElement('button');
        warn.className = 'md-warn';
        warn.type = 'button';
        warn.innerHTML = WARN_ICON;
        warn.appendChild(document.createTextNode(holes.length + ' between'));
        warn.addEventListener('click', (e) => {
          e.stopPropagation();
          const B = ui();
          if (!B || !B.menu) return;
          const names = holes.map(nameOfBlk).join(', ');
          const m = B.menu(warn, {
            head: {
              title: holes.length === 1 ? '1 block in between'
                : holes.length + ' blocks in between',
              text: names + (holes.length === 1 ? ' reads' : ' read') +
                ' from this ' + noun + ' and feed' +
                (holes.length === 1 ? 's' : '') + ' back into it without' +
                ' being in it, so its rows cannot follow the flow.'
            },
            items: [free.length
              ? {
                label: 'Add ' + (free.length === holes.length
                  ? (free.length === 1 ? 'it' : 'them')
                  : free.map(nameOfBlk).join(', ')) + ' to the ' + noun,
                onSelect: () => emit('stack_join', { blocks: free, stack: stack.id })
              }
              : {
                label: 'Add them to the ' + noun, disabled: true,
                reason: 'They are in another ' + noun + ', so they cannot join this one'
              }],
            onClose: () => { if (closePicker === m.close) closePicker = null; }
          });
          closePicker = m.close;
        });
        el.appendChild(warn);
      }

      const spring = document.createElement('span');
      spring.className = 'md-spring';
      el.appendChild(spring);

      const end = rowEnd(el);

      const extraS = stackTools(stack, false);
      if (extraS) end.tools.appendChild(extraS);

      const aside = stackAside(stack, false);
      if (aside) end.el.appendChild(aside);

      // The header selects the whole group, same as the collapsed row: they
      // are two views of one object, so one gesture.
      el.addEventListener('click', (e) => {
        if (e.target.closest('button') || e.target.isContentEditable) return;
        if (!opts.stacks) return;
        selectStack(stack, e.metaKey || e.ctrlKey || e.shiftKey);
      });

      // Double-click on the header toggles the focus -- "double-click drills
      // in" is muscle memory from every file manager, and the button alone
      // was only discoverable, not intuitive. The NAME keeps its own
      // double-click (rename), so the gesture lives on the rest of the row.
      if (opts.focusView) {
        el.addEventListener('dblclick', (e) => {
          if (e.target.closest('button, .md-name') || e.target.isContentEditable) return;
          if (stackFocus === stack.id) clearStackFocus();
          else setStackFocus(stack.id);
        });
      }

      // The way INTO the focused view -- always visible, quiet until
      // hovered. Not the header click: that means "select the group" and the
      // action bar depends on it. Not hover-revealed like `md-rm`: an
      // affordance nobody has seen is one nobody uses, and this is the one
      // button on the row that changes what the whole list shows.
      // remove first, so the focus tool keeps its place when it appears.
      // A host with a row menu carries both in the header's "…" instead.
      if (!opts.rowMenu) {
        const rm = toolBtn('md-rm md-reveal', icon('x'), opts.stackRmTitle);
        rm.addEventListener('click', () => emit('stack_rm', { id: stack.id }));
        end.tools.appendChild(rm);
      }

      if (opts.focusView && !opts.rowMenu) {
        const on = stackFocus === stack.id;
        // a pressed icon button while the view is focused on this stack
        const fb = toolBtn('md-focusbtn' + (on ? ' active' : ''), FOCUS_ICON,
          on ? 'Back to the whole ' + opts.crumbRootText.toLowerCase()
            : 'Show only ' + stack.name);
        fb.setAttribute('aria-pressed', on ? 'true' : 'false');
        fb.addEventListener('click', (e) => {
          e.stopPropagation();
          if (stackFocus === stack.id) clearStackFocus();
          else setStackFocus(stack.id);
        });
        end.tools.appendChild(fb);
      }

      // The fold, at the end of the header. No collapse offer on the focused
      // stack: its rows ARE the view.
      if (stackFocus !== stack.id) {
        const chev = chevBtn('', true, 'Collapse ' + opts.stackNoun.toLowerCase());
        chev.addEventListener('click', (e) => {
          e.stopPropagation();
          collapsed.add(stack.id);
          render();
        });
        end.el.appendChild(chev);
      }

      return el;
    };

    /* ---- the current row ----
     *
     * The block open in the dock is the current row (design system, "Block
     * lists"): `bg-selected` and `text-accent`. Inside a folded stack, the
     * folded row stands for it.
     */
    const paintCurrent = () => {
      deckEl.querySelectorAll('.md-current').forEach((x) => {
        x.classList.remove('md-current');
        x.removeAttribute('aria-current');
      });
      if (!current) return;
      let row = deckEl.querySelector('.md-chip[data-id="' + CSS.escape(current) + '"]');
      if (!row) {
        const s = stackOf(current);
        if (s) row = deckEl.querySelector('.md-chip[data-id="' + CSS.escape('stack:' + s.id) + '"]');
      }
      if (row) {
        row.classList.add('md-current');
        row.setAttribute('aria-current', 'true');
      }
    };

    /* ---- the keyboard row ----
     *
     * One row takes Tab (a roving tabindex, set in `render`); the arrows
     * move it, Enter opens it as a click does, and Alt+Up/Down moves the
     * row as a drag would (design system, "Block lists"). The outline's
     * order comes from the links, so the one place a row can move to is
     * across the edge of a stack: into the band above or below it, or out
     * of the one it is in from its first or last place.
     */
    const rowEls = () =>
      [...deckEl.querySelectorAll('.md-rows .md-chip, .md-rows .md-stackhead')];

    deckEl.addEventListener('focusin', (e) => {
      const row = e.target.closest('.md-chip, .md-stackhead');
      if (!row || row !== e.target) return;
      rowEls().forEach((x) => { x.tabIndex = x === row ? 0 : -1; });
      kbKey = rowKey(row);
    });

    // a refusal said at the row, the way a drag says it at the pointer
    const sayAtRow = (row, text) => {
      const r = row.getBoundingClientRect();
      sayAt({ clientX: r.left + 16, clientY: r.bottom - 12 }, text, true);
      setTimeout(unsay, 1600);
    };

    const kbMove = (row, down) => {
      const id = row.dataset.id;
      if (!opts.stacks || !id || id.startsWith('stack:')) return;
      const all = rowEls();
      const i = all.indexOf(row);
      const bandEl = (x) => x ? x.closest('.md-stackframe') : null;
      const mine = bandEl(row);
      const next = all[down ? i + 1 : i - 1];
      let join = null, leave = false;
      if (mine) {
        // out of the band from its edge: the first member going up (the
        // header is above it), the last going down
        leave = down ? bandEl(next) !== mine
          : !!next && next.matches('.md-stackhead') && bandEl(next) === mine;
      } else if (bandEl(next)) {
        join = bandEl(next).dataset.stack;
      }
      if (!join && !leave) return;
      const ids = [id];
      if (join && !rowDragOk(ids, join)) {
        sayAtRow(row, 'That grouping would tangle the flow');
        return;
      }
      kbKey = id;
      kbMoved = true;
      if (join) emit('stack_join', { blocks: ids, stack: join });
      else emit('stack_leave', { blocks: ids });
    };

    deckEl.addEventListener('keydown', (e) => {
      const row = e.target;
      if (!row.matches || !row.matches('.md-chip, .md-stackhead')) return;
      if (e.key === 'ArrowUp' || e.key === 'ArrowDown') {
        e.preventDefault();
        const down = e.key === 'ArrowDown';
        if (e.altKey) { kbMove(row, down); return; }
        const all = rowEls();
        const to = all[all.indexOf(row) + (down ? 1 : -1)];
        if (to) to.focus();
      } else if (e.key === 'Enter') {
        e.preventDefault();
        row.click();
      }
    });

    /* ---- lineage on hover ---- */

    // ONE delegated listener with hover intent, not a pair per row. Rows are
    // 28px with a 6px gap, so per-row enter/leave made a pointer travelling
    // down the list clear and re-apply the whole highlight through every gap
    // -- a strobe. The gap now reads as "still on the last row", the enter is
    // debounced so passing over a row does not light it, and leaving has a
    // grace period so a wander through the gutter does not drop the focus.
    // cold start is slower than a move between rows (once you are reading
    // lineage you want it to follow), and both debounce: a pointer sweeping
    // past a row never paints it, it only paints where you settle.
    // A dwell, not a brush. `FOCUS_MOVE_MS` used to be 60, so once engaged
    // the lineage re-struck about sixteen times a second while the pointer
    // travelled -- the outline answering a question on every row it passed.
    // Both thresholds now want the pointer to stop somewhere. Leaving stays
    // quick: releasing late would keep a stale chain lit after you have gone.
    const FOCUS_IN_MS = 320, FOCUS_MOVE_MS = 320, FOCUS_OUT_MS = 260;

    let focusId = null, focusTimer = null;

    // Whether hovering a row dims everything unrelated to it. On by default;
    // a host turns it off while it is using opacity for something else. The
    // board does exactly that when a view is focused for membership editing:
    // two fades that mean different things, both triggered by the pointer,
    // read as one broken one.
    let hoverFocus = true;

    const relatedTo = (railId) => {
      if (railId.startsWith('stack:')) {
        const s = stacks.find((x) => x.id === railId.slice(6));
        const keep = new Set(s ? s.blocks : []);
        (s ? s.blocks : []).forEach((m) => {
          upstream(m).forEach((x) => keep.add(x));
          downstream(m).forEach((x) => keep.add(x));
        });
        return keep;
      }
      const keep = upstream(railId);
      downstream(railId).forEach((x) => keep.add(x));
      keep.add(railId);
      return keep;
    };

    // The focus is two classes -- one on the outline, `md-rel` on the few rows
    // that stay lit -- so CSS owns the fade and a hover costs a handful of DOM
    // writes instead of one per row and one per edge (184 of them on the CDEX
    // board, every time the pointer crossed a gap).
    const paintFocus = (railId) => {
      if (focusId === railId) return;
      focusId = railId;
      if (!railId) {
        deckEl.classList.remove('md-focused');
        deckEl.querySelectorAll('.md-rel').forEach((e) => e.classList.remove('md-rel'));
        return;
      }
      const keep = relatedTo(railId);
      const lit = (id) => {
        if (!id) return false;
        if (id.startsWith('stack:')) {
          const s = stacks.find((x) => x.id === id.slice(6));
          return !!s && s.blocks.some((m) => keep.has(m));
        }
        return keep.has(id);
      };
      deckEl.querySelectorAll('.md-rel').forEach((e) => e.classList.remove('md-rel'));
      deckEl.querySelectorAll('.md-chip').forEach((c) => {
        if (lit(c.dataset.id)) c.classList.add('md-rel');
      });
      deckEl.querySelectorAll('.md-stackhead').forEach((h) => {
        const s = stacks.find((x) => x.id === h.dataset.stack);
        if (s && s.blocks.some((m) => keep.has(m))) h.classList.add('md-rel');
      });
      deckEl.querySelectorAll('.md-edge').forEach((p) => {
        if (lit(p.dataset.from) && lit(p.dataset.to)) p.classList.add('md-rel');
      });
      deckEl.classList.add('md-focused');
    };

    const wantFocus = (railId) => {
      clearTimeout(focusTimer);
      if (railId === focusId) return;
      focusTimer = setTimeout(
        () => paintFocus(railId),
        railId ? (focusId ? FOCUS_MOVE_MS : FOCUS_IN_MS) : FOCUS_OUT_MS
      );
    };

    deckEl.addEventListener('mouseover', (ev) => {
      if (dragging || !hoverFocus || (searchEl && searchEl.value.trim())) return;
      const row = ev.target.closest('.md-chip, .md-stackhead');
      if (!row) return;
      const id = row.dataset.id ||
        (row.dataset.stack ? 'stack:' + row.dataset.stack : null);
      if (id) wantFocus(id);
    });

    deckEl.addEventListener('mouseleave', () => wantFocus(null));

    const clearFocus = () => { clearTimeout(focusTimer); paintFocus(null); };

    /* ---- status badges ---- */

    const stackStatus = (stack) => {
      let worst = null, worstRank = 0;
      stack.blocks.forEach((m) => {
        const st = statuses.get(m);
        if (st && (STATUS_RANK[st.status] || 0) > worstRank) {
          worstRank = STATUS_RANK[st.status] || 0;
          worst = st;
        }
      });
      return worst;
    };

    const renderBadges = () => {
      // The dot is a status mark, not a control, so it has no tooltip; its
      // words go to assistive technology.
      const paint = (el, st) => {
        el.style.background = st ? st.color : 'transparent';
        if (st && st.label) {
          el.setAttribute('role', 'img');
          el.setAttribute('aria-label', st.label);
        } else {
          el.removeAttribute('role');
          el.removeAttribute('aria-label');
        }
        el.classList.toggle('on', !!st);
      };
      deckEl.querySelectorAll('.md-status[data-for]').forEach((el) => {
        paint(el, statuses.get(el.dataset.for));
      });
      deckEl.querySelectorAll('.md-status[data-stack]').forEach((el) => {
        const s = stacks.find((x) => x.id === el.dataset.stack);
        paint(el, s ? stackStatus(s) : null);
      });
    };

    /* ---- search ---- */

    const nameOf = (el) => {
      const n = el.querySelector('.md-name');
      if (!n) return '';
      return (n.value != null ? n.value : n.textContent) || '';
    };

    // Read the query and work out what the list is allowed to show. Sets
    // `searchKeep` / `searchHits` and rebuilds -- it does NOT touch the DOM
    // itself, because a narrowed list is a different set of rows and a
    // different rail, not the same one with classes on it.
    const applySearch = () => {
      if (!searchEl) return;
      const q = searchEl.value.trim().toLowerCase();
      clearEl.hidden = !searchEl.value;
      if (!q) {
        searchKeep = null;
        searchHits = null;
        hitsEl.textContent = '';
        render();
        return;
      }
      clearFocus();

      const match = (b) => (b.name || '').toLowerCase().includes(q);

      // Inside a focused stack the query scopes to the MEMBERS: hits only,
      // no ancestor pull-in -- the frame is the context, and the neighbour
      // doorways stay regardless (drawModel applies this set to members
      // alone). The counter's denominator says the same: n of the stack.
      const fs = focusedStack();
      if (fs) {
        const hits = fs.blocks.filter((id) => {
          const b = blockOf(id);
          return b && match(b);
        });
        searchHits = new Set(hits);
        searchKeep = new Set(hits);
        hitsEl.textContent = hits.length + ' / ' + fs.blocks.length;
        render();
        return;
      }

      const hits = blocks.filter(match).map((b) => b.id);
      // a group whose NAME matches brings its members in whole: the row you
      // matched is the group, and half a group is not one
      const named = stacks.filter((s) => (s.name || '').toLowerCase().includes(q));
      const seed = hits.concat(named.flatMap((s) => s.blocks));

      searchHits = new Set(seed);
      const keep = new Set(seed);
      seed.forEach((id) => upstream(id).forEach((p) => keep.add(p)));
      searchKeep = keep;

      hitsEl.textContent = searchHits.size + ' / ' + blocks.length;
      render();
    };

    // Which of the drawn rows the query actually matched, as opposed to the
    // ones that are here because something downstream needs them. Called from
    // `render()` once the rows exist.
    const paintHits = () => {
      // The context rows are here to make the picture whole, not to be read,
      // so the deck says which mode it is in and the stylesheet does the rest.
      deckEl.classList.toggle('md-searching', !!searchHits);
      if (!searchHits) return;
      deckEl.querySelectorAll('.md-chip[data-id]').forEach((c) => {
        const id = c.dataset.id;
        if (id.startsWith('stack:')) {
          const s = stacks.find((x) => x.id === id.slice(6));
          c.classList.toggle('md-hit',
            !!s && s.blocks.some((m) => searchHits.has(m)));
          return;
        }
        c.classList.toggle('md-hit', searchHits.has(id));
      });
      deckEl.querySelectorAll('.md-stackhead[data-stack]').forEach((h) => {
        const s = stacks.find((x) => x.id === h.dataset.stack);
        h.classList.toggle('md-hit', !!s && s.blocks.some((m) => searchHits.has(m)));
      });
    };

    if (searchEl) {
      searchEl.addEventListener('input', applySearch);
      // Esc peels one layer at a time: the query first, then the stack
      // focus, then the input's own keyboard focus -- so backing all the way
      // out is the same key pressed until there is nothing left to clear.
      searchEl.addEventListener('keydown', (e) => {
        if (e.key !== 'Escape') return;
        if (searchEl.value) { searchEl.value = ''; applySearch(); return; }
        if (stackFocus) { clearStackFocus(); return; }
        searchEl.blur();
      });
    }

    // No document-level Esc: a global key hook was the one piece of this
    // feature with app-wide reach, and the gain was minimal next to the
    // exits that need no reach at all -- the pill's ×, the header's
    // double-click toggle, and Esc inside the search box (above), which
    // peels query first, then focus. If a page-wide Esc ever comes back,
    // it must be registered in the CAPTURE phase: a rename's own Esc
    // handler blurs the field, and blurring flips `isContentEditable` off
    // before the event bubbles, so a bubble-phase guard misreads the field
    // as plain and steals the key mid-rename.

    /* ---- selection ---- */

    // A stack row carries `.sel` when the whole group is selected, which is
    // the only way it can be: `selection` holds block ids, and a collapsed
    // stack has no member rows to mark.
    const wholeStackSelected = (s) =>
      s.blocks.length > 0 && s.blocks.every((id) => selection.has(id));

    // One mark per selected thing. When a whole stack is selected the GROUP
    // is what is selected, so the ring goes on its row and comes off the
    // members -- otherwise a two-block stack drew three rings for one
    // selection, on top of the frame the stack already has.
    const paintStackSel = () => {
      deckEl.querySelectorAll('.md-stackchip').forEach((el) => {
        const s = stacks.find((x) => 'stack:' + x.id === el.dataset.id);
        el.classList.toggle('sel', !!s && wholeStackSelected(s));
      });
      deckEl.querySelectorAll('.md-stackframe').forEach((el) => {
        const s = stacks.find((x) => x.id === el.dataset.stack);
        el.classList.toggle('sel', !!s && wholeStackSelected(s));
      });
      deckEl.querySelectorAll('.md-stackhead').forEach((el) => {
        const s = stacks.find((x) => x.id === el.dataset.stack);
        const whole = !!s && wholeStackSelected(s);
        // the header is the stack's row, so it carries the selected look
        el.classList.toggle('sel', whole);
        if (whole) {
          s.blocks.forEach((id) => {
            const row = deckEl.querySelector(
              '.md-chip[data-id="' + CSS.escape(id) + '"]'
            );
            if (row) row.classList.remove('sel');
          });
        }
      });
    };

    const selectStack = (stack, additive) => {
      const whole = wholeStackSelected(stack);
      if (!additive) {
        selection.clear();
        deckEl.querySelectorAll('.md-chip.sel, .md-stackhead.sel').forEach(
          (x) => x.classList.remove('sel')
        );
      }
      stack.blocks.forEach((id) => {
        if (whole && additive) selection.delete(id); else selection.add(id);
      });
      deckEl.querySelectorAll('.md-chip[data-id]').forEach((el) => {
        el.classList.toggle('sel', selection.has(el.dataset.id));
      });
      selAnchor = stack.blocks[stack.blocks.length - 1] || null;
      updateBar();
    };

    const updateBar = () => {
      // One selected row has no look of its own: it is the row just
      // clicked, which the dock then shows as the current one. Two or more
      // are a selection with somewhere to go, and are filled.
      deckEl.classList.toggle('md-multi', selection.size >= 2);
      if (!barEl) return;
      paintStackSel();
      barEl.classList.remove('err');

      const stacked = [...selection].filter((id) => stackOf(id));
      const whole = stacked.length === selection.size && selection.size > 0
        ? stackOf([...selection][0]) : null;
      const allOne = whole && wholeStackSelected(whole) &&
        whole.blocks.length === selection.size;

      // The offer stands for stacked rows too: grouping them MOVES them out
      // of the stack they are in, the same move as dragging them out of the
      // frame and grouping them.

      // A selected stack needs no bar: the frame's ring already says so, and
      // the only thing the bar could offer -- grouping -- is exactly what
      // cannot apply. The bar is for a selection that has somewhere to go.
      barEl.classList.toggle('on', selection.size >= 2 && !allOne);

      selcountEl.textContent = selection.size + ' ' + opts.stackUnit;
    };

    /* ---- links: a thing you point at (design system, "Links in the outline")
     *
     * A link under the pointer thickens in the accent colour and its tooltip
     * names it ("Two species → Sepal ratio", "into data" muted). Nothing is
     * drawn on it. A click opens its menu: Insert a block here, Rename input
     * or Move to input (whichever the target allows), then Remove link after
     * a divider. Every row but Remove comes from an adapter hook, so a host
     * without it gets a menu of one.
     */

    // The last drawn edges and their straight runs, for a click to find the
    // siblings lying on the same stretch of bus.
    let edgeGeo = { edges: [], vRun: new Map() };
    // The edge whose menu is open stays lit.
    let heldPath = null;

    const linksBehind = (railFrom, railTo) =>
      links.filter((l) => railIdOf(l.from) === railFrom && railIdOf(l.to) === railTo);

    // A row of the rail by id: a block's name, or a folded stack's.
    const railName = (id) => {
      if (String(id).startsWith('stack:')) {
        const s = stacks.find((x) => x.id === id.slice(6));
        return s ? s.name : id;
      }
      const b = blockOf(id);
      return b ? b.name : id;
    };
    const linkTitle = (l) => railName(l.from) + ' → ' + railName(l.to);

    // The links under a click on an edge's band (x, y in rail coordinates):
    // its own, then the siblings out of the same producer whose straight run
    // passes the same point, nearest first.
    const linksAt = (e, back, x, y) => {
      const own = linksBehind(e.from, e.to);
      if (back) return own;
      const on = (o) => {
        const v = edgeGeo.vRun.get(o);
        return v && Math.abs(v.x - x) <= 4 && y > v.y0 + 1 && y < v.y1 - 1;
      };
      const more = edgeGeo.edges
        .filter((o) => o !== e && o.from === e.from && on(o))
        .sort((a, b) => edgeGeo.vRun.get(a).y1 - edgeGeo.vRun.get(b).y1)
        .flatMap((o) => linksBehind(o.from, o.to));
      return own.concat(more);
    };

    // The tooltip naming a link is the design system's light card, shown at
    // the pointer after the tooltip's 300ms rest. Blockr.tooltip would place
    // it above the path's bounding box, which on a link between two rows is
    // on top of the row the link comes from.
    let edgeTipTimer = null;
    const edgeTip = (e, ev) => {
      const behind = linksBehind(e.from, e.to);
      if (!behind.length) return;
      const inp = linkInput(behind[0]);
      sayAt(ev, linkTitle(behind[0]), false,
        (inp ? 'into ' + inp : '') +
        (behind.length > 1 ? (inp ? ', ' : '') + (behind.length - 1) + ' more' : ''));
    };
    const edgeTipOff = () => {
      clearTimeout(edgeTipTimer);
      edgeTipTimer = null;
      if (!dragging) unsay();
    };

    const wireEdge = (e, path, hit, back) => {
      hit.addEventListener('mouseenter', (ev) => {
        if (dragging) return;
        path.classList.add('md-hot');
        clearTimeout(edgeTipTimer);
        edgeTipTimer = setTimeout(() => edgeTip(e, ev), 300);
      });
      hit.addEventListener('mousemove', (ev) => {
        if (dragging) return;
        if (dragTipEl) edgeTip(e, ev);
        else {
          clearTimeout(edgeTipTimer);
          edgeTipTimer = setTimeout(() => edgeTip(e, ev), 300);
        }
      });
      hit.addEventListener('mouseleave', () => {
        if (heldPath !== path) path.classList.remove('md-hot');
        edgeTipOff();
      });
      hit.setAttribute('aria-label', linksBehind(e.from, e.to).map(linkTitle).join(', '));
      hit.addEventListener('click', (ev) => {
        ev.stopPropagation();
        edgeTipOff();
        const at = { el: null, x: ev.clientX, y: ev.clientY };
        const box = hit.ownerSVGElement.getBoundingClientRect();
        const all = linksAt(e, back, ev.clientX - box.left, ev.clientY - box.top);
        const hold = () => {
          heldPath = path;
          path.classList.add('md-hot');
        };
        const release = () => {
          if (heldPath === path) heldPath = null;
          path.classList.remove('md-hot');
        };
        if (all.length === 1) {
          hold();
          openLinkMenu(all[0], at, false, release);
          return;
        }
        if (!all.length) return;
        hold();
        openAt(at, {
          caption: all.length + ' links run here',
          items: all.map((l) => ({
            label: linkTitle(l),
            meta: linkInput(l) || undefined,
            onSelect: () => { hold(); openLinkMenu(l, at, false, release); }
          }))
        }, release);
      });
    };

    /* ---- menus ------------------------------------------------------------
     *
     * The design system's action menu (`Blockr.menu`), placed by
     * `Blockr.place`: under a row, or at the point a link was clicked or a
     * drag released. A pick in one menu can open the next (a link in the
     * connections list opens that link's menu) at the same place.
     */

    const slotLabel = (input) => input === '' ? 'new input' : input;

    // Open a menu and remember how to close it, so a render (which rebuilds
    // the rows it hangs from) takes it away rather than leaving it pointing
    // at a detached row.
    const openMenu = (anchor, config, temp, onClose) => {
      if (closePicker) closePicker();
      const B = ui();
      if (!B || !B.menu) { if (temp) temp.remove(); return null; }
      let m = null;
      m = B.menu(anchor, Object.assign({}, config, {
        onClose: () => {
          if (temp) temp.remove();
          if (m && closePicker === m.close) closePicker = null;
          if (onClose) onClose();
        }
      }));
      closePicker = m.close;
      return m;
    };

    // Where a menu goes: `{el}` hangs it under an element while that is on
    // the page, else it opens at the point `{x, y}` (viewport) the element
    // was at, which is what a chain of menus needs once the first is gone.
    const atOf = (el) => {
      const r = el.getBoundingClientRect();
      return { el: el, x: r.left, y: r.bottom };
    };
    const anchorOf = (at) => {
      if (at.el && at.el.isConnected) return { el: at.el, temp: null };
      const a = pointAnchor(at.x, at.y);
      return { el: a, temp: a };
    };
    const openAt = (at, config, onClose) => {
      const a = anchorOf(at);
      return openMenu(a.el, config, a.temp, onClose);
    };

    // Blocks in the order the list draws them, the ones not drawn (inside a
    // folded stack, outside a search) after them in board order.
    const inRowOrder = (list) => list.slice().sort((a, b) =>
      (lastPos.get('n:' + a.id) ?? 1e9) - (lastPos.get('n:' + b.id) ?? 1e9));

    // A block as a menu row: the adapter's mark and meta (the block type).
    const blockItem = (b, onSelect) => Object.assign(
      { label: b.name || b.id, keywords: b.id }, nodeItem(b) || {},
      { onSelect: onSelect });

    /* A link's menu. The head names it (from → to, and the input). */
    const openLinkMenu = (l, at, renaming, onClose) => {
      const edit = linkEdit(l) || null;
      const items = [];
      if (linkInsert) {
        items.push({
          label: 'Insert a block here', icon: 'plus',
          onSelect: () => linkInsert(l)
        });
      }
      // Rename and Move open the next surface at the same place, so the
      // menu closing on the way does not end the hold on the link: a pick
      // closes the menu first and runs the row after, so the end of the
      // hold waits a tick to see whether a row chained on.
      let chained = false;
      const chain = (fn) => () => { chained = true; fn(); };
      if (edit && edit.rename) {
        items.push({
          label: 'Rename input',
          onSelect: chain(() => openLinkMenu(l, at, true, onClose))
        });
      } else if (edit && edit.move && edit.move.length) {
        items.push({
          label: 'Move to input',
          onSelect: chain(() => openMoveInput(l, edit.move, at, onClose))
        });
      }
      if (items.length) items.push({ divider: true });
      items.push({
        label: 'Remove link', icon: 'trash', danger: true,
        onSelect: () => emit('link_rm', { ids: [l.id] })
      });
      const inp = linkInput(l);
      const m = openAt(at, {
        head: { title: linkTitle(l), text: inp ? 'into input ' + inp : undefined },
        items: items
      }, () => setTimeout(() => { if (!chained && onClose) onClose(); }, 0));
      if (renaming && m && edit && edit.rename) {
        renameField(m, l, edit, chain(() => openLinkMenu(l, at, false, onClose)));
      }
      return m;
    };

    /* Rename input: the row turns into a field in place, the name selected.
     * Enter commits, Escape restores the row, a click elsewhere commits. An
     * empty name, or one another input of the block has, is refused in
     * place: the danger edge and one line under the field. */
    const renameField = (m, l, edit, restore) => {
      const row = [...m.el.querySelectorAll('.blockr-menu__item')]
        .find((r) => r.textContent.trim() === 'Rename input');
      if (!row) return;
      const wrap = document.createElement('div');
      wrap.className = 'md-menu-field';
      const inp = document.createElement('input');
      inp.type = 'text';
      inp.className = 'md-menu-field__input';
      inp.value = l.input;
      inp.spellcheck = false;
      inp.autocomplete = 'off';
      inp.setAttribute('aria-label', 'Input name');
      const err = document.createElement('div');
      err.className = 'md-menu-field__error';
      err.setAttribute('role', 'alert');
      err.hidden = true;
      wrap.appendChild(inp);
      wrap.appendChild(err);
      row.replaceWith(wrap);
      inp.focus({ preventScroll: true });
      inp.select();

      let done = false;
      const verdict = () => {
        const v = inp.value.trim();
        return v === l.input ? '' : inputNameError(v, edit.taken);
      };
      const commit = () => {
        const v = inp.value.trim();
        if (v && v !== l.input && !verdict()) emit('link_mod', { id: l.id, input: v });
      };
      inp.addEventListener('input', () => {
        wrap.classList.remove('md-bad');
        err.hidden = true;
      });
      // The menu's own keys (arrows, Enter picks, Space, Escape closes) are
      // the field's while it has the focus.
      inp.addEventListener('keydown', (e) => {
        e.stopPropagation();
        if (e.key === 'Enter') {
          e.preventDefault();
          const bad = verdict();
          if (bad) {
            wrap.classList.add('md-bad');
            err.textContent = bad;
            err.hidden = false;
            return;
          }
          done = true;
          commit();
          m.close();
        } else if (e.key === 'Escape') {
          e.preventDefault();
          done = true;
          restore();
        } else if (e.key === 'Tab') {
          m.close();
        }
      });
      // a click elsewhere closes the menu, and commits a name that passes
      const panel = m.el;
      const obs = new MutationObserver(() => {
        if (panel.isConnected) return;
        obs.disconnect();
        if (!done) { done = true; commit(); }
      });
      obs.observe(document.body, { childList: true });
    };

    /* Move to input: a Select.menu of the target's other free named inputs,
     * the current one selected. */
    const openMoveInput = (l, free, at, onClose) => {
      const B = ui();
      if (!B || !B.Select || !B.Select.menu) { if (onClose) onClose(); return; }
      if (closePicker) closePicker();
      const a = anchorOf(at);
      const h = B.Select.menu(a.el, {
        options: [l.input].concat(free.filter((s) => s !== l.input)),
        selected: l.input,
        title: 'Move to input',
        search: false,
        onChange: (v) => {
          if (v && v !== l.input) emit('link_mod', { id: l.id, input: v });
        },
        onClose: () => {
          if (a.temp) a.temp.remove();
          if (closePicker === h.close) closePicker = null;
          if (onClose) onClose();
        }
      });
      closePicker = h.close;
    };

    /* A click on a row's dot: the block's connections, inputs then outputs,
     * each with the input it goes into as meta text. A click on one opens
     * that link's menu, under the same row. */
    const openConn = (el, railId) => {
      const ins = links.filter((l) => railIdOf(l.to) === railId && railIdOf(l.from) !== railId);
      const outs = links.filter((l) => railIdOf(l.from) === railId && railIdOf(l.to) !== railId);
      const items = [];
      const at = atOf(el);
      const section = (title, list, other) => {
        if (!list.length) return;
        items.push({ title: title });
        list.forEach((l) => {
          items.push({
            label: railName(railIdOf(other(l))),
            meta: linkInput(l) || undefined,
            onSelect: () => openLinkMenu(l, at)
          });
        });
      };
      section('Inputs', ins, (l) => l.from);
      section('Outputs', outs, (l) => l.to);
      if (!items.length) {
        items.push({
          label: 'No connections yet', disabled: true,
          reason: 'Drag this row’s dot onto another row to connect them'
        });
      }
      openAt(at, {
        caption: 'Connections of ' + railName(railId),
        items: items
      });
    };

    /* A click on an input marker: the board's blocks that can feed that
     * input ('' for a new input on a block that takes any number). */
    const openFeeders = (to, input, at) => {
      const cands = inRowOrder(feedersFor(blocks, links, to, input, slotsFor));
      const items = cands.length
        ? cands.map((b) => blockItem(b, () =>
          emit('link_add', { from: b.id, to: to.id, input: input })))
        : [{
          label: 'No block can feed it', disabled: true,
          reason: 'Every other block reads from ' + to.name
        }];
      openAt(at, {
        caption: input ? 'Connect a block to ' + input : 'Connect a block',
        filter: cands.length > 8 ? 'Search blocks on the board' : false,
        minWidth: 260,
        items: items
      });
    };

    /* "Connect to…" in a row menu: this block's output into another block,
     * then the input when it has several free. */
    const connectFrom = (fromId, at) => {
      const from = blockOf(fromId);
      if (!from) return;
      const cands = inRowOrder(targetsFor(blocks, links, from, slotsFor));
      const pick = (b) => {
        const free = slotsFor(from, b);
        if (free.length === 1) {
          emit('link_add', { from: fromId, to: b.id, input: free[0] });
          return;
        }
        openAt(at, {
          caption: slotPrompt(from, b),
          items: free.map((slot) => ({
            label: slotLabel(slot),
            onSelect: () => emit('link_add', { from: fromId, to: b.id, input: slot })
          }))
        });
      };
      openAt(at, {
        caption: 'Connect ' + from.name + ' to',
        filter: cands.length > 8 ? 'Search blocks on the board' : false,
        minWidth: 260,
        items: cands.length ? cands.map((b) => blockItem(b, () => pick(b)))
          : [{
            label: 'No block can take it', disabled: true,
            reason: 'Every other block has its inputs taken or feeds ' + from.name
          }]
      });
    };

    // The input markers after a row's name: an 8px open circle per free
    // named input, "∞" for a block that takes any number. Each is a 16px
    // target; a click lists the blocks that can feed it.
    const markersEl = (b) => {
      if (!inputMarkers) return null;
      const mk = inputMarkers(b);
      if (!mk || (!(mk.inputs || []).length && !mk.variadic)) return null;
      const wrap = document.createElement('span');
      wrap.className = 'md-pips';
      const one = (input, cls, text, label, lines) => {
        const p = document.createElement('button');
        p.type = 'button';
        p.className = 'md-pip' + (cls ? ' ' + cls : '');
        if (text) p.textContent = text;
        p.setAttribute('aria-label', label);
        tip(p, lines);
        p.addEventListener('click', (ev) => {
          ev.stopPropagation();
          openFeeders(b, input, atOf(p));
        });
        wrap.appendChild(p);
      };
      (mk.inputs || []).forEach((s) => one(s, '', '', s + ': not connected',
        { name: s, label: 'not connected' }));
      if (mk.variadic) {
        one('', 'md-pip--inf', '∞', 'Takes any number of inputs',
          'Takes any number of inputs');
      }
      return wrap;
    };

    /* ---- slot picker: which slot the new edge occupies ---- */

    const openSlotPicker = (x, y, from, to, free, onPick) => {
      const box = deckEl.getBoundingClientRect();
      const a = pointAnchor(box.left + x, box.top + y);
      openMenu(a, {
        caption: slotPrompt(from, to),
        items: free.map((slot) => ({
          label: slotLabel(slot),
          onSelect: () => onPick(slot)
        }))
      }, a);
    };

    /* ---- what a drag will do, at the pointer ----
     *
     * A drag with no verdict is a drag you have to release to find out
     * about. The verdict is the design system's light tooltip card, fixed to
     * the viewport (it follows the pointer, not the outline), with the
     * danger text when the release would be refused.
     */
    let dragTipEl = null;
    // `muted`: a second part in text-muted, as a tooltip's meta text
    const sayAt = (e, text, bad, muted) => {
      if (!dragTipEl) {
        dragTipEl = document.createElement('div');
        dragTipEl.className = 'blockr-tooltip md-droptip';
        dragTipEl.setAttribute('role', 'status');
        document.body.appendChild(dragTipEl);
      }
      dragTipEl.textContent = text;
      if (muted) {
        const m = document.createElement('span');
        m.className = 'blockr-tooltip__meta';
        m.textContent = muted;
        dragTipEl.append(' ', m);
      }
      dragTipEl.classList.toggle('no', !!bad);
      dragTipEl.style.left = (e.clientX + 14) + 'px';
      dragTipEl.style.top = (e.clientY + 16) + 'px';
    };
    const unsay = () => {
      if (dragTipEl) { dragTipEl.remove(); dragTipEl = null; }
    };

    /* ---- move a row into (or out of) a stack ----
     *
     * Membership used to be editable only by dissolving the group and
     * rebuilding it from a fresh selection -- on a twelve-block stack, twelve
     * ⌘-clicks to add a thirteenth. The frames are on screen and they ARE
     * what membership looks like, so dragging a row into one, or out of every
     * one, is the gesture. Same shape as the port drag: a threshold, live
     * verdict, drop feedback on the target.
     */

    const rowDragOk = (ids, stackId) => {
      const trial = stacks.map((s) => Object.assign({}, s, {
        blocks: s.id === stackId
          ? s.blocks.concat(ids.filter((id) => !s.blocks.includes(id)))
          : s.blocks.filter((id) => !ids.includes(id))
      })).filter((s) => s.blocks.length);
      // A board that is ALREADY tangled must not lock out its own repair:
      // the question is whether this move makes it worse, not whether the
      // result is perfect. Dropping the missing block into the stack it
      // belongs to is the fix, and it has to be allowed to happen.
      return !G.superOrder(model(), trial).hadCycle ||
        G.superOrder(model(), stacks).hadCycle;
    };

    const startRowDrag = (ev, b, el) => {
      // Dragging one row of a selection moves the selection; dragging a row
      // outside it moves that row, and leaves the selection alone.
      const ids = selection.has(b.id) && selection.size > 1
        ? [...selection] : [b.id];
      const home = stackOf(b.id);
      const label = ids.length > 1 ? ids.length + ' blocks' : b.name;
      const x0 = ev.clientX, y0 = ev.clientY;
      let moved = false, frames = [], deckBox = null, drop = null;
      // where the row lands, per target: the drop line is worked out once
      // for each target the pointer visits, not on every mousemove
      const lineAt = new Map();
      let lineEl = null;

      const unpaint = () => {
        deckEl.querySelectorAll('.md-stackframe').forEach(
          (f) => f.classList.remove('drop-ok', 'drop-no', 'drop-out')
        );
        if (lineEl) lineEl.hidden = true;
        unsay();
      };

      // The 2px accent line where the row will land (design system, "Block
      // lists"). The outline's order comes from the links, so where that is
      // is asked of the layout: the list as it would be drawn after the
      // move, and the row the moved one would come after.
      const landing = (joinId) => {
        const key = joinId || '';
        if (lineAt.has(key)) return lineAt.get(key);
        let at = null;
        const trial = stacks.map((s) => Object.assign({}, s, {
          blocks: s.id === joinId
            ? s.blocks.concat(ids.filter((id) => !s.blocks.includes(id)))
            : s.blocks.filter((id) => !ids.includes(id))
        })).filter((s) => s.blocks.length);
        const rowsT = G.displayRows(Object.assign(model(), { stacks: trial }));
        const i = rowsT.findIndex((r) => r.t === 'node' && ids.includes(r.node.id));
        if (i >= 0 && !searchKeep && !stackFocus) {
          const bandT = rowsT[i].inStack ? rowsT[i].inStack.id : null;
          let p = i - 1;
          while (p >= 0 && rowsT[p].t === 'node' && ids.includes(rowsT[p].node.id)) p--;
          const q = (s) => deckEl.querySelector(s);
          const bandEl = (sid) => q('.md-stackframe[data-stack="' + CSS.escape(sid) + '"]');
          let ref = null, below = false, inside = null;
          if (p < 0) {
            ref = q('.md-rows > *');
          } else {
            const r = rowsT[p];
            below = true;
            if (r.t === 'header') {
              ref = q('.md-stackhead[data-stack="' + CSS.escape(r.stack.id) + '"]');
              inside = bandEl(r.stack.id);
            } else if (r.t === 'stack') {
              ref = bandEl(r.stack.id);
            } else if (r.inStack && r.inStack.id === bandT) {
              ref = q('.md-chip[data-id="' + CSS.escape(r.node.id) + '"]');
              inside = bandEl(bandT);
            } else if (r.inStack) {
              ref = bandEl(r.inStack.id);
            } else {
              ref = q('.md-chip[data-id="' + CSS.escape(r.node.id) + '"]');
            }
          }
          if (ref) at = { ref: ref, below: below, inside: inside };
        }
        lineAt.set(key, at);
        return at;
      };

      const showLine = (at) => {
        if (!at || !at.ref.isConnected) return;
        const dBox = deckEl.getBoundingClientRect();
        const rBox = at.ref.getBoundingClientRect();
        const wBox = (at.inside || deckEl.querySelector('.md-rows'))
          .getBoundingClientRect();
        const inset = at.inside ? STACK_PAD : 0;
        // on the boundary: under the row it follows, or halfway into the air
        // after a band
        const air = at.below && at.ref.classList.contains('md-stackframe')
          ? STACK_GAP / 2 : 0;
        const y = (at.below ? rBox.bottom + air : rBox.top) - dBox.top;
        if (!lineEl) {
          lineEl = document.createElement('div');
          lineEl.className = 'md-dropline';
          deckEl.appendChild(lineEl);
        }
        lineEl.hidden = false;
        lineEl.style.top = (y - 1) + 'px';
        lineEl.style.left = (wBox.left - dBox.left + inset) + 'px';
        lineEl.style.width = (wBox.width - 2 * inset) + 'px';
      };

      const say = sayAt;

      const onMove = (e) => {
        if (!moved) {
          if (Math.abs(e.clientX - x0) + Math.abs(e.clientY - y0) < 6) return;
          moved = true;
          dragging = true;
          rootEl.classList.add('md-dragging');
          clearFocus();
          if (closePicker) closePicker();
          el.classList.add('md-moving');
          deckBox = deckEl.getBoundingClientRect();
          frames = [...deckEl.querySelectorAll('.md-stackframe')].map((f) => ({
            el: f, id: f.dataset.stack, box: f.getBoundingClientRect()
          }));
        }
        e.preventDefault();
        unpaint();
        drop = null;

        const inBox = (r) => e.clientX >= r.left && e.clientX <= r.right &&
          e.clientY >= r.top && e.clientY <= r.bottom;
        const over = frames.find((f) => inBox(f.box));

        if (over && (!home || over.id !== home.id)) {
          const target = stacks.find((s) => s.id === over.id) || {};
          if (rowDragOk(ids, over.id)) {
            drop = { join: over.id };
            over.el.classList.add('drop-ok');
            showLine(landing(over.id));
            say(e, 'Add ' + label + ' to ' + target.name);
          } else {
            over.el.classList.add('drop-no');
            say(e, 'That grouping would tangle the flow', true);
          }
          return;
        }
        // Only within the outline: a release over the view list or off the panel
        // is a cancelled drag, not an instruction to break the group up.
        if (!over && home && deckBox && inBox(deckBox)) {
          drop = { leave: true };
          const f = frames.find((x) => x.id === home.id);
          if (f) f.el.classList.add('drop-out');
          showLine(landing(null));
          say(e, 'Take ' + label + ' out of ' + home.name);
        }
      };

      const onUp = () => {
        document.removeEventListener('mousemove', onMove);
        document.removeEventListener('mouseup', onUp);
        unpaint();
        if (lineEl) { lineEl.remove(); lineEl = null; }
        rootEl.classList.remove('md-dragging');
        el.classList.remove('md-moving');
        if (!moved) {
          return;                 // never crossed the threshold: a plain click
        }
        dragging = false;
        // The click that follows this mouseup lands on whatever the row was
        // dropped on, and would select it.
        const kill = (e) => { e.stopPropagation(); e.preventDefault(); };
        document.addEventListener('click', kill, true);
        setTimeout(() => document.removeEventListener('click', kill, true), 0);

        if (drop && drop.join) {
          emit('stack_join', { blocks: ids, stack: drop.join });
        } else if (drop && drop.leave) {
          emit('stack_leave', { blocks: ids });
        }
        if (pendingRender) render();
      };

      document.addEventListener('mousemove', onMove);
      document.addEventListener('mouseup', onUp);
    };

    /* ---- port drag ---- */

    // A menu opened on mouseup would be closed by the click that follows it
    // in the same gesture (a click outside a menu closes it). So it opens
    // once that click has been dispatched, or shortly after the mouseup if
    // none comes (a drag released over another element fires none).
    const afterClick = (fn) => {
      let done = false;
      const run = () => {
        if (done) return;
        done = true;
        document.removeEventListener('click', onClick, true);
        fn();
      };
      const onClick = () => setTimeout(run, 0);
      document.addEventListener('click', onClick, true);
      setTimeout(run, 80);
    };

    const startDrag = (e, railId, port, onTap) => {
      e.preventDefault();
      e.stopPropagation();
      if (closePicker) closePicker();
      clearFocus();
      const wire = deckEl.querySelector('svg.md-wire');
      const deckBox = deckEl.getBoundingClientRect();
      const p0 = port.getBoundingClientRect();
      const x0 = p0.left + p0.width / 2 - deckBox.left;
      const y0 = p0.top + p0.height / 2 - deckBox.top;
      let moved = false, target = null, ghost = null;
      dragging = true;

      const path = svgEl('path');
      path.setAttribute('fill', 'none');
      path.setAttribute('class', 'md-wire-path');
      path.setAttribute('stroke-width', '1.6');
      path.setAttribute('stroke-dasharray', '4 3');
      wire.appendChild(path);

      // Two drop zones, one per mental model: in the CHIP column the whole
      // row band is a target (forgiving); in the RAIL gutter only the dots
      // themselves are — the line and the blank gutter read as canvas.
      const bandRows = geo.rows;
      const rowRail = bandRows.map((r) => r.t === 'node' ? railIdOf(r.node.id)
        : r.t === 'stack' ? 'stack:' + r.stack.id : null);
      const listH = geo.height;
      const railW = parseInt(deckEl.querySelector('.md-rows').style.marginLeft, 10) || 40;
      const dotPos = [...deckEl.querySelectorAll('svg.md-rail circle.md-dot')].map((c) => ({
        id: c.dataset.rail,
        x: +c.getAttribute('cx'), y: +c.getAttribute('cy')
      }));

      const bandAt = (x, y) => {
        if (x >= railW - 8 && x <= deckEl.clientWidth + 8) {
          const idx = rowAtY(y);
          if (idx < 0) return null;
          let band = rowRail[idx];
          if (!band && idx + 1 < rowRail.length) band = rowRail[idx + 1];
          return band === railId ? 'self' : band;
        }
        if (x >= -8 && x < railW - 8) {
          const near = dotPos.find((d) =>
            (d.x - x) * (d.x - x) + (d.y - y) * (d.y - y) <= 15 * 15);
          if (!near) return null;
          return near.id === railId ? 'self' : near.id;
        }
        return null;
      };

      const onMove = (ev) => {
        const x = ev.clientX - deckBox.left, y = ev.clientY - deckBox.top;
        if (Math.abs(x - x0) + Math.abs(y - y0) > 6) {
          moved = true;
          rootEl.classList.add('md-dragging');
        }
        const dx = x - x0, dy = y - y0;
        const bend = Math.min(36, Math.abs(dx) * 0.5) * (dx < 0 ? -1 : 1);
        path.setAttribute('d', 'M' + x0 + ',' + y0 +
          ' C' + (x0 + bend) + ',' + (y0 + dy * 0.2) +
          ' ' + (x - bend) + ',' + (y - dy * 0.2) + ' ' + x + ',' + y);
        deckEl.querySelectorAll('.md-chip').forEach((c) =>
          c.classList.remove('drop-ok', 'drop-no'));
        if (ghost) { ghost.remove(); ghost = null; }
        target = null;
        const band = bandAt(x, y);
        if (band === 'self') unsay();
        if (band && band !== 'self') {
          const verdict = dropVerdict(railId, band);
          const chipEl = deckEl.querySelector('.md-chip[data-id="' + CSS.escape(band) + '"]');
          if (chipEl) {
            chipEl.classList.add(verdict === 'ok' ? 'drop-ok' : 'drop-no');
          }
          // why a drop would be refused, said at the pointer
          if (verdict === 'full') sayAt(ev, 'All inputs are taken', true);
          else if (verdict === 'cycle') sayAt(ev, 'That would make a cycle', true);
          else if (verdict === 'ok') {
            // what the release does: the block it connects to, and the
            // input when there is one to name
            const tb = targetBlock(railId, band);
            const free = tb ? slotsFor(blockOf(sinkOf(railId)), tb) : [];
            sayAt(ev, 'Connect to ' + (tb ? tb.name : railName(band)), false,
              free.length === 1 && free[0] !== '' ? 'into ' + free[0] : '');
          } else unsay();
          if (verdict === 'ok') target = band;
        } else if (!band && moved) {
          unsay();
          // Where the appended block would land, drawn WHERE THE POINTER IS.
          // It used to be pinned at `listH`, the end of the list: correct as
          // a statement about the topological order, invisible on any board
          // long enough to scroll. Releasing over the gutter is a legal
          // append at every scroll position, and it looked like a dead zone
          // purely because its only confirmation was rendered off-screen.
          ghost = document.createElement('div');
          ghost.className = 'md-ghost';
          const rows = deckEl.querySelector('.md-rows');
          ghost.style.left = rows.style.marginLeft;
          ghost.style.right = '0';
          ghost.style.top = Math.max(0, Math.min(y - ROW_H / 2, listH)) + 'px';
          deckEl.appendChild(ghost);
        }
        // The gutter is the drop zone that is always reachable, so say so for
        // as long as the drag is live rather than leaving it to be discovered.
        deckEl.classList.toggle('md-dropzone', !band && moved);
      };

      const onUp = (ev) => {
        document.removeEventListener('mousemove', onMove);
        document.removeEventListener('mouseup', onUp);
        dragging = false;
        rootEl.classList.remove('md-dragging');
        unsay();
        path.remove();
        if (ghost) ghost.remove();
        deckEl.classList.remove('md-dropzone');
        deckEl.querySelectorAll('.md-chip').forEach((c) =>
          c.classList.remove('drop-ok', 'drop-no'));
        const x = ev.clientX - deckBox.left, y = ev.clientY - deckBox.top;
        if (target) {
          if (pendingRender) render();
          afterClick(() => doConnect(railId, target, x, Math.min(y, listH)));
          return;
        }
        if (!moved) {
          if (pendingRender) render();
          if (onTap) afterClick(onTap);
          return;
        }
        // released on the canvas: append a new node wired from the source
        // (the board opens its block browser, a process its kind picker). A
        // release on a row that was no valid target (own row, cycle, full)
        // is a strict no-op.
        if (bandAt(x, y) !== null) {
          if (pendingRender) render();
          return;
        }
        // x/y so an adapter that answers with a picker can open it where the
        // drag was released
        if (pendingRender) render();
        const from = sinkOf(railId);
        afterClick(() => emit('block_append', { from: from, x, y }));
      };

      document.addEventListener('mousemove', onMove);
      document.addEventListener('mouseup', onUp);
    };

    /* ---- inbound ---- */

    const asArr = (x) => x == null ? [] : (Array.isArray(x) ? x : [x]);

    const setData = (msg) => {
      blocks = asArr(msg.blocks);
      links = asArr(msg.links).map((l) => Object.assign({}, l, {
        input: l.input == null ? '' : l.input
      }));
      stacks = asArr(msg.stacks).map((s) => Object.assign({}, s, {
        blocks: asArr(s.blocks)
      }));
      const ids = new Set(blocks.map((b) => b.id));
      selection = new Set([...selection].filter((id) => ids.has(id)));
      [...collapsed].forEach((id) => {
        if (!stacks.some((s) => s.id === id)) collapsed.delete(id);
      });
      [...statuses.keys()].forEach((id) => {
        if (!ids.has(id)) statuses.delete(id);
      });
      // A focus on a stack that no longer exists (dissolved, or its last
      // member removed elsewhere) falls back to the whole board -- silently
      // holding a dead filter would show everything while claiming one stack.
      if (stackFocus && !stacks.some((s) => s.id === stackFocus)) {
        stackFocus = null;
        resetSearch();
        emit('stack_focus', { id: null });
      }
      render();
    };

    const setBadge = (msg) => {
      if (msg.color) {
        statuses.set(msg.id, {
          color: msg.color, label: msg.label || '', status: msg.status || ''
        });
      } else {
        statuses.delete(msg.id);
      }
      renderBadges();
    };

    return {
      el: rootEl,
      setData,
      setBadge,
      render,
      // The block open in the host's dock, by id (null for none).
      setCurrent: (id) => {
        current = id == null ? null : String(id);
        paintCurrent();
      },
      setHoverFocus: (on) => {
        hoverFocus = !!on;
        if (!hoverFocus) clearFocus();
      },
      // The selection, and a way to replace it wholesale. A host adapter with a
      // context menu needs both: the menu acts on the selection, and a
      // right-click on a row OUTSIDE it has to collapse to that row first, or
      // the menu would speak for rows carrying no mark.
      selection: () => [...selection],
      // The link gestures a host reaches from its own menus (the board's row
      // menu): a link's menu, and "Connect to…". `anchor` is the element to
      // hang the menu under.
      openLinkMenu: (id, anchor) => {
        const l = links.find((x) => x.id === id);
        if (l && anchor) openLinkMenu(l, atOf(anchor));
      },
      connectFrom: (id, anchor) => {
        if (anchor) connectFrom(String(id), atOf(anchor));
      },
      // Start the inline rename on a row, by block id or `stack:<id>`. The
      // gesture is a double-click on the name; a host adapter with a menu needs
      // to reach the same editor, and putting the caret in the row is the only
      // consistent answer -- a modal would rename in a different place from
      // where the double-click does.
      editName: (id) => {
        // a stack's name is on its header, or on its folded row
        const sel = String(id).startsWith('stack:')
          ? '.md-stackhead[data-stack="' + CSS.escape(String(id).slice(6)) +
            '"] .md-name, .md-chip[data-id="' + CSS.escape(String(id)) +
            '"] .md-name'
          : '.md-chip[data-id="' + CSS.escape(String(id)) + '"] .md-name';
        const name = deckEl.querySelector(sel);
        if (!name || name.tagName === 'INPUT') return false;
        if (name._mdEdit) { name._mdEdit(); return true; }
        name.contentEditable = 'true';
        name.focus();
        document.getSelection().selectAllChildren(name);
        return true;
      },
      selectOnly: (ids) => {
        selection.clear();
        (Array.isArray(ids) ? ids : [ids]).forEach((id) => selection.add(id));
        selAnchor = [...selection].pop() || null;
        deckEl.querySelectorAll('.md-chip[data-id]').forEach((el) => {
          el.classList.toggle('sel', selection.has(el.dataset.id));
        });
        updateBar();
      },
      // The focused stack, and a way to set it from outside: the host
      // adapter reads it to decide whether a freshly created block should
      // join a stack at birth, and a future deep link would enter through
      // the setter.
      stackFocus: () => stackFocus,
      focusStack: (id) => id == null
        ? clearStackFocus() : setStackFocus(String(id)),
      inspect: () => ({
        blocks, links, stacks,
        statuses: Object.fromEntries(statuses),
        collapsed: [...collapsed],
        stackFocus: stackFocus,
        selection: [...selection]
      })
    };
  }

  // `toolBtn`, `icon`, `tip` and `pointAnchor` are the design-system pieces
  // the board adapter draws its own controls with, so a tool in a row looks
  // the same whoever built it.
  return {
    create, LANE_COLORS, escapeHtml, hexA, toolBtn, icon, tip, pointAnchor,
    rowTops: rowTopsFor, DEFAULT_METRICS,
    linkReach, feedersFor, targetsFor, inputNameError
  };
});
