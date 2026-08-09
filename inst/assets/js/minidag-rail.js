/* Minidag rail: the list+rail editor, with no idea what a board is.
 *
 * The drawing, the gestures and the geometry that `minidag-layout.js` does
 * not own. Everything that knows about blockr boards -- Shiny, the dock
 * extension protocol, block arity, block icons -- lives in an ADAPTER that
 * the host passes in. `minidag.js` is the board adapter; blockr.process has
 * a second one whose nodes are process steps.
 *
 * Model (the adapter pushes it in via `setData`, same field names as
 * minidag-layout.js): nodes are `blocks` [{id, name, ...}], edges are `links`
 * [{id, from, to, input}], `stacks` [{id, name, color, blocks[]}]. The
 * renderer never mutates it: a gesture calls `adapter.emit(name, payload)`
 * and the host is expected to push a new model back.
 *
 * adapter = {
 *   emit(name, payload)          gestures out: link_add, link_rm, block_rm,
 *                                block_rename, block_select, block_append,
 *                                block_add, stack_add, stack_rename, stack_rm
 *   nodeLead(node) -> Element    row content before the name (icon, ports)
 *   nodeTrail(node) -> Element   row content after the name (chips, fields)
 *   nodeAside(node) -> Element   row content PAST the spring, so it right-
 *                                aligns into a column down the deck (the
 *                                board paints view membership here)
 *   stackAside(stack) -> Element the same, for a stack header and for the
 *                                collapsed stack row. Stack rows are built
 *                                entirely by the renderer, so this is their
 *                                only adapter hook.
 *   slotsFor(from, to) -> []     which slots this edge could occupy; EMPTY
 *                                means "refuse the connection". Boards ask
 *                                the consumer (free named inputs, '' when
 *                                variadic); a process asks the producer
 *                                (which outcome does this branch leave on).
 *   slotPrompt(from, to)         caption of the slot picker
 *   showSlot(link) -> bool       whether that slot is worth naming
 *   opts { search, stacks, remove, status, allowCycles, nameEdit, edgeLabels,
 *          labelPad, searchPlaceholder, emptyText, emptyAddText, metrics }
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
    root.minidagRail = api;
  }
})(typeof globalThis !== 'undefined' ? globalThis : this, function () {
  'use strict';

  const DEFAULT_METRICS = {
    LANE_W: 16, ROW_H: 28, GAP: 6, RAIL_L: 10, RAIL_R: 8, DOT_R: 4,
    // Air between a stack frame and the rows it encloses. The frame is drawn
    // as an absolute box behind the rows, so this is the ONLY thing that
    // insets them -- and it has to be applied on all four sides by hand:
    // FRAME_PAD_X is subtracted on the left and taken off the row width on
    // the right. Y is half of X because rows sit GAP (6px) apart, so a full
    // FRAME_PAD_X above the header would put the frame edge against the row
    // above it.
    FRAME_PAD_X: 6, FRAME_PAD_Y: 3
  };
  const LANE_COLORS = ['#9ca3af', '#2563eb', '#0d9488', '#7c3aed', '#b45309', '#be185d'];
  const STATUS_RANK = { failed: 3, waiting: 2, unset: 1 };

  const SVG = 'http://www.w3.org/2000/svg';
  const svgEl = (tag) => document.createElementNS(SVG, tag);

  const CHEV_D = '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.4"><path d="m6 9 6 6 6-6"/></svg>';
  const CHEV_R = '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.4"><path d="m9 6 6 6-6 6"/></svg>';
  const STACK_ICON = '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2.2"><path d="m12 3 9 5-9 5-9-5 9-5z"/><path d="m3 13 9 5 9-5"/></svg>';
  const SEARCH_ICON = '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2"><circle cx="11" cy="11" r="7"/><path d="m21 21-4.3-4.3"/></svg>';

  const escapeHtml = (s) => String(s).replace(/[&<>"']/g, (c) => ({
    '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;'
  }[c]));

  const hexA = (hex, a) => {
    const h = hex.replace('#', '');
    const r = parseInt(h.slice(0, 2), 16), g = parseInt(h.slice(2, 4), 16),
      b = parseInt(h.slice(4, 6), 16);
    return 'rgba(' + r + ',' + g + ',' + b + ',' + a + ')';
  };

  let uidCounter = 0;

  function create(rootEl, adapter) {

    const uid = 'md' + uidCounter++;
    const emit = (name, payload) => adapter.emit(name, payload);
    const opts = Object.assign({
      search: true, stacks: true, remove: true, status: true,
      allowCycles: false, nameEdit: 'dblclick',
      edgeLabels: false, labelPad: 34,
      searchPlaceholder: 'Search blocks…',
      emptyText: 'No blocks yet.',
      emptyAddText: '+ Add a block'
    }, adapter.opts || {});
    const M = Object.assign({}, DEFAULT_METRICS, opts.metrics || {});
    const { LANE_W, ROW_H, GAP, RAIL_L, RAIL_R, DOT_R } = M;
    const { FRAME_PAD_X, FRAME_PAD_Y } = M;
    const PITCH = ROW_H + GAP;

    const slotsFor = adapter.slotsFor;
    const nodeLead = adapter.nodeLead || (() => null);
    const nodeTrail = adapter.nodeTrail || (() => null);
    const nodeAside = adapter.nodeAside || (() => null);
    const stackAside = adapter.stackAside || (() => null);
    const slotPrompt = adapter.slotPrompt ||
      ((from, to) => 'Into which input of ' + to.name + '?');
    // whether the connections popover names the slot an edge occupies
    const showSlot = adapter.showSlot || ((l) => l.input !== '');

    /* ---- skeleton ---- */

    rootEl.innerHTML = '';
    // minidag.css is scoped to `.minidag`, and the renderer owns the skeleton
    // that stylesheet describes -- so it puts the class on rather than making
    // every host remember to (the board's container already has it).
    rootEl.classList.add('minidag');
    // The row height is a metric, so CSS reads it from here rather than
    // hard-coding 28px: a process step row is taller than a block row.
    rootEl.style.setProperty('--md-row-h', ROW_H + 'px');
    rootEl.style.setProperty('--md-row-gap', GAP + 'px');

    // Everything that is ABOUT the list rather than in it -- search, and the
    // selection actions -- rides in one sticky header. On a 92-row board the
    // action bar used to be the last child, pinned to the bottom edge of a
    // panel you had scrolled far away from: you ctrl-clicked two rows at the
    // top and the "Stack them" it offered was a screen below, if the panel
    // scrolled at all. Both now stay under the search box as the list moves.
    const headEl = document.createElement('div');
    headEl.className = 'md-head';
    rootEl.appendChild(headEl);

    let searchEl = null, hitsEl = null;
    if (opts.search) {
      const searchRow = document.createElement('div');
      searchRow.className = 'md-search-row';
      searchRow.innerHTML = '<span class="md-search-icon">' + SEARCH_ICON + '</span>';
      searchEl = document.createElement('input');
      searchEl.className = 'md-search';
      searchEl.placeholder = opts.searchPlaceholder;
      searchRow.appendChild(searchEl);
      hitsEl = document.createElement('span');
      hitsEl.className = 'md-hits';
      searchRow.appendChild(hitsEl);
      const addBtn = document.createElement('button');
      addBtn.className = 'md-add';
      addBtn.textContent = '+';
      addBtn.title = 'Add block';
      addBtn.addEventListener('click', () => emit('block_add', true));
      searchRow.appendChild(addBtn);
      headEl.appendChild(searchRow);
    }

    let barEl = null, selcountEl = null;
    if (opts.stacks) {
      barEl = document.createElement('div');
      barEl.className = 'md-actionbar';
      selcountEl = document.createElement('span');
      barEl.appendChild(selcountEl);
      const mkstackBtn = document.createElement('button');
      mkstackBtn.className = 'md-go';
      mkstackBtn.textContent = 'Stack them';
      barEl.appendChild(mkstackBtn);
      const clearselBtn = document.createElement('button');
      clearselBtn.className = 'md-no';
      clearselBtn.textContent = 'Clear';
      barEl.appendChild(clearselBtn);
      headEl.appendChild(barEl);

      clearselBtn.addEventListener('click', () => {
        selection.clear();
        // and the anchor with it: "Clear" means start over, so the next
        // shift-click must not sweep from a row cleared three actions ago
        selAnchor = null;
        render();
      });

      mkstackBtn.addEventListener('click', () => {
        const members = [...selection];
        const trial = [...stacks, { id: '_trial', name: '', blocks: members }];
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
    let selection = new Set();
    // the row a shift-click measures its range from: the last row clicked at
    // all, plain or cmd, which is what every list in every file manager does
    let selAnchor = null;
    let lastPos = new Map();
    let closePicker = null, dragging = false, pendingRender = false;

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

    /* ---- ordering + lanes: the pure geometry lives in minidag-layout.js ---- */

    // `minidag-layout.js` owns row order and lane assignment so `node --test`
    // can hold it to its invariants (tests/js/). Everything below draws.
    const G = (typeof globalThis !== 'undefined' ? globalThis : window).minidagLayout;

    const model = () => ({ blocks, links, stacks, collapsed, lastPos });

    const displayRows = () => G.displayRows(model());
    const innerOrder = (s) => G.innerOrder(model(), s);
    const railIdOf = (id) => G.railIdOf(model(), id);
    const railModel = (rows) => G.railModel(model(), rows);
    const layout = (entries, rl, rowOf, back) => G.layout(entries, rl, rowOf, back);

    const laneX = (l) => RAIL_L + l * LANE_W;
    const dotY = (r) => r * PITCH + ROW_H / 2;

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
      hideEdgeXNow();
      // The relatedness focus is painted on rows this rebuild is about to
      // destroy, so it has to be dropped -- but it is HOVER state, and the
      // pointer has not moved. Clearing it and leaving it cleared is what
      // made every board change flash the dim off and then, one mousemove
      // later, back on: a click that reveals a block pushes a fresh model,
      // and on a 92-row board the gap is long enough to read as a glitch.
      // So remember it here and repaint it below, once the rows exist again.
      const refocus = focusId;
      clearFocus();
      deckEl.innerHTML = '';

      if (!blocks.length) {
        const empty = document.createElement('div');
        empty.className = 'md-empty';
        empty.innerHTML = '<p>' + escapeHtml(opts.emptyText) + '</p>';
        const b = document.createElement('button');
        b.className = 'md-empty-add';
        b.textContent = opts.emptyAddText;
        b.addEventListener('click', () => emit('block_add', true));
        empty.appendChild(b);
        deckEl.appendChild(empty);
        updateBar();
        return;
      }

      const rows = displayRows();
      lastPos = new Map();
      rows.forEach((r, i) => {
        if (r.t === 'node') lastPos.set('n:' + r.node.id, i);
        else lastPos.set('s:' + r.stack.id, i);
      });
      const { entries, rowOf, rl, back } = railModel(rows);
      const { laneOf, edges, backs, nLanes } = layout(entries, rl, rowOf, back);
      const labelPad = opts.edgeLabels ? opts.labelPad : 0;
      const railW = RAIL_L + nLanes * LANE_W + RAIL_R + labelPad;
      const H = rows.length * PITCH;

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
      // is where its ✕ appears and where the hover highlight fires.
      const hitFrom = new Map();
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
        hit.addEventListener('mouseenter', () => showEdgeX(e, p, hit));
        hit.addEventListener('mouseleave', hideEdgeXSoon);
        svg.appendChild(hit);
      });

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
          ' C' + (xF + LANE_W) + ',' + yF + ' ' + xB + ',' + (yF - GAP) +
          ' ' + xB + ',' + (yF - PITCH * 0.5) +
          ' L' + xB + ',' + (yT + PITCH * 0.5) +
          ' C' + xB + ',' + (yT + GAP) + ' ' + (xT + LANE_W) + ',' + yT +
          ' ' + (xT + DOT_R + 2) + ',' + yT);
        p.setAttribute('fill', 'none');
        p.setAttribute('stroke', LANE_COLORS[e.lane % LANE_COLORS.length]);
        p.setAttribute('stroke-width', '1.6');
        p.setAttribute('stroke-dasharray', '3 3');
        p.setAttribute('marker-end', 'url(#' + arrowId + ')');
        p.setAttribute('class', 'md-edge md-edge-back');
        p.dataset.from = e.from;
        p.dataset.to = e.to;
        svg.appendChild(p);
        const hit = svgEl('path');
        hit.setAttribute('d', p.getAttribute('d'));
        hit.setAttribute('fill', 'none');
        hit.setAttribute('stroke', 'transparent');
        hit.setAttribute('stroke-width', '12');
        hit.setAttribute('class', 'md-edge-hit');
        hit.addEventListener('mouseenter', () => showEdgeX(e, p, hit));
        hit.addEventListener('mouseleave', hideEdgeXSoon);
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
          t.setAttribute('y', dotY(r) - 5 - n * 9);
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
        const c = svgEl('circle');
        c.setAttribute('cx', laneX(laneOf.get(e.id)));
        c.setAttribute('cy', dotY(e.row));
        c.setAttribute('r', baseR);
        c.setAttribute('fill', isStack ? (stackCol || laneCol) : '#fff');
        c.setAttribute('stroke', isStack ? '#fff' : laneCol);
        c.setAttribute('stroke-width', '2');
        c.setAttribute('class', 'md-dot');
        c.dataset.rail = e.id;
        svg.appendChild(c);
        const hc = svgEl('circle');
        hc.setAttribute('cx', laneX(laneOf.get(e.id)));
        hc.setAttribute('cy', dotY(e.row));
        hc.setAttribute('r', '11');
        hc.setAttribute('fill', 'transparent');
        hc.setAttribute('pointer-events', 'all');
        hc.style.cursor = 'crosshair';
        const tip = svgEl('title');
        tip.textContent = 'Drag to connect or append · click for connections';
        hc.appendChild(tip);
        hc.addEventListener('mouseenter', () => c.setAttribute('r', String(baseR + 1.5)));
        hc.addEventListener('mouseleave', () => c.setAttribute('r', String(baseR)));
        hc.addEventListener('mousedown', (ev) => {
          const anchor = deckEl.querySelector('.md-chip[data-id="' + CSS.escape(e.id) + '"]');
          startDrag(ev, e.id, hc, () => { if (anchor) openConn(anchor, e.id); });
        });
        svg.appendChild(hc);
      });
      deckEl.appendChild(svg);

      rows.forEach((r, i) => {
        if (r.t !== 'header') return;
        const size = r.stack.blocks.length;
        const frame = document.createElement('div');
        frame.className = 'md-stackframe';
        if (r.stack.color) {
          frame.style.borderColor = r.stack.color;
          frame.style.background = hexA(r.stack.color, 0.06);
        }
        frame.style.left = (railW - FRAME_PAD_X) + 'px';
        frame.style.right = '0px';
        frame.style.top = (i * PITCH - FRAME_PAD_Y) + 'px';
        frame.style.height = ((1 + size) * PITCH - GAP + 2 * FRAME_PAD_Y) + 'px';
        deckEl.appendChild(frame);
      });

      const list = document.createElement('div');
      list.className = 'md-rows';
      list.style.marginLeft = railW + 'px';
      rows.forEach((r) => {
        if (r.t === 'node') list.appendChild(chip(r.node, r.inStack));
        else if (r.t === 'stack') list.appendChild(stackChip(r.stack));
        else list.appendChild(stackHead(r.stack));
      });
      deckEl.appendChild(list);

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
      applySearch();
      renderBadges();

      // Only if the row is still there: a block removed by the very update
      // that triggered this render would light nothing, leaving the whole
      // deck dimmed with no explanation.
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
        if (stackOf(id)) return;
        selection.add(id);
        const row = deckEl.querySelector('.md-chip[data-id="' + CSS.escape(id) + '"]');
        if (row) row.classList.add('sel');
      });
      selAnchor = toId;
      updateBar();
    };

    /* ---- row builders ---- */

    // The renderer owns the row shell (identity, selection, drop feedback,
    // rename, status, remove); the adapter paints what is inside it.
    const chip = (b, inStack) => {
      const el = document.createElement('div');
      el.className = 'md-chip' + (inStack ? ' instack' : '') +
        (selection.has(b.id) ? ' sel' : '');
      el.dataset.id = b.id;
      // the frame ends at the deck's right edge, so a framed row has to stop
      // short of it or the border is drawn ON the row -- padded on the left,
      // clipped on the right, which is how it read before
      if (inStack) el.style.marginRight = FRAME_PAD_X + 'px';

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

      const trail = nodeTrail(b);
      if (trail) el.appendChild(trail);

      const spring = document.createElement('span');
      spring.className = 'md-spring';
      el.appendChild(spring);

      // Past the spring, so it right-aligns: `nodeTrail` sits beside the name
      // and is the wrong place for anything that wants to read as a column
      // down the deck. Kept as a separate hook rather than moving `nodeTrail`,
      // which blockr.process's adapter uses for exactly the beside-the-name
      // job its name promises.
      const aside = nodeAside(b);
      if (aside) el.appendChild(aside);

      if (opts.remove) {
        const rm = document.createElement('button');
        rm.className = 'md-rm';
        rm.textContent = '×';
        rm.title = 'Remove block';
        rm.addEventListener('click', (e) => {
          e.stopPropagation();
          emit('block_rm', { id: b.id });
        });
        el.appendChild(rm);
      }

      el.addEventListener('click', (e) => {
        if (e.target.closest('button, input, select, textarea')) return;
        if (name.isContentEditable) return;

        if (opts.stacks && e.shiftKey && selAnchor && selAnchor !== b.id) {
          selectRange(selAnchor, b.id);
          return;
        }
        if (opts.stacks && (e.metaKey || e.ctrlKey || e.shiftKey)) {
          if (stackOf(b.id)) return; // stacked blocks: dissolve first
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
          if (!stackOf(b.id)) {
            selection.add(b.id);
            el.classList.add('sel');
          }
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
      name.title = 'Click to open · double-click to rename · ⌘-click to select';
      name.addEventListener('dblclick', () => {
        name.contentEditable = 'true';
        name.focus();
        document.getSelection().selectAllChildren(name);
      });
      name.addEventListener('blur', () => {
        if (name.contentEditable !== 'true') return;
        name.contentEditable = 'false';
        const nm = name.textContent.trim();
        if (nm && nm !== obj.name) {
          obj.name = nm;
          commit(nm);
        }
        name.textContent = obj.name;
        if (pendingRender) render();
      });
      name.addEventListener('keydown', (e) => {
        if (e.key === 'Enter') { e.preventDefault(); name.blur(); }
        if (e.key === 'Escape') { name.textContent = obj.name; name.blur(); }
      });
      return name;
    };

    const stackChip = (stack) => {
      const el = document.createElement('div');
      el.className = 'md-chip md-stackchip';
      el.dataset.id = 'stack:' + stack.id;

      const k = document.createElement('span');
      k.className = 'md-kind';
      k.innerHTML = STACK_ICON;
      if (stack.color) k.style.background = stack.color;
      k.title = 'Stack';
      el.appendChild(k);

      // Same position as on a block row (see chip()): the collapsed stack's
      // worst member status, read where a status dot always lives.
      const status = document.createElement('span');
      status.className = 'md-status';
      status.dataset.stack = stack.id;
      el.appendChild(status);

      const name = document.createElement('span');
      name.className = 'md-name';
      name.textContent = stack.name;
      el.appendChild(name);

      const badge = document.createElement('span');
      badge.className = 'md-badge';
      badge.textContent = stack.blocks.length + ' blocks';
      el.appendChild(badge);

      const spring = document.createElement('span');
      spring.className = 'md-spring';
      el.appendChild(spring);

      const aside = stackAside(stack, true);
      if (aside) el.appendChild(aside);

      const chev = document.createElement('button');
      chev.className = 'md-chev';
      chev.innerHTML = CHEV_R;
      chev.title = 'Expand stack';
      chev.addEventListener('click', (e) => {
        e.stopPropagation();
        collapsed.delete(stack.id);
        render();
      });
      el.appendChild(chev);

      el.addEventListener('click', (e) => {
        if (e.metaKey || e.ctrlKey || e.target.closest('button')) return;
        openConn(el, 'stack:' + stack.id);
      });

      return el;
    };

    const stackHead = (stack) => {
      const el = document.createElement('div');
      el.className = 'md-stackhead';
      el.dataset.stack = stack.id;
      el.style.marginRight = FRAME_PAD_X + 'px';   // inside the frame too

      const cap = document.createElement('span');
      cap.className = 'md-cap';
      cap.innerHTML = STACK_ICON;
      if (stack.color) cap.style.color = stack.color;
      el.appendChild(cap);

      const name = document.createElement('span');
      name.className = 'md-name';
      name.textContent = stack.name;
      name.title = 'Double-click to rename';
      name.addEventListener('dblclick', () => {
        name.contentEditable = 'true';
        name.focus();
        document.getSelection().selectAllChildren(name);
      });
      name.addEventListener('blur', () => {
        if (name.contentEditable !== 'true') return;
        name.contentEditable = 'false';
        const nm = name.textContent.trim();
        if (nm && nm !== stack.name) {
          stack.name = nm;
          emit('stack_rename', { id: stack.id, name: nm });
        }
        name.textContent = stack.name;
        if (pendingRender) render();
      });
      name.addEventListener('keydown', (e) => {
        if (e.key === 'Enter') { e.preventDefault(); name.blur(); }
        if (e.key === 'Escape') { name.textContent = stack.name; name.blur(); }
      });
      el.appendChild(name);

      const badge = document.createElement('span');
      badge.className = 'md-badge';
      badge.textContent = String(stack.blocks.length);
      el.appendChild(badge);

      const spring = document.createElement('span');
      spring.className = 'md-spring';
      el.appendChild(spring);

      const aside = stackAside(stack, false);
      if (aside) el.appendChild(aside);

      const rm = document.createElement('button');
      rm.className = 'md-rm';
      rm.textContent = '×';
      rm.title = 'Dissolve stack (blocks stay)';
      rm.addEventListener('click', () => emit('stack_rm', { id: stack.id }));
      el.appendChild(rm);

      const chev = document.createElement('button');
      chev.className = 'md-chev';
      chev.innerHTML = CHEV_D;
      chev.title = 'Collapse stack';
      chev.addEventListener('click', () => {
        collapsed.add(stack.id);
        render();
      });
      el.appendChild(chev);

      return el;
    };

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
    const FOCUS_IN_MS = 120, FOCUS_MOVE_MS = 60, FOCUS_OUT_MS = 260;

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

    // The focus is two classes -- one on the deck, `md-rel` on the few rows
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
      deckEl.querySelectorAll('.md-status[data-for]').forEach((el) => {
        const st = statuses.get(el.dataset.for);
        el.style.background = st ? st.color : 'transparent';
        el.title = st ? st.label : '';
        el.classList.toggle('on', !!st);
      });
      deckEl.querySelectorAll('.md-status[data-stack]').forEach((el) => {
        const s = stacks.find((x) => x.id === el.dataset.stack);
        const st = s ? stackStatus(s) : null;
        el.style.background = st ? st.color : 'transparent';
        el.title = st ? st.label : '';
        el.classList.toggle('on', !!st);
      });
    };

    /* ---- search ---- */

    const nameOf = (el) => {
      const n = el.querySelector('.md-name');
      if (!n) return '';
      return (n.value != null ? n.value : n.textContent) || '';
    };

    const applySearch = () => {
      if (!searchEl) return;
      const q = searchEl.value.trim().toLowerCase();
      if (q) clearFocus();
      const chips = deckEl.querySelectorAll('.md-chip');
      const heads = deckEl.querySelectorAll('.md-stackhead');
      if (!q) {
        chips.forEach((c) => c.classList.remove('dim'));
        heads.forEach((h) => h.classList.remove('dim'));
        // drop the inline value rather than pinning it to 1: the lineage
        // focus dims edges from CSS, and an inline opacity would outrank it
        deckEl.querySelectorAll('.md-edge').forEach((p) => {
          p.style.removeProperty('opacity');
        });
        deckEl.querySelectorAll('.md-badge').forEach((b) => b.classList.remove('hit'));
        hitsEl.textContent = '';
        return;
      }
      const match = (b) => (b.name || '').toLowerCase().includes(q);
      const hits = blocks.filter(match).map((b) => b.id);
      chips.forEach((c) => {
        const id = c.dataset.id;
        if (id.startsWith('stack:')) {
          const s = stacks.find((x) => x.id === id.slice(6));
          const inner = s.blocks.filter((m) => {
            const b = blockOf(m);
            return b && match(b);
          }).length;
          const lit = inner > 0 || s.name.toLowerCase().includes(q);
          c.classList.toggle('dim', !lit);
          const bd = c.querySelector('.md-badge');
          bd.classList.toggle('hit', inner > 0);
          bd.textContent = inner > 0
            ? inner + ' of ' + s.blocks.length + ' match'
            : s.blocks.length + ' blocks';
        } else {
          c.classList.toggle('dim', !hits.includes(id));
        }
      });
      heads.forEach((h) => {
        const s = stacks.find((x) => x.id === h.dataset.stack);
        const lit = s.name.toLowerCase().includes(q) ||
          s.blocks.some((m) => { const b = blockOf(m); return b && match(b); });
        h.classList.toggle('dim', !lit);
      });
      deckEl.querySelectorAll('.md-edge').forEach((p) => { p.style.opacity = '0.15'; });
      hitsEl.textContent = hits.length + ' / ' + blocks.length;
    };

    if (searchEl) {
      searchEl.addEventListener('input', applySearch);
      searchEl.addEventListener('keydown', (e) => {
        if (e.key === 'Escape') { searchEl.value = ''; applySearch(); searchEl.blur(); }
      });
    }

    /* ---- selection ---- */

    const updateBar = () => {
      if (!barEl) return;
      barEl.classList.toggle('on', selection.size >= 2);
      barEl.classList.remove('err');
      selcountEl.textContent = selection.size + ' blocks selected';
    };

    /* ---- unlink: hover a rail edge for a ✕ at its midpoint ---- */

    let edgeXEl = null, edgeXTimer = null, edgeXPath = null;

    const linksBehind = (railFrom, railTo) =>
      links.filter((l) => railIdOf(l.from) === railFrom && railIdOf(l.to) === railTo);

    // `path` is what lights up, `band` the stretch this edge owns alone (they
    // differ once siblings share a bus) -- the ✕ goes on the band, so two
    // edges out of one block never put their ✕ in the same place.
    const showEdgeX = (e, path, band) => {
      if (dragging) return;
      clearTimeout(edgeXTimer);
      hideEdgeXNow();
      path.setAttribute('stroke-width', '3.2');
      edgeXPath = path;
      const at = band || path;
      const m = at.getPointAtLength(at.getTotalLength() / 2);
      const behind = linksBehind(e.from, e.to);
      edgeXEl = document.createElement('button');
      edgeXEl.className = 'md-edge-x';
      edgeXEl.textContent = '×';
      edgeXEl.title = behind.length > 1
        ? 'Remove connection (' + behind.length + ' links)'
        : 'Remove connection';
      edgeXEl.style.left = (m.x - 8) + 'px';
      edgeXEl.style.top = (m.y - 8) + 'px';
      edgeXEl.addEventListener('mouseenter', () => clearTimeout(edgeXTimer));
      edgeXEl.addEventListener('mouseleave', hideEdgeXSoon);
      edgeXEl.addEventListener('click', () => {
        emit('link_rm', { ids: behind.map((l) => l.id) });
        hideEdgeXNow();
      });
      deckEl.appendChild(edgeXEl);
    };

    const hideEdgeXNow = () => {
      if (edgeXPath) { edgeXPath.setAttribute('stroke-width', '2'); edgeXPath = null; }
      if (edgeXEl) { edgeXEl.remove(); edgeXEl = null; }
    };
    const hideEdgeXSoon = () => {
      clearTimeout(edgeXTimer);
      edgeXTimer = setTimeout(hideEdgeXNow, 300);
    };

    /* ---- connections popover ---- */

    const slotLabel = (input) => input === '' ? 'new input' : input;

    const openConn = (el, railId) => {
      if (closePicker) closePicker();
      const ins = links.filter((l) => railIdOf(l.to) === railId && railIdOf(l.from) !== railId);
      const outs = links.filter((l) => railIdOf(l.from) === railId && railIdOf(l.to) !== railId);
      const pop = document.createElement('div');
      pop.className = 'md-picker md-conn';
      pop.style.left = Math.min(el.offsetLeft + 24, Math.max(0, deckEl.clientWidth - 230)) + 'px';
      const estH = 30 + (ins.length + outs.length) * 26 + (ins.length && outs.length ? 20 : 0);
      const below = el.offsetTop + ROW_H + 2;
      pop.style.top = (below + estH > deckEl.clientHeight + PITCH
        ? Math.max(0, el.offsetTop - estH - 4) : below) + 'px';
      const section = (title, list, dir, other, withSlot) => {
        if (!list.length) return;
        const cap = document.createElement('div');
        cap.className = 'md-sect';
        cap.textContent = title;
        pop.appendChild(cap);
        list.forEach((l) => {
          const row = document.createElement('div');
          row.className = 'md-crow';
          const nm = blockOf(other(l));
          row.innerHTML = '<span class="md-dir">' + dir + '</span>' +
            '<span class="md-who">' + escapeHtml(nm ? nm.name : other(l)) + '</span>' +
            (withSlot && showSlot(l)
              ? '<span class="md-slot">' + escapeHtml(l.input) + '</span>' : '');
          const x = document.createElement('button');
          x.className = 'md-unlink';
          x.textContent = '×';
          x.title = 'Remove this connection';
          x.addEventListener('click', () => emit('link_rm', { ids: [l.id] }));
          row.appendChild(x);
          pop.appendChild(row);
        });
      };
      section('Inputs', ins, 'from', (l) => l.from, true);
      section('Outputs', outs, 'to', (l) => l.to, true);
      if (!ins.length && !outs.length) {
        const none = document.createElement('div');
        none.className = 'md-none';
        none.textContent = 'No connections yet — drag this block’s dot.';
        pop.appendChild(none);
      }
      deckEl.appendChild(pop);
      const onDoc = (ev) => { if (!pop.contains(ev.target)) close(); };
      const close = () => {
        document.removeEventListener('mousedown', onDoc);
        pop.remove();
        closePicker = null;
      };
      setTimeout(() => document.addEventListener('mousedown', onDoc), 0);
      closePicker = close;
    };

    /* ---- slot picker: which slot the new edge occupies ---- */

    const openSlotPicker = (x, y, from, to, free, onPick) => {
      if (closePicker) closePicker();
      const pop = document.createElement('div');
      pop.className = 'md-picker';
      pop.style.left = Math.max(0, Math.min(x, deckEl.clientWidth - 170)) + 'px';
      pop.style.top = Math.max(0, y) + 'px';
      const cap = document.createElement('div');
      cap.className = 'md-sect';
      cap.textContent = slotPrompt(from, to);
      pop.appendChild(cap);
      free.forEach((slot) => {
        const item = document.createElement('div');
        item.className = 'md-pick';
        item.innerHTML = '<b>' + escapeHtml(slotLabel(slot)) + '</b>';
        item.addEventListener('click', () => { close(); onPick(slot); });
        pop.appendChild(item);
      });
      deckEl.appendChild(pop);
      const onDoc = (ev) => { if (!pop.contains(ev.target)) close(); };
      const close = () => {
        document.removeEventListener('mousedown', onDoc);
        pop.remove();
        closePicker = null;
      };
      setTimeout(() => document.addEventListener('mousedown', onDoc), 0);
      closePicker = close;
    };

    /* ---- port drag ---- */

    const startDrag = (e, railId, port, onTap) => {
      e.preventDefault();
      e.stopPropagation();
      if (closePicker) closePicker();
      hideEdgeXNow();
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
      path.setAttribute('stroke', '#2563eb');
      path.setAttribute('stroke-width', '1.6');
      path.setAttribute('stroke-dasharray', '4 3');
      wire.appendChild(path);

      // Two drop zones, one per mental model: in the CHIP column the whole
      // row band is a target (forgiving); in the RAIL gutter only the dots
      // themselves are — the line and the blank gutter read as canvas.
      const bandRows = displayRows();
      const rowRail = bandRows.map((r) => r.t === 'node' ? railIdOf(r.node.id)
        : r.t === 'stack' ? 'stack:' + r.stack.id : null);
      const listH = bandRows.length * PITCH;
      const railW = parseInt(deckEl.querySelector('.md-rows').style.marginLeft, 10) || 40;
      const dotPos = [...deckEl.querySelectorAll('svg.md-rail circle.md-dot')].map((c) => ({
        id: c.dataset.rail,
        x: +c.getAttribute('cx'), y: +c.getAttribute('cy')
      }));

      const bandAt = (x, y) => {
        if (x >= railW - 8 && x <= deckEl.clientWidth + 8) {
          let idx = -1;
          if (y >= -8 && y < listH) idx = Math.max(0, Math.floor(y / PITCH));
          else if (y >= listH && y < listH + 12) idx = rowRail.length - 1;
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
        if (Math.abs(x - x0) + Math.abs(y - y0) > 6) moved = true;
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
        if (band && band !== 'self') {
          const verdict = dropVerdict(railId, band);
          const chipEl = deckEl.querySelector('.md-chip[data-id="' + CSS.escape(band) + '"]');
          if (chipEl) {
            chipEl.classList.add(verdict === 'ok' ? 'drop-ok' : 'drop-no');
            if (verdict === 'full') chipEl.title = 'All inputs are taken';
            else if (verdict === 'cycle') chipEl.title = 'Would create a cycle';
            else chipEl.title = '';
          }
          if (verdict === 'ok') target = band;
        } else if (!band && moved) {
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
        path.remove();
        if (ghost) ghost.remove();
        deckEl.classList.remove('md-dropzone');
        deckEl.querySelectorAll('.md-chip').forEach((c) =>
          c.classList.remove('drop-ok', 'drop-no'));
        const x = ev.clientX - deckBox.left, y = ev.clientY - deckBox.top;
        if (target) {
          doConnect(railId, target, x, Math.min(y, listH));
          if (pendingRender) render();
          return;
        }
        if (!moved) {
          if (onTap) onTap();
          if (pendingRender) render();
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
        emit('block_append', { from: sinkOf(railId), x, y });
        if (pendingRender) render();
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
      setHoverFocus: (on) => {
        hoverFocus = !!on;
        if (!hoverFocus) clearFocus();
      },
      inspect: () => ({
        blocks, links, stacks,
        statuses: Object.fromEntries(statuses),
        collapsed: [...collapsed],
        selection: [...selection]
      })
    };
  }

  return { create, LANE_COLORS, escapeHtml, hexA };
});
