/* Minidag rail geometry: the pure part of the deck.
 *
 * model -> display rows -> lanes and edges. No DOM, no Shiny, no side
 * effects, so it can be exercised by `node --test` (see tests/js/) as well as
 * by the browser. `minidag.js` owns everything that draws.
 *
 * model = {
 *   blocks:    [{id, name, inputs[], variadic, ...}],
 *   links:     [{id, from, to, input}],
 *   stacks:    [{id, name, color, blocks[]}],
 *   collapsed: Set of stack ids,
 *   lastPos:   Map of 'n:<block>' / 's:<stack>' -> previous row (drag stability)
 * }
 *
 * THE RULE the lane assignment keeps, and the reason it exists:
 *
 *   A DOT TERMINATES ITS LANE.
 *
 * A block may sit on a producer's line only when it is that line's last
 * consumer. Park an earlier consumer there and the line keeps descending
 * straight through its dot -- and a fan-out becomes pixel-identical to a
 * chain. On the CDEX board that misdrew 44 of 92 dots: the AE summary's line
 * ran through the AE flags dot, which reads as "flags feeds the summary" when
 * the two are siblings. Costs about one extra lane; buys a picture that
 * cannot be misread.
 */
(function (root, factory) {
  'use strict';
  var api = factory();
  if (typeof module === 'object' && module.exports) {
    module.exports = api;
  }
  if (root) {
    root.minidagLayout = api;
  }
})(typeof globalThis !== 'undefined' ? globalThis : this, function () {
  'use strict';

  /* ---- ordering: stacks are super-nodes ---- */

  // Topological order with a stable tie-break: among the blocks whose parents
  // are already placed, take the one that sat highest last time (`seqOf`), so
  // an edit nudges the list instead of reshuffling it.
  const kahn = (ids, edgesOf, seqOf) => {
    const order = [], placed = new Set();
    const pending = [...ids].sort((a, b) => seqOf(a) - seqOf(b));
    let hadCycle = false;
    while (pending.length) {
      let pick = pending.findIndex((u) => edgesOf(u).every((p) => placed.has(p)));
      if (pick < 0) { pick = 0; hadCycle = true; }
      const u = pending[pick];
      pending.splice(pick, 1);
      placed.add(u);
      order.push(u);
    }
    return { order, hadCycle };
  };

  /* ---- back edges ---- */

  // A board is acyclic, a process is not: a QS check that fails sends the
  // work back for rework, and that arrow points UP the list. Such an edge is
  // classified out of the ordering (otherwise every loop would make the
  // topological sort give up and fall back to array order) and drawn in its
  // own gutter.
  //
  // The rule is `tidybpmn::loop_deps()`, ported so R and the browser agree on
  // which arrow is the loop: `from -> to` is a loop-back iff `from` is
  // reachable from `to` AND `from` sits no closer to the roots than `to`.
  // Plain reachability is not enough -- in a 2-cycle it holds both ways, and
  // the depth is what breaks the tie.
  const backEdges = (model) => {
    const links = model.links || [];
    const ids = model.blocks.map((b) => b.id);
    const succ = new Map(ids.map((id) => [id, []]));
    links.forEach((l) => {
      if (succ.has(l.from)) succ.get(l.from).push(l.to);
    });

    const hasParent = new Set(links.map((l) => l.to));
    const depth = new Map(ids.map((id) => [id, Infinity]));
    let queue = ids.filter((id) => !hasParent.has(id));
    queue.forEach((id) => depth.set(id, 0));
    while (queue.length) {
      const x = queue.shift();
      (succ.get(x) || []).forEach((nx) => {
        const d = depth.get(x) + 1;
        if (depth.get(nx) > d) { depth.set(nx, d); queue.push(nx); }
      });
    }

    const reach = new Map();
    const reachable = (from) => {
      if (reach.has(from)) return reach.get(from);
      const seen = new Set();
      const q = [...(succ.get(from) || [])];
      while (q.length) {
        const x = q.shift();
        if (seen.has(x)) continue;
        seen.add(x);
        q.push(...(succ.get(x) || []));
      }
      reach.set(from, seen);
      return seen;
    };

    const back = new Set();
    links.forEach((l) => {
      if (l.from === l.to) return;
      if (reachable(l.to).has(l.from) && depth.get(l.from) >= depth.get(l.to)) {
        back.add(l.from + '>' + l.to);
      }
    });
    return back;
  };

  // memoised per model object rather than stored on it: `railFor` walks the
  // same model three times and the classification is not free, but the model
  // belongs to the caller
  const backCache = new WeakMap();
  const backSet = (model) => {
    let s = backCache.get(model);
    if (!s) { s = backEdges(model); backCache.set(model, s); }
    return s;
  };

  const isBack = (model, l) => backSet(model).has(l.from + '>' + l.to);

  const parentsOf = (model, id) =>
    model.links.filter((l) => l.to === id && !isBack(model, l)).map((l) => l.from);

  const superOrder = (model, stks) => {
    const blocks = model.blocks;
    const lastPos = model.lastPos || new Map();
    const sOf = (id) => {
      const s = stks.find((x) => x.blocks.includes(id));
      return s ? 's:' + s.id : 'n:' + id;
    };
    const units = new Set(blocks.map((b) => sOf(b.id)));
    const arrSeq = (u) => u.startsWith('s:')
      ? Math.min(...stks.find((s) => s.id === u.slice(2)).blocks
        .map((m) => blocks.findIndex((b) => b.id === m)))
      : blocks.findIndex((b) => b.id === u.slice(2));
    const seqOf = (u) => lastPos.has(u) ? lastPos.get(u) : 1000 + arrSeq(u);
    const parentUnits = (u) => {
      const members = u.startsWith('s:')
        ? stks.find((s) => s.id === u.slice(2)).blocks
        : [u.slice(2)];
      const ps = new Set();
      members.forEach((m) => parentsOf(model, m).forEach((p) => {
        const pu = sOf(p);
        if (pu !== u) ps.add(pu);
      }));
      return [...ps];
    };
    return Object.assign({}, kahn(units, parentUnits, seqOf), { sOf });
  };

  const innerOrder = (model, s) => kahn(
    s.blocks,
    (id) => parentsOf(model, id).filter((p) => s.blocks.includes(p)),
    (id) => (model.lastPos || new Map()).has('n:' + id)
      ? model.lastPos.get('n:' + id)
      : 1000 + model.blocks.findIndex((b) => b.id === id)
  ).order;

  const displayRows = (model) => {
    const stacks = model.stacks || [];
    const collapsed = model.collapsed || new Set();
    const blockOf = (id) => model.blocks.find((b) => b.id === id);
    const { order } = superOrder(model, stacks);
    const rows = [];
    order.forEach((u) => {
      if (u.startsWith('n:')) {
        rows.push({ t: 'node', node: blockOf(u.slice(2)) });
        return;
      }
      const stack = stacks.find((s) => s.id === u.slice(2));
      if (collapsed.has(stack.id)) { rows.push({ t: 'stack', stack }); return; }
      rows.push({ t: 'header', stack });
      innerOrder(model, stack).forEach((id) =>
        rows.push({ t: 'node', node: blockOf(id), inStack: stack }));
    });
    return rows;
  };

  /* ---- rail: collapsed stacks contract to one node ---- */

  const railIdOf = (model, id) => {
    const collapsed = model.collapsed || new Set();
    const s = (model.stacks || []).find((x) => x.blocks.includes(id));
    return s && collapsed.has(s.id) ? 'stack:' + s.id : id;
  };

  const railModel = (model, rows) => {
    const entries = rows
      .map((r, i) => r.t === 'node' ? { id: r.node.id, row: i }
        : r.t === 'stack' ? { id: 'stack:' + r.stack.id, row: i } : null)
      .filter(Boolean);
    const rowOf = new Map(entries.map((e) => [e.id, e.row]));
    const seen = new Set();
    const rl = [], back = [];
    model.links.forEach((l) => {
      const f = railIdOf(model, l.from), t = railIdOf(model, l.to);
      if (f === t) return;
      const key = f + '>' + t;
      if (seen.has(key)) return;
      seen.add(key);
      (isBack(model, l) ? back : rl).push({ from: f, to: t });
    });
    return { entries, rowOf, rl, back };
  };

  /* ---- lanes ---- */

  // All out-edges of a block ride ONE lane -- its line. A lane per EDGE is the
  // git-commit-graph rule, fine for a history that forks two or three ways;
  // boards fan out by eight (one global filter feeding every panel) and the
  // gutter grew wider than the list it annotates. One line per producer,
  // consumers as stations along it, is the metro-map reading and holds the
  // CDEX board to 7 lanes instead of 17. Fan-IN keeps a hook per edge, so a
  // merge still shows both parents arriving.
  // Loop-backs get their own gutter to the RIGHT of every forward lane. They
  // are rare (a process has one or two), they run the other way, and mixing
  // them into the forward lanes would make a line that descends and a line
  // that climbs share a column -- which is exactly the picture that cannot be
  // read. Two loops only share a gutter lane when their spans do not overlap.
  const backLanes = (back, rowOf, from) => {
    const spans = [];       // lane -> [{a, b}]
    return back.map((l) => {
      const a = Math.min(rowOf.get(l.to), rowOf.get(l.from));
      const b = Math.max(rowOf.get(l.to), rowOf.get(l.from));
      let lane = 0;
      while (lane < spans.length &&
             spans[lane].some((s) => a < s.b && s.a < b)) lane++;
      if (lane === spans.length) spans.push([]);
      spans[lane].push({ a, b });
      return { from: l.from, to: l.to, lane: from + lane };
    });
  };

  const layout = (entries, rl, rowOf, back) => {
    const lanes = [];                  // lane index -> occupied? (null = free)
    const laneOf = new Map();          // rail id -> the lane its dot sits in
    const busOf = new Map();           // rail id -> the lane its out-edges ride
    const edges = [];

    // Prefer a free lane at or right of `from`, so a consumer lands to the
    // right of its producer rather than jumping back across the gutter.
    const firstFree = (from) => {
      const start = from || 0;
      for (let i = start; i < lanes.length; i++) if (lanes[i] === null) return i;
      for (let i = 0; i < start && i < lanes.length; i++) {
        if (lanes[i] === null) return i;
      }
      lanes.push(null);
      return lanes.length - 1;
    };

    // the row each producer's line can be retired at
    const busEnd = new Map();
    rl.forEach((l) => {
      const r = rowOf.get(l.to);
      if (r === undefined) return;
      const cur = busEnd.has(l.from) ? busEnd.get(l.from) : -1;
      busEnd.set(l.from, Math.max(cur, r));
    });

    entries.forEach((e) => {
      const ins = rl.filter((l) => l.to === e.id);
      const outs = rl.filter((l) => l.from === e.id);

      // Sit on a producer's line only if this row RETIRES it -- see THE RULE.
      const inherit = ins
        .filter((l) => busOf.has(l.from) && busEnd.get(l.from) === e.row)
        .map((l) => busOf.get(l.from))
        .sort((a, b) => a - b);

      const near = ins
        .filter((l) => busOf.has(l.from))
        .map((l) => busOf.get(l.from))
        .sort((a, b) => a - b);

      const lane = inherit.length ? inherit[0] : firstFree(near.length ? near[0] : 0);

      laneOf.set(e.id, lane);
      lanes[lane] = 1;

      ins.forEach((l) => edges.push({
        from: l.from,
        to: l.to,
        lane: busOf.has(l.from) ? busOf.get(l.from) : lane
      }));

      // Retire every line whose last consumer is this row, BEFORE this block
      // claims one: a straight chain then keeps its parent's lane.
      busOf.forEach((bl, src) => {
        if ((busEnd.has(src) ? busEnd.get(src) : -1) <= e.row) {
          lanes[bl] = null;
          busOf.delete(src);
        }
      });

      if (outs.length) {
        lanes[lane] = 1;
        busOf.set(e.id, lane);   // the dot's own line continues from here
      } else {
        lanes[lane] = null;      // a leaf frees its lane immediately
      }
    });

    const nFwd = Math.max(1, lanes.length);
    const backs = backLanes(
      (back || []).filter((l) =>
        rowOf.has(l.from) && rowOf.has(l.to)),
      rowOf,
      nFwd
    );
    const nLanes = backs.reduce((n, b) => Math.max(n, b.lane + 1), nFwd);

    return { laneOf, edges, backs, nLanes, nFwdLanes: nFwd };
  };

  /* ---- invariants ---- */

  // What has to be true of any rail we draw. Tests assert this over real
  // boards, hand-written shapes and random graphs; a wrong-looking rail is
  // usually a broken invariant rather than a wrong graph.
  const invariants = (entries, rl, rowOf, res, back) => {
    const bad = [];
    const at = new Map(entries.map((e) => [e.row, e.id]));
    const rows = entries.map((e) => e.row).sort((a, b) => a - b);
    const say = (rule, detail) => bad.push({ rule, detail });

    if (res.edges.length !== rl.length) {
      say('edge-per-link', rl.length + ' links -> ' + res.edges.length + ' edges');
    }

    res.edges.forEach((e) => {
      const a = rowOf.get(e.from), b = rowOf.get(e.to);
      if (a === undefined || b === undefined) {
        say('edge-endpoints', e.from + '>' + e.to + ' has no row');
        return;
      }
      if (!(a < b)) {
        say('edge-downward', e.from + '>' + e.to + ' runs ' + a + ' -> ' + b);
      }
      if (res.laneOf.get(e.from) === undefined ||
          res.laneOf.get(e.to) === undefined) {
        say('edge-endpoints', e.from + '>' + e.to + ' has no lane');
      }
      // THE RULE: nothing but its own endpoints may sit on an edge's lane
      // while that edge passes by.
      rows.forEach((r) => {
        if (r <= a || r >= b) return;
        const id = at.get(r);
        if (id !== e.from && id !== e.to && res.laneOf.get(id) === e.lane) {
          say('no-pass-through',
            e.from + '>' + e.to + ' (lane ' + e.lane + ') crosses ' + id);
        }
      });
    });

    // Two producers' lines may reuse a lane, never at the same time.
    const spans = new Map();   // lane -> [{from, a, b}]
    res.edges.forEach((e) => {
      const a = rowOf.get(e.from), b = rowOf.get(e.to);
      const arr = spans.get(e.lane) || [];
      arr.push({ from: e.from, a, b });
      spans.set(e.lane, arr);
    });
    spans.forEach((arr, lane) => {
      arr.forEach((x, i) => arr.slice(i + 1).forEach((y) => {
        if (x.from === y.from) return;
        if (x.a < y.b && y.a < x.b) {
          say('lane-exclusive',
            'lane ' + lane + ': ' + x.from + ' and ' + y.from + ' overlap');
        }
      }));
    });

    entries.forEach((e) => {
      const l = res.laneOf.get(e.id);
      if (!(l >= 0 && l < res.nLanes)) {
        say('lane-in-range', e.id + ' at lane ' + l + ' of ' + res.nLanes);
      }
    });

    /* loop-backs */

    const backs = res.backs || [];
    if (backs.length !== (back || []).length) {
      say('back-per-link',
        (back || []).length + ' loops -> ' + backs.length + ' drawn');
    }

    const bspans = new Map();
    backs.forEach((e) => {
      const a = rowOf.get(e.from), b = rowOf.get(e.to);
      if (a === undefined || b === undefined) {
        say('edge-endpoints', e.from + '>' + e.to + ' has no row');
        return;
      }
      // a loop-back is the one arrow allowed to point up the list
      if (!(a >= b)) {
        say('back-upward', e.from + '>' + e.to + ' runs ' + a + ' -> ' + b);
      }
      // and it must stay out of the forward lanes entirely
      if (e.lane < res.nFwdLanes) {
        say('back-own-gutter',
          e.from + '>' + e.to + ' at lane ' + e.lane +
          ' among ' + res.nFwdLanes + ' forward lanes');
      }
      const arr = bspans.get(e.lane) || [];
      arr.push({ id: e.from + '>' + e.to, a: Math.min(a, b), b: Math.max(a, b) });
      bspans.set(e.lane, arr);
    });
    bspans.forEach((arr, lane) => {
      arr.forEach((x, i) => arr.slice(i + 1).forEach((y) => {
        if (x.a < y.b && y.a < x.b) {
          say('back-lane-exclusive',
            'lane ' + lane + ': ' + x.id + ' and ' + y.id + ' overlap');
        }
      }));
    });

    return bad;
  };

  const railFor = (model) => {
    const rows = displayRows(model);
    const rm = railModel(model, rows);
    const res = layout(rm.entries, rm.rl, rm.rowOf, rm.back);
    return Object.assign({ rows }, rm, res);
  };

  return {
    kahn,
    backEdges,
    superOrder,
    displayRows,
    innerOrder,
    railModel,
    railIdOf,
    layout,
    invariants,
    railFor
  };
});
