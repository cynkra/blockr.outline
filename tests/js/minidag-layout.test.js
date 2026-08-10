/* Rail geometry tests: `npm test` (i.e. `node --test tests/js/`).
 *
 * These assert INVARIANTS, not snapshots. A rail's pixel layout changes
 * whenever the aesthetics do; what must never change is that the picture
 * cannot be misread -- above all that no line passes through a dot it is not
 * connected to, which is how a fan-out came to look like a chain on the CDEX
 * board (44 of 92 dots) before this suite existed.
 *
 * Three corpora, because each catches a different class of mistake:
 *   real      - boards people actually built (fixtures/*.json)
 *   synthetic - the shapes an algorithm trips on, written by hand
 *   random    - a seeded sweep, for the shapes nobody thought of
 */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const fs = require('node:fs');
const path = require('node:path');

const G = require('../../inst/assets/js/minidag-layout.js');

/* ---- helpers ---- */

const mkModel = (blocks, links, stacks, collapsed) => ({
  blocks: blocks.map((b) => typeof b === 'string'
    ? { id: b, name: b, inputs: ['data'], variadic: false }
    : b),
  links: links.map((l, i) => Object.assign({ id: 'l' + i, input: 'data' }, l)),
  stacks: stacks || [],
  collapsed: new Set(collapsed || []),
  lastPos: new Map()
});

// every invariant, over one model
const check = (model, label) => {
  const r = G.railFor(model);
  const bad = G.invariants(r.entries, r.rl, r.rowOf, r, r.back);
  assert.deepStrictEqual(
    bad.map((b) => b.rule + ': ' + b.detail),
    [],
    label + ' violated its invariants'
  );
  return r;
};

const chain = (n) => {
  const ids = Array.from({ length: n }, (_, i) => 'b' + i);
  const links = ids.slice(1).map((id, i) => ({ from: ids[i], to: id }));
  return mkModel(ids, links);
};

const fanOut = (n) => {
  const ids = ['root'].concat(Array.from({ length: n }, (_, i) => 'k' + i));
  const links = ids.slice(1).map((id) => ({ from: 'root', to: id }));
  return mkModel(ids, links);
};

/* ---- real boards ---- */

const fixtures = fs.readdirSync(path.join(__dirname, 'fixtures'))
  .filter((f) => f.endsWith('.json'));

test('real boards keep the invariants', (t) => {
  assert.ok(fixtures.length, 'no fixtures found');
  for (const f of fixtures) {
    const raw = JSON.parse(
      fs.readFileSync(path.join(__dirname, 'fixtures', f), 'utf8')
    );
    const model = mkModel(raw.blocks, raw.links, raw.stacks);
    const r = check(model, f);
    t.diagnostic(
      f + ': ' + raw.blocks.length + ' blocks, ' + raw.links.length +
      ' links, ' + r.nLanes + ' lanes'
    );
  }
});

// The gutter is the whole point of the list shape; a regression here is as
// real as a wrong line. 92 blocks fitting in single digits is the bar.
test('the CDEX board stays narrow', () => {
  const raw = JSON.parse(
    fs.readFileSync(path.join(__dirname, 'fixtures', 'cdex.json'), 'utf8')
  );
  const r = G.railFor(mkModel(raw.blocks, raw.links, raw.stacks));
  assert.ok(r.nLanes <= 9, '92 blocks took ' + r.nLanes + ' lanes');
});

/* ---- shapes ---- */

test('a chain is one lane', () => {
  const r = check(chain(12), 'chain');
  assert.strictEqual(r.nLanes, 1);
});

test('a fan-out never passes a line through a sibling', () => {
  // The CDEX regression in miniature: root feeds eight blocks, and the line
  // to the last one must not run through the first seven dots.
  const r = check(fanOut(8), 'fan-out');
  assert.strictEqual(r.edges.length, 8);
});

test('a fan-out with tails still holds', () => {
  // each consumer has its own follow-on block, so lanes must nest and free
  const ids = ['root'];
  const links = [];
  for (let i = 0; i < 5; i++) {
    ids.push('k' + i, 't' + i);
    links.push({ from: 'root', to: 'k' + i }, { from: 'k' + i, to: 't' + i });
  }
  check(mkModel(ids, links), 'fan-out with tails');
});

test('a diamond holds', () => {
  check(mkModel(
    ['a', 'b', 'c', 'd'],
    [{ from: 'a', to: 'b' }, { from: 'a', to: 'c' },
      { from: 'b', to: 'd' }, { from: 'c', to: 'd', input: 'y' }]
  ), 'diamond');
});

test('deep fan-in holds', () => {
  const ids = ['sink'];
  const links = [];
  for (let i = 0; i < 6; i++) {
    ids.unshift('s' + i);
    links.push({ from: 's' + i, to: 'sink', input: '' });
  }
  check(mkModel(ids, links), 'fan-in');
});

test('islands hold', () => {
  check(mkModel(
    ['a', 'b', 'c', 'd', 'lonely'],
    [{ from: 'a', to: 'b' }, { from: 'c', to: 'd' }]
  ), 'islands');
});

test('a collapsed stack contracts to one rail node', () => {
  const model = mkModel(
    ['a', 'b', 'c', 'd'],
    [{ from: 'a', to: 'b' }, { from: 'b', to: 'c' }, { from: 'c', to: 'd' }],
    [{ id: 's1', name: 'prep', blocks: ['b', 'c'] }],
    ['s1']
  );
  const r = check(model, 'collapsed stack');
  assert.ok(r.entries.some((e) => e.id === 'stack:s1'));
  assert.ok(!r.entries.some((e) => e.id === 'b'));
});

test('an expanded stack keeps its members on the rail', () => {
  const model = mkModel(
    ['a', 'b', 'c', 'd'],
    [{ from: 'a', to: 'b' }, { from: 'b', to: 'c' }, { from: 'c', to: 'd' }],
    [{ id: 's1', name: 'prep', blocks: ['b', 'c'] }]
  );
  const r = check(model, 'expanded stack');
  assert.ok(r.entries.some((e) => e.id === 'b'));
});

test('the empty board is legal', () => {
  const r = G.railFor(mkModel([], []));
  assert.strictEqual(r.entries.length, 0);
  assert.strictEqual(r.nLanes, 1);
});

/* ---- random sweep ---- */

// A deliberately dumb LCG: reproducible, and a failing seed is quotable in a
// bug report. Graphs are generated over a random topological order, so they
// are acyclic by construction -- the layout is not expected to survive a
// cycle, the board cannot contain one.
const rng = (seed) => () => (seed = (seed * 1103515245 + 12345) & 0x7fffffff) / 0x7fffffff;

const randomDag = (rand) => {
  const n = 2 + Math.floor(rand() * 30);
  const ids = Array.from({ length: n }, (_, i) => 'n' + i);
  const links = [];
  const density = rand();
  for (let j = 1; j < n; j++) {
    for (let i = 0; i < j; i++) {
      if (rand() < density * 0.35) {
        links.push({ from: ids[i], to: ids[j], input: 'i' + i });
      }
    }
  }
  // shuffle the declaration order: the layout must not depend on it
  const shuffled = ids.slice();
  for (let i = shuffled.length - 1; i > 0; i--) {
    const j = Math.floor(rand() * (i + 1));
    [shuffled[i], shuffled[j]] = [shuffled[j], shuffled[i]];
  }
  return mkModel(shuffled, links);
};

test('400 random DAGs keep the invariants', () => {
  for (let seed = 1; seed <= 400; seed++) {
    const model = randomDag(rng(seed));
    const r = G.railFor(model);
    const bad = G.invariants(r.entries, r.rl, r.rowOf, r, r.back);
    assert.deepStrictEqual(
      bad.map((b) => b.rule + ': ' + b.detail),
      [],
      'seed ' + seed + ' (' + model.blocks.length + ' blocks, ' +
      model.links.length + ' links) violated its invariants'
    );
  }
});

test('random stacks keep the invariants', () => {
  for (let seed = 1; seed <= 100; seed++) {
    const rand = rng(seed * 7919);
    const model = randomDag(rand);
    const ids = model.blocks.map((b) => b.id);
    // group a contiguous slice of the declaration order into a stack
    const at = Math.floor(rand() * Math.max(1, ids.length - 3));
    const members = ids.slice(at, at + 2 + Math.floor(rand() * 2));
    if (members.length < 2) continue;
    model.stacks = [{ id: 's', name: 'g', blocks: members }];
    if (rand() < 0.5) model.collapsed = new Set(['s']);
    // NOT skipped when the grouping tangles the flow: the deck refuses to
    // CREATE such a stack, but a board can arrive with one (dock's stack
    // editor makes them, and a link added later can tangle a stack that was
    // fine) and it still has to draw.
    const r = G.railFor(model);
    const bad = G.invariants(r.entries, r.rl, r.rowOf, r, r.back);
    assert.deepStrictEqual(
      bad.map((b) => b.rule + ': ' + b.detail),
      [],
      'stack seed ' + seed + ' violated its invariants'
    );
  }
});

/* ---- non-convex stacks: the shape that broke on the CDEX board ---- */

// vs -> display -> filter -> {chart1, chart2}, with everything but `display`
// stacked. The frame has to keep its rows together, so `display` cannot sit
// between them: it lands outside the group and the link that feeds the stack
// back has to climb.
const interleaved = (collapsed) => mkModel(
  ['vs', 'display', 'filter', 'chart1', 'chart2'],
  [
    { from: 'vs', to: 'display' },
    { from: 'display', to: 'filter' },
    { from: 'filter', to: 'chart1' },
    { from: 'filter', to: 'chart2' }
  ],
  [{ id: 's', name: 'VS', blocks: ['vs', 'filter', 'chart1', 'chart2'] }],
  collapsed
);

test('a non-convex stack still draws a legal rail', () => {
  check(interleaved(), 'interleaved stack');
  check(interleaved(['s']), 'interleaved stack, collapsed');
});

test('the link the order cannot honour climbs the loop gutter', () => {
  const r = G.railFor(interleaved());
  assert.strictEqual(r.back.length, 1);
  assert.ok(r.back[0].up, 'flagged as an ordering casualty, not a loop');
  // and the cheaper break is the one taken: one arrow, not two
  assert.strictEqual(r.rl.length, 3);
});

test('the stall breaks on the cheaper side', () => {
  // The CDEX shape: one block outside the stack feeds EIGHT of its members
  // and reads from one. Placing the stack first would cost eight arrows
  // climbing the gutter; placing the block first costs one.
  const kids = Array.from({ length: 8 }, (_, i) => 'c' + i);
  const m = mkModel(
    ['vs', 'display'].concat(kids),
    [{ from: 'vs', to: 'display' }].concat(
      kids.map((k) => ({ from: 'display', to: k }))
    ),
    [{ id: 's', name: 'VS', blocks: ['vs'].concat(kids) }]
  );
  const r = check(m, 'fan-in interloper');
  assert.strictEqual(r.back.length, 1);
  const rowOf = (id) => r.rows.findIndex(
    (x) => x.t === 'node' && x.node.id === id
  );
  assert.ok(rowOf('display') < rowOf('vs'), 'the interloper goes first');
});

test('the block in the way is named', () => {
  const m = interleaved();
  assert.deepStrictEqual(G.stackHoles(m, m.stacks[0]), ['display']);
  // pulling it in is what makes the stack convex again
  m.stacks[0].blocks.push('display');
  assert.deepStrictEqual(G.stackHoles(m, m.stacks[0]), []);
  assert.ok(!G.superOrder(m, m.stacks).hadCycle);
  assert.strictEqual(G.railFor(m).back.length, 0);
});

test('a stack with nothing between its members has no holes', () => {
  const m = mkModel(
    ['a', 'b', 'c', 'd'],
    [{ from: 'a', to: 'b' }, { from: 'b', to: 'c' }, { from: 'c', to: 'd' }],
    [{ id: 's', name: 'g', blocks: ['b', 'c'] }]
  );
  assert.deepStrictEqual(G.stackHoles(m, m.stacks[0]), []);
  // a sibling of a member is not in the way: it reads from the stack but
  // nothing in the stack reads from it
  const sib = mkModel(
    ['a', 'b', 'c'],
    [{ from: 'a', to: 'b' }, { from: 'a', to: 'c' }],
    [{ id: 's', name: 'g', blocks: ['a', 'b'] }]
  );
  assert.deepStrictEqual(G.stackHoles(sib, sib.stacks[0]), []);
});

/* ---- loop-backs: the shapes a process makes and a board never does ---- */

test('a rework loop is classified, not sorted around', () => {
  // deliver -> clean -> qs -> rework -> qs   (the quarterly example's shape)
  const m = mkModel(
    ['deliver', 'clean', 'qs', 'rework', 'signoff'],
    [
      { from: 'deliver', to: 'clean' },
      { from: 'clean', to: 'qs' },
      { from: 'qs', to: 'rework' },
      { from: 'rework', to: 'qs' },
      { from: 'qs', to: 'signoff' }
    ]
  );

  assert.deepStrictEqual([...G.backEdges(m)], ['rework>qs']);

  const r = check(m, 'rework loop');
  // the loop is out of the ordering, so the list still reads top to bottom
  assert.deepStrictEqual(
    r.rows.map((x) => x.node.id),
    ['deliver', 'clean', 'qs', 'rework', 'signoff']
  );
  assert.strictEqual(r.back.length, 1);
  assert.strictEqual(r.backs.length, 1);
  // and it climbs, in a lane of its own
  assert.ok(r.rowOf.get('rework') > r.rowOf.get('qs'));
  assert.ok(r.backs[0].lane >= r.nFwdLanes);
});

test('a 2-cycle picks the arrow that climbs by depth, not by reachability', () => {
  // both edges are reachable-both-ways; only b -> a is the loop-back, since
  // `a` is the one the roots reach first
  const m = mkModel(['root', 'a', 'b'], [
    { from: 'root', to: 'a' },
    { from: 'a', to: 'b' },
    { from: 'b', to: 'a' }
  ]);
  assert.deepStrictEqual([...G.backEdges(m)], ['b>a']);
  const r = check(m, '2-cycle');
  assert.deepStrictEqual(r.rows.map((x) => x.node.id), ['root', 'a', 'b']);
});

test('two loops share a gutter lane only when their spans do not overlap', () => {
  const ids = ['a', 'b', 'c', 'd', 'e', 'f'];
  const links = [
    { from: 'a', to: 'b' }, { from: 'b', to: 'c' }, { from: 'c', to: 'd' },
    { from: 'd', to: 'e' }, { from: 'e', to: 'f' },
    { from: 'c', to: 'b' },   // loop over rows 1-2
    { from: 'f', to: 'e' }    // loop over rows 4-5, disjoint
  ];
  const r = check(mkModel(ids, links), 'disjoint loops');
  assert.strictEqual(r.backs.length, 2);
  assert.strictEqual(r.backs[0].lane, r.backs[1].lane);

  const nested = check(mkModel(ids, links.slice(0, 5).concat([
    { from: 'e', to: 'b' },   // rows 1-4
    { from: 'd', to: 'c' }    // rows 2-3, inside it
  ])), 'nested loops');
  assert.strictEqual(nested.backs.length, 2);
  assert.notStrictEqual(nested.backs[0].lane, nested.backs[1].lane);
});

test('a board with no loops is unchanged by the loop pass', () => {
  const r = check(chain(6), 'plain chain');
  assert.deepStrictEqual(r.back, []);
  assert.deepStrictEqual(r.backs, []);
  assert.strictEqual(r.nLanes, r.nFwdLanes);
});
