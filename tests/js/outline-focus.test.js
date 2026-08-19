/* Stack-focus model tests: `npm test` (i.e. `node --test tests/js/`).
 *
 * `focusModel` narrows a board to one stack plus its interface. What must
 * hold: members stay (all of them, or exactly the searched ones), a
 * neighbour with an edge in or out survives as a collapsed stack, everything
 * untouched is gone, and no link is drawn unless at least one endpoint is a
 * member -- two neighbours' own relationship is not this view's business.
 * And whatever the narrowing produces, the rail invariants still hold,
 * because a focused view is drawn by the same geometry as the whole board.
 */
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const fs = require('node:fs');
const path = require('node:path');

const G = require('../../inst/assets/js/outline-layout.js');

const mkModel = (blocks, links, stacks, collapsed) => ({
  blocks: blocks.map((b) => typeof b === 'string'
    ? { id: b, name: b, inputs: ['data'], variadic: false }
    : b),
  links: links.map((l, i) => Object.assign({ id: 'l' + i, input: 'data' }, l)),
  stacks: stacks || [],
  collapsed: new Set(collapsed || []),
  lastPos: new Map()
});

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

const ids = (m) => m.blocks.map((b) => b.id).sort();

// A miniature of the CDEX shape: source data feeding domain stacks, domain
// tables collected by one loose downstream block, one stack that touches
// nothing.
const board = () => mkModel(
  ['adsl', 'adae', 'dm1', 'dm2', 'ae1', 'ae2', 'ae3', 'ex1', 'deck'],
  [
    { from: 'adsl', to: 'dm1' }, { from: 'dm1', to: 'dm2' },
    { from: 'adae', to: 'ae1' }, { from: 'adsl', to: 'ae1' },
    { from: 'ae1', to: 'ae2' }, { from: 'ae2', to: 'ae3' },
    { from: 'dm2', to: 'deck' }, { from: 'ae3', to: 'deck' }
  ],
  [
    { id: 'data', name: 'Source data', blocks: ['adsl', 'adae'] },
    { id: 'dm', name: 'Demographics', blocks: ['dm1', 'dm2'] },
    { id: 'ae', name: 'Adverse events', blocks: ['ae1', 'ae2', 'ae3'] },
    { id: 'ex', name: 'Exposure', blocks: ['ex1'] }
  ]
);

test('focus keeps the stack, its doorways, and nothing else', () => {
  const m = G.focusModel(board(), 'ae');
  // members whole; the data stack (feeds ae1) and the loose collector
  // (fed by ae3) stay; the demographics stack and the untouched one do not
  assert.deepStrictEqual(
    ids(m), ['adae', 'adsl', 'ae1', 'ae2', 'ae3', 'deck']
  );
  assert.deepStrictEqual(
    m.stacks.map((s) => s.id).sort(), ['ae', 'data']
  );
  // the neighbour is folded by the VIEW, not by the user's collapsed set
  assert.deepStrictEqual([...m.collapsed], ['data']);
});

test('focus cuts links to the stack interface', () => {
  const m = G.focusModel(board(), 'ae');
  const pairs = m.links.map((l) => l.from + '>' + l.to).sort();
  // dm2>deck is gone although deck is drawn: neither endpoint is a member
  assert.deepStrictEqual(pairs, [
    'adae>ae1', 'adsl>ae1', 'ae1>ae2', 'ae2>ae3', 'ae3>deck'
  ]);
});

test('the member filter narrows the stack but not the doorways', () => {
  const m = G.focusModel(board(), 'ae', new Set(['ae2']));
  const s = m.stacks.find((x) => x.id === 'ae');
  assert.deepStrictEqual(s.blocks, ['ae2']);
  // doorways survive a query that matches none of their blocks
  assert.ok(ids(m).includes('adsl'));
  assert.ok(ids(m).includes('deck'));
  // every remaining link touches a KEPT member; ae2's own edges went with
  // ae1 and ae3, so here that means none
  assert.deepStrictEqual(m.links, []);
});

test('focus on a missing stack is null, focus is drawable', () => {
  assert.strictEqual(G.focusModel(board(), 'nope'), null);
  const m = G.focusModel(board(), 'ae');
  const rows = G.displayRows(m);
  // the focused stack expands under its header; the neighbour is one row
  assert.ok(rows.some((r) => r.t === 'header' && r.stack.id === 'ae'));
  assert.ok(rows.some((r) => r.t === 'stack' && r.stack.id === 'data'));
  check(m, 'focused ae');
  check(G.focusModel(board(), 'data'), 'focused data');
  check(G.focusModel(board(), 'ex'), 'focused ex (island)');
});

test('CDEX-sized focus keeps the invariants', (t) => {
  const raw = JSON.parse(
    fs.readFileSync(path.join(__dirname, 'fixtures', 'cdex.json'), 'utf8')
  );
  // the fixture carries no stacks, so cut two from the drawn row order --
  // contiguous runs of real flow, exactly what "Stack them" would produce
  const base = mkModel(raw.blocks, raw.links, []);
  const order = G.railFor(base).rows
    .filter((r) => r.t === 'node').map((r) => r.node.id);
  const stacks = [
    { id: 'sA', name: 'A', blocks: order.slice(10, 18) },
    { id: 'sB', name: 'B', blocks: order.slice(30, 41) }
  ];
  const model = mkModel(raw.blocks, raw.links, stacks);
  ['sA', 'sB'].forEach((sid) => {
    const m = G.focusModel(model, sid);
    const r = check(m, 'cdex focused ' + sid);
    t.diagnostic('cdex focus ' + sid + ': ' + m.blocks.length + ' of ' +
      raw.blocks.length + ' blocks, ' + r.nLanes + ' lanes');
    assert.ok(m.blocks.length < raw.blocks.length / 2,
      'focus left ' + m.blocks.length + ' of ' + raw.blocks.length);
  });
});
