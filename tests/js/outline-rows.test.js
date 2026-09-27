/* Row geometry tests: `node --test tests/js/`.
 *
 * The rail draws a dot at each row's centre from `rowTops`, and the list
 * lays its rows out from the same numbers (margins and the band padding).
 * What must hold for the design system's block list: 34px rows with no space
 * between them, a stack's band 2px around its header and members, 4px of air
 * between a band and whatever is next to it, and the first row at the top.
 */
'use strict';

const test = require('node:test');
const assert = require('node:assert');

const G = require('../../inst/assets/js/outline-layout.js');
const R = require('../../inst/assets/js/outline-rail.js');

const mkModel = (blocks, links, stacks, collapsed) => ({
  blocks: blocks.map((id) => ({ id, name: id, inputs: ['data'], variadic: false })),
  links: links.map((l, i) => Object.assign({ id: 'l' + i, input: 'data' }, l)),
  stacks: stacks || [],
  collapsed: new Set(collapsed || []),
  lastPos: new Map()
});

// the harness board: two stacks, then two loose rows
const iris = (collapsed) => mkModel(
  ['data', 'filt', 'mut', 'summ', 'audit', 'plot', 'slide'],
  [
    { from: 'data', to: 'filt' }, { from: 'filt', to: 'mut' },
    { from: 'mut', to: 'summ' }, { from: 'mut', to: 'plot' },
    { from: 'mut', to: 'audit' }, { from: 'summ', to: 'slide' }
  ],
  [
    { id: 'prep', name: 'Data prep', blocks: ['data', 'filt', 'mut'] },
    { id: 'tables', name: 'Tables', blocks: ['summ', 'audit'] }
  ],
  collapsed
);

test('the defaults are the block list row', () => {
  const M = R.DEFAULT_METRICS;
  assert.strictEqual(M.ROW_H, 34);
  assert.strictEqual(M.GAP, 0);
  assert.strictEqual(M.STACK_PAD, 2);
  assert.strictEqual(M.STACK_GAP, 4);
});

test('loose rows sit back to back, the first at the top', () => {
  const rows = G.displayRows(mkModel(['a', 'b', 'c'],
    [{ from: 'a', to: 'b' }, { from: 'b', to: 'c' }]));
  const { tops, gaps, height } = R.rowTops(rows);
  assert.deepStrictEqual(tops, [0, 34, 68]);
  assert.deepStrictEqual(gaps, [0, 0, 0]);
  assert.strictEqual(height, 102);
});

test('a stack is a band: padding around its rows, air around the band', () => {
  const rows = G.displayRows(iris());
  assert.deepStrictEqual(rows.map((r) => r.t === 'node' ? r.node.id : r.t + ':' + r.stack.id),
    ['header:prep', 'data', 'filt', 'mut', 'header:tables', 'summ', 'audit', 'plot', 'slide']);
  const { tops, gaps, height } = R.rowTops(rows);
  // prep: 2px pad, four rows; then 2px pad + 4px air + 2px pad into tables;
  // then 2px pad + 4px air to the loose rows
  assert.deepStrictEqual(tops, [2, 36, 70, 104, 146, 180, 214, 254, 288]);
  assert.deepStrictEqual(gaps, [0, 0, 0, 0, 4, 0, 0, 4, 0]);
  assert.strictEqual(height, 322);
  // no two rows overlap, and inside a band they touch
  for (let i = 1; i < tops.length; i++) {
    assert.ok(tops[i] >= tops[i - 1] + 34, 'row ' + i + ' overlaps the one above');
  }
});

test('a folded stack is a band of one row', () => {
  const rows = G.displayRows(iris(['tables']));
  const i = rows.findIndex((r) => r.t === 'stack');
  const { tops } = R.rowTops(rows);
  // after prep's last member: 2px pad + 4px air + 2px pad
  assert.strictEqual(tops[i] - (tops[i - 1] + 34), 8);
  // and 2px pad + 4px air to the next loose row
  assert.strictEqual(tops[i + 1] - (tops[i] + 34), 6);
});

test('two folded stacks in a row keep 4px of air between their bands', () => {
  const rows = G.displayRows(iris(['prep', 'tables']));
  const { tops } = R.rowTops(rows);
  assert.strictEqual(rows[0].t, 'stack');
  assert.strictEqual(rows[1].t, 'stack');
  assert.strictEqual(tops[0], 2);
  assert.strictEqual(tops[1] - (tops[0] + 34), 2 + 4 + 2);
});

test('metrics a host passes win over the defaults', () => {
  const rows = G.displayRows(iris());
  const { tops } = R.rowTops(rows, { ROW_H: 40, STACK_PAD: 0, STACK_GAP: 0 });
  assert.deepStrictEqual(tops.slice(0, 3), [0, 40, 80]);
});
