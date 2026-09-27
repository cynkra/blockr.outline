/* Link model tests: `node --test tests/js/`.
 *
 * The rail's pure link functions (who can feed an input, what "Connect to…"
 * offers, the rename rules) and the board adapter's arity functions (free
 * inputs, the markers, what a link's menu can do to its input), held to the
 * design system's "Links in the outline".
 */
'use strict';

const test = require('node:test');
const assert = require('node:assert');

const R = require('../../inst/assets/js/outline-rail.js');
const B = require('../../inst/assets/js/outline.js');

// the harness board: a join with y free, a bind rows with two inputs
const blocks = [
  { id: 'data', name: 'Iris data', inputs: [] },
  { id: 'filt', name: 'Two species', inputs: ['data'] },
  { id: 'mut', name: 'Sepal ratio', inputs: ['data'] },
  { id: 'summ', name: 'Ratio by species', inputs: ['data'] },
  { id: 'join', name: 'Join lookup', inputs: ['x', 'y'] },
  { id: 'bind', name: 'Stack both', inputs: [], variadic: true }
];
const links = [
  { id: 'l1', from: 'data', to: 'filt', input: 'data' },
  { id: 'l2', from: 'filt', to: 'mut', input: 'data' },
  { id: 'l3', from: 'mut', to: 'summ', input: 'data' },
  { id: 'l4', from: 'mut', to: 'join', input: 'x' },
  { id: 'l5', from: 'filt', to: 'bind', input: '1' },
  { id: 'l6', from: 'mut', to: 'bind', input: '2' }
];
const blk = (id) => blocks.find((b) => b.id === id);
const slotsFor = (from, to) => B.freeSlots(to, links);
const ids = (xs) => xs.map((b) => b.id);

test('free slots: free named inputs, and a new one on a variadic block', () => {
  assert.deepStrictEqual(B.freeSlots(blk('join'), links), ['y']);
  assert.deepStrictEqual(B.freeSlots(blk('mut'), links), []);
  assert.deepStrictEqual(B.freeSlots(blk('bind'), links), ['']);
  assert.deepStrictEqual(B.freeSlots(blk('data'), links), []);
});

test('markers: a circle per free named input, the infinity sign for variadic', () => {
  assert.deepStrictEqual(B.inputMarkers(blk('join'), links), { inputs: ['y'], variadic: false });
  assert.deepStrictEqual(B.inputMarkers(blk('bind'), links), { inputs: [], variadic: true });
  // a linked input draws nothing, and a block with none free has no markers
  assert.strictEqual(B.inputMarkers(blk('mut'), links), null);
  assert.strictEqual(B.inputMarkers(blk('data'), links), null);
});

test('a link into a variadic block renames; the other inputs are taken', () => {
  const l5 = links.find((l) => l.id === 'l5');
  assert.deepStrictEqual(B.linkEdit(l5, blocks, links), { rename: true, taken: ['2'] });
});

test('a link into a fixed block moves to another free input, or cannot', () => {
  const l4 = links.find((l) => l.id === 'l4');
  assert.deepStrictEqual(B.linkEdit(l4, blocks, links), { move: ['y'] });
  const l3 = links.find((l) => l.id === 'l3');
  assert.strictEqual(B.linkEdit(l3, blocks, links), null);
});

test('insert before: the links into a block', () => {
  assert.deepStrictEqual(B.inputLinks('bind', links).map((l) => l.id), ['l5', 'l6']);
  assert.deepStrictEqual(B.inputLinks('data', links), []);
});

test('reach follows links down and up', () => {
  assert.deepStrictEqual([...R.linkReach(links, 'filt', true)].sort(),
    ['bind', 'join', 'mut', 'summ']);
  assert.deepStrictEqual([...R.linkReach(links, 'join', false)].sort(),
    ['data', 'filt', 'mut']);
});

test('feeders of a free input: not itself, nothing downstream', () => {
  // everything upstream or beside the join can feed its y
  assert.deepStrictEqual(ids(R.feedersFor(blocks, links, blk('join'), 'y', slotsFor)),
    ['data', 'filt', 'mut', 'summ', 'bind']);
  // a new input of bind: not the two already feeding it, not bind
  assert.deepStrictEqual(ids(R.feedersFor(blocks, links, blk('bind'), '', slotsFor)),
    ['data', 'summ', 'join']);
  // an input that is not free has no feeders
  assert.deepStrictEqual(R.feedersFor(blocks, links, blk('join'), 'x', slotsFor), []);
});

test('connect to: a free or new input, no cycle, no second link into a variadic', () => {
  // from data: join (y free) and bind (new input); the rest are full
  assert.deepStrictEqual(ids(R.targetsFor(blocks, links, blk('data'), slotsFor)),
    ['join', 'bind']);
  // from mut: bind already reads it, join's y is free
  assert.deepStrictEqual(ids(R.targetsFor(blocks, links, blk('mut'), slotsFor)),
    ['join']);
  // from the join: bind, not its own ancestors
  assert.deepStrictEqual(ids(R.targetsFor(blocks, links, blk('join'), slotsFor)),
    ['bind']);
});

test('an input name is refused when empty or taken', () => {
  assert.strictEqual(R.inputNameError('left', ['2']), '');
  assert.strictEqual(R.inputNameError('  ', ['2']), 'An input needs a name');
  assert.strictEqual(R.inputNameError(' 2 ', ['2']), 'Another input is called 2');
  assert.strictEqual(R.inputNameError(null, []), 'An input needs a name');
});
