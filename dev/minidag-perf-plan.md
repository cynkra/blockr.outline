# Minidag interaction-performance plan

`dev/minidag-perf.js` drives a live app with Playwright and holds every
gesture to a **render budget**. Run it after any change to
`inst/assets/js/minidag-rail.js`, `minidag.js` or `minidag-layout.js`:

```sh
# a board with stacks must be running; either of:
Rscript blockr.outline/dev/minidag-demo.R 3843          # small, 3 stacks
Rscript _scratch/minidag-focus/run-cdex-stacks.R 3843   # real 92-block CDEX

node dev/minidag-perf.js http://127.0.0.1:3843           # mutation-free
node dev/minidag-perf.js http://127.0.0.1:3843 --mutate  # + one real append
```

Exit code 0 = all within budget. CI-able in principle; today it is a
pre-commit habit for rail work.

## Method

The rail has exactly one expensive operation: the full deck rebuild
(rows + SVG rail). Every rebuild inserts exactly one `svg.md-rail`, so a
`MutationObserver` counting those insertions **is** the render counter, and
"no extra loops" becomes a set of exact assertions instead of a feeling.
On top of that:

- **PerformanceObserver long tasks** — main-thread stalls; anything ≥ 200ms
  attributed to a gesture fails it.
- **Wall-clock to first render** — dispatch of the gesture until the render
  counter moves.
- **Leak probe** — 30 focus/unfocus cycles; the last five must not be
  slower than 1.6× the first five (leaked listeners/observers show up as a
  drift).
- **Idle watch** — 5 s untouched, zero renders allowed, preceded by a
  quiet-down beat so a straggling server roundtrip is not booked as idle
  noise.

The instrumentation is installed via `addInitScript` observing `document`
(NOT `documentElement`, which does not exist yet at init-script time), so
boot renders are counted too.

## Budgets and why

| gesture | budget | why |
| --- | --- | --- |
| boot | 1–3 | model push, registry-glyph redraw, and possibly one early badge-before-glyphs redraw |
| hover a row (lineage dim) | 0 | class painting only, by design |
| select / clear selection | 0 | class painting + action-bar update; a rebuild here is a regression (the Clear button had exactly this bug, found by this suite 2026-08-19) |
| search keystroke | 1 | each keystroke narrows the drawn set once; no debounce, so 1 per char is the contract |
| search clear (Esc) | 1 | back to the full set |
| focus enter / exit / hop (any entry point) | 1 | one view change, one rebuild — a second render means a redundant push |
| fold all / unfold / single collapse / expand | 1 | same |
| ToC dblclick → focus | 1 | the fold-clear and the focus land in ONE render |
| row click (reveal panel) | 0–2 | the board answers a reveal with a model push (by design); it can lag by seconds on a heavy board while the panel evaluates — informational |
| append while focused (`--mutate`) | 1–2 | one insert+join board update, plus the panel/view push |
| idle 5 s | 0 | nothing renders on its own |

Latency is reported, not asserted: on the 92-block CDEX board the
2026-08-19 baseline is **70–160 ms** to first render for every gesture and
zero long tasks; treat a jump past ~300 ms as a finding even though the
suite will not fail on it.

## Baseline (2026-08-19, CDEX 92 blocks / 92 links / 10 stacks)

All gestures exactly on budget; first-render 70–160 ms; no long tasks;
focus cycles 99 → 83 ms over 30 iterations (no drift); idle silent; append
while focused: 2 renders, first at ~570 ms.

## Findings log

- **2026-08-19 — Clear-selection full rebuild.** The action bar's "Clear"
  called `render()` — a full 92-row rebuild to remove CSS classes. Every
  other selection change paints. Fixed in the same commit as this suite.
- **2026-08-19 — reveal push tail.** A plain row click's model push can
  arrive seconds later (the revealed panel evaluates first). Not a bug, but
  a suite trap: it must settle long enough or the tail lands in the idle
  measurement.

## Extending

A new gesture gets a row here first, with a budget and a why. If a gesture
genuinely needs two renders, the second one needs a sentence justifying
it — "conceptually separate" is not a reason, and unexplained budget bumps
are how extra loops creep back in.
