#!/usr/bin/env node
/* Minidag interaction-performance suite: drives a LIVE app with Playwright
 * and holds every gesture to its render budget.
 *
 *   node dev/minidag-perf.js [url] [--mutate]
 *
 * The one expensive operation in the rail is a full deck rebuild (rows +
 * SVG rail), and every rebuild inserts exactly one `svg.md-rail` -- so a
 * MutationObserver counting those IS the render counter, and "no extra
 * loops" becomes a set of exact assertions: most gestures re-render once,
 * hover and selection re-render zero times. On top of that: PerformanceObserver
 * long tasks (main-thread stalls > 50ms), wall-clock latency to the first
 * render, and a leak probe (repeated focus cycles must not slow down).
 *
 * The default run is MUTATION-FREE (safe against a board you care about);
 * --mutate adds one real append-while-focused, which changes the board.
 *
 * Companion plan: dev/minidag-perf-plan.md. Needs a Chromium at $CHROME or
 * /usr/lib/chromium/chromium and playwright-core resolvable (falls back to
 * the playwright-mcp npx cache in the devcontainer).
 */
'use strict';

const URL = process.argv.find((a) => a.startsWith('http')) ||
  'http://127.0.0.1:3843';
const MUTATE = process.argv.includes('--mutate');

const pwPath = () => {
  const cands = [
    'playwright-core',
    '/home/dev/.npm/_npx/9833c18b2d85bc59/node_modules/playwright-core'
  ];
  for (const c of cands) {
    try { return require(c); } catch (e) { /* next */ }
  }
  throw new Error('playwright-core not found; npm i -g playwright-core');
};

(async () => {
  const pw = pwPath();
  const browser = await pw.chromium.launch({
    executablePath: process.env.CHROME || '/usr/lib/chromium/chromium',
    headless: true,
    args: ['--no-sandbox', '--disable-dev-shm-usage']
  });
  const page = await browser.newPage({ viewport: { width: 1600, height: 1000 } });
  const pageErrors = [];
  page.on('pageerror', (e) => pageErrors.push(String(e)));

  // Instrumentation from time zero, so BOOT renders are counted too.
  await page.addInitScript(() => {
    window.__mdPerf = { renders: 0, longTasks: [] };
    new MutationObserver((muts) => {
      for (const m of muts) {
        for (const n of m.addedNodes) {
          if (n.nodeType === 1 && n.matches && n.matches('svg.md-rail')) {
            window.__mdPerf.renders++;
          }
        }
      }
    // `document` itself, not documentElement: an init script runs before
    // the root element exists, and document is already a valid Node.
    }).observe(document, { childList: true, subtree: true });
    try {
      new PerformanceObserver((l) => {
        l.getEntries().forEach((e) => window.__mdPerf.longTasks.push({
          d: Math.round(e.duration), t: Math.round(e.startTime)
        }));
      }).observe({ entryTypes: ['longtask'] });
    } catch (e) { /* longtask unsupported: maxLong stays 0 */ }
  });

  await page.goto(URL);
  await page.waitForSelector('.minidag .md-stackhead',
    { state: 'attached', timeout: 120000 });
  await page.waitForTimeout(2500);
  const tab = await page.$('.dv-tab.dv-inactive-tab:has-text("Minidag")');
  if (tab) await tab.click();
  await page.waitForSelector('.minidag .md-stackhead',
    { state: 'visible', timeout: 20000 });
  await page.waitForTimeout(1500);

  const results = [];

  // Run one gesture, then wait until the render counter is quiet.
  const act = async (label, expected, fn, opts = {}) => {
    const settle = opts.settle || 400, timeout = opts.timeout || 10000;
    const b = await page.evaluate(() => ({
      r: __mdPerf.renders, l: __mdPerf.longTasks.length
    }));
    const t0 = Date.now();
    await fn();
    let last = b.r, lastChange = Date.now(), first = null;
    while (Date.now() - t0 < timeout) {
      await page.waitForTimeout(60);
      const r = await page.evaluate(() => __mdPerf.renders);
      if (r !== last) {
        if (first === null) first = Date.now() - t0;
        last = r;
        lastChange = Date.now();
      }
      if (Date.now() - lastChange > settle) break;
    }
    const a = await page.evaluate(() => ({
      r: __mdPerf.renders, lt: __mdPerf.longTasks
    }));
    const lts = a.lt.slice(b.l).map((x) => x.d);
    results.push({
      label,
      renders: a.r - b.r,
      expected,
      firstMs: first,
      maxLong: lts.length ? Math.max(...lts) : 0
    });
  };

  const $head = (name) => page.locator('.minidag .md-stackhead', {
    has: page.locator('.md-name', { hasText: name })
  }).first();
  const clickFocusBtn = (name) => page.evaluate((nm) => {
    const h = [...document.querySelectorAll('.minidag .md-stackhead')]
      .find((x) => x.querySelector('.md-name').textContent === nm);
    h.querySelector('.md-focusbtn').click();
  }, name);

  // Boot budget: model + registry-glyph redraw = 2 (a third can arrive when
  // the first badge lands before the glyphs); more means redundant pushes.
  const boot = await page.evaluate(() => __mdPerf.renders);
  results.push({ label: 'boot (page load)', renders: boot, expected: [1, 3],
    firstMs: null, maxLong: 0 });

  // Hover lineage: class painting only, NEVER a rebuild.
  await act('hover a row (lineage dim)', 0, async () => {
    const row = page.locator('.minidag .md-chip[data-id]').nth(5);
    await row.hover();
    await page.waitForTimeout(500);
    await page.mouse.move(10, 10);
  }, { settle: 300 });

  // Selection: class painting only.
  await act('ctrl-click select 3 rows', 0, async () => {
    for (let i = 2; i <= 4; i++) {
      await page.locator('.minidag .md-chip[data-id]').nth(i)
        .click({ modifiers: ['Control'] });
    }
  }, { settle: 300 });
  await act('clear selection (bar)', 0, () =>
    page.evaluate(() => {
      const b2 = document.querySelector('.minidag .md-actionbar .md-no');
      if (b2) b2.click();
    }), { settle: 300 });

  // Search: one rebuild per keystroke, one for the clear.
  for (const ch of ['l', 'a', 'b']) {
    await act('search keystroke "' + ch + '"', 1, () =>
      page.type('.minidag .md-search', ch), { settle: 350 });
  }
  await act('search Esc (clear query)', 1, () =>
    page.press('.minidag .md-search', 'Escape'), { settle: 350 });

  // Focus lifecycle.
  await act('focus enter (button)', 1, () => clickFocusBtn('Laboratory'));
  await act('search-within keystroke', 1, () =>
    page.type('.minidag .md-search', 'a'), { settle: 350 });
  await act('search-within Esc', 1, () =>
    page.press('.minidag .md-search', 'Escape'), { settle: 350 });
  await act('hop via doorway row', 1, () => page.evaluate(() => {
    document.querySelector('.minidag .md-chip.md-hop').click();
  }));
  await act('focus exit (pill x)', 1, () =>
    page.click('.minidag .md-crumb-x'));
  await act('focus enter (dblclick header)', 1, () =>
    $head('Laboratory').dblclick({ position: { x: 8, y: 12 } }));
  await act('focus exit (Esc on body)', 1, async () => {
    await page.evaluate(() =>
      document.activeElement && document.activeElement.blur());
    await page.keyboard.press('Escape');
  });

  // Fold.
  await act('fold all', 1, () => page.click('.minidag .md-fold'));
  await act('unfold all', 1, () => page.click('.minidag .md-fold'));
  await act('collapse one (chevron)', 1, () => page.evaluate(() => {
    document.querySelector('.minidag .md-stackhead .md-chev').click();
  }));
  await act('expand one (chevron)', 1, () => page.evaluate(() => {
    document.querySelector('.minidag .md-stackchip .md-chev').click();
  }));

  // ToC pick: fold-all -> dblclick a collapsed row = focus AND fold-clear;
  // one rebuild for the fold, one for the focus.
  await act('fold all (again)', 1, () => page.click('.minidag .md-fold'));
  await act('ToC dblclick -> focus', 1, () =>
    page.locator('.minidag .md-stackchip').first()
      .dblclick({ position: { x: 200, y: 12 } }));
  await act('exit -> full list', 1, () => page.click('.minidag .md-crumb-x'));

  // A plain row click reveals the block's panel: the board answers with a
  // model push, so ONE rebuild is the design (informational budget <= 2).
  // The push can lag by SECONDS on a heavy board -- the revealed panel
  // evaluates first -- so the settle here is generous, or the tail lands in
  // the idle measurement below and reads as a phantom render.
  await act('row click (reveal panel)', [0, 2], () =>
    page.locator('.minidag .md-chip[data-id]').nth(3).click(),
    { settle: 2500, timeout: 15000 });

  // Leak / degradation probe: 30 focus cycles; late cycles must not be
  // slower than early ones (would mean leaked observers or listeners).
  const cycle = [];
  for (let i = 0; i < 30; i++) {
    const t0 = Date.now();
    await clickFocusBtn('Laboratory');
    await page.waitForSelector('.minidag .md-crumb-cur', { timeout: 5000 });
    await page.click('.minidag .md-crumb-x');
    await page.waitForSelector('.minidag .md-crumb[hidden]',
      { state: 'attached', timeout: 5000 });
    cycle.push(Date.now() - t0);
  }
  const avg = (xs) => Math.round(xs.reduce((a, b2) => a + b2, 0) / xs.length);
  const early = avg(cycle.slice(0, 5)), late = avg(cycle.slice(-5));
  results.push({ label: '30x focus cycle early->late ms', renders: null,
    expected: null, firstMs: early + ' -> ' + late,
    maxLong: late <= early * 1.6 ? 0 : late });

  // Idle: nothing may render on its own. Preceded by a quiet-down beat so a
  // straggling roundtrip from earlier gestures is not booked as idle noise.
  await page.waitForTimeout(3000);
  await act('idle 5s', 0, () => page.waitForTimeout(5000),
    { settle: 100, timeout: 5600 });

  if (MUTATE) {
    await act('append while focused (MUTATES)', [1, 2], async () => {
      await clickFocusBtn('Laboratory');
      await page.waitForTimeout(400);
      const row = page.locator('.minidag .md-chip.instack').nth(2);
      await row.hover();
      await row.locator('.md-rowadd').click();
      await page.waitForSelector('.minidag .md-pick-row', { timeout: 8000 });
      await page.evaluate(() =>
        document.querySelector('.minidag .md-pick-row').click());
    }, { settle: 1200, timeout: 20000 });
    await page.click('.minidag .md-crumb-x');
  }

  /* ---- report ---- */
  const inBudget = (r) => {
    if (r.expected == null || r.renders == null) return true;
    const [lo, hi] = Array.isArray(r.expected)
      ? r.expected : [r.expected, r.expected];
    return r.renders >= lo && r.renders <= hi;
  };
  let fail = 0;
  console.log('\n%s | %s | %s | %s | %s', 'gesture'.padEnd(34),
    'renders', 'budget', 'first ms', 'long task');
  console.log('-'.repeat(78));
  for (const r of results) {
    const ok = inBudget(r) && (r.maxLong || 0) < 200;
    if (!ok) fail++;
    console.log('%s | %s | %s | %s | %s %s',
      r.label.padEnd(34),
      String(r.renders == null ? '-' : r.renders).padEnd(7),
      String(r.expected == null ? '-'
        : Array.isArray(r.expected) ? r.expected.join('-') : r.expected).padEnd(6),
      String(r.firstMs == null ? '-' : r.firstMs).padEnd(8),
      String(r.maxLong || '-').padEnd(6),
      ok ? '' : '  <-- OVER BUDGET');
  }
  if (pageErrors.length) {
    fail++;
    console.log('\nJS errors:', JSON.stringify(pageErrors));
  }
  console.log('\n' + (fail ? fail + ' OVER BUDGET' : 'all within budget'));
  await browser.close();
  process.exit(fail ? 1 : 0);
})().catch((e) => { console.error('FAIL', e); process.exit(2); });
