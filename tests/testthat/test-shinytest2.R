# Real browser-driven e2e tests, modeled on blockr.viz / blockr.dock. ONE
# shared board app (apps/outline-e2e) is launched once and driven through a
# headless chromium with shinytest2. We exercise the client-side contract
# end to end:
#
#   * the outline renders (chips, chapters, the excluded-block marker),
#   * the report toggle: the custom-message input the switch JS emits ->
#     extension state -> the projection reflects it (no board mutation),
#   * the R-script and Document (qmd) views render the generated code with
#     server-side highlighting,
#   * the Render download produces a self-contained html report.
#
# Reactive/pure logic is covered without a browser by the test-ext-server-* and
# test-export-* files; this layer adds the real browser + custom-message +
# render path.
#
# Skipped where no headless browser exists, and under R CMD check (launching
# chromium there leaves temp detritus and is flaky). Runs under
# devtools::test() / CI, where the JS is meant to be exercised.

library(shinytest2)

# ---------------------------------------------------------------------------
# Shared app instance. Built once at file load; guarded so that with no browser
# (or under R CMD check) `app` stays NULL and every test skips.
# ---------------------------------------------------------------------------
run_browser <-
  !nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")) &&
  requireNamespace("shinytest2", quietly = TRUE) &&
  requireNamespace("chromote", quietly = TRUE) &&
  chromote_works()

app <- NULL
if (run_browser) {
  # shinytest2 refuses to run "On CRAN"; serve()'s app trips that guard unless
  # NOT_CRAN is set. We are explicitly not on CRAN here.
  Sys.setenv(NOT_CRAN = "true")
  configure_chromote()

  app <- tryCatch(
    AppDriver$new(
      test_path("apps", "outline-e2e"),
      name         = "outline-e2e",
      load_timeout = 60 * 1000,
      timeout      = 25 * 1000
    ),
    error = function(e) {
      message("outline-e2e app launch failed: ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(app)) {
    app$wait_for_idle()
    withr::defer(app$stop(), testthat::teardown_env())
  }
}

skip_if_no_app <- function() {
  testthat::skip_if(is.null(app), "outline-e2e browser app unavailable")
}

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# The extension namespaces its inputs/outputs under `board-ext_outline-`.
ext <- function(x) paste0("board-ext_outline-", x)

# Emit a custom-message input the way the outline JS does (priority: event),
# then let the server settle.
send <- function(id, payload) {
  json <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  app$run_js(sprintf(
    "Shiny.setInputValue('%s', %s, {priority: 'event'});", ext(id), json
  ))
  app$wait_for_idle()
}

# Switch the outline/script/qmd view (a radioGroupButtons). The view swap
# re-renders in place, which shinytest2's set_inputs output-diff does not
# reliably register, so drive it without waiting for an output and settle
# explicitly.
set_view <- function(view) {
  app$set_inputs(!!ext("code_view") := view, wait_ = FALSE)
  app$wait_for_idle()
}

count <- function(selector) {
  app$get_js(sprintf("document.querySelectorAll('%s').length", selector))
}

pre_text <- function() {
  app$get_js(sprintf(
    "(document.getElementById('%s') || {}).innerText || ''", ext("code_pre")
  ))
}

# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------

test_that("the outline lists the report blocks; the rest are searchable", {
  skip_if_no_app()
  set_view("outline")

  # `audit` is report = FALSE -> not listed, offered by the search box.
  expect_equal(count(".blockr-otl-chip"), 3)
  expect_equal(count(".blockr-otl-offchip"), 0)
  expect_equal(count(".blockr-otl-search"), 1)

  # The pool count sits in the control, before anything is typed.
  expect_match(
    app$get_js("document.querySelector('.blockr-otl-searchcount').textContent"),
    "not in report"
  )

  # Both chapter headings are present (Outputs keeps its listed member).
  body <- app$get_js("document.body.innerText")
  expect_match(body, "Data prep")
  expect_match(body, "Outputs")
})

test_that("the R script view renders the generated code", {
  skip_if_no_app()
  set_view("script")
  txt <- pre_text()
  expect_match(txt, "data <- ")
  # audit is excluded and nothing reported depends on it -> pruned from the
  # document entirely (its code must not run at render). The outline view
  # still shows it (the offchip assertion above).
  expect_no_match(txt, "audit")
  set_view("outline")
})

test_that("the Document view renders the qmd with the board title", {
  skip_if_no_app()
  set_view("qmd")
  txt <- pre_text()
  expect_match(txt, "Iris e2e report")   # the document title
  expect_match(txt, "label:")            # chunk labels
  set_view("outline")
})

test_that("the report toggle lists and unlists without a board update", {
  skip_if_no_app()
  set_view("outline")
  expect_equal(count(".blockr-otl-chip"), 3)

  # Include `audit` in the report: its row appears.
  send("outline_toggle", list(id = "audit", report = TRUE))
  expect_equal(count(".blockr-otl-chip"), 4)
  expect_equal(count(".blockr-otl-offchip"), 0)

  # Restore the fixture state for any later test / re-run.
  send("outline_toggle", list(id = "audit", report = FALSE))
  expect_equal(count(".blockr-otl-chip"), 3)
})

test_that("otl_include lists a block from the pool", {
  skip_if_no_app()
  set_view("outline")
  expect_equal(count(".blockr-otl-chip"), 3)

  # The input the search menu writes when a pool entry is chosen.
  send("otl_include", "audit")
  expect_equal(count(".blockr-otl-chip"), 4)

  send("outline_toggle", list(id = "audit", report = FALSE))
  expect_equal(count(".blockr-otl-chip"), 3)
})

test_that("Render downloads a self-contained html report", {
  skip_if_no_app()
  skip_if_not(
    rmarkdown::pandoc_available() || nzchar(Sys.which("quarto")),
    "no render engine (quarto / pandoc) available"
  )
  set_view("outline")
  app$set_inputs(!!ext("code_render_format") := "html", wait_ = FALSE)
  app$wait_for_idle()

  path <- tryCatch(
    app$get_download(ext("code_render")),
    error = function(e) {
      testthat::skip(paste("render download unavailable:", conditionMessage(e)))
    }
  )
  expect_true(file.exists(path))
  expect_gt(file.info(path)$size, 0)
  # A self-contained html document.
  head_txt <- paste(readLines(path, n = 40, warn = FALSE), collapse = "\n")
  expect_match(head_txt, "<!DOCTYPE html>|<html", ignore.case = TRUE)
})

# ---------------------------------------------------------------------------
# Interactivity: the delegated click handlers must actually be ATTACHED. The
# tests above emit their inputs with send() (Shiny.setInputValue directly),
# which bypasses the JS click handlers entirely -- so a JS error that aborts
# outline_js before the handlers attach passes every one of them while the
# real outline is inert. These tests click real DOM nodes and assert the
# resulting input fires. Regression guard for: the visibility poll calling
# Shiny.setInputValue at DOM ready, before Shiny was up, throwing and taking
# every later handler (gear, drag, OPEN / ADD / chapter) down with it.

# Capture every Shiny.setInputValue name fired after `expr` runs on the client.
fired_inputs <- function(click_js) {
  app$run_js(paste0(
    "window._otlFired = [];",
    "if (!window._otlHooked) {",
    "  window._otlHooked = true;",
    "  var orig = Shiny.setInputValue;",
    "  Shiny.setInputValue = function(n, v, o) {",
    "    if (window._otlFired) window._otlFired.push(n);",
    "    return orig.apply(this, arguments);",
    "  };",
    "}"
  ))
  app$run_js(click_js)
  Sys.sleep(0.6)          # the OPEN handler debounces 250ms
  app$wait_for_idle()
  app$get_js("JSON.stringify(window._otlFired)")
}

test_that("clicking a block chip fires outline_open (handlers attached)", {
  skip_if_no_app()
  set_view("outline")
  fired <- fired_inputs("document.querySelector('.blockr-otl-chip').click();")
  expect_match(fired, "outline_open")
})

test_that("clicking the report switch fires outline_toggle", {
  skip_if_no_app()
  set_view("outline")
  fired <- fired_inputs(
    "document.querySelector('.blockr-otl-sw').click();"
  )
  expect_match(fired, "outline_toggle")
  app$wait_for_idle()
  # Put the flipped block back so later re-runs start from the fixture.
  set_view("outline")
})

test_that("the gear button toggles the settings band (client-only handler)", {
  skip_if_no_app()
  set_view("outline")
  # The gear handler sits AFTER the visibility poll in outline_js; if the
  # poll threw, this handler never attached and the class never toggles.
  band_open <- function() {
    app$get_js(paste0(
      "document.getElementById('", ext("otl_settings"), "')",
      ".classList.contains('blockr-settings--open')"
    ))
  }
  before <- band_open()
  app$run_js(sprintf(
    "document.getElementById('%s').click();", ext("otl_gear")
  ))
  app$wait_for_idle()
  after <- band_open()
  expect_false(isTRUE(before))
  expect_true(isTRUE(after))
})

test_that("the chevron collapses a chapter; the title does not", {
  skip_if_no_app()
  set_view("outline")

  # Chevrons only render on grouped chapters; skip if the fixture has none.
  n_chev <- app$get_js("document.querySelectorAll('.blockr-otl-chevwrap').length")
  skip_if(identical(n_chev, 0), "no grouped chapter in the fixture")

  collapsed <- function() {
    app$get_js(paste0(
      "document.querySelector('.blockr-otl-chevwrap')",
      ".closest('.blockr-otl-chap').classList.contains('collapsed')"
    ))
  }

  # Clicking the title (rename target) must NOT toggle collapse.
  app$run_js("document.querySelector('.blockr-otl-chlabel').click();")
  app$wait_for_idle()
  expect_false(isTRUE(collapsed()))

  # Clicking the chevron collapses immediately (no debounce).
  app$run_js("document.querySelector('.blockr-otl-chevwrap').click();")
  app$wait_for_idle()
  expect_true(isTRUE(collapsed()))

  # And toggles back.
  app$run_js("document.querySelector('.blockr-otl-chevwrap').click();")
  app$wait_for_idle()
  expect_false(isTRUE(collapsed()))
})

test_that("Exclude all / Include all flip every block's report flag", {
  skip_if_no_app()
  set_view("outline")

  chips <- function() {
    app$get_js("document.querySelectorAll('.blockr-otl-chip').length")
  }

  # Exclude all -> the whole list empties into the picker.
  app$run_js("document.querySelector('.blockr-otl-bulk[data-bulk=\"exclude\"]').click();")
  app$wait_for_idle()
  expect_equal(chips(), 0)
  expect_equal(count(".blockr-otl-emptydoc"), 1)

  # Include all -> every block listed.
  app$run_js("document.querySelector('.blockr-otl-bulk[data-bulk=\"include\"]').click();")
  app$wait_for_idle()
  expect_equal(chips(), 4)

  # Restore the fixture (audit is report = FALSE by default).
  send("outline_toggle", list(id = "audit", report = FALSE))
  expect_equal(chips(), 3)
})

test_that("the report title is shown and renames in place", {
  skip_if_no_app()
  set_view("outline")

  # The document title heads the column.
  expect_equal(
    app$get_js("document.querySelector('.blockr-otl-doctitle').textContent"),
    "Iris e2e report"
  )

  # Double-click -> inline editor seeded with the current title.
  app$run_js(paste0(
    "document.querySelector('.blockr-otl-doctitle')",
    ".dispatchEvent(new MouseEvent('dblclick', {bubbles:true}));"
  ))
  editor <- app$get_js(
    "!!document.querySelector('.blockr-otl-doctitle-row input')"
  )
  expect_true(isTRUE(editor))

  # Commit a new title; it re-renders from state.
  app$run_js(paste0(
    "var i = document.querySelector('.blockr-otl-doctitle-row input');",
    "i.value = 'Renamed report';",
    "i.dispatchEvent(new KeyboardEvent('keydown', {key:'Enter', bubbles:true}));",
    "i.blur();"
  ))
  app$wait_for_idle()
  expect_equal(
    app$get_js("document.querySelector('.blockr-otl-doctitle').textContent"),
    "Renamed report"
  )

  # The document view reflects the new title (proves it is real state).
  set_view("qmd")
  expect_match(pre_text(), "Renamed report")
  set_view("outline")
})

test_that("the search box lists the document and the pool in one menu", {
  skip_if_no_app()
  set_view("outline")

  entries <- function() {
    jsonlite::fromJSON(app$get_js(paste0(
      "JSON.stringify(Array.from(",
      "document.querySelectorAll('.blockr-otl-searchmenu ",
      ".blockr-block-browser-card'))",
      ".map(function(o){return o.dataset.blk + ':' + ",
      "(o.dataset.listed === '1' ? 'in' : 'out');}))"
    )))
  }
  type <- function(q) {
    app$run_js(paste0(
      "var s = document.querySelector('.blockr-otl-searchinput');",
      "s.focus(); s.value = '", q, "';",
      "s.dispatchEvent(new Event('input', {bubbles:true}));"
    ))
    app$wait_for_idle()
  }

  # Focus with no query opens on the WHOLE board: three listed blocks plus
  # the excluded one, each labelled by what choosing it would do.
  type("")
  all4 <- entries()
  expect_setequal(all4, c("data:in", "sub:in", "plot:in", "audit:out"))
  # Listed first, pool after.
  expect_identical(all4[[4L]], "audit:out")

  # Typing narrows across both groups (audit is a head block named "Head").
  type("setosa")
  expect_identical(entries(), "sub:in")

  # Escape clears the query, the menu is back on the whole board.
  app$run_js(paste0(
    "var s = document.querySelector('.blockr-otl-searchinput');",
    "s.dispatchEvent(new KeyboardEvent('keydown', ",
    "{key: 'Escape', bubbles: true}));"
  ))
  app$wait_for_idle()
  expect_equal(
    app$get_js("document.querySelector('.blockr-otl-searchinput').value"), ""
  )
  expect_length(entries(), 4)
})

test_that("choosing a pool entry adds it to the report, menu stays open", {
  skip_if_no_app()
  set_view("outline")

  app$run_js(paste0(
    "var s = document.querySelector('.blockr-otl-searchinput');",
    "s.focus(); s.dispatchEvent(new Event('input', {bubbles:true}));"
  ))
  app$wait_for_idle()

  # Click the pool entry (mousedown is what the handler listens for).
  app$run_js(paste0(
    "document.querySelector('.blockr-otl-searchmenu ",
    ".blockr-block-browser-card[data-blk=\"audit\"]')",
    ".dispatchEvent(new MouseEvent('mousedown', {bubbles: true}));"
  ))
  app$wait_for_idle()

  # It is a row now, and the menu is still open with the entry moved into
  # the first group.
  expect_equal(count(".blockr-otl-chip"), 4)
  expect_true(app$get_js(
    "document.querySelector('.blockr-otl-search').classList.contains('open')"
  ))
  expect_identical(app$get_js(paste0(
    "document.querySelector('.blockr-otl-searchmenu ",
    ".blockr-block-browser-card[data-blk=\"audit\"]').dataset.listed"
  )), "1")

  # Put the board back the way the other tests expect it.
  send("outline_toggle", list(id = "audit", report = FALSE))
  app$run_js(
    "document.querySelector('.blockr-otl-searchinput').blur();"
  )
  app$wait_for_idle()
  expect_equal(count(".blockr-otl-chip"), 3)
})
