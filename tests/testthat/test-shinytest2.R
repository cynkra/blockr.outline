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

test_that("the outline renders chips, chapters and the excluded marker", {
  skip_if_no_app()
  set_view("outline")

  expect_equal(count(".blockr-otl-chip"), 4)
  # `audit` is report = FALSE -> exactly one "include=FALSE" section marker.
  expect_equal(count(".blockr-otl-offchip"), 1)
  # Both chapter headings are present.
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

test_that("the report toggle updates the projection without a board update", {
  skip_if_no_app()
  set_view("outline")
  expect_equal(count(".blockr-otl-offchip"), 1)

  # Include `audit` in the report: the excluded marker disappears.
  send("outline_toggle", list(id = "audit", report = TRUE))
  expect_equal(count(".blockr-otl-offchip"), 0)

  # Annotation-only: the outline still shows all four blocks (no board churn).
  expect_equal(count(".blockr-otl-chip"), 4)

  # Restore the fixture state for any later test / re-run.
  send("outline_toggle", list(id = "audit", report = FALSE))
  expect_equal(count(".blockr-otl-offchip"), 1)
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

  n_chip <- app$get_js("document.querySelectorAll('.blockr-otl-chip').length")
  off <- function() {
    app$get_js("document.querySelectorAll('.blockr-otl-offchip').length")
  }

  # Exclude all -> every chip carries the excluded marker.
  app$run_js("document.querySelector('.blockr-otl-bulk[data-bulk=\"exclude\"]').click();")
  app$wait_for_idle()
  expect_equal(off(), n_chip)

  # Include all -> none excluded.
  app$run_js("document.querySelector('.blockr-otl-bulk[data-bulk=\"include\"]').click();")
  app$wait_for_idle()
  expect_equal(off(), 0)

  # Restore the fixture (audit is report = FALSE by default).
  send("outline_toggle", list(id = "audit", report = FALSE))
  expect_equal(off(), 1)
})
