skip_if_no_renderer <- function() {
  testthat::skip_if_not(
    blockr.outline:::quarto_usable() || rmarkdown::pandoc_available(),
    "no quarto CLI and no pandoc"
  )
}

qmd_doc <- function(body) {
  paste(c("---", "title: \"T\"", "---", "", "```{r}", body, "```"),
        collapse = "\n")
}

test_that("a failed render names the cause instead of the wrapper's noise", {
  skip_if_no_renderer()

  # The bug this covers: render_err() called showNotification() unguarded. A
  # download handler can run without a reactive domain, so that call threw IN
  # PLACE OF the render error and the browser got a bare "an error has
  # occurred" with the real cause nowhere. Called here with no Shiny session
  # at all, which is the same shape.
  err <- tryCatch(
    render_report(qmd_doc("stop(\"the actual cause\")"), "", "html",
                  tempfile(fileext = ".html"), "T"),
    error = conditionMessage
  )

  expect_match(err, "Report render failed")
  expect_match(err, "the actual cause")
})

test_that("render_logged restores sinks exactly, on success and on failure", {
  # A dangling sink would silence every message the app writes afterwards,
  # which is worse than the render failure that caused it.
  before <- c(sink.number(), sink.number(type = "message"))

  ok <- render_logged(cat("hello\n"))
  expect_null(ok$error)
  expect_true(any(grepl("hello", ok$log)))
  expect_identical(c(sink.number(), sink.number(type = "message")), before)

  bad <- render_logged(stop("nope"))
  expect_s3_class(bad$error, "error")
  expect_identical(conditionMessage(bad$error), "nope")
  expect_identical(c(sink.number(), sink.number(type = "message")), before)

  # Output written before the failure is still captured, which is where a
  # renderer's own diagnostics live.
  partial <- render_logged({
    cat("progress line\n")
    stop("later")
  })
  expect_true(any(grepl("progress line", partial$log)))
  expect_identical(c(sink.number(), sink.number(type = "message")), before)
})

test_that("render_logged returns the value for callers that need it", {
  # The rmarkdown branch uses the returned path.
  res <- render_logged({
    cat("noise\n")
    "the-value"
  })
  expect_identical(res$value, "the-value")
})

test_that("notify_render_error never throws without a session", {
  # It is called from the error path; if it can throw, it destroys the very
  # error it was asked to report.
  expect_silent(notify_render_error("boom"))
  expect_null(notify_render_error("boom"))
})

test_that("a successful render still produces the file", {
  skip_if_no_renderer()
  out <- tempfile(fileext = ".html")
  render_report(qmd_doc("1 + 1"), "#+ echo=TRUE\n1 + 1\n", "html", out, "T")
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)
})
