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

test_that("render_logged does not divert output away from the app's log", {
  # This once tee'd both streams to a file with sink(). sink() is
  # process-wide, so the app logged nothing for the length of a render, and a
  # process killed mid-render (a Connect timeout) took the whole window with
  # it: minutes of silence, then "Execution halted". The renderer's output has
  # to stream as it happens.
  before <- c(sink.number(), sink.number(type = "message"))

  ok <- render_logged(cat("hello\n"))
  expect_null(ok$error)
  expect_identical(c(sink.number(), sink.number(type = "message")), before)

  bad <- render_logged(stop("nope"))
  expect_s3_class(bad$error, "error")
  expect_identical(conditionMessage(bad$error), "nope")
  expect_identical(c(sink.number(), sink.number(type = "message")), before)

  # Output reaches the caller's stdout rather than being swallowed.
  expect_output(render_logged(cat("progress line\n")), "progress line")
  expect_identical(c(sink.number(), sink.number(type = "message")), before)
})

test_that("render_logged returns the value for callers that need it", {
  # The rmarkdown branch uses the returned path.
  res <- render_logged("the-value")
  expect_identical(res$value, "the-value")
  expect_null(res$error)
})

test_that("notify_render_error never throws without a session", {
  # It is called from the error path; if it can throw, it destroys the very
  # error it was asked to report.
  expect_silent(notify_render_error("boom"))
  expect_null(notify_render_error("boom"))
})

test_that("execute_mode defaults to quarto and rejects nonsense", {
  withr::local_options(list(blockr.outline.execute = NULL))
  expect_identical(execute_mode(), "quarto")

  withr::local_options(list(blockr.outline.execute = "in-process"))
  expect_identical(execute_mode(), "in-process")

  withr::local_options(list(blockr.outline.execute = "sideways"))
  expect_warning(mode <- execute_mode(), "quarto")
  expect_identical(mode, "quarto")
})

test_that("in-process rendering sees state that only this session has", {
  skip_if_no_renderer()

  # The reason the mode exists. Both mechanisms at once: a function reached
  # through an option set at app startup, and a package that app.R
  # pkgload::load_all()s out of inst/ rather than installing. Neither exists
  # in the fresh R session quarto starts.
  withr::local_options(list(
    blockr.outline.execute = "in-process",
    blockr.outline.test.reader = function(x) data.frame(id = x, n = 1:3)
  ))

  doc <- qmd_doc(paste(
    "d <- getOption(\"blockr.outline.test.reader\")(\"study-1\")",
    "cat(nrow(d))",
    sep = "\n"
  ))

  out <- tempfile(fileext = ".html")
  render_report(doc, "", "html", out, "T")
  expect_true(file.exists(out))
  expect_match(paste(readLines(out, warn = FALSE), collapse = ""), "3")
})

test_that("in-process leaves the app session exactly as it found it", {
  skip_if_no_renderer()
  withr::local_options(list(blockr.outline.execute = "in-process"))

  wd <- getwd()
  chunk_opts <- knitr::opts_chunk$get()
  sinks <- c(sink.number(), sink.number(type = "message"))

  render_report(qmd_doc("1 + 1"), "", "html", tempfile(fileext = ".html"), "T")
  expect_identical(getwd(), wd)
  expect_identical(knitr::opts_chunk$get(), chunk_opts)
  expect_identical(c(sink.number(), sink.number(type = "message")), sinks)

  # And after a failure, which is when leaking would hurt most.
  expect_error(
    render_report(qmd_doc("stop(\"boom\")"), "", "html", tempfile(), "T"),
    "boom"
  )
  expect_identical(getwd(), wd)
  expect_identical(knitr::opts_chunk$get(), chunk_opts)
  expect_identical(c(sink.number(), sink.number(type = "message")), sinks)
})

test_that("in-process stops on a failing chunk rather than embedding it", {
  skip_if_no_renderer()
  withr::local_options(list(blockr.outline.execute = "in-process"))

  # knit() defaults to error = TRUE, which would paste the traceback into the
  # document and hand the user a downloaded report with no other signal that
  # anything went wrong. quarto stops; so must this.
  out <- tempfile(fileext = ".html")
  expect_error(
    render_report(qmd_doc("stop(\"boom\")"), "", "html", out, "T"),
    "boom"
  )
  expect_false(file.exists(out))
})

test_that("a successful render still produces the file", {
  skip_if_no_renderer()
  out <- tempfile(fileext = ".html")
  render_report(qmd_doc("1 + 1"), "#+ echo=TRUE\n1 + 1\n", "html", out, "T")
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)
})
