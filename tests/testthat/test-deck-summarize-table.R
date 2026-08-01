# The summarize table on a slide, end to end.
#
# Its block is a transform block: its RESULT is the input frame, and the table
# exists only as a push to the browser. So the deck reaches it through
# report_call(), and the exhibit that comes back renders itself per target --
# painted pages for pptx (a DrawingML cell cannot hold a glyph), the app's own
# markup for HTML. Both routes are pinned here, because either one silently
# degrades to "a table of the raw input rows" if the seam breaks.

summarize_board <- function() {
  blockr.core::new_board(
    blocks = c(
      data = blockr.core::new_dataset_block("iris"),
      sm = blockr.viz::new_summarize_table_block(
        by = "Species",
        summaries = list(
          list(type = "simple", func = "count", show = "bar", name = "n"),
          list(type = "dist", col = "Sepal.Length", style = "box",
               inner = "median_q1_q3", name = "Sepal length")
        ),
        block_name = "Species overview"
      )
    ),
    links = blockr.core::links(from = "data", to = "sm")
  )
}

summarize_exprs <- function() {
  structure(
    list(data = quote(datasets::iris), sm = quote(identity(data))),
    pending = character()
  )
}

test_that("the deck asks the block to rebuild its table", {
  skip_if_not_installed("blockr.viz")
  skip_if_not(is.function(
    tryCatch(getExportedValue("blockr.viz", "static_summarize_table"),
             error = function(e) NULL)))

  s <- slide_sections(summarize_exprs(), summarize_board(), slides = "sm")

  # Without the block's report call the deck would print `identity(data)`,
  # which is the iris frame: 150 rows of raw data where a summary belongs.
  expect_match(sect_output(s, which(s$ids == "sm")),
               "static_summarize_table", fixed = TRUE)
})

test_that("a summarize table reaches a pptx slide as a picture", {
  skip_if_not_installed("officer")
  skip_if_not_installed("blockr.viz")
  skip_if_not(is.function(
    tryCatch(getExportedValue("blockr.viz", "static_summarize_table"),
             error = function(e) NULL)))

  s <- slide_sections(summarize_exprs(), summarize_board(), slides = "sm")

  f <- withr::local_tempfile(fileext = ".pptx")
  render_pptx_officer(s, f, "Deck", template = NULL, title_slide = FALSE)

  expect_true(file.exists(f))
  files <- utils::unzip(f, list = TRUE)$Name
  expect_true(any(grepl("^ppt/media/.*\\.png$", files)))
  # One slide, and it carries the picture rather than an empty content box.
  expect_gte(length(officer::read_pptx(f)), 1L)
})

test_that("the same table reaches an HTML slide as the app's own markup", {
  skip_if_not_installed("blockr.viz")
  skip_if_not(is.function(
    tryCatch(getExportedValue("blockr.viz", "static_summarize_table"),
             error = function(e) NULL)))

  s <- slide_sections(summarize_exprs(), summarize_board(), slides = "sm")

  f <- withr::local_tempfile(fileext = ".html")
  render_deck_html(s, f, "Overview")

  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")

  # The marks, not a screenshot of them and not a bare data frame.
  expect_true(grepl("blockr-rank-table", txt, fixed = TRUE))
  expect_true(grepl("lane-box", txt, fixed = TRUE))
  # Still self-contained: the rank table's CSS and JS are inlined like every
  # other dependency the deck carries.
  expect_false(grepl("<script[^>]+src=", txt))
})

test_that("a chart is placed by the same method its own download calls", {
  skip_if_not_installed("officer")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("blockr.viz")
  skip_if_not(!is.null(pptx_exhibit_method("gg")))

  board <- blockr.core::new_board(
    blocks = c(
      data = blockr.core::new_dataset_block("iris"),
      ch = blockr.viz::new_chart_block(chart_type = "bar", group = "Species",
                                       func = "count", block_name = "Chart")
    ),
    links = blockr.core::links(from = "data", to = "ch")
  )
  exprs <- structure(
    list(data = quote(datasets::iris), ch = quote(identity(data))),
    pending = character()
  )
  s <- slide_sections(exprs, board, slides = "ch")

  # The chart rebuilds itself as a ggplot rather than printing its data.
  expect_match(sect_output(s, which(s$ids == "ch")), "ggplot2::ggplot",
               fixed = TRUE)

  f <- withr::local_tempfile(fileext = ".pptx")
  render_pptx_officer(s, f, "Deck", template = NULL, title_slide = FALSE)

  files <- utils::unzip(f, list = TRUE)$Name
  expect_true(any(grepl("^ppt/media/", files)))
  expect_identical(length(officer::read_pptx(f)), 1L)
})
