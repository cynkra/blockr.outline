# The HTML deck: one self-contained file, written in this process.

deck_html_board <- function() {
  blockr.core::new_board(
    blocks = c(
      data = blockr.core::new_dataset_block("iris"),
      tbl = blockr.viz::new_table_block(block_name = "Flower measurements"),
      other = blockr.viz::new_table_block(block_name = "Just the head")
    ),
    links = blockr.core::links(from = c("data", "data"),
                               to = c("tbl", "other"))
  )
}

deck_html_exprs <- function() {
  structure(
    list(
      data = quote(datasets::iris),
      tbl = quote(blockr.viz::as_annotated_df(data)),
      other = quote(utils::head(blockr.viz::as_annotated_df(data), 3L))
    ),
    pending = character()
  )
}

test_that("the deck is one file that carries everything it needs", {
  skip_if_not_installed("blockr.viz")

  s <- slide_sections(deck_html_exprs(), deck_html_board(),
                      slides = c("tbl", "other"))

  f <- withr::local_tempfile(fileext = ".html")
  render_deck_html(s, f, "Iris topline")

  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")

  # Self-contained by construction: a deck is something you send, so a linked
  # stylesheet or script would be a file the download does not carry and a
  # table that arrives unstyled on the machine that opens it.
  expect_false(grepl("<script[^>]+src=", txt))
  expect_false(grepl("<link[^>]+href=", txt))

  # The deck's own layer, inlined.
  expect_match(txt, "bd-canvas", fixed = TRUE)
  expect_match(txt, "<title>Iris topline</title>", fixed = TRUE)
})

test_that("a slide's table is the same renderer the block download uses", {
  skip_if_not_installed("blockr.viz")

  s <- slide_sections(deck_html_exprs(), deck_html_board(), slides = "other")

  f <- withr::local_tempfile(fileext = ".html")
  render_deck_html(s, f, "Deck")
  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")

  # html_exhibit()'s markup, which is what write_exhibit_html() writes too --
  # NOT flextable's html, which is what the exhibit object itself would print
  # as outside knitr. A slide and a downloaded table are one artifact.
  expect_match(txt, "blockr-ht", fixed = TRUE)
  expect_false(grepl("tabwid", txt, fixed = TRUE))
})

test_that("the deck opens on a title slide and keeps the picked order", {
  skip_if_not_installed("blockr.viz")

  s <- slide_sections(deck_html_exprs(), deck_html_board(),
                      slides = c("other", "tbl"))

  f <- withr::local_tempfile(fileext = ".html")
  render_deck_html(s, f, "Deck")
  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")

  expect_match(txt, "bd-slide--title", fixed = TRUE)
  # The same line the pptx title slide carries under the same title.
  expect_match(txt, deck_title_date(), fixed = TRUE)
  # Two exhibits plus the title slide.
  expect_length(gregexpr("class=\"bd-canvas\"", txt)[[1L]], 3L)
  # Picked head-first, and the file says so.
  expect_lt(
    regexpr("Just the head", txt, fixed = TRUE),
    regexpr("Flower measurements", txt, fixed = TRUE)
  )
})

test_that("a block with no exhibit costs its slide, not the deck", {
  skip_if_not_installed("blockr.viz")

  board <- blockr.core::new_board(
    blocks = c(
      data = blockr.core::new_dataset_block("iris"),
      tbl = blockr.viz::new_table_block(block_name = "Fine"),
      dud = blockr.viz::new_table_block(block_name = "Draws to the device")
    ),
    links = blockr.core::links(from = c("data", "data"), to = c("tbl", "dud"))
  )
  exprs <- structure(
    list(
      data = quote(datasets::iris),
      tbl = quote(utils::head(blockr.viz::as_annotated_df(data), 3L)),
      # A base plot's block: draws, returns nothing.
      dud = quote(invisible(NULL))
    ),
    pending = character()
  )

  s <- slide_sections(exprs, board, slides = c("tbl", "dud"))

  f <- withr::local_tempfile(fileext = ".html")
  # ...and says which slide went missing, on stderr, rather than silently
  # coming back short.
  expect_output(render_deck_html(s, f, "Deck"), NA)
  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")

  expect_match(txt, "Fine", fixed = TRUE)
  expect_false(grepl("Draws to the device", txt, fixed = TRUE))
})

test_that("quarto's word for the format still restores as the format", {
  # LEGACY: the HTML deck was a revealjs render before it was written here.
  expect_identical(deck_format("revealjs"), "html")
  expect_identical(deck_format("pptx"), "pptx")
  expect_identical(deck_format(NULL), "pptx")
  expect_true(all(deck_formats() %in% c("pptx", "html")))
})
