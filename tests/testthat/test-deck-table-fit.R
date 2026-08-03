# A table too tall for its slide, end to end through the deck.
#
# blockr.viz shrinks the type before it splits, down to a floor a board sets
# ("Smallest table font", blockr.viz::new_exhibit_font_option()). The deck
# passes no font arguments at all -- the floor is read inside the paginator,
# so a slide and the same block's own PowerPoint download cannot disagree
# about it. This pins that the deck actually moves with the setting, which is
# the sort of seam that otherwise degrades silently into "it still splits".

fit_board <- function() {
  blockr.core::new_board(
    blocks = c(tbl = blockr.core::new_dataset_block("iris")),
    links = blockr.core::links()
  )
}

fit_exprs <- function(n = 26L) {
  structure(
    list(
      tbl = bquote(
        data.frame(
          .label = sprintf("Preferred term %d", seq_len(.(n))),
          .indent = 0L,
          Placebo = sprintf("%d (%.1f%%)", seq_len(.(n)), seq_len(.(n)) / 2),
          Drug = sprintf("%d (%.1f%%)", seq_len(.(n)), seq_len(.(n)) / 3),
          check.names = FALSE
        )
      )
    ),
    pending = character()
  )
}

deck_slides <- function(floor) {
  withr::local_options(blockr.viz.ft_min_font_size = floor)

  s <- slide_sections(fit_exprs(), fit_board(), slides = "tbl")
  f <- withr::local_tempfile(fileext = ".pptx")

  notes <- list()
  withCallingHandlers(
    render_pptx_officer(s, f, "Deck", template = NULL, title_slide = FALSE),
    blockr_exhibit_split = function(cnd) {
      notes[[length(notes) + 1L]] <<- cnd
      invokeRestart("muffleMessage")
    }
  )

  list(slides = length(officer::read_pptx(f)), notes = notes)
}

test_that("a lower floor keeps a deck's table on one slide", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  skip_if_not_installed("blockr.viz")
  skip_if_not(is.function(
    tryCatch(getExportedValue("blockr.viz", "new_exhibit_font_option"),
             error = function(e) NULL)))

  house <- deck_slides(13)
  small <- deck_slides(8)

  expect_gt(house$slides, 1L)
  expect_identical(small$slides, 1L)

  # And the split deck says which table and what it would have taken, which
  # is what the download turns into one notification.
  expect_length(house$notes, 1L)
  expect_gt(house$notes[[1L]]$fit_size, 0)
  expect_length(small$notes, 0L)
})
