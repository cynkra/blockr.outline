# Prose on slides through the officer path. The report extension no longer
# uses officer (it renders through quarto only); this seam stays for the
# slides extension, so the projection is exercised as the deck's --
# slide_sections() with an annotation map.

test_that("a note lands on its exhibit's slide, and the intro gets its own", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  skip_if_not_installed("blockr.viz", "0.2.38")

  s <- slide_sections(
    otl_exprs(), otl_board(stacks = FALSE),
    slides = "head",
    annotations = list(
      head = list(description = "The first rows, **as a sanity check**.")
    )
  )

  f <- withr::local_tempfile(fileext = ".pptx")
  render_pptx_officer(s, f, "Iris pilot", template = NULL,
                      intro = "Why we look at this.")

  smry <- officer::pptx_summary(officer::read_pptx(f))
  paras <- smry[smry$content_type == "paragraph", ]

  # Slide 1 the title, slide 2 the intro, slide 3 the exhibit with its note
  # (markdown flattened to text) under the block title.
  expect_identical(paras$text[paras$slide_id == 1][1], "Iris pilot")
  expect_true("Why we look at this." %in% paras$text[paras$slide_id == 2])
  expect_true(any(grepl("as a sanity check", paras$text[paras$slide_id == 3])))
  expect_true("Head" %in% paras$text[paras$slide_id == 3])

  # The exhibit still landed as a real table, on the note's slide.
  expect_true(3 %in% smry$slide_id[smry$content_type == "table cell"])
})

test_that("a deck with no prose renders exactly as before", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  skip_if_not_installed("blockr.viz", "0.2.38")

  s <- slide_sections(
    otl_exprs(), otl_board(stacks = FALSE),
    slides = "head", annotations = list()
  )

  f <- withr::local_tempfile(fileext = ".pptx")
  render_pptx_officer(s, f, "Deck", template = NULL)

  smry <- officer::pptx_summary(officer::read_pptx(f))
  # Two slides: title, exhibit. No intro slide, no note paragraph.
  expect_identical(sort(unique(smry$slide_id)), c(1L, 2L))
})
