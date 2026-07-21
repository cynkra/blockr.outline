# R/export.R :: the qmd / spin document emitters and chapter helpers.

# A stacked board with a description on the block and the chapter, one block
# excluded from the report -- exercises every emitter branch.
sects_fixture <- function() {
  outline_sections(
    otl_exprs(),
    otl_board(stacks = TRUE),
    annotations = list(
      data = list(description = "The iris data.", report = TRUE),
      sub  = list(description = "Setosa only.", report = TRUE),
      head = list(report = FALSE)
    ),
    stack_annotations = list(prep = list(description = "Data preparation."))
  )
}

test_that("export_qmd renders the expected document", {
  expect_snapshot(cat(export_qmd(sects_fixture(), "Iris report")))
})

test_that("export_spin renders the expected script", {
  expect_snapshot(cat(export_spin(sects_fixture())))
})

test_that("report exhibits get a caption, excluded blocks are hidden", {
  qmd <- export_qmd(sects_fixture())
  # data is a report block classified "tbl" -> tbl-cap caption, bare label.
  # The label carries no tbl-/fig- prefix: that would make quarto treat the
  # output as a cross-reference float, which pandoc's pptx path cannot
  # render a flextable inside.
  expect_match(qmd, "#\\| label: data")
  expect_no_match(qmd, "#\\| label: tbl-data")
  expect_match(qmd, "#\\| tbl-cap:")
  # head is excluded -> include:false and no echo of its value.
  expect_match(qmd, "#\\| include: false")
})

test_that("qmd escapes double quotes in the title", {
  qmd <- export_qmd(sects_fixture(), 'A "quoted" title')
  expect_match(qmd, "title: \"A \\\\\"quoted\\\\\" title\"")
})

test_that("section_chapters emits one heading per contiguous run", {
  s <- sects_fixture()
  ch <- section_chapters(s)
  # Exactly one non-NA heading (the single `prep` run), on its first member.
  expect_equal(sum(!is.na(ch)), 1L)
  expect_identical(ch[s$stack_ids == "prep"][[1L]], "Stack")
})

test_that("a run with no report-included block gets no heading", {
  s <- outline_sections(
    otl_exprs(),
    otl_board(stacks = TRUE),
    annotations = list(
      data = list(report = FALSE),
      sub  = list(report = FALSE)
    )
  )
  ch <- section_chapters(s)
  expect_true(all(is.na(ch[s$stack_ids == "prep"])))
})

test_that("chapter_intro emits the stack description only under a fresh heading", {
  s <- sects_fixture()
  ch <- section_chapters(s)
  first <- which(!is.na(ch))[[1L]]
  expect_identical(chapter_intro(s, ch, first), "Data preparation.")

  # A row that is not a chapter start yields nothing.
  non_head <- which(is.na(ch))[[1L]]
  expect_length(chapter_intro(s, ch, non_head), 0L)
})
