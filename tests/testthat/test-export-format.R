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
  # head is excluded and nothing reported depends on it -> dropped from the
  # document entirely (its code must not run at render), not include:false.
  expect_no_match(qmd, "#\\| label: head")
})

test_that("an excluded block stays as include:false iff the report needs it", {
  # data is excluded but sub (reported) depends on it: its code must still
  # run, silently. head is excluded and independent: it vanishes.
  s <- outline_sections(
    otl_exprs(),
    otl_board(stacks = TRUE),
    annotations = list(
      data = list(report = FALSE),
      head = list(report = FALSE)
    )
  )

  expect_identical(s$exported, c(data = TRUE, sub = TRUE, head = FALSE)[s$ids])

  for (txt in list(export_qmd(s), export_spin(s))) {
    expect_match(txt, "data <- ", fixed = TRUE)
    expect_match(txt, "include")
    expect_no_match(txt, "head")
  }
})

test_that("nothing reported yields a document with no chunks", {
  s <- outline_sections(
    otl_exprs(),
    otl_board(stacks = TRUE),
    annotations = list(
      data = list(report = FALSE),
      sub  = list(report = FALSE),
      head = list(report = FALSE)
    )
  )

  expect_false(any(s$exported))
  expect_no_match(export_qmd(s), "```")
  expect_identical(export_spin(s), "")
})

test_that("a pending block exports as a comment, never as code", {
  # The placeholder expression backing a pending block must not reach the
  # document as `id <- invisible(NULL)`; it renders as a comment and its
  # value is not echoed.
  ex <- otl_exprs()
  ex$head <- quote(invisible(NULL))
  attr(ex, "pending") <- "head"

  s <- outline_sections(ex, otl_board(stacks = TRUE), list())

  for (txt in list(export_qmd(s), export_spin(s))) {
    expect_match(txt, "head: waiting for R code to be generated", fixed = TRUE)
    expect_no_match(txt, "invisible(NULL)", fixed = TRUE)
    expect_no_match(txt, "\nhead\n", fixed = TRUE)
  }
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

test_that("a block-supplied report call wins the chunk output line", {
  # The chart block states its printed form through blockr.viz::report_call
  # (emitting a gg_chart call over the result variable). A head block
  # wearing the chart_block class exercises the dispatch + emission
  # plumbing without pulling chart fixtures into this suite; its state env
  # has none of the chart names, so the emitted call is the minimal one.
  skip_if_not_installed("blockr.viz")

  blocks <- c(
    data = blockr.core::new_dataset_block("iris"),
    ch   = blockr.core::new_head_block()
  )
  class(blocks[["ch"]]) <- c("chart_block", class(blocks[["ch"]]))
  board <- blockr.core::new_board(
    blocks = blocks,
    links = blockr.core::links(from = "data", to = "ch")
  )
  exprs <- structure(
    list(data = quote(datasets::iris), ch = quote(utils::head(data, 3))),
    pending = character()
  )

  s <- outline_sections(exprs, board, annotations = list())
  expect_match(
    unname(s$report_calls[s$ids == "ch"]),
    "^blockr\\.viz::gg_chart\\(ch"
  )

  for (txt in list(export_qmd(s), export_spin(s))) {
    expect_match(txt, "blockr.viz::gg_chart(ch", fixed = TRUE)
  }
})

test_that("viz table blocks print through the flextable report renderer", {
  # A blockr.viz table / summary_table block returns a bare annotated data
  # frame (the styled table lives in its Shiny UI), so the exporters wrap
  # the result variable in blockr.viz::ft_table(). Class check only -- a
  # head block wearing the class stands in, no blockr.viz needed.
  blocks <- c(
    data = blockr.core::new_dataset_block("iris"),
    tbl  = blockr.core::new_head_block()
  )
  class(blocks[["tbl"]]) <- c("table_block", class(blocks[["tbl"]]))
  board <- blockr.core::new_board(
    blocks = blocks,
    links = blockr.core::links(from = "data", to = "tbl")
  )
  exprs <- structure(
    list(data = quote(datasets::iris), tbl = quote(utils::head(data, 3))),
    pending = character()
  )

  s <- outline_sections(exprs, board, annotations = list(),
                        stack_annotations = list())
  expect_identical(unname(s$renderers[s$ids == "tbl"]), "blockr.viz::ft_table")
  expect_identical(unname(s$renderers[s$ids == "data"]), "")

  for (txt in list(export_qmd(s), export_spin(s))) {
    expect_match(txt, "blockr.viz::ft_table(tbl)", fixed = TRUE)
    # the untouched block still prints bare
    expect_match(txt, "\ndata\n", fixed = TRUE)
  }
})
