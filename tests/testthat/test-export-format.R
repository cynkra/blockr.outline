# R/export.R :: the qmd / spin document emitters and chapter helpers.

# A stacked board with a description on the block and the chapter, one block
# excluded from the report -- exercises every emitter branch.
sects_fixture <- function() {
  outline_sections(
    otl_exprs(),
    otl_board(stacks = TRUE),
    annotations = otl_ann(
      data = list(description = "The iris data."),
      sub  = list(description = "Setosa only."),
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
    annotations = otl_ann(
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

  s <- outline_sections(ex, otl_board(stacks = TRUE), otl_ann())

  for (txt in list(export_qmd(s), export_spin(s))) {
    expect_match(txt, "head: waiting for R code to be generated", fixed = TRUE)
    expect_no_match(txt, "invisible(NULL)", fixed = TRUE)
    expect_no_match(txt, "\nhead\n", fixed = TRUE)
  }
})

test_that("slides break once per reported block, never before the first", {
  s <- sects_fixture()

  # A document carries no slide breaks at all.
  expect_no_match(export_qmd(s, slides = FALSE), "\n----\n", fixed = TRUE)

  qmd <- export_qmd(s, slides = TRUE)

  # One break per reported block, minus the ones a heading already breaks
  # (the first reported block, and any block opening a chapter).
  chapters <- section_chapters(s)
  breaking <- s$report & !is.na(chapters)
  expected <- max(0L, sum(s$report) - sum(breaking) - as.integer(!any(breaking)))

  expect_equal(
    lengths(regmatches(qmd, gregexpr("\n----\n", qmd, fixed = TRUE)))[[1L]],
    expected
  )

  # A break never opens the document: the deck would start on a blank slide.
  expect_no_match(qmd, "^---\ntitle.*\n----\n", perl = TRUE)

  # An excluded block is `include: false` -- invisible, so a break in front of
  # it would be a slide with nothing on it.
  expect_no_match(qmd, "----\n\n```{r}\n#| label: head\n#| include: false",
                  fixed = TRUE)
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
    annotations = otl_ann(
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
  # (emitting a static_chart call over the result variable). A head block
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

  s <- outline_sections(exprs, board,
                        annotations = otl_ann(ids = c("data", "ch")))
  expect_match(
    unname(s$report_calls[s$ids == "ch"]),
    "^blockr\\.viz::static_chart\\(ch"
  )

  for (txt in list(export_qmd(s), export_spin(s))) {
    expect_match(txt, "blockr.viz::static_chart(ch", fixed = TRUE)
  }
})

test_that("data-shaped blocks print through the static exhibit renderer", {
  # A block that returns a display table returns a bare annotated data frame
  # (the styled table lives in its Shiny UI), so the exporters wrap the result
  # variable in blockr.viz::static_exhibit(), which picks the renderer from the
  # VALUE at render time. Deliberately not a table-block class check: a
  # function block emitting a composer table must print the same way.
  skip_if_not_installed("blockr.viz", "0.2.38")

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

  s <- outline_sections(exprs, board,
                        annotations = otl_ann(ids = c("data", "tbl")),
                        stack_annotations = list())
  expect_identical(unname(s$renderers[s$ids == "tbl"]),
                   "blockr.viz::static_exhibit")
  expect_identical(unname(s$renderers[s$ids == "data"]),
                   "blockr.viz::static_exhibit")

  for (txt in list(export_qmd(s), export_spin(s))) {
    expect_match(txt, "blockr.viz::static_exhibit(tbl)", fixed = TRUE)
    expect_match(txt, "blockr.viz::static_exhibit(data)", fixed = TRUE)
  }
})

test_that("figure blocks are not wrapped", {
  # static_exhibit() is a no-op on a ggplot, so wrapping one would only
  # clutter the generated script.
  blk <- blockr.core::new_head_block()
  class(blk) <- c("plot_block", class(blk))
  expect_identical(block_report_renderer(blk), "")
})
