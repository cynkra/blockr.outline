# The report and slides panels and the slide block, as markup: the inputs
# the servers read keep their ids, and the controls are the design system's,
# not Bootstrap's.

has_field <- function(html, id, kind) {
  grepl(
    sprintf("id=\"%s\"[^>]*data-kind=\"%s\"", id, kind),
    html
  ) || grepl(sprintf("data-kind=\"%s\"[^>]*id=\"%s\"", kind, id), html)
}

test_that("the report panel keeps its input ids on design-system fields", {
  html <- as.character(report_ext_ui("r", NULL))

  expect_true(has_field(html, "r-rpt_title", "name"))
  expect_true(has_field(html, "r-rpt_view", "segmented"))
  expect_true(has_field(html, "r-rpt_set_format", "select"))
  expect_true(has_field(html, "r-rpt_set_embed", "checkbox"))
  expect_true(has_field(html, "r-rpt_set_titles", "select"))
  expect_true(has_field(html, "r-rpt_set_figw", "number"))
  expect_true(has_field(html, "r-rpt_set_figh", "number"))
  for (id in c("toc", "numbers", "fold", "warnings")) {
    expect_true(has_field(html, paste0("r-rpt_set_", id), "checkbox"))
  }
  expect_match(html, "id=\"r-rpt_go\"", fixed = TRUE)
  expect_match(html, "action-button", fixed = TRUE)
  expect_match(html, "class=\"blockr-gear-btn\"", fixed = TRUE)

  expect_no_match(html, "form-control|form-select|btn-primary|btn-success")
  expect_no_match(html, " title=\"")
})

test_that("the slides panel downloads through an action menu", {
  html <- as.character(slides_ext_ui("s", NULL))

  expect_true(has_field(html, "s-sld_title", "name"))
  expect_match(html, "blockr-action-menu", fixed = TRUE)
  expect_match(html, "data-format=\"pptx\"", fixed = TRUE)
  expect_match(html, "data-format=\"html\"", fixed = TRUE)
  expect_match(html, "id=\"s-sld_dl\"", fixed = TRUE)
  expect_no_match(html, "form-control|form-select|btn-primary|btn-success")
  expect_no_match(html, " title=\"")
})

test_that("the slide block's options sit in a gear tray", {
  blk <- new_slide_block(title = "T", layout = "bullets")
  html <- as.character(blockr.core::expr_ui("b", blk))

  expect_match(html, "blockr-settings", fixed = TRUE)
  expect_true(has_field(html, "b-expr-layout", "tiles"))
  expect_true(has_field(html, "b-expr-title", "text"))
  expect_true(has_field(html, "b-expr-text", "textarea"))
  expect_true(has_field(html, "b-expr-paginate", "checkbox"))
  expect_no_match(html, "form-control|btn-primary")
  expect_no_match(html, " title=\"")
})

test_that("select options survive quotes and backslashes", {
  json <- otl_options_json(c("A \"B\"" = "a\\b"))
  expect_identical(
    jsonlite::fromJSON(json, simplifyVector = FALSE),
    list(list(value = "a\\b", label = "A \"B\""))
  )
})

# ---- block lists (design system, "Block lists") ----------------------------

test_that("a report row keeps code and output on the row, pressed when shown", {
  meta <- list(name = "Sepal scatter", kind = "fig", mark = NULL)
  html <- as.character(
    report_row(list(block = "plot", code = FALSE, output = TRUE), 3L, meta, NS("r"))
  )

  expect_match(html, "data-act=\"code\"", fixed = TRUE)
  expect_match(html, "aria-pressed=\"false\"", fixed = TRUE)
  expect_match(html, "aria-pressed=\"true\"", fixed = TRUE)
  expect_match(html, "data-act=\"output\"", fixed = TRUE)
  expect_match(html, "Output shown in the report", fixed = TRUE)
  expect_match(html, "Code hidden from the report", fixed = TRUE)

  # The "…" opens the menu; there is no remove button on the row.
  expect_match(html, "blockr-otl-row__more", fixed = TRUE)
  expect_no_match(html, "data-act=\"rm\"", fixed = TRUE)
  expect_no_match(html, "__tools|__state|__rm")

  # The name renames in place and is not a native title.
  expect_match(html, "class=\"blockr-otl-row__name\" data-blockr-editable", fixed = TRUE)
  expect_no_match(html, " title=\"")

  off <- as.character(
    report_row(list(block = "plot", code = TRUE, output = FALSE), 1L, meta, NS("r"))
  )
  expect_match(off, "Output hidden from the report", fixed = TRUE)
  expect_match(off, "Code shown in the report", fixed = TRUE)
})

test_that("a deck row has the name, the number and the '…', and no remove", {
  html <- as.character(slides_row("plot", 2L, list(name = "Sepal scatter")))

  expect_match(html, "blockr-otl-row__num blockr-sld-num\">2<", fixed = TRUE)
  expect_match(html, "data-blockr-editable", fixed = TRUE)
  expect_match(html, "blockr-otl-row__more", fixed = TRUE)
  expect_no_match(html, "data-act=\"rm\"", fixed = TRUE)
})

test_that("both lists end with a 30px quiet add button", {
  rpt <- as.character(report_ext_ui("r", NULL))
  sld <- as.character(slides_ext_ui("s", NULL))

  for (html in list(rpt, sld)) {
    expect_match(
      html,
      "blockr-otl-btn--quiet blockr-otl-btn--s[^\"]*blockr-otl-add"
    )
  }
  expect_match(rpt, "Add block", fixed = TRUE)
  expect_match(sld, "Add slide", fixed = TRUE)
  expect_match(rpt, "blockr-empty blockr-empty--panel blockr-rpt-empty", fixed = TRUE)
})

test_that("a row rename becomes a block_name update, and junk does not", {
  board <- otl_dock_board()

  expect_identical(
    rename_block_delta(board, list(id = "plot", name = "  Sepal plot ")),
    list(blocks = list(mod = list(plot = list(block_name = "Sepal plot"))))
  )
  expect_null(rename_block_delta(board, list(id = "plot", name = "  ")))
  expect_null(rename_block_delta(board, list(id = "gone", name = "X")))
  expect_null(rename_block_delta(board, list(id = NULL, name = "X")))
})
