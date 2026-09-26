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
