# R/export.R :: the per-block expression wrapping and annotation accessors.

test_that("bquoted expressions substitute their input args", {
  out <- wrap_block_expr(
    quote(subset(.(data), Species == "setosa")),
    list(data = as.name("raw")),
    "bquoted"
  )
  expect_identical(out, quote(subset(raw, Species == "setosa")))
})

test_that("bquoted with no args does not crash (block-removal edge case)", {
  # When an upstream block is removed, args is NULL. bquote(where = NULL) is
  # defunct, so this branch must be skipped rather than abort the projection.
  ex <- quote(subset(.(data), Species == "setosa"))
  expect_error(wrap_block_expr(ex, NULL, "bquoted"), NA)
  expect_true(is.call(wrap_block_expr(ex, NULL, "bquoted")))
})

test_that("quoted expressions are wrapped in with()", {
  out <- wrap_block_expr(
    quote(datasets::iris),
    list(data = as.name("data")),
    "quoted"
  )
  expect_true(is.call(out))
  expect_identical(out[[1L]], as.name("with"))
})

test_that("local() wraps a braced block but not a single call", {
  braced <- wrap_block_expr(quote({
    x <- 1
    x
  }), NULL, "other")
  expect_identical(braced[[1L]], as.name("local"))

  single <- wrap_block_expr(quote(datasets::iris), NULL, "other")
  expect_identical(single, quote(datasets::iris))
})

test_that("block_assignment builds `name <- value`", {
  out <- block_assignment("foo", quote(bar(1)))
  expect_identical(out, quote(foo <- bar(1)))
})

test_that("ann_description / ann_report default sensibly", {
  ann <- list(
    a = list(description = "hi", report = FALSE),
    b = list()
  )
  expect_identical(ann_description(ann, "a"), "hi")
  expect_false(ann_report(ann, "a"))

  # Missing entry: empty description, report included by default.
  expect_identical(ann_description(ann, "b"), "")
  expect_true(ann_report(ann, "b"))
  expect_identical(ann_description(ann, "missing"), "")
  expect_true(ann_report(ann, "missing"))
})
