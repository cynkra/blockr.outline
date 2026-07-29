# R/ext.R :: sanitize_annotations -- coerces stored / restored annotation
# state into a stable {description: chr(1), report: lgl(1)} shape.

test_that("empty or non-list input becomes an empty list", {
  expect_identical(sanitize_annotations(NULL), list())
  expect_identical(sanitize_annotations(list()), list())
  expect_identical(sanitize_annotations("nope"), list())
})

test_that("a full entry passes through normalised", {
  out <- sanitize_annotations(
    list(a = list(description = "hello", report = FALSE))
  )
  expect_identical(out$a$description, "hello")
  expect_false(out$a$report)
})

test_that("report defaults to FALSE and description to empty string", {
  out <- sanitize_annotations(list(a = list()))
  expect_identical(out$a$description, "")
  expect_false(out$a$report)
})

test_that("description is coerced to a length-one character", {
  # A list-wrapped or multi-element description (as JSON round-trips can
  # produce) is flattened and truncated to one string.
  out <- sanitize_annotations(
    list(a = list(description = list("x"), report = 1))
  )
  expect_identical(out$a$description, "x")
  expect_type(out$a$description, "character")
  expect_length(out$a$description, 1L)
  # report uses isTRUE semantics: a truthy non-logical is not TRUE.
  expect_false(out$a$report)
})

test_that("names (block ids) are preserved", {
  out <- sanitize_annotations(
    list(one = list(report = TRUE), two = list(description = "d"))
  )
  expect_named(out, c("one", "two"))
})
