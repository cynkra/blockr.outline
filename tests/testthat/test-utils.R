# R/utils.R -- small helpers used throughout the projection.

test_that("coal returns the first non-NULL argument", {
  expect_equal(coal(NULL, NULL, 3), 3)
  expect_equal(coal("a", "b"), "a")
  expect_equal(coal(NULL, "x"), "x")
  # A present-but-falsy value still wins over a later one.
  expect_equal(coal(FALSE, TRUE), FALSE)
  expect_equal(coal(0L, 9L), 0L)
  expect_null(coal(NULL, NULL))
  expect_null(coal())
})

test_that("chr_ply / lgl_ply vapply with the right prototype", {
  expect_identical(chr_ply(1:3, function(i) letters[i]), c("a", "b", "c"))
  expect_identical(lgl_ply(1:3, function(i) i > 1L), c(FALSE, TRUE, TRUE))

  # Extra arguments are forwarded (vapply's ... contract). Character input
  # picks up USE.NAMES names, which is expected.
  expect_identical(
    chr_ply(c("A", "B"), paste0, "!"),
    c(A = "A!", B = "B!")
  )

  # Empty input yields the zero-length typed vector, not a list.
  expect_identical(unname(chr_ply(character(), identity)), character())
  expect_identical(lgl_ply(integer(), function(i) TRUE), logical())
})

test_that("pkg_version reports the installed package version", {
  expect_s3_class(numeric_version(pkg_version()), "numeric_version")
})
