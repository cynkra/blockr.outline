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

test_that("effective_template is the app option, then the bundled deck", {
  withr::local_options(list(blockr.outline.template = NULL))
  # Nobody configured anything: the bundled deck, not officer's 4:3 stock
  # deck (see default_template()'s doc comment for why that distinction
  # matters -- every exhibit sizes to a ~11.9in widescreen assumption).
  expect_identical(effective_template(), default_template())
  expect_true(file.exists(default_template()))

  # The deployment's deck, declared once by the app -- and it reaches every
  # board, including those saved before the app declared one.
  withr::local_options(list(blockr.outline.template = "/app/house.pptx"))
  expect_identical(effective_template(), "/app/house.pptx")
})

test_that("a board carries no template of its own", {
  # The gear's template field is gone and `template =` is an ignored legacy
  # argument, so nothing a board was saved with can override the deployment's
  # deck -- the failure mode that motivated the removal was a stored ABSOLUTE
  # path from another machine, silently losing to the fallback deck while the
  # app default sat unused.
  house <- withr::local_tempfile(fileext = ".pptx")
  file.create(house)
  withr::local_options(list(blockr.outline.template = house))

  ext <- new_outline_extension(template = "/saved/on/another/machine.pptx")
  expect_s3_class(ext, "outline_extension")

  deck <- new_slides_extension(template = "/saved/on/another/machine.pptx")
  expect_s3_class(deck, "slides_extension")

  expect_identical(effective_template(), house)
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
