# R/render.R :: capability probes and server-side syntax highlighting.
# The actual quarto/rmarkdown render is exercised in the e2e layer; here we
# cover the pure helpers and their degradation paths.

test_that("highlight_r_code degrades to NULL without downlit", {
  # Force the "downlit missing" branch regardless of what is installed.
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) if (pkg == "downlit") FALSE else TRUE,
    .package = "base"
  )
  expect_null(highlight_r_code("x <- 1"))
})

test_that("highlight_r_code returns chroma markup when downlit parses", {
  skip_if_not_installed("downlit")
  out <- highlight_r_code("x <- mean(1:10)")
  expect_type(out, "character")
  expect_match(out, "chroma")
})

test_that("highlight_r_code returns NULL on unparseable input", {
  skip_if_not_installed("downlit")
  # downlit fails to highlight a syntax error -> NA -> NULL.
  expect_null(highlight_r_code("x <- <-"))
})

test_that("highlight_qmd_code marks up yaml, headings and chunks", {
  skip_if_not_installed("downlit")
  txt <- paste(
    "---",
    "title: \"T\"",
    "---",
    "# Heading",
    "Some **bold** and a @fig-x reference.",
    "```{r}",
    "x <- 1",
    "```",
    sep = "\n"
  )
  out <- highlight_qmd_code(txt)
  expect_match(out, "<pre class=\"chroma\">")
  expect_match(out, "class=\"gh\"")   # h1 heading
  expect_match(out, "class=\"gs\"")   # bold
  expect_match(out, "class=\"na\"")   # cross-reference / yaml key
})

test_that("highlight_qmd_code degrades to NULL without downlit", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) if (pkg == "downlit") FALSE else TRUE,
    .package = "base"
  )
  expect_null(highlight_qmd_code("# hi"))
})

test_that("quarto_usable reflects quarto availability", {
  # No quarto namespace -> not usable.
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) if (pkg == "quarto") FALSE else TRUE,
    .package = "base"
  )
  expect_false(quarto_usable())
})

test_that("report_pdf_available is TRUE when a latex engine is on PATH", {
  local_mocked_bindings(
    Sys.which = function(x) c(pdflatex = "/usr/bin/pdflatex")[x],
    .package = "base"
  )
  expect_true(report_pdf_available())

  local_mocked_bindings(
    Sys.which = function(x) setNames("", x),
    requireNamespace = function(pkg, ...) FALSE,
    .package = "base"
  )
  expect_false(report_pdf_available())
})
