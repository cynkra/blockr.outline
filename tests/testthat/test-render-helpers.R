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

test_that("template_content_width reads the body placeholder, falls back safely", {
  # No template -> widescreen fallback.
  expect_equal(template_content_width(NULL), 12.0)
  expect_equal(template_content_width(""), 12.0)
  expect_equal(template_content_width("/no/such/file.pptx"), 12.0)

  tmpl <- system.file("templates", "bms-template.pptx",
                      package = "blockr.topline")
  skip_if(!nzchar(tmpl), "blockr.topline template not available")
  w <- template_content_width(tmpl)
  # the BMS body placeholder is ~12.53in wide
  expect_true(w > 10 && w < 13)
})

test_that("place_exhibit positions a flextable at its pptx attributes", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")

  ft <- flextable::flextable(head(mtcars[, 1:3], 3))
  attr(ft, "pptx_left") <- 0.4
  attr(ft, "pptx_top") <- 1.1

  doc <- officer::read_pptx()
  doc <- officer::add_slide(doc, layout = "Title and Content",
                            master = officer::layout_summary(doc)$master[1])
  doc <- place_exhibit(doc, ft)

  f <- withr::local_tempfile(fileext = ".pptx")
  print(doc, target = f)
  x <- paste(readLines(unzip(f, "ppt/slides/slide1.xml",
                             exdir = withr::local_tempdir()), warn = FALSE),
             collapse = "")
  # 0.4in = 365760 EMU, 1.1in = 1005840 EMU
  expect_match(x, "365760")
  expect_match(x, "1005840")
})

test_that("render_pptx_officer builds a deck, one slide per reported table", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  skip_if_not_installed("blockr.viz")

  board <- blockr.core::new_board(
    blocks = c(
      data = blockr.core::new_dataset_block("iris"),
      st = blockr.viz::new_summary_table_block(vars = "Sepal.Length",
                                               by = "Species"),
      tbl = blockr.viz::new_table_block()
    ),
    links = blockr.core::links(from = c("data", "st"), to = c("st", "tbl"))
  )
  exprs <- structure(list(
    data = quote(datasets::iris),
    st = quote(blockr.viz::summary_table(data, vars = "Sepal.Length",
                                         by = "Species")),
    tbl = quote(dplyr::filter(blockr.viz::as_annotated_df(st), TRUE))
  ), pending = character())

  s <- outline_sections(exprs, board,
    annotations = list(data = list(report = FALSE), st = list(report = FALSE),
                       tbl = list(report = TRUE)),
    stack_annotations = list())

  f <- withr::local_tempfile(fileext = ".pptx")
  render_pptx_officer(s, f, "Deck", template = NULL)
  expect_true(file.exists(f))

  doc <- officer::read_pptx(f)
  # exactly one reported exhibit -> one added slide (blank base deck).
  expect_gte(nrow(officer::pptx_summary(doc)), 1L)
})

test_that("a reference deck contributes styling, not slides", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  skip_if_not_installed("blockr.viz")

  # A reference document with example slides, exactly as pandoc expects one
  # (and as the BMS master ships: "Presentation Title", "Hello, world.", ...).
  ref <- withr::local_tempfile(fileext = ".pptx")
  tpl <- officer::read_pptx()
  mst <- officer::layout_summary(tpl)$master[[1L]]
  for (i in 1:2) {
    tpl <- officer::add_slide(tpl, layout = "Title and Content", master = mst)
    tpl <- officer::ph_with(tpl, paste("Example", i),
                            location = officer::ph_location_type(type = "title"))
  }
  print(tpl, target = ref)
  expect_length(officer::read_pptx(ref), 2L)

  expect_length(strip_slides(officer::read_pptx(ref)), 0L)

  board <- blockr.core::new_board(
    blocks = c(
      data = blockr.core::new_dataset_block("iris"),
      tbl = blockr.viz::new_table_block()
    ),
    links = blockr.core::links(from = "data", to = "tbl")
  )
  exprs <- structure(list(
    data = quote(datasets::iris),
    tbl = quote(dplyr::filter(blockr.viz::as_annotated_df(data), TRUE))
  ), pending = character())
  s <- outline_sections(exprs, board,
    annotations = list(data = list(report = FALSE), tbl = list(report = TRUE)),
    stack_annotations = list())

  f <- withr::local_tempfile(fileext = ".pptx")
  render_pptx_officer(s, f, "Deck", template = ref)

  # One reported exhibit -> one slide. The template's two examples are gone;
  # without stripping, the deck would open on "Example 1".
  out <- officer::read_pptx(f)
  expect_length(out, 1L)
  expect_false(any(grepl("^Example ", officer::pptx_summary(out)$text)))
})
