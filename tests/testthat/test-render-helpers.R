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

test_that("only formats that render are offered", {
  # Not a tautology: pdf was offered off a PATH probe for a toolchain quarto
  # does not use, so it reached users as a download that failed. Whatever is
  # in here is a promise the deployment has to keep.
  expect_equal(report_formats(), c("html", "pptx"))
  expect_false("pdf" %in% report_formats())
})

test_that("template_content_width reads the body placeholder, falls back safely", {
  # No template -> widescreen fallback.
  expect_equal(template_content_width(NULL), 12.0)
  expect_equal(template_content_width(""), 12.0)
  expect_equal(template_content_width("/no/such/file.pptx"), 12.0)

  # The bundled fallback deck is widescreen: a body placeholder wider than
  # the 10in stock reference doc.
  w <- template_content_width(default_template())
  expect_true(w > 10 && w < 13.34)
})

# A reference deck whose theme font scheme is `face`, built from officer's
# stock deck so the fixture needs no asset of its own.
local_font_template <- function(face, env = parent.frame()) {
  src <- withr::local_tempfile(fileext = ".pptx", .local_envir = env)
  print(officer::read_pptx(), target = src)

  dir <- withr::local_tempdir(.local_envir = env)
  utils::unzip(src, exdir = dir)

  theme <- file.path(dir, "ppt", "theme", "theme1.xml")
  xml <- paste(readLines(theme, warn = FALSE), collapse = "")
  writeLines(gsub("<a:latin typeface=\"[^\"]*\"",
                  paste0("<a:latin typeface=\"", face, "\""), xml), theme)

  out <- withr::local_tempfile(fileext = ".pptx", .local_envir = env)
  zip::zip(out, list.files(dir), root = dir, mode = "cherry-pick")
  out
}

test_that("the bundled fallback deck carries nobody's branding", {
  tmpl <- default_template()
  expect_true(file.exists(tmpl))

  parts <- utils::unzip(tmpl, list = TRUE)$Name

  # No logo, no picture: an image in a template can only be somebody's.
  expect_length(grep("^ppt/media/", parts), 0L)

  # No authored text either. Placeholders carry sample prompts ("Click to edit
  # Master title style") which never render; a shape WITHOUT a <p:ph> is a
  # plain text box, and one of those on the master prints on every slide --
  # which is exactly how a client's footer came to ship in this package.
  master <- template_part(tmpl, "ppt/slideMasters/slideMaster1.xml")
  shapes <- regmatches(
    master, gregexpr("(?s)<p:sp>.*?</p:sp>", master, perl = TRUE)
  )[[1L]]
  free <- shapes[!grepl("<p:ph", shapes, fixed = TRUE)]

  expect_length(unlist(regmatches(free, gregexpr("<a:t>[^<]*", free))), 0L)
})

test_that("template_body_font reads the deck's font scheme, falls back safely", {
  skip_if_not_installed("officer")
  skip_if_not_installed("zip")

  expect_null(template_body_font(NULL))
  expect_null(template_body_font(""))
  expect_null(template_body_font("/no/such/file.pptx"))

  expect_equal(template_body_font(local_font_template("Papyrus")), "Papyrus")

  # The bundled deck names one face, whatever it is.
  face <- template_body_font(default_template())
  expect_type(face, "character")
  expect_true(nzchar(face))
})

test_that("a deck sets its exhibits in the template's own font", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  skip_if_not_installed("blockr.viz")
  skip_if_not_installed("zip")

  tmpl <- local_font_template("Papyrus")

  sects <- outline_sections(
    structure(list(data = quote(datasets::iris)), pending = character()),
    blockr.core::new_board(
      blocks = c(data = blockr.core::new_dataset_block("iris"))
    ),
    annotations = list(data = list(report = TRUE)),
    stack_annotations = list()
  )

  slide_xml <- function(f) {
    paste(readLines(utils::unzip(f, "ppt/slides/slide1.xml",
                                 exdir = withr::local_tempdir()),
                    warn = FALSE),
          collapse = "")
  }

  f <- withr::local_tempfile(fileext = ".pptx")
  withr::with_options(list(blockr.viz.ft_font = NULL), {
    render_pptx_officer(sects, f, "Deck", template = tmpl)
  })
  expect_match(slide_xml(f), "Papyrus")

  # An app that named a font has said what it wants; the deck does not
  # overrule it.
  g <- withr::local_tempfile(fileext = ".pptx")
  withr::with_options(list(blockr.viz.ft_font = "Trebuchet MS"), {
    render_pptx_officer(sects, g, "Deck", template = tmpl)
  })
  x <- slide_xml(g)
  expect_match(x, "Trebuchet MS")
  expect_false(grepl("Papyrus", x, fixed = TRUE))

  # ... and the option is left exactly as it was found.
  expect_null(getOption("blockr.viz.ft_font"))
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

# The Output preview's failure notes. A preview error almost never
# originates in the block you are looking at -- an ancestor outside the
# report throws, its variable never binds, and every dependent dies with
# "object not found" -- so the note has to carry the condition itself.
test_that("the Output preview names the error that stopped a block", {

  # Ids deliberately unlike any base function: the eval env's parent is
  # globalenv(), so a block id that shadows one (`sub`, `data`) resolves to
  # the FUNCTION instead of failing when its upstream never bound.
  board <- blockr.core::new_board(
    blocks = c(
      src = blockr.core::new_dataset_block("iris"),
      mid = blockr.core::new_subset_block(),
      leaf = blockr.core::new_head_block()
    ),
    links = blockr.core::links(from = c("src", "mid"), to = c("mid", "leaf"))
  )

  exprs <- structure(
    list(
      src = quote(stop("pin not found")),
      mid = quote(subset(src, Species == "setosa")),
      leaf = quote(utils::head(mid, 3))
    ),
    pending = character()
  )

  s <- outline_sections(
    exprs, board,
    annotations = list(src = list(report = FALSE), mid = list(report = FALSE),
                       leaf = list(report = TRUE)),
    stack_annotations = list()
  )

  html <- as.character(outline_output_map(s)[["leaf"]])

  expect_match(html, "An upstream block could not be evaluated")
  # Two blocks downstream of the failure, and it still names the ROOT block
  # and the root message: the trail is not re-wrapped at every hop.
  expect_match(html, "upstream block 'src'")
  expect_match(html, "pin not found")
})

test_that("an exhibit call that throws is told apart from no output", {

  # Fields outline_output_map() reads, hand-built: the failure lives in the
  # report_call, which no fixture block emits.
  s <- list(ids = "a", pending = FALSE, exported = TRUE, report = TRUE,
            code = "a <- 1", report_calls = "stop('no such column')",
            renderers = "")

  html <- as.character(outline_output_map(s)[["a"]])

  expect_match(html, "Could not render this exhibit")
  expect_match(html, "no such column")
})

test_that("a renderer that degrades instead of failing says so", {

  # static_chart() warns and returns the chart's DATA when it cannot draw the
  # requested type -- the preview then shows a table where a chart belongs.
  s <- list(ids = "a", pending = FALSE, exported = TRUE, report = TRUE,
            code = "a <- 1",
            report_calls = "{ warning('cannot draw scatter'); a }",
            renderers = "")

  html <- as.character(outline_output_map(s)[["a"]])

  expect_match(html, "blockr-otl-exhibit")
  expect_match(html, "cannot draw scatter")
})

test_that("an unevaluated ancestor is named, not shadowed by a base function", {

  # The eval env chains to globalenv(), so a block id that names a base
  # function ("data" here) used to resolve to THAT function when the block
  # never bound: the dependent then failed with "no applicable method for
  # 'filter' applied to an object of class \"function\"", naming neither the
  # block nor the cause. The seeded promise wins over the search path.
  board <- blockr.core::new_board(
    blocks = c(
      data = blockr.core::new_dataset_block("iris"),
      leaf = blockr.core::new_head_block()
    ),
    links = blockr.core::links(from = "data", to = "leaf")
  )

  exprs <- structure(
    list(data = quote(stop("pin not found")),
         leaf = quote(dplyr::filter(data, TRUE))),
    pending = character()
  )

  s <- outline_sections(
    exprs, board,
    annotations = list(data = list(report = FALSE), leaf = list(report = TRUE)),
    stack_annotations = list()
  )

  html <- as.character(outline_output_map(s)[["leaf"]])

  expect_match(html, "upstream block 'data'")
  expect_match(html, "pin not found")
  expect_no_match(html, "applied to an object of class")
})

test_that("a block still building is reported as such downstream", {

  s <- list(
    ids = c("up", "down"), pending = c(TRUE, FALSE),
    exported = c(TRUE, TRUE), report = c(FALSE, TRUE),
    code = c("# up: waiting", "down <- utils::head(up, 3)"),
    report_calls = c("", ""), renderers = c("", "")
  )

  html <- as.character(outline_output_map(s)[["down"]])

  expect_match(html, "upstream block 'up' has not finished building")
})

test_that("a block dropped from the projection is named too", {

  # Second way an id goes missing: `data` reports no expression this flush,
  # so outline_sections() drops it entirely -- no row, no code, no section --
  # while the chart's chunk still names it. Only the BOARD still knows the
  # id, so the preview has to be told about it or the name falls through to
  # utils::data.
  board <- blockr.core::new_board(
    blocks = c(
      data = blockr.core::new_dataset_block("iris"),
      leaf = blockr.core::new_head_block()
    ),
    links = blockr.core::links(from = "data", to = "leaf")
  )

  exprs <- structure(list(leaf = quote(dplyr::filter(data, TRUE))),
                     pending = character())

  s <- outline_sections(
    exprs, board,
    annotations = list(data = list(report = FALSE), leaf = list(report = TRUE)),
    stack_annotations = list()
  )

  expect_identical(s$ids, "leaf")

  html <- as.character(
    outline_output_map(s, blockr.core::board_block_ids(board))[["leaf"]]
  )

  expect_match(html, "upstream block 'data' is not reporting any code")
  expect_no_match(html, "applied to an object of class")
})

test_that("a name no block binds is listed next to the error", {

  # Third way, and the one no seeding can cover: the chunk names something
  # NEITHER the projection nor the board knows (a removed block, a slot name
  # that was never substituted). `data` then resolves to utils::data and the
  # error names nothing.
  board <- blockr.core::new_board(
    blocks = c(leaf = blockr.core::new_head_block())
  )

  s <- outline_sections(
    structure(list(leaf = quote(dplyr::filter(data, TRUE))),
              pending = character()),
    board,
    annotations = list(leaf = list(report = TRUE)),
    stack_annotations = list()
  )

  html <- as.character(
    outline_output_map(s, blockr.core::board_block_ids(board))[["leaf"]]
  )

  expect_match(html, "no block on the board binds")
  expect_match(html, "`data`")
})

test_that("a healthy chunk gets no unbound-name noise", {

  s <- list(ids = "a", pending = FALSE, exported = TRUE, report = TRUE,
            code = "a <- datasets::iris", report_calls = "", renderers = "")

  html <- as.character(outline_output_map(s, "a")[["a"]])

  expect_match(html, "blockr-otl-exhibit")
  expect_no_match(html, "no block on the board binds")
})

test_that("a block nothing feeds is a wiring note, not an R condition", {

  # The chart has no incoming link, so its chunk still names the input SLOT
  # (`data`), which no block declares. The raw condition is about
  # utils::data and says nothing; the link table is what turns it into a
  # sentence.
  board <- blockr.core::new_board(
    blocks = c(
      src = blockr.core::new_dataset_block("iris"),
      leaf = blockr.core::new_head_block()
    )
  )

  s <- outline_sections(
    structure(list(src = quote(datasets::iris),
                   leaf = quote(dplyr::filter(data, TRUE))),
              pending = character()),
    board,
    annotations = list(src = list(report = FALSE), leaf = list(report = TRUE)),
    stack_annotations = list()
  )

  html <- as.character(
    outline_output_map(
      s, blockr.core::board_block_ids(board), blockr.core::board_links(board)
    )[["leaf"]]
  )

  expect_match(html, "Nothing is connected to this block")
  expect_match(html, "the input slot no block fills")
  # The condition stays: on a deployment it is the only forensic trace.
  expect_match(html, "applied to an object of class")
})

test_that("a linked block that fails is not called unconnected", {

  board <- blockr.core::new_board(
    blocks = c(
      src = blockr.core::new_dataset_block("iris"),
      leaf = blockr.core::new_head_block()
    ),
    links = blockr.core::links(from = "src", to = "leaf")
  )

  s <- outline_sections(
    structure(list(src = quote(datasets::iris),
                   leaf = quote(stop("boom"))),
              pending = character()),
    board,
    annotations = list(src = list(report = FALSE), leaf = list(report = TRUE)),
    stack_annotations = list()
  )

  html <- as.character(
    outline_output_map(
      s, blockr.core::board_block_ids(board), blockr.core::board_links(board)
    )[["leaf"]]
  )

  expect_match(html, "Could not evaluate this block")
  expect_match(html, "boom")
  expect_no_match(html, "Nothing is connected")
})

test_that("a failure note carries the version that wrote it", {

  # A note off a deployment is often the only evidence available; without
  # the version, "is this the build I just pushed" cannot be answered.
  s <- list(ids = "a", pending = FALSE, exported = TRUE, report = TRUE,
            code = "a <- stop('boom')", report_calls = "", renderers = "")

  html <- as.character(outline_output_map(s, "a")[["a"]])

  expect_match(html, "blockr-otl-outver")
  expect_match(html, paste("blockr.outline", pkg_version()), fixed = TRUE)
})

test_that("an upstream holding a function accuses the upstream, not the leaf", {

  # The shape that survived three rounds of diagnosis: a pass-through block
  # with nothing connected evaluates its own input slot, binds utils::data
  # and SUCCEEDS, so it never gets a note. The first visible symptom is a
  # dependent failing with an error about a function nobody wrote.
  board <- blockr.core::new_board(
    blocks = c(
      up = blockr.core::new_dataset_block("iris"),
      leaf = blockr.core::new_head_block()
    ),
    links = blockr.core::links(from = "up", to = "leaf")
  )

  s <- outline_sections(
    structure(list(up = quote(base::sum),
                   leaf = quote(dplyr::filter(up, TRUE))),
              pending = character()),
    board,
    annotations = list(up = list(report = FALSE), leaf = list(report = TRUE)),
    stack_annotations = list()
  )

  html <- as.character(
    outline_output_map(
      s, blockr.core::board_block_ids(board), blockr.core::board_links(board)
    )[["leaf"]]
  )

  expect_match(html, "An upstream block produced a function, not data")
  expect_match(html, "Check what feeds `up`")
  # The class of every id the chunk reads: the fact that settles it.
  expect_match(html, "reads up = function")
})

test_that("a healthy chunk reads no failure line", {

  s <- list(ids = c("up", "leaf"), pending = c(FALSE, FALSE),
            exported = c(TRUE, TRUE), report = c(FALSE, TRUE),
            code = c("up <- datasets::iris", "leaf <- utils::head(up, 2)"),
            report_calls = c("", ""), renderers = c("", ""))

  html <- as.character(outline_output_map(s, c("up", "leaf"))[["leaf"]])

  expect_match(html, "blockr-otl-exhibit")
  expect_no_match(html, "reads up")
})
