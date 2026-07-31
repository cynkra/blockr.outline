# R/ext.R :: what a board update is allowed to redraw.
#
# The outline is a report surface over the whole board, so it reads board
# reactives -- and a board update carries far more than document changes.
# Fronting a dock tab commits a views delta: no block, link, stack or
# expression moves, yet a plain reactive() re-emits and the whole content
# pass (projection, display geometry, catalogue, syntax highlighting) ran
# again to produce byte-identical output. Measured at 5 blocks, one row
# click cost three full re-highlights.
#
# Two mechanisms stop that, and these tests pin both:
#   * board_shape / exprs_store -- identical-skip stores in front of the
#     projection's two inputs, so a views-only update never reaches it,
#   * the code_html cache -- per-block memoised markup, so a redraw that
#     changed no code re-highlights nothing.

otl_redraw_board <- function() {
  blockr.dock::new_dock_board(
    blocks = c(
      data  = blockr.core::new_dataset_block("iris"),
      sub   = blockr.core::new_subset_block(),
      audit = blockr.core::new_head_block()
    ),
    links = blockr.core::links(
      from = c("data", "sub"),
      to   = c("sub",  "audit")
    ),
    views = list(Main = c("data", "sub", "audit"))
  )
}

# The same board with one panel dropped from the active view: a views-only
# delta, the shape a dock panel gesture commits. Blocks, links and stacks
# are the SAME objects -- only the view moved.
drop_from_view <- function(brd, id) {
  views <- blockr.dock::board_views(brd)
  view <- blockr.dock::active_view(views)
  pid <- as.character(blockr.dock::as_block_panel_id(id))

  views[[view]] <- blockr.dock::dock_view(
    setdiff(blockr.dock::view_members(views[[view]]), pid),
    name = blockr.dock::view_name(views[[view]])
  )

  blockr.dock::board_views(brd) <- views
  brd
}

test_that("a views-only board update does not reach the projection", {
  testServer(
    outline_ext_srv(otl_ann(ids = c("data", "sub", "audit")), character(), "T"),
    {
      session$flushReact()

      board_before <- board$board
      shape_before <- board_shape()
      sects_before <- sections_store()
      exprs_before <- exprs_store()
      skel_before <- skel_store()

      expect_false(is.null(shape_before))
      expect_false(is.null(sects_before))

      board$board <- drop_from_view(board$board, "audit")
      session$flushReact()

      # The board itself really did change -- otherwise the rest is vacuous.
      expect_false(identical(board$board, board_before))

      # ...but nothing the document is made of did, so the projection's two
      # inputs hold still and outline_sections is never re-entered. Identity
      # is the assertion: a re-emitted store IS the re-run.
      expect_identical(board_shape(), shape_before)
      expect_identical(exprs_store(), exprs_before)
      expect_identical(sections_store(), sects_before)

      # The skip is narrow, not blanket: view membership drives dormancy,
      # so the skeleton does follow it.
      expect_false(identical(skel_store(), skel_before))
    },
    args = list(board = otl_board_args(otl_redraw_board()),
                update = reactiveVal())
  )
})

test_that("a structural board update does reach the projection", {
  # The counterpart: the skip must be narrow. Dropping a link changes the
  # shape signature, so the store re-emits and the document re-projects.
  testServer(
    outline_ext_srv(otl_ann(ids = c("data", "sub", "audit")), character(), "T"),
    {
      session$flushReact()

      shape_before <- board_shape()
      sects_before <- sections_store()

      brd <- board$board
      lnks <- blockr.core::board_links(brd)
      blockr.core::board_links(brd) <- lnks[lnks$to != "audit"]

      board$board <- brd
      session$flushReact()

      expect_false(identical(board_shape(), shape_before))
      expect_false(identical(sections_store(), sects_before))
    },
    args = list(board = otl_board_args(otl_redraw_board()),
                update = reactiveVal())
  )
})

test_that("the code map serves memoised markup and re-highlights on a change", {
  sects <- list(
    ids = c("a", "b"),
    code = c("a <- head(iris)", "b <- head(a)"),
    report = c(TRUE, FALSE),
    pending = c(FALSE, FALSE),
    report_calls = c(NA_character_, NA_character_),
    renderers = c(NA_character_, NA_character_)
  )

  cache <- new.env(parent = emptyenv())

  first <- outline_code_map(sects, cache = cache)
  expect_setequal(names(first), c("a", "b"))
  expect_setequal(ls(cache), c("a", "b"))

  # A second pass over unchanged sections returns the same markup. Proven
  # by the cache, not by recomputation: poison the stored html and watch it
  # come back, which only happens if the highlighter was skipped.
  cache[["a"]]$html <- "<poisoned/>"
  expect_identical(outline_code_map(sects, cache = cache)[["a"]], "<poisoned/>")

  # Edit a's chunk: the key moves, the poison is discarded, b is untouched.
  edited <- sects
  edited$code[1L] <- "a <- head(iris, 3)"

  out <- outline_code_map(edited, cache = cache)
  expect_false(identical(out[["a"]], "<poisoned/>"))
  expect_identical(out[["b"]], first[["b"]])

  # The report flag is part of the key too: flipping it rewrites the chunk
  # header (`include=FALSE`) and appends the exhibit line.
  flipped <- sects
  flipped$report[2L] <- TRUE
  expect_false(
    identical(outline_code_map(flipped, cache = cache)[["b"]], first[["b"]])
  )
})

otl_row_sects <- function() {
  list(
    ids = c("a", "b"),
    names = c("A", "B"),
    icons = c(NA_character_, NA_character_),
    descriptions = c("", ""),
    report = c(TRUE, TRUE),
    exported = c(TRUE, TRUE),
    active = c(TRUE, FALSE),
    gated = TRUE,
    movable = c(FALSE, FALSE),
    pending = c(FALSE, FALSE),
    drop_lo = c(1L, 1L),
    drop_hi = c(2L, 2L),
    stack_ids = c(NA_character_, NA_character_),
    stack_names = c(NA_character_, NA_character_),
    stack_colors = character(),
    stack_descriptions = list(),
    chap_targets = list(),
    code_html = list(a = "<pre>a</pre>", b = "<pre>b</pre>")
  )
}

test_that("the row map holds the same rows the full render draws", {
  sects <- otl_row_sects()
  ns <- NS("otl")

  rows <- outline_row_map(sects, ns)
  expect_setequal(names(rows), c("a", "b"))

  # Each row must be the self-contained element the client swaps, and the
  # full render must contain that same element -- the identity that keeps a
  # pushed row and a rendered one from drifting apart. Compared with
  # whitespace collapsed: htmltools indents by nesting depth, so the
  # standalone row and the row inside the grid are the same markup laid out
  # differently (which is also why the client cannot compare a row against
  # the DOM's own serialization -- the server decides what changed).
  squeeze <- function(x) gsub("\\s+", " ", trimws(x))

  full <- squeeze(as.character(outline_tags(sects, ns)))

  for (id in names(rows)) {
    expect_match(rows[[id]], "^<div class=\"blockr-otl-grow")
    expect_true(grepl(squeeze(rows[[id]]), full, fixed = TRUE))
  }
})

test_that("activating a block changes only that block's row", {
  # The gesture behind the complaint: a row click activates one block. Every
  # other row has to come out byte-identical, or the push degenerates into
  # a full redraw by another name.
  before <- outline_row_map(otl_row_sects(), NS("otl"))

  sects <- otl_row_sects()
  sects$active <- c(TRUE, TRUE)
  after <- outline_row_map(sects, NS("otl"))

  expect_identical(after[["a"]], before[["a"]])
  expect_false(identical(after[["b"]], before[["b"]]))
})

test_that("the layout key ignores per-row changes and catches structural ones", {
  base <- otl_row_sects()
  key <- outline_layout_key(base)

  # Per-row: a row swap carries these, so they must not force a render.
  for (mut in list(
    function(s) {s$active <- c(TRUE, TRUE); s},
    function(s) {s$names <- c("A2", "B"); s},
    function(s) {s$descriptions <- c("new", ""); s},
    function(s) {s$code_html <- list(a = "<pre>x</pre>", b = "<pre>b</pre>"); s}
  )) {
    expect_identical(outline_layout_key(mut(base)), key)
  }

  # Structural: the row set, its order, and the modes that redraw anyway.
  reordered <- base
  reordered$ids <- c("b", "a")
  expect_false(identical(outline_layout_key(reordered), key))

  dropped <- base
  dropped$ids <- "a"
  dropped$report <- TRUE
  expect_false(identical(outline_layout_key(dropped), key))

  # An open editor is a Shiny-bound widget, so it has to bind through
  # renderUI rather than arrive as raw markup.
  expect_false(identical(outline_layout_key(base, editing = "a"), key))

  out <- base
  out$body_mode <- "output"
  expect_false(identical(outline_layout_key(out), key))
})

test_that("a report flip on an unstacked board is not a layout change", {
  # No chapters, no chapter action label -- so the commonest gesture there
  # is must travel as a row push rather than a full redraw.
  base <- otl_row_sects()
  flipped <- base
  flipped$report <- c(TRUE, FALSE)

  expect_identical(outline_layout_key(flipped), outline_layout_key(base))
  expect_false(
    identical(
      outline_row_map(flipped, NS("otl"))[["b"]],
      outline_row_map(base, NS("otl"))[["b"]]
    )
  )
})

test_that("a render triggered elsewhere paints the pushed state, not a snapshot", {
  # The regression this pins: the sections renderUI paints were once held in
  # a reactiveVal written ONLY on layout changes, so any render fired for
  # another reason -- switching the code view to R script and back is the
  # one users hit -- repainted the document as it was before every row that
  # had been pushed since, silently undoing them.
  # The class the `sub` row is wearing, read out of the rendered document.
  # `sub` rather than a leaf: an unreported LEAF is outside the export
  # closure and draws no row at all, so there would be nothing to inspect.
  sub_row_class <- function(html) {
    line <- grep(
      "blockr-otl-grow[^>]*data-blk=\"sub\"",
      strsplit(as.character(html), "\n")[[1L]],
      value = TRUE
    )
    expect_length(line, 1L)
    sub(".*class=\"([^\"]*)\".*", "\\1", line)
  }

  testServer(
    outline_ext_srv(
      otl_ann(sub = list(report = FALSE), ids = c("data", "sub", "audit")),
      character(), "T"
    ),
    {
      session$flushReact()

      expect_false(grepl("\\bon\\b", sub_row_class(output$outline_out)))

      # A row-level change: flip a report flag. Layout holds, so this goes
      # out as a push and renderUI is NOT re-run.
      session$setInputs(outline_toggle = list(id = "sub", report = TRUE))
      session$flushReact()

      expect_true(ann_report(rv_ann(), "sub"))

      # Now make renderUI run for an unrelated reason.
      session$setInputs(code_view = "script")
      session$flushReact()
      session$setInputs(code_view = "outline")
      session$flushReact()

      # The repaint has to carry the flip that was pushed while it was away.
      expect_true(grepl("\\bon\\b", sub_row_class(output$outline_out)))
    },
    args = list(board = otl_board_args(otl_redraw_board()),
                update = reactiveVal())
  )
})

test_that("a chapter's action label keeps its whole-run aggregate in the key", {
  # `exclude all` shows only when every block of the run is on, so the key
  # carries that aggregate -- toggling ONE block of a chapter leaves the
  # layout alone, toggling the last one does not.
  base <- otl_row_sects()
  base$stack_ids <- c("s", "s")
  base$stack_names <- c("S", "S")
  base$stack_colors <- c(s = "#000000")

  key <- outline_layout_key(base)

  one_off <- base
  one_off$report <- c(TRUE, FALSE)
  expect_false(identical(outline_layout_key(one_off), key))

  # ...and back on again is the same layout it started as.
  expect_identical(outline_layout_key(base), key)
})

test_that("the code map without a cache is unchanged", {
  # Every non-server caller (exporters, tests) passes no cache and must get
  # freshly highlighted markup.
  sects <- list(
    ids = "a", code = "a <- head(iris)", report = TRUE, pending = FALSE,
    report_calls = NA_character_, renderers = NA_character_
  )

  expect_identical(
    outline_code_map(sects),
    outline_code_map(sects, cache = new.env(parent = emptyenv()))
  )
})
