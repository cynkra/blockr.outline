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
