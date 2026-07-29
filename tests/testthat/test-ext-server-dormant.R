# R/ext.R :: dormant-by-default rows on view-gated boards.
#
# On a multi-view board a block outside the active view is DORMANT in the
# outline: its skeleton row carries active = FALSE (rendered as one
# condensed title + description line, no code cell), and the code map is
# narrowed to the active blocks so dormant chunks are never highlighted.
# Activation is view MEMBERSHIP -- the same board state a row click
# commits through the open handler -- so the dock stays the single writer
# of core's `required` channel. The gear's show-all checkbox and the
# single-view default both disable the gating.

# Two views: the active one holds data + sub, the second plot + audit.
otl_two_view_board <- function() {
  blockr.dock::new_dock_board(
    blocks = c(
      data  = blockr.core::new_dataset_block("iris"),
      sub   = blockr.core::new_subset_block(),
      plot  = blockr.core::new_scatter_block("Sepal.Length", "Sepal.Width"),
      audit = blockr.core::new_head_block()
    ),
    links = blockr.core::links(
      from = c("data", "sub",  "sub"),
      to   = c("sub",  "plot", "audit")
    ),
    views = list(
      Main  = c("data", "sub"),
      Extra = c("plot", "audit")
    )
  )
}

test_that("blocks outside the active view are dormant in the skeleton", {
  testServer(
    outline_ext_srv(list(), character(), "T"),
    {
      session$flushReact()

      skel <- skel_store()
      expect_false(is.null(skel))
      expect_true(skel$gated)

      active <- setNames(skel$active, skel$ids)
      expect_true(active[["data"]])
      expect_true(active[["sub"]])
      expect_false(active[["plot"]])
      expect_false(active[["audit"]])

      # The code map only carries the active blocks' markup.
      expect_setequal(names(code_store()), c("data", "sub"))
    },
    args = list(board = otl_board_args(otl_two_view_board()),
                update = reactiveVal())
  )
})

test_that("the show-all override disables the gating", {
  testServer(
    outline_ext_srv(list(), character(), "T"),
    {
      session$flushReact()
      expect_false(all(skel_store()$active))

      session$setInputs(otl_show_all = TRUE)
      session$flushReact()

      skel <- skel_store()
      expect_false(skel$gated)
      expect_true(all(skel$active))
      expect_setequal(names(code_store()), skel$ids)
    },
    args = list(board = otl_board_args(otl_two_view_board()),
                update = reactiveVal())
  )
})

test_that("a single-view board keeps every block active", {
  # The default dock board holds all panels in its one view, so nothing
  # condenses -- the pre-dormancy rendering, unchanged.
  testServer(
    outline_ext_srv(list(), character(), "T"),
    {
      session$flushReact()

      skel <- skel_store()
      expect_true(all(skel$active))
      expect_setequal(names(code_store()), skel$ids)
    },
    args = list(board = otl_board_args(), update = reactiveVal())
  )
})

test_that("outline_hide removes the block's panel from the active view", {
  testServer(
    outline_ext_srv(list(), character(), "T"),
    {
      session$flushReact()

      session$setInputs(outline_hide = list(id = "data"))
      session$flushReact()

      upd <- isolate(update())
      expect_false(is.null(upd$views$mod))

      view <- names(upd$views$mod)
      expect_length(view, 1L)

      pid <- as.character(blockr.dock::as_block_panel_id("data"))
      expect_identical(upd$views$mod[[view]]$rm, pid)
    },
    args = list(board = otl_board_args(otl_two_view_board()),
                update = reactiveVal())
  )
})

test_that("hiding a block not in the active view is a no-op", {
  testServer(
    outline_ext_srv(list(), character(), "T"),
    {
      session$flushReact()

      # `plot` lives in the Extra view; the active view has nothing to
      # remove, so no update must be committed.
      session$setInputs(outline_hide = list(id = "plot"))
      session$flushReact()

      expect_null(isolate(update()))
    },
    args = list(board = otl_board_args(otl_two_view_board()),
                update = reactiveVal())
  )
})

test_that("dormant rows render condensed: no code cell, one line", {
  sects <- list(
    ids = c("a", "b"),
    names = c("A", "B"),
    icons = c(NA_character_, NA_character_),
    descriptions = c("", "The **second** block."),
    report = c(TRUE, TRUE),
    exported = c(TRUE, TRUE),
    pending = c(FALSE, FALSE),
    movable = c(FALSE, FALSE),
    drop_lo = c(0L, 0L),
    drop_hi = c(1L, 1L),
    stack_ids = c(NA_character_, NA_character_),
    stack_names = c(NA_character_, NA_character_),
    stack_colors = character(),
    stack_descriptions = list(),
    chap_targets = list(),
    active = c(TRUE, FALSE),
    gated = TRUE,
    code_html = list(a = "<pre>code a</pre>", b = "<pre>code b</pre>"),
    body_mode = "code"
  )

  html <- as.character(outline_tags(sects, shiny::NS("x")))

  # Active row: code cell present, no dormant chrome on it.
  expect_match(html, "x-code-a")
  expect_match(html, "code a", fixed = TRUE)

  # Dormant row: condensed line, no code cell, no code markup.
  expect_match(html, "blockr-otl-dormant")
  expect_no_match(html, "x-code-b")
  expect_no_match(html, "code b")

  # The hide affordance sits on the ACTIVE row only.
  expect_match(html, "blockr-otl-eyeoff")

  # Markdown in the dormant description flattens to plain text.
  expect_match(html, "The second block.", fixed = TRUE)
  expect_no_match(html, "<strong>second</strong>")
})

test_that("output mode ignores dormancy: the preview is the document", {
  sects <- list(
    ids = c("a", "b"),
    names = c("A", "B"),
    icons = c(NA_character_, NA_character_),
    descriptions = c("", ""),
    report = c(TRUE, TRUE),
    exported = c(TRUE, TRUE),
    pending = c(FALSE, FALSE),
    movable = c(FALSE, FALSE),
    drop_lo = c(0L, 0L),
    drop_hi = c(1L, 1L),
    stack_ids = c(NA_character_, NA_character_),
    stack_names = c(NA_character_, NA_character_),
    stack_colors = character(),
    stack_descriptions = list(),
    chap_targets = list(),
    active = c(TRUE, FALSE),
    gated = TRUE,
    code_html = list(
      a = htmltools::div("exhibit a"),
      b = htmltools::div("exhibit b")
    ),
    body_mode = "output"
  )

  html <- as.character(outline_tags(sects, shiny::NS("x")))

  # Both exhibits render; the dormant condensation does not apply.
  expect_match(html, "exhibit a")
  expect_match(html, "exhibit b")
  expect_no_match(html, "blockr-otl-dormant")
})

test_that("outline_code_map narrows to the requested ids", {
  sects <- list(
    ids = c("a", "b"),
    report = c(TRUE, TRUE),
    exported = c(TRUE, TRUE),
    pending = c(FALSE, FALSE),
    code = c("head(x)", "tail(x)"),
    kinds = c("transform", "transform"),
    renderers = c(NA_character_, NA_character_),
    report_calls = list(NULL, NULL)
  )

  full <- outline_code_map(sects)
  expect_setequal(names(full), c("a", "b"))

  narrowed <- outline_code_map(sects, "b")
  expect_identical(names(narrowed), "b")
  expect_identical(narrowed$b, full$b)

  # Unknown ids are dropped, not errors.
  expect_identical(names(outline_code_map(sects, c("b", "zzz"))), "b")
})

# ---- report-only listing + include picker -----------------------------

test_that("the skeleton lists only reported blocks; the rest are addable", {
  testServer(
    outline_ext_srv(
      list(
        plot = list(report = FALSE),
        audit = list(report = FALSE)
      ),
      character(), "T"
    ),
    {
      session$flushReact()

      skel <- skel_store()
      expect_setequal(skel$ids, c("data", "sub"))
      expect_setequal(unname(skel$addable), c("plot", "audit"))

      # Names label the picker entries.
      expect_true(all(nzchar(names(skel$addable))))

      # The code map narrows to the listed rows.
      expect_true(all(names(code_store()) %in% c("data", "sub")))

      # Sections stay FULL: the exporters still see the whole document.
      expect_setequal(sections_store()$ids, c("data", "sub", "plot", "audit"))
    },
    args = list(board = otl_board_args(), update = reactiveVal())
  )
})

test_that("picking a block from the pool lists it", {
  testServer(
    outline_ext_srv(
      list(plot = list(report = FALSE)),
      character(), "T"
    ),
    {
      session$flushReact()
      expect_false("plot" %in% skel_store()$ids)

      session$setInputs(otl_include = "plot")
      session$flushReact()

      skel <- skel_store()
      expect_true("plot" %in% skel$ids)
      expect_false("plot" %in% skel$addable)
      expect_true(sections_store()$report[sections_store()$ids == "plot"])
    },
    args = list(board = otl_board_args(), update = reactiveVal())
  )
})

test_that("show-all restores the full board overview", {
  testServer(
    outline_ext_srv(
      list(plot = list(report = FALSE)),
      character(), "T"
    ),
    {
      session$flushReact()
      expect_false("plot" %in% skel_store()$ids)

      session$setInputs(otl_show_all = TRUE)
      session$flushReact()

      skel <- skel_store()
      expect_setequal(skel$ids, c("data", "sub", "plot", "audit"))
      expect_length(skel$addable, 0L)
    },
    args = list(board = otl_board_args(), update = reactiveVal())
  )
})

test_that("display geometry keeps order pinned through hidden blocks", {
  # a -> b -> c with b hidden: a and c must stay pinned (the dependency
  # runs THROUGH the hidden block), which only holds when reachability
  # walks the full graph.
  b <- blockr.core::new_board(
    blocks = c(
      a = blockr.core::new_dataset_block("iris"),
      b = blockr.core::new_subset_block(),
      c = blockr.core::new_head_block()
    ),
    links = blockr.core::links(from = c("a", "b"), to = c("b", "c"))
  )

  full <- outline_sections(
    structure(
      list(
        a = quote(iris), b = quote(subset(a)), c = quote(utils::head(b))
      ),
      pending = character()
    ),
    b, list()
  )

  disp <- display_sections(full, c("a", "c"), blockr.core::board_links(b))

  expect_identical(disp$ids, c("a", "c"))
  expect_false(any(disp$movable))

  # Drag ranges collapse to the no-op gap for both rows.
  expect_identical(disp$drop_lo, c(0L, 1L))
  expect_identical(disp$drop_hi, c(0L, 1L))
})
