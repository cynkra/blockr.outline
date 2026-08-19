# The report server: state contract, the item list's interactions,
# lifecycle GC. Uses the blind board (expressions error on touch), which
# doubles as proof the builder interactions never evaluate a block.

test_that("state names are exactly the constructor formals, and round-trip", {
  testServer(
    report_ext_srv(
      list(
        list(text = "Why we look."),
        list(block = "audit", code = TRUE, output = FALSE),
        list(block = "plot")
      ),
      "Iris report",
      list(toc = TRUE, warnings = TRUE)
    ),
    {
      session$flushReact()

      state <- session$getReturned()$state

      expect_named(state, c("items", "title", "settings"))

      items <- state$items()
      expect_length(items, 3L)
      expect_identical(items[[1L]]$text, "Why we look.")
      expect_identical(items[[2L]]$block, "audit")
      expect_true(items[[2L]]$code)
      expect_false(items[[2L]]$output)
      expect_identical(items[[3L]]$block, "plot")
      expect_false(items[[3L]]$code)
      expect_true(items[[3L]]$output)

      expect_identical(state$title(), "Iris report")
      expect_true(state$settings()$toc)
      expect_true(state$settings()$warnings)
      expect_identical(state$settings()$block_titles, "headings")

      # The restore contract, pinned: blockr.dock deserializes an extension
      # by do.call(ctor, payload), so every state name must be a formal.
      # By NAME, as blockr.dock's ctor_fun() resolves it -- a function value
      # breaks the ctor's own call introspection.
      payload <- lapply(state, function(rv) rv())
      expect_no_error(do.call("new_report_extension", payload))
    },
    args = list(board = blind_board_args(), update = reactiveVal())
  )
})

test_that("a v1 payload (blocks + annotations + intro) converts to items", {
  items <- legacy_report_items(
    c("audit", "plot"),
    list(audit = list(description = "A note.")),
    "The intro."
  )

  expect_length(items, 4L)
  expect_identical(items[[1L]]$text, "The intro.")
  expect_identical(items[[2L]]$text, "A note.")
  expect_identical(items[[3L]]$block, "audit")
  expect_identical(items[[4L]]$block, "plot")

  # Converted picks are output-only, the v1 document's shape.
  expect_false(items[[3L]]$code)
  expect_true(items[[3L]]$output)

  # And the ctor takes the legacy formals directly.
  expect_no_error(
    new_report_extension(
      blocks = "audit",
      annotations = list(audit = list(description = "A note.")),
      intro = "The intro.",
      format = "pptx"
    )
  )
})

test_that("sanitize_items dedupes blocks and drops junk", {
  items <- sanitize_items(list(
    list(block = "a"),
    list(block = "a", code = TRUE),
    list(text = "kept"),
    list(bogus = 1),
    "not an item",
    list(block = "b", output = FALSE, fig_width = 11, full_width = TRUE)
  ))

  expect_length(items, 3L)
  expect_identical(item_block_ids(items), c("a", "b"))
  expect_identical(items[[2L]]$text, "kept")
  expect_false(items[[3L]]$output)
  expect_identical(items[[3L]]$fig_width, 11)
  expect_true(items[[3L]]$full_width)
})

test_that("unknown settings error; known ones coerce", {
  expect_error(
    sanitize_settings(list(tocc = TRUE)),
    "Unknown report setting"
  )
  expect_error(
    sanitize_settings(list(block_titles = "chapter")),
    "block_titles"
  )

  s <- sanitize_settings(list(fig_width = "12", code_fold = 1))
  expect_identical(s$fig_width, 12)
  expect_false(s$code_fold)
  expect_identical(s$fig_height, 4.5)
})

test_that("the toggles flip flags, and the menu sets figure size", {
  testServer(
    report_ext_srv(list(list(block = "audit")), "Report", list()),
    {
      session$flushReact()

      session$setInputs(rpt_act = list(idx = 1, act = "code"))
      expect_true(rv_items()[[1L]]$code)

      session$setInputs(rpt_act = list(idx = 1, act = "output"))
      expect_false(rv_items()[[1L]]$output)

      # Both off is legal: the item stays.
      expect_length(rv_items(), 1L)

      session$setInputs(rpt_act = list(idx = 1, act = "fullw"))
      expect_true(rv_items()[[1L]]$full_width)

      session$setInputs(rpt_fig = list(idx = 1, w = 12, h = 4))
      expect_identical(rv_items()[[1L]]$fig_width, 12)
      expect_identical(rv_items()[[1L]]$fig_height, 4)

      # Back to the document default: both cleared.
      session$setInputs(rpt_fig = list(idx = 1, w = NULL, h = NULL))
      expect_null(rv_items()[[1L]]$fig_width)
      expect_null(rv_items()[[1L]]$fig_height)
    },
    args = list(board = blind_board_args(), update = reactiveVal())
  )
})

test_that("text items insert, edit save-then-close, and cancel discards", {
  testServer(
    report_ext_srv(list(list(block = "audit")), "Report", list()),
    {
      session$flushReact()

      # Insert above: the text lands at the block's old index, its editor
      # open.
      session$setInputs(rpt_act = list(idx = 1, act = "text_above"))
      expect_length(rv_items(), 2L)
      expect_identical(rv_items()[[1L]]$text, "")
      expect_identical(editing(), 1L)

      session$setInputs(rpt_desc_edit = "A **lead** paragraph.")
      session$setInputs(rpt_desc_save = 1)
      expect_null(editing())
      expect_identical(rv_items()[[1L]]$text, "A **lead** paragraph.")

      # Cancel discards: the item keeps its saved value.
      session$setInputs(rpt_act = list(idx = 1, act = "edit"))
      expect_identical(editing(), 1L)
      session$setInputs(rpt_desc_edit = "scratch that")
      session$setInputs(rpt_desc_cancel = 1)
      expect_null(editing())
      expect_identical(rv_items()[[1L]]$text, "A **lead** paragraph.")

      # Insert below. The pristine cancel removes the fresh empty item
      # again (no litter; pinned in its own test).
      session$setInputs(rpt_act = list(idx = 2, act = "text_below"))
      expect_length(rv_items(), 3L)
      expect_identical(editing(), 3L)
      session$setInputs(rpt_desc_cancel = 1)
      expect_length(rv_items(), 2L)
    },
    args = list(board = blind_board_args(), update = reactiveVal())
  )
})

test_that("drag moves an item by index", {
  testServer(
    report_ext_srv(
      list(
        list(text = "lead"),
        list(block = "audit"),
        list(block = "plot")
      ),
      "Report", list()
    ),
    {
      session$flushReact()

      # Drop the text after the last block.
      session$setInputs(rpt_move = list(from = 1, to = 3, after = TRUE))
      expect_identical(
        chr_ply(rv_items(), function(x) coal(x$block, x$text)),
        c("audit", "plot", "lead")
      )

      # And back to the front.
      session$setInputs(rpt_move = list(from = 3, to = 1, after = FALSE))
      expect_identical(
        chr_ply(rv_items(), function(x) coal(x$block, x$text)),
        c("lead", "audit", "plot")
      )
    },
    args = list(board = blind_board_args(), update = reactiveVal())
  )
})

test_that("a removed block loses its item; text items survive", {
  testServer(
    report_ext_srv(
      list(
        list(text = "kept"),
        list(block = "plot"),
        list(block = "audit")
      ),
      "Report", list()
    ),
    {
      session$flushReact()
      expect_identical(item_block_ids(rv_items()), c("plot", "audit"))

      board$board <- drop_block(board$board, "plot")
      session$flushReact()

      expect_identical(item_block_ids(rv_items()), "audit")
      expect_identical(rv_items()[[1L]]$text, "kept")
    },
    args = list(board = blind_board_args(), update = reactiveVal())
  )
})

test_that("the settings inputs write through the sanitizer", {
  testServer(
    report_ext_srv(list(), "Report", list()),
    {
      session$flushReact()

      session$setInputs(rpt_set_titles = "captions")
      expect_identical(rv_settings()$block_titles, "captions")

      session$setInputs(rpt_set_toc = TRUE)
      expect_true(rv_settings()$toc)

      session$setInputs(rpt_set_figw = 10)
      expect_identical(rv_settings()$fig_width, 10)

      session$setInputs(rpt_set_warnings = TRUE)
      expect_true(rv_settings()$warnings)
    },
    args = list(board = blind_board_args(), update = reactiveVal())
  )
})

test_that("cancelling a fresh, empty text item removes it again", {
  testServer(
    report_ext_srv(list(list(block = "audit")), "Report", list()),
    {
      session$flushReact()

      # + then click elsewhere: no litter.
      session$setInputs(rpt_act = list(idx = 1, act = "text_below"))
      expect_length(rv_items(), 2L)
      session$setInputs(rpt_desc_cancel = 1)
      expect_length(rv_items(), 1L)

      # Same for the insert above.
      session$setInputs(rpt_act = list(idx = 1, act = "text_above"))
      expect_length(rv_items(), 2L)
      session$setInputs(rpt_desc_cancel = 1)
      expect_length(rv_items(), 1L)

      # A SAVED item survives a later pristine cancel.
      session$setInputs(rpt_act = list(idx = 1, act = "text_below"))
      session$setInputs(rpt_desc_edit = "kept")
      session$setInputs(rpt_desc_save = 1)
      session$setInputs(rpt_act = list(idx = 2, act = "edit"))
      session$setInputs(rpt_desc_cancel = 1)
      expect_length(rv_items(), 2L)
      expect_identical(rv_items()[[2L]]$text, "kept")
    },
    args = list(board = blind_board_args(), update = reactiveVal())
  )
})
