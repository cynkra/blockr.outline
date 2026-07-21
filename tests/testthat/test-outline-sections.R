# R/export.R :: outline_sections -- the board -> document projection.

test_that("happy path projects every block with the expected fields", {
  b <- otl_board()
  s <- outline_sections(
    otl_exprs(),
    b,
    annotations = list(
      data = list(description = "the data", report = TRUE),
      head = list(report = FALSE)
    )
  )

  expect_setequal(s$ids, c("data", "sub", "head"))
  expect_topo <- match("data", s$ids) < match("sub", s$ids)
  expect_true(expect_topo)

  expect_identical(unname(s$names), c("Dataset", "Subset", "Head"))
  expect_identical(unname(s$descriptions[s$ids == "data"]), "the data")
  # report defaults TRUE, head was set FALSE.
  expect_true(s$report[s$ids == "data"])
  expect_false(s$report[s$ids == "head"])

  # Code carries the block assignment.
  expect_match(s$code[s$ids == "data"], "data <- ")
})

test_that("no expressions available is an error", {
  b <- otl_board()
  expect_error(
    outline_sections(structure(list(), pending = character()), b, list()),
    "no block expressions"
  )
})

test_that("a block with a missing expression is dropped, not fatal", {
  # `sub` reports no expression this flush: the projection narrows onto the
  # remaining blocks rather than aborting (the narrowing branch). A link
  # naming the dropped block must not break the rebuild.
  b <- otl_board()
  s <- outline_sections(otl_exprs(keep = c("data", "head")), b, list())
  expect_setequal(s$ids, c("data", "head"))
})

test_that("pending ids flow through to the pending flag", {
  b <- otl_board()
  s <- outline_sections(otl_exprs(pending = "head"), b, list())
  expect_true(s$pending[s$ids == "head"])
  expect_false(s$pending[s$ids == "data"])
})

test_that("kinds classify from the block category", {
  b <- otl_board_parallel()
  s <- outline_sections(otl_exprs_parallel(), b, list())
  # scatter -> plot -> "fig"; subset/head -> transform -> "tbl".
  expect_identical(unname(s$kinds[s$ids == "plot"]), "fig")
  expect_identical(unname(s$kinds[s$ids == "sub"]), "tbl")
})

test_that("movable is TRUE for parallel branches, FALSE in a linear chain", {
  linear <- outline_sections(otl_exprs(), otl_board(), list())
  expect_false(any(linear$movable))

  par <- otl_board_parallel()
  s <- outline_sections(otl_exprs_parallel(), par, list())
  # plot and audit are siblings off sub -> each can pass the other.
  expect_true(s$movable[s$ids == "plot"])
  expect_true(s$movable[s$ids == "audit"])
})

test_that("stack metadata is projected onto member blocks", {
  b <- otl_board(stacks = TRUE)
  s <- outline_sections(otl_exprs(), b, list())
  expect_identical(s$stack_ids[s$ids == "data"], "prep")
  expect_true(is.na(s$stack_ids[s$ids == "head"]))
  # A colour is always resolved (falls back when the stack has none).
  expect_match(s$stack_colors[["prep"]], "^#")
})

test_that("stack descriptions come from stack_annotations", {
  b <- otl_board(stacks = TRUE)
  s <- outline_sections(
    otl_exprs(), b, list(),
    stack_annotations = list(prep = list(description = "prep chapter"))
  )
  expect_identical(s$stack_descriptions[["prep"]], "prep chapter")
})
