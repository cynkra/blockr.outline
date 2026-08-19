# The report projection: slide_sections() with the item list's block ids
# as the picks. Pure-function layer, same fixtures as the deck's own tests.

test_that("added blocks are picked; ancestors ride along exported", {
  s <- report_sections(
    otl_exprs(), otl_board(stacks = FALSE),
    picked = "head"
  )

  expect_true(s$report[s$ids == "head"])
  expect_false(s$report[s$ids == "sub"])
  # An unpicked ancestor is still evaluated for the render.
  expect_true(s$exported[s$ids == "sub"])
  # Prose lives in the item list now, never in the projection.
  expect_identical(unique(unname(s$descriptions)), "")
})

test_that("item order is the ordering preference, snapped to legality", {
  s <- report_sections(
    otl_exprs(), otl_board(stacks = FALSE),
    picked = c("head", "sub")
  )

  # head is picked first but depends on sub: the snap keeps evaluation
  # order while honouring the preference where it can.
  expect_lt(match("sub", s$ids), match("head", s$ids))
})
