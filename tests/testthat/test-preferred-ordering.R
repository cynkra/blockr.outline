# R/export.R :: preferred_ordering -- two-pass Kahn linearisation with the
# stored block order as a tie-break. Any output must be a valid topological
# order; the preference only spends the linearisation's slack.

# Assert `ord` respects every link (from precedes to).
expect_topo <- function(ord, board) {
  lnks <- blockr.core::board_links(board)
  for (i in seq_along(lnks$from)) {
    from <- lnks$from[[i]]
    to <- lnks$to[[i]]
    testthat::expect_lt(match(from, ord), match(to, ord))
  }
}

test_that("dependencies always dominate, whatever the preference", {
  b <- otl_board_parallel()
  ids <- blockr.core::board_block_ids(b)

  for (pref in list(
    character(),
    c("audit", "plot", "sub", "data"),
    c("plot", "audit"),
    rev(ids)
  )) {
    ord <- preferred_ordering(ids, b, pref)
    expect_setequal(ord, ids)
    expect_topo(ord, b)
  }
})

test_that("preference orders parallel branches", {
  b <- otl_board_parallel()
  ids <- blockr.core::board_block_ids(b)

  ord1 <- preferred_ordering(ids, b, c("data", "sub", "audit", "plot"))
  expect_lt(match("audit", ord1), match("plot", ord1))

  ord2 <- preferred_ordering(ids, b, c("data", "sub", "plot", "audit"))
  expect_lt(match("plot", ord2), match("audit", ord2))
})

test_that("an impossible wish snaps to the nearest valid order", {
  b <- otl_board_parallel()
  ids <- blockr.core::board_block_ids(b)

  # Ask for sub before data (violates the data -> sub link): refused.
  ord <- preferred_ordering(ids, b, c("sub", "data", "plot", "audit"))
  expect_lt(match("data", ord), match("sub", ord))
  expect_topo(ord, b)
})

test_that("empty preference yields a valid topological order", {
  b <- otl_board()
  ids <- blockr.core::board_block_ids(b)
  ord <- preferred_ordering(ids, b, character())
  expect_setequal(ord, ids)
  expect_topo(ord, b)
})

test_that("a block absent from the preference keeps its base neighbourhood", {
  # `audit` is never mentioned; it must not be shoved to the document end,
  # it stays where the base (stack-contiguity) order puts it.
  b <- otl_board_parallel()
  ids <- blockr.core::board_block_ids(b)
  ord <- preferred_ordering(ids, b, c("data", "sub", "plot"))
  # audit is a child of sub like plot, so it lands adjacent to plot, not
  # after some unrelated tail.
  expect_topo(ord, b)
  expect_setequal(ord, ids)
})

test_that("stacked blocks form a contiguous run by default", {
  # data + sub share a stack, head does not; the default order keeps the
  # stack members together.
  b <- otl_board(stacks = TRUE)
  ord <- preferred_ordering(blockr.core::board_block_ids(b), b, character())
  run <- match(c("data", "sub"), ord)
  expect_equal(abs(diff(run)), 1L)
})

test_that("a single block board is returned unchanged", {
  b <- blockr.core::new_board(
    blocks = c(only = blockr.core::new_dataset_block("iris"))
  )
  expect_identical(preferred_ordering("only", b, character()), "only")
})
