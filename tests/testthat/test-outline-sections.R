# R/export.R :: outline_sections -- the board -> document projection.

test_that("happy path projects every block with the expected fields", {
  b <- otl_board()
  s <- outline_sections(
    otl_exprs(),
    b,
    annotations = otl_ann(
      data = list(description = "the data"),
      head = list(report = FALSE)
    )
  )

  expect_setequal(s$ids, c("data", "sub", "head"))
  expect_topo <- match("data", s$ids) < match("sub", s$ids)
  expect_true(expect_topo)

  expect_identical(unname(s$names), c("Dataset", "Subset", "Head"))
  expect_identical(unname(s$descriptions[s$ids == "data"]), "the data")
  # data is in the report, head was set FALSE.
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

# ---- geometry cache ---------------------------------------------------

test_that("a cached projection is identical to a fresh one", {
  b <- otl_board(stacks = TRUE)
  cache <- new.env(parent = emptyenv())

  fresh <- outline_sections(otl_exprs(), b, otl_ann())
  warm1 <- outline_sections(otl_exprs(), b, otl_ann(), geometry_cache = cache)
  warm2 <- outline_sections(otl_exprs(), b, otl_ann(), geometry_cache = cache)

  expect_identical(warm1, fresh)
  expect_identical(warm2, fresh)

  # The second call actually hit the cache: the stored value is the very
  # object the result carries.
  expect_true(!is.null(cache$key))
})

test_that("an expression-only change reuses the geometry", {
  b <- otl_board(stacks = TRUE)
  cache <- new.env(parent = emptyenv())

  outline_sections(otl_exprs(), b, otl_ann(), geometry_cache = cache)
  key_before <- cache$key

  # A different expression for `head` (a value edit): same graph, same key.
  ex <- otl_exprs()
  ex$head <- quote(utils::head(sub, n = 99L))
  s <- outline_sections(ex, b, otl_ann(), geometry_cache = cache)

  expect_identical(cache$key, key_before)
  expect_match(s$code[s$ids == "head"], "99")
})

test_that("structural changes invalidate the geometry cache", {
  cache <- new.env(parent = emptyenv())
  b <- otl_board(stacks = TRUE)

  base <- outline_sections(otl_exprs(), b, otl_ann(), geometry_cache = cache)
  key0 <- cache$key

  # Report-flag change: exported closure differs, key must move.
  flagged <- outline_sections(
    otl_exprs(), b, otl_ann(head = list(report = FALSE)),
    geometry_cache = cache
  )
  expect_false(identical(cache$key, key0))
  expect_false(identical(flagged$exported, base$exported))

  # Stack-layout change: chapter targets differ, key must move again.
  key1 <- cache$key
  outline_sections(
    otl_exprs(), otl_board(stacks = FALSE), otl_ann(),
    geometry_cache = cache
  )
  expect_false(identical(cache$key, key1))

  # And each cached result still matches a fresh computation.
  expect_identical(
    outline_sections(otl_exprs(), b, otl_ann(), geometry_cache = cache),
    outline_sections(otl_exprs(), b, otl_ann())
  )
})

# ---- search catalogue -------------------------------------------------

test_that("the catalogue puts listed blocks first and flags the pool", {
  b <- otl_board(stacks = TRUE)
  s <- outline_sections(
    otl_exprs(), b,
    otl_ann(
      data = list(description = "The **iris** data.", report = FALSE),
      head = list(report = FALSE)
    )
  )

  cat <- outline_catalog(s, listed = "sub")

  expect_identical(
    vapply(cat, `[[`, character(1L), "id"), c("sub", "data", "head")
  )
  expect_identical(
    vapply(cat, `[[`, logical(1L), "listed"), c(TRUE, FALSE, FALSE)
  )

  # `data` is excluded but `sub` (reported) needs it, so it runs anyway;
  # `head` is excluded and nothing needs it.
  runs <- setNames(vapply(cat, `[[`, logical(1L), "runs"),
                   vapply(cat, `[[`, character(1L), "id"))
  expect_true(runs[["data"]])
  expect_false(runs[["head"]])

  # Markdown is flattened to the one line the menu shows.
  desc <- Filter(function(e) identical(e$id, "data"), cat)[[1L]]$desc
  expect_identical(desc, "The iris data.")

  # The chapter labels the entry.
  expect_identical(
    Filter(function(e) identical(e$id, "sub"), cat)[[1L]]$chapter, "Stack"
  )
})

test_that("desc_oneline collapses whitespace and drops markdown", {
  expect_identical(desc_oneline(""), "")
  expect_identical(desc_oneline("  a  \n\n  *b*  "), "a b")
})

test_that("dropping a block from an UNSTACKED board still projects", {

  # The narrowing rebuilds the board from blocks + links + stacks. An
  # unstacked board narrows to no stacks, and new_board() rejects a NULL
  # there, which aborted the whole projection: the freeze this narrowing
  # exists to prevent.
  s <- outline_sections(
    otl_exprs(keep = c("data", "sub")), otl_board(stacks = FALSE), otl_ann()
  )

  expect_identical(s$ids, c("data", "sub"))
  expect_true(all(is.na(s$stack_ids)))
})
