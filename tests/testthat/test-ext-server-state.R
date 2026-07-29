# R/ext.R :: outline_ext_srv -- the serialization contract and lifecycle GC.

test_that("the server exposes every state reactive", {
  srv <- outline_ext_srv(list(), character(), "T")
  testServer(
    srv,
    {
      st <- session$getReturned()$state
      expect_named(
        st,
        c(
          "annotations", "block_order", "title", "stack_annotations",
          "stack_title_level", "block_title_level", "template"
        )
      )
      expect_true(all(vapply(st, is.function, logical(1L))))
    },
    args = list(board = otl_board_args(), update = reactiveVal())
  )
})

test_that("initial annotations and title round-trip through sanitisation", {
  srv <- outline_ext_srv(
    annotations = list(data = list(description = "hi")),
    block_order = character(),
    title = "My report"
  )
  testServer(
    srv,
    {
      st <- session$getReturned()$state
      expect_identical(st$title(), "My report")
      expect_identical(st$annotations()[["data"]][["description"]], "hi")
      # sanitize_annotations fills the report default: OFF until included.
      expect_false(st$annotations()[["data"]][["report"]])
    },
    args = list(board = otl_board_args(), update = reactiveVal())
  )
})

test_that("a blank / missing title falls back to the default", {
  srv <- outline_ext_srv(list(), character(), title = character())
  testServer(
    srv,
    expect_identical(session$getReturned()$state$title(), "Board report"),
    args = list(board = otl_board_args(), update = reactiveVal())
  )
})

test_that("removing a block garbage-collects its annotation and order entry", {
  srv <- outline_ext_srv(
    annotations = list(
      data = list(description = "keep"),
      sub  = list(description = "drop me")
    ),
    block_order = c("data", "sub", "plot", "audit"),
    title = "T"
  )
  testServer(
    srv,
    {
      session$flushReact()
      st <- session$getReturned()$state
      expect_true("sub" %in% names(st$annotations()))

      # Commit a board without `sub`.
      isolate(
        board$board <- blockr.core::new_board(
          blocks = c(data = blockr.core::new_dataset_block("iris"))
        )
      )
      session$flushReact()

      expect_false("sub" %in% names(st$annotations()))
      expect_false("sub" %in% st$block_order())
      expect_true("data" %in% names(st$annotations()))
    },
    args = list(board = otl_board_args(), update = reactiveVal())
  )
})
