# R/ext.R :: outline_ext_srv -- structural edits that reorder blocks, move
# them between chapters, rename chapters, and reveal a block's dock panel.

test_that("a drag reorders the stored block order", {
  srv <- outline_ext_srv(list(), character(), "T")
  testServer(
    srv,
    {
      session$flushReact()
      st <- session$getReturned()$state
      # Displayed order is data, sub, audit, plot; move plot before audit.
      session$setInputs(
        outline_move = list(id = "plot", target = "audit", after = FALSE)
      )
      ord <- st$block_order()
      expect_lt(match("plot", ord), match("audit", ord))
    },
    args = list(board = otl_board_args(), update = reactiveVal())
  )
})

test_that("a block dragged next to a chapter joins it (position = membership)", {
  upd <- reactiveVal()
  srv <- outline_ext_srv(list(), character(), "T")
  testServer(
    srv,
    {
      session$flushReact()
      # Drop audit right after sub, which is in the `prep` stack.
      session$setInputs(
        outline_move = list(id = "audit", target = "sub", after = TRUE)
      )
      mod <- isolate(upd())$stacks$mod
      expect_true("audit" %in% mod$prep$blocks)
    },
    args = list(board = otl_board_args(), update = upd)
  )
})

test_that("renaming a chapter emits a stacks-mod update", {
  upd <- reactiveVal()
  srv <- outline_ext_srv(list(), character(), "T")
  testServer(
    srv,
    {
      session$setInputs(
        outline_rename_stack = list(stack = "prep", name = "Data prep")
      )
      expect_identical(isolate(upd())$stacks$mod$prep$name, "Data prep")
    },
    args = list(board = otl_board_args(), update = upd)
  )
})

test_that("renaming to an unchanged name is a no-op", {
  upd <- reactiveVal()
  srv <- outline_ext_srv(list(), character(), "T")
  testServer(
    srv,
    {
      session$setInputs(
        outline_rename_stack = list(stack = "prep", name = "Prep")
      )
      expect_null(isolate(upd()))
    },
    args = list(board = otl_board_args(), update = upd)
  )
})

test_that("opening a block selects its dock panel", {
  upd <- reactiveVal()
  srv <- outline_ext_srv(list(), character(), "T")
  testServer(
    srv,
    {
      session$setInputs(outline_open = list(id = "data"))
      mod <- isolate(upd())$views$mod
      # One view, whose ops select the data block's panel.
      op <- mod[[1L]]
      expect_identical(op$select, "block_panel-data")
    },
    args = list(board = otl_board_args(), update = upd)
  )
})

test_that("opening an unknown block does nothing", {
  upd <- reactiveVal()
  srv <- outline_ext_srv(list(), character(), "T")
  testServer(
    srv,
    {
      session$setInputs(outline_open = list(id = "nope"))
      expect_null(isolate(upd()))
    },
    args = list(board = otl_board_args(), update = upd)
  )
})
