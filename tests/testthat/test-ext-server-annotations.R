# R/ext.R :: outline_ext_srv -- annotation edits (report flag, descriptions).
# These are extension-only state: they must NOT emit a board update().

test_that("the report toggle flips state without touching the board", {
  upd <- reactiveVal()
  srv <- outline_ext_srv(list(), character(), "T")
  testServer(
    srv,
    {
      st <- session$getReturned()$state
      session$setInputs(outline_toggle = list(id = "data", report = FALSE))
      expect_false(st$annotations()[["data"]][["report"]])
      # Annotation-only: no board mutation.
      expect_null(isolate(upd()))
    },
    args = list(board = otl_board_args(), update = upd)
  )
})

test_that("toggling to the current value is a no-op", {
  upd <- reactiveVal()
  srv <- outline_ext_srv(
    annotations = list(data = list(report = TRUE)),
    block_order = character(),
    title = "T"
  )
  testServer(
    srv,
    {
      st <- session$getReturned()$state
      before <- st$annotations()
      # report already defaults TRUE -> setting TRUE changes nothing.
      session$setInputs(outline_toggle = list(id = "data", report = TRUE))
      expect_identical(st$annotations(), before)
    },
    args = list(board = otl_board_args(), update = upd)
  )
})

test_that("editing then saving writes a block description", {
  srv <- outline_ext_srv(list(), character(), "T")
  testServer(
    srv,
    {
      st <- session$getReturned()$state
      session$setInputs(outline_edit = list(id = "sub"))
      session$setInputs(desc_edit = "Setosa only.", desc_save = 1)
      expect_identical(
        st$annotations()[["sub"]][["description"]],
        "Setosa only."
      )
    },
    args = list(board = otl_board_args(), update = reactiveVal())
  )
})

test_that("a stack-prefixed key saves to stack annotations", {
  srv <- outline_ext_srv(list(), character(), "T")
  testServer(
    srv,
    {
      st <- session$getReturned()$state
      session$setInputs(outline_edit = list(id = "stack:prep"))
      session$setInputs(desc_edit = "The prep chapter.", desc_save = 1)
      expect_identical(
        st$stack_annotations()[["prep"]][["description"]],
        "The prep chapter."
      )
      # Block annotations untouched.
      expect_false("stack:prep" %in% names(st$annotations()))
    },
    args = list(board = otl_board_args(), update = reactiveVal())
  )
})

test_that("desc_cancel clears the editing target without writing", {
  srv <- outline_ext_srv(list(), character(), "T")
  testServer(
    srv,
    {
      st <- session$getReturned()$state
      session$setInputs(outline_edit = list(id = "sub"))
      session$setInputs(desc_cancel = 1)
      session$setInputs(desc_edit = "should not land", desc_save = 1)
      # After a cancel, editing() is NULL, so desc_save has no target.
      expect_false("sub" %in% names(st$annotations()))
    },
    args = list(board = otl_board_args(), update = reactiveVal())
  )
})
