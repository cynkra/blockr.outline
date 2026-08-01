# R/ext.R :: outline_ext_srv -- every constructor argument must survive into
# state, and must survive the UI's initial input delivery.
#
# WHY THIS FILE EXISTS
# `new_outline_extension(block_title_level = "##")` accepted the argument,
# stored it, and then discarded it one flush later: outline_settings_band()
# hardcodes `selected` on its two selectInputs (it only receives `ns`, so it
# cannot see the arguments), Shiny delivers that hardcoded default at session
# start, and the sync observers overwrote state with it. Programmatic boards
# silently got "caption", and a restored board lost whatever the user had
# chosen.
#
# The existing state tests did not catch it for two compounding reasons, and
# the second is the interesting one:
#
#   1. They only ever exercised three of the arguments (annotations,
#      block_order, title), passing the rest positionally as defaults.
#   2. More fundamentally, testServer() renders no UI. With no UI there is no
#      hardcoded default to deliver, so the clobber cannot happen and the
#      assertion passes either way. A test that simply read
#      state$block_title_level() would have been GREEN throughout.
#
# So asserting the argument arrives is necessary but not sufficient. The
# regression test has to simulate the UI announcing its own default --
# session$setInputs(otl_block_level = "caption") -- which is what the browser
# really does. Any future "UI default overwrites constructor argument" bug in
# this extension is caught by that pattern and by nothing weaker.

# Non-default values for every argument, so nothing can pass by coinciding
# with a default.
otl_srv_all_args <- function() {
  outline_ext_srv(
    annotations = list(data = list(description = "hi", report = FALSE)),
    block_order = c("sub", "data"),
    title = "My report",
    stack_annotations = list(prep = list(description = "prep stack")),
    stack_title_level = "##",
    block_title_level = "###"
  )
}

test_that("every constructor argument surfaces in state", {
  testServer(
    otl_srv_all_args(),
    {
      session$flushReact()
      st <- session$getReturned()$state

      expect_identical(st$title(), "My report")
      expect_identical(st$annotations()[["data"]][["description"]], "hi")
      expect_false(st$annotations()[["data"]][["report"]])
      expect_identical(st$block_order(), c("sub", "data"))
      expect_identical(
        st$stack_annotations()[["prep"]][["description"]], "prep stack"
      )
      expect_identical(st$stack_title_level(), "##")
      expect_identical(st$block_title_level(), "###")
    },
    args = list(board = otl_board_args(), update = reactiveVal())
  )
})

test_that("the arguments survive the startup flush", {
  # The regression, as far as testServer can express it.
  #
  # A note on what this can and cannot check. In the browser the hardcoded
  # `selected` arrives as part of session init, which is precisely the
  # delivery ignoreInit skips. testServer has no UI, so there is no such
  # delivery to reproduce: calling session$setInputs() afterwards is
  # indistinguishable -- to the test AND to the server -- from a user
  # picking that value on purpose, and it SHOULD win then (see the next
  # test). So the honest assertion here is that a full startup flush leaves
  # the arguments intact, which is what regressed. The seeding test below
  # covers the other half.
  testServer(
    otl_srv_all_args(),
    {
      session$flushReact()
      session$flushReact()
      st <- session$getReturned()$state

      expect_identical(st$stack_title_level(), "##")
      expect_identical(st$block_title_level(), "###")
    },
    args = list(board = otl_board_args(), update = reactiveVal())
  )
})

test_that("a real user change to the heading selects still takes effect", {
  # The guard above must not freeze the controls: ignoreInit skips only the
  # initial delivery, so a genuine later choice has to win.
  testServer(
    otl_srv_all_args(),
    {
      session$flushReact()
      st <- session$getReturned()$state

      # Startup noise first, then a deliberate change.
      session$setInputs(otl_block_level = "caption")
      session$flushReact()
      session$setInputs(otl_block_level = "#")
      session$flushReact()

      expect_identical(st$block_title_level(), "#")
    },
    args = list(board = otl_board_args(), update = reactiveVal())
  )
})

test_that("the heading selects are seeded so the gear shows what is in effect", {
  # State being right is only half of it: a gear reading "Caption" while the
  # document renders "###" headings is its own bug, and it is the half a
  # state-only assertion misses. MockShinySession keeps no record of input
  # messages, so record the seeding call itself.
  seeded <- list()

  testthat::local_mocked_bindings(
    updateSelectInput = function(session, inputId, ...) {
      dots <- list(...)
      seeded[[inputId]] <<- dots[["selected"]]
      invisible(NULL)
    },
    .package = "blockr.outline"
  )

  testServer(
    otl_srv_all_args(),
    {
      session$flushReact()

      expect_setequal(names(seeded), c("otl_stack_level", "otl_block_level"))
      expect_identical(seeded[["otl_block_level"]], "###")
      expect_identical(seeded[["otl_stack_level"]], "##")
    },
    args = list(board = otl_board_args(), update = reactiveVal())
  )
})

test_that("defaults still apply when no arguments are given", {
  testServer(
    outline_ext_srv(list(), character(), "T"),
    {
      session$flushReact()
      st <- session$getReturned()$state
      expect_identical(st$stack_title_level(), "#")
      expect_identical(st$block_title_level(), "caption")
    },
    args = list(board = otl_board_args(), update = reactiveVal())
  )
})
