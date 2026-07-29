# R/ext.R :: the Output preview's demand pre-step.
#
# Flipping the body toggle to Output runs the same transient demand as the
# download: pending EXPORTED blocks are constructed through core's
# visibility channel, the preview fills in as their expressions report,
# and the demand is withdrawn once the closure is complete (the dock reads
# non-NA required as "card built", so a leftover TRUE would lie). Flipping
# back to Code cancels a preview-only wait. Mirrors
# test-ext-server-render.R, which owns the download half.

test_that("flipping to Output demands the pending exported blocks", {
  shown <- list()
  removed <- character()

  testthat::local_mocked_bindings(
    showNotification = function(ui, ...) {
      shown[[length(shown) + 1L]] <<- ui
      "note-1"
    },
    removeNotification = function(id, ...) {
      removed <<- c(removed, id)
      invisible(NULL)
    },
    .package = "blockr.outline"
  )

  b <- pending_plot_board()
  vis <- fake_visibility(c("data", "sub", "plot", "audit"))

  testServer(
    outline_ext_srv(otl_dock_ann(), character(), "T"),
    {
      session$flushReact()

      session$setInputs(otl_body = "output")
      session$flushReact()

      # The flip demanded the pending block; constructed ones stay
      # undeclared (core pulls ancestors along on its own).
      expect_true(isolate(vis$required$plot()))
      expect_identical(isolate(vis$required$data()), NA)

      expect_length(shown, 1L)
      expect_match(shown[[1L]], "Evaluating")
      expect_length(removed, 0L)

      # The block "constructs": its expr starts reporting. The waiting
      # observer restores the demand and drops the note -- and does NOT
      # fire a download.
      isolate(
        b$blocks[["plot"]]$server$expr <- reactive(
          quote(plot(sub$Sepal.Length, sub$Sepal.Width))
        )
      )
      session$flushReact()

      expect_identical(removed, "note-1")
      expect_identical(isolate(vis$required$plot()), NA)
    },
    args = list(board = b, update = reactiveVal(), visibility = vis)
  )
})

test_that("Output with nothing pending demands nothing", {
  shown <- list()

  testthat::local_mocked_bindings(
    showNotification = function(ui, ...) {
      shown[[length(shown) + 1L]] <<- ui
      "note-1"
    },
    .package = "blockr.outline"
  )

  vis <- fake_visibility(c("data", "sub", "plot", "audit"))

  testServer(
    outline_ext_srv(otl_dock_ann(), character(), "T"),
    {
      session$flushReact()
      session$setInputs(otl_body = "output")
      session$flushReact()

      expect_identical(isolate(vis$required$plot()), NA)
      expect_length(shown, 0L)
    },
    args = list(board = otl_board_args(), update = reactiveVal(),
                visibility = vis)
  )
})

test_that("flipping back to Code cancels a preview-only demand", {
  removed <- character()

  testthat::local_mocked_bindings(
    showNotification = function(ui, ...) "note-1",
    removeNotification = function(id, ...) {
      removed <<- c(removed, id)
      invisible(NULL)
    },
    .package = "blockr.outline"
  )

  vis <- fake_visibility(c("data", "sub", "plot", "audit"))

  testServer(
    outline_ext_srv(otl_dock_ann(), character(), "T"),
    {
      session$flushReact()

      session$setInputs(otl_body = "output")
      session$flushReact()
      expect_true(isolate(vis$required$plot()))

      # Back to Code while `plot` is still pending: the demand is
      # withdrawn and the note removed.
      session$setInputs(otl_body = "code")
      session$flushReact()

      expect_identical(isolate(vis$required$plot()), NA)
      expect_identical(removed, "note-1")
    },
    args = list(board = pending_plot_board(), update = reactiveVal(),
                visibility = vis)
  )
})

test_that("Output without a visibility channel warns instead of hanging", {
  shown <- list()

  testthat::local_mocked_bindings(
    showNotification = function(ui, ...) {
      shown[[length(shown) + 1L]] <<- ui
      "note-1"
    },
    .package = "blockr.outline"
  )

  testServer(
    outline_ext_srv(otl_dock_ann(), character(), "T"),
    {
      session$flushReact()
      session$setInputs(otl_body = "output")
      session$flushReact()

      expect_length(shown, 1L)
      expect_match(shown[[1L]], "not initialized")
    },
    args = list(board = pending_plot_board(), update = reactiveVal())
  )
})
