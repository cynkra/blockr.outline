# R/ext.R :: the two-stage download (code_render_go -> hidden code_render).
#
# On a deferred board (background_construction_delay = Inf) a block no view
# has shown is never constructed and its outline row is pending. Clicking
# Download must demand the pending EXPORTED blocks through core's visibility
# channel (`visibility$required[[id]](TRUE)`), wait until none of them is
# pending, and only then fire the real download. Blocks outside the export
# closure must never be demanded.
#
# testServer cannot click a browser link, and MockShinySession's
# sendCustomMessage is a noop -- so the observable outcomes are the
# visibility slots (did the click demand the right blocks?) and the
# notification lifecycle (waiting note shown, then removed once the code
# arrives), with showNotification / removeNotification recorded via mocks.

# fake_visibility() and pending_plot_board() live in helper-server.R,
# shared with the Output preview demand tests.

test_that("download demands pending exported blocks and waits for them", {
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

      session$setInputs(code_render_go = 1)
      session$flushReact()

      # The click demanded the pending block; the constructed ones stay
      # undeclared (core pulls ancestors along on its own).
      expect_true(isolate(vis$required$plot()))
      expect_identical(isolate(vis$required$data()), NA)

      # And a waiting note is up, not removed yet.
      expect_length(shown, 1L)
      expect_match(shown[[1L]], "Generating R code")
      expect_length(removed, 0L)

      # The block "constructs": its expr starts reporting. The waiting
      # observer sees nothing exported pending and fires the download,
      # removing the note on the way.
      isolate(
        b$blocks[["plot"]]$server$expr <- reactive(
          quote(plot(sub$Sepal.Length, sub$Sepal.Width))
        )
      )
      session$flushReact()

      expect_identical(removed, "note-1")

      # The demand is withdrawn once the download fired: the dock reads
      # non-NA required as "card built", and this card never was.
      expect_identical(isolate(vis$required$plot()), NA)
    },
    args = list(board = b, update = reactiveVal(), visibility = vis)
  )
})

test_that("download fires straight away when nothing exported is pending", {
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
      session$setInputs(code_render_go = 1)
      session$flushReact()

      # No demand, no waiting note.
      expect_identical(isolate(vis$required$plot()), NA)
      expect_length(shown, 0L)
    },
    args = list(board = otl_board_args(), update = reactiveVal(),
                visibility = vis)
  )
})

test_that("a pending block outside the export closure is not demanded", {
  # `plot` is pending but excluded from the report, and nothing reported
  # depends on it: the download must neither demand it nor wait for it.
  vis <- fake_visibility(c("data", "sub", "plot", "audit"))

  testServer(
    outline_ext_srv(
      list(plot = list(report = FALSE)), character(), "T"
    ),
    {
      session$flushReact()
      session$setInputs(code_render_go = 1)
      session$flushReact()

      expect_identical(isolate(vis$required$plot()), NA)
    },
    args = list(board = pending_plot_board(), update = reactiveVal(),
                visibility = vis)
  )
})

test_that("pending blocks without a visibility channel warn instead of hang", {
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
      session$setInputs(code_render_go = 1)
      session$flushReact()

      expect_length(shown, 1L)
      expect_match(shown[[1L]], "not initialized")
    },
    args = list(board = pending_plot_board(), update = reactiveVal())
  )
})
