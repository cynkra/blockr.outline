# R/ext.R :: the panel-visibility gate on the (expensive) projection.
#
# The outline's outputs are live from board startup (the dock mounts every
# extension eagerly), and `outline_out` is suspendWhenHidden = FALSE. Without
# a gate the O(n^2) projection would run on every board change even while the
# panel is closed. The client reports visibility via `otl_visible`; the gate
# is `req(panel_visible())` at the top of sections_calc, so while hidden the
# projection -- and the store it feeds -- does not update.

test_that("panel_visible tracks the client's otl_visible signal", {
  testServer(
    outline_ext_srv(list(), character(), "T"),
    {
      session$flushReact()
      expect_true(panel_visible()) # seeded visible (fail-safe)

      session$setInputs(otl_visible = FALSE)
      session$flushReact()
      expect_false(panel_visible())

      session$setInputs(otl_visible = TRUE)
      session$flushReact()
      expect_true(panel_visible())
    },
    args = list(board = otl_board_args(), update = reactiveVal())
  )
})

test_that("a board change while hidden does NOT re-run the projection", {
  testServer(
    outline_ext_srv(list(), character(), "T"),
    {
      session$flushReact()
      # Visible at startup: the projection ran and populated the store.
      expect_false(is.null(sections_store()))

      # Hide the panel.
      session$setInputs(otl_visible = FALSE)
      session$flushReact()
      before <- sections_store()

      # A change that WOULD alter the projection (flip a report flag) must
      # not touch the store while hidden -- sections_calc req(panel_visible)
      # short-circuits, so the store observer sees nothing new.
      session$setInputs(
        outline_toggle = list(id = "audit", report = FALSE)
      )
      session$flushReact()
      expect_identical(sections_store(), before)

      # Re-show: the pending change is projected now.
      session$setInputs(otl_visible = TRUE)
      session$flushReact()
      expect_false(identical(sections_store(), before))
      expect_false(sections_store()$report[sections_store()$ids == "audit"])
    },
    args = list(board = otl_board_args(), update = reactiveVal())
  )
})
