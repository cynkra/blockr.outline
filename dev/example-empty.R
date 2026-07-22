# blockr.outline: an empty board with the full showcase stack loaded.
#
# The same packages as example-penguins.R, but nothing on the board --
# a blank canvas. Add blocks from the browser (+), wire them in the DAG,
# annotate them in the Outline, and render. Everything the penguins demo
# uses is available: stats (model / broom), ggplot, viz, dplyr, the
# HTML table preview, and project save / load / versions.
#
# Run from the workspace root:
#   Rscript blockr.outline/dev/example-empty.R [port]

port <- local({
  a <- commandArgs(trailingOnly = TRUE)
  if (length(a)) as.integer(a[[1L]]) else
    as.integer(Sys.getenv("BLOCKR_PORT", "3838"))
})

options(
  shiny.port = port, shiny.host = "0.0.0.0",
  blockr.dock_is_locked = FALSE,
  # nice paginated / sortable HTML tables for data.frame outputs
  # (the blockr.extra table-preview glue).
  blockr.tabular_display = blockr.ui::html_table_display,
  blockr.background_construction_delay = 0
)

message("Open http://127.0.0.1:", port, "/")

root <- "."
deps <- c(
  "dockViewR", "blockr.core", "blockr.ui", "blockr.session", "blockr.dag",
  "blockr.dock", "blockr.dplyr", "blockr.ggplot", "blockr.viz",
  "blockr.extra", "blockr.stats", "blockr.outline"
)
for (d in deps) {
  pkgload::load_all(
    file.path(root, d),
    helpers = FALSE, attach_testthat = FALSE, export_all = FALSE
  )
}

library(palmerpenguins)   # so the penguins dataset is a click away too

# No blocks, no links, no stacks -- just the two authoring extensions on
# an empty board. Add blocks and they appear in the DAG and the Outline.
board <- new_dock_board(
  extensions = list(
    blockr.dag::new_dag_extension(),
    blockr.outline::new_outline_extension(title = "Untitled report")
  )
)

# blockr.session's manage_project plugin: save / load / version the
# workflow from the app.
serve(
  board,
  plugins = custom_plugins(manage_project())
)
