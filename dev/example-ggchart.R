# blockr.outline + blockr.viz chart blocks: the report renders the canvas
# charts as ggplots (blockr.viz::static_chart, emitted via report_call), so the
# html report and the officer pptx deck show real charts instead of the
# charts' underlying data frames.
#
# Run from the workspace root:
#   Rscript blockr.outline/dev/example-ggchart.R [port]
# Port resolution: argument, then BLOCKR_PORT, then 3838.

port <- local({
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args)) {
    as.integer(args[[1L]])
  } else {
    as.integer(Sys.getenv("BLOCKR_PORT", "3838"))
  }
})

options(shiny.port = port, shiny.host = "0.0.0.0")
options(blockr.dock_is_locked = FALSE)
options(blockr.background_construction_delay = 0)

message("Open http://127.0.0.1:", port, "/")

# All-or-nothing: load_all EVERY blockr package involved (cdex-style), so
# the demo always runs the working trees, never a stale library.
root <- "."
deps <- c("dockViewR", "blockr.core", "blockr.dplyr", "blockr.ui",
          "blockr.dag", "blockr.dock", "blockr.viz", "blockr.outline")
for (d in deps) {
  pkgload::load_all(
    file.path(root, d),
    helpers = FALSE, attach_testthat = FALSE, export_all = FALSE
  )
}

board <- new_dock_board(
  blocks = c(
    data = new_dataset_block("iris", block_name = "Iris data"),
    bar = new_chart_block(
      chart_type = "bar",
      group = "Species",
      func = "count",
      count_on = "axis",
      block_name = "Rows per species"
    ),
    sc = new_chart_block(
      chart_type = "scatter",
      x = "Sepal.Length",
      y = "Sepal.Width",
      color = "Species",
      smoother = "lm",
      block_name = "Sepal scatter"
    )
  ),
  links = links(from = c("data", "data"), to = c("bar", "sc")),
  extensions = list(
    blockr.dag::new_dag_extension(),
    blockr.outline::new_outline_extension(
      title = "Iris chart report",
      annotations = list(
        data = list(description = "The classic **iris** dataset.",
                    report = FALSE),
        bar = list(description = "Row counts per species.", report = TRUE),
        sc = list(
          description = "Sepal geometry, colored by species.",
          report = TRUE
        )
      )
    )
  )
)

serve(board)
