# blockr.outline deck builder, three exhibit kinds on the BMS master:
#
#   * `bar` -- a blockr.viz chart block, printed through blockr.viz::static_chart
#     (canvas chart rebuilt as a ggplot for the deck).
#   * `grid` -- a blockr.ggplot::new_grid_block combining TWO ggplot blocks
#     (a scatter and a boxplot) into ONE patchwork exhibit, so several plots
#     land on a single slide. patchwork objects are ggplots, so the outline's
#     officer path places the whole grid like any other plot exhibit.
#   * `st` -- a blockr.viz summary table through static_table().
#
# The attrition plot (blockr.topline::new_attrition_plot_block) is the other
# specialised topline exhibit; it needs the clinical RTF data volume, so it
# lives in the follow-up 8-slide topline port (see the handover), not here.
#
#   Rscript blockr.outline/dev/example-ggchart-grid.R [port]

port <- local({
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args)) as.integer(args[[1L]]) else
    as.integer(Sys.getenv("BLOCKR_PORT", "3838"))
})

options(shiny.port = port, shiny.host = "0.0.0.0")
options(blockr.dock_is_locked = FALSE)
options(blockr.background_construction_delay = 0)
options(blockr.viz.ft_header_bg = c(.stub = "#EEEEEE",
                                    "#A59F9F", "#33D6F1", "#FDA97C"))

message("Open http://127.0.0.1:", port, "/")

root <- "."
deps <- c("dockViewR", "blockr.core", "blockr.dplyr", "blockr.ui",
          "blockr.dag", "blockr.dock", "blockr.ggplot", "blockr.extra",
          "blockr.viz", "blockr.topline", "blockr.outline")
for (d in deps) {
  pkgload::load_all(
    file.path(root, d),
    helpers = FALSE, attach_testthat = FALSE, export_all = FALSE
  )
}

bms_template <- system.file(
  "templates", "bms-template.pptx", package = "blockr.topline"
)
if (!nzchar(bms_template)) {
  bms_template <- file.path(root, "blockr.topline", "inst", "templates",
                            "bms-template.pptx")
}

board <- new_dock_board(
  blocks = c(
    data = new_dataset_block("iris", block_name = "Iris data"),
    bar = blockr.viz::new_chart_block(
      chart_type = "bar", group = "Species", value = "Sepal.Length",
      func = "mean", count_on = "axis",
      title = "Mean sepal length by species",
      block_name = "Sepal length bar"
    ),
    p_scatter = blockr.ggplot::new_ggplot_block(
      type = "point", x = "Sepal.Length", y = "Petal.Length",
      color = "Species", block_name = "Scatter"
    ),
    p_box = blockr.ggplot::new_ggplot_block(
      type = "boxplot", x = "Species", y = "Sepal.Width",
      fill = "Species", block_name = "Boxplot"
    ),
    grid = blockr.ggplot::new_grid_block(
      ncol = "2", title = "Sepal geometry (combined)",
      block_name = "Plot grid"
    ),
    st = blockr.viz::new_summary_table_block(
      vars = c("Sepal.Length", "Sepal.Width", "Petal.Length"),
      by = "Species", block_name = "Iris summary"
    )
  ),
  links = links(
    from = c("data", "data", "data", "p_scatter", "p_box", "data"),
    to = c("bar", "p_scatter", "p_box", "grid", "grid", "st")
  ),
  stacks = stacks(
    deck = new_dock_stack(
      c("data", "bar", "p_scatter", "p_box", "grid", "st"),
      name = "Deck", color = "#7c3aed"
    )
  ),
  extensions = list(
    blockr.dag::new_dag_extension(),
    blockr.outline::new_outline_extension(
      annotations = list(
        data = list(report = FALSE),
        bar = list(description = "Mean sepal length per species (chart block via static_chart).", report = TRUE),
        p_scatter = list(report = FALSE),
        p_box = list(report = FALSE),
        grid = list(description = "Two ggplots combined on one slide with patchwork (grid block).", report = TRUE),
        st = list(description = "Summary statistics by species (summary table via static_table).", report = TRUE)
      ),
      stack_annotations = list(
        deck = list(description = "Charts, a combined plot grid, and a table in one BMS deck.")
      ),
      title = "Chart + grid deck",
      template = bms_template
    )
  )
)

serve(board)
