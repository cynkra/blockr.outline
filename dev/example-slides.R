# blockr.outline SLIDE BUILDER: the deck half of the package, side by side
# with the outline it was carved out of.
#
# Same board as dev/example-perf.R:
#
#   data ──> mut1 ──> tbl_detail          (table 1)
#              └────> summ ──> tbl_summary (table 2)
#                       └────> chart       (a bar chart)
#
# ...and two extensions over it, both on the Main view, so the difference
# is a tab click:
#
#   Outline  the whole Quarto document -- prose, code cells, chapters.
#   Slides   pick blocks, order them, download a deck. Nothing else.
#
# What this script is for:
#
#   * Pick "Mean ratio by species" and then "Flower measurements", in that
#     order. The deck opens on the summary and puts the detail second --
#     the REVERSE of the DAG order. That is the point of the slide builder:
#     evaluation follows the dependencies, the deck does not.
#   * Drag a row, or use the up / down arrows. Every order is legal here,
#     because no slide can reference another slide's variable.
#   * Download as PowerPoint, then as HTML. Same slides, same order.
#   * Watch the console. Nothing is evaluated until the download is
#     clicked; the picker and the list read block NAMES off the board.
#
# Run from the workspace root:
#   Rscript blockr.outline/dev/example-slides.R [port]
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

# Deferred by default (Inf), so the two-stage download is exercised: a block
# no view has shown is never constructed, and pressing Download is what
# demands it. Set BLOCKR_EAGER=1 for the everything-is-live path.
options(
  blockr.background_construction_delay =
    if (nzchar(Sys.getenv("BLOCKR_EAGER"))) 0 else Inf
)

message("Open http://127.0.0.1:", port, "/")

root <- "."
deps <- c("dockViewR", "blockr.core", "blockr.dag", "blockr.dock",
          "blockr.dplyr", "blockr.viz", "blockr.outline")
for (d in deps) {
  pkgload::load_all(
    file.path(root, d),
    helpers = FALSE, attach_testthat = FALSE, export_all = FALSE
  )
}

board <- new_dock_board(
  blocks = c(
    data = new_dataset_block("iris", block_name = "Iris data"),
    mut1 = blockr.dplyr::new_mutate_block(
      mutations = list(
        list(name = "ratio1", expr = "Sepal.Length / Sepal.Width")
      ),
      block_name = "Sepal ratio"
    ),
    summ = blockr.dplyr::new_summarize_block(
      summaries = list(
        list(type = "simple", name = "avg_ratio", func = "mean",
             col = "ratio1")
      ),
      by = list("Species"),
      block_name = "Ratio by species"
    ),
    tbl_detail = blockr.viz::new_table_block(
      block_name = "Flower measurements"
    ),
    tbl_summary = blockr.viz::new_table_block(
      block_name = "Mean ratio by species"
    ),
    # A plot, so the deck has one slide that is not a table -- and CONFIGURED,
    # because a block that returns no object has no exhibit to put on a
    # slide. Two ways to get that wrong, both of which this script hit while
    # it was being written: an unconfigured chart block builds nothing, and
    # blockr.core's plot blocks draw to the graphics device and return NULL.
    # Either way the deck comes back a slide short and says so on the console
    # ("[deck] no slide for ...").
    chart = blockr.viz::new_chart_block(
      chart_type = "bar",
      x = "Species",
      y = "avg_ratio",
      block_name = "Mean ratio, charted"
    )
  ),
  links = links(
    from = c("data", "mut1", "mut1", "summ", "summ"),
    to   = c("mut1", "tbl_detail", "summ", "tbl_summary", "chart")
  ),
  views = list(
    Main = c("tbl_detail", "tbl_summary", "slides", "outline", "dag")
  ),
  active = "Main",
  extensions = list(
    blockr.dag::new_dag_extension(),
    # Seeded deliberately out of DAG order: summary first, detail second.
    # A restored board has to come back showing exactly this.
    blockr.outline::new_slides_extension(
      title = "Iris topline",
      slides = c("tbl_summary", "tbl_detail")
    ),
    blockr.outline::new_outline_extension(
      title = "Iris report",
      annotations = list(
        tbl_detail = list(
          description = "Every flower, with the derived sepal ratio.",
          report = TRUE
        ),
        tbl_summary = list(
          description = "Mean sepal ratio per species.",
          report = TRUE
        )
      )
    )
  )
)

serve(board)
