# blockr.outline with blockr.viz TABLE blocks: the outline's report renders
# them through blockr.viz::static_table() (annotated df -> styled flextable), so
# the pptx / docx / html downloads carry real styled tables instead of a
# bare kable of the underlying frame.
#
# What to try:
#   * the `tbl` / `tbl2` blocks show the interactive styled table in their
#     control pane, as usual,
#   * open the outline's Document view: their chunks end in
#     `blockr.viz::static_table(<id>)` -- the report seam,
#   * pick a format (pptx!) in the outline gear and click Download: the
#     tables arrive as native, editable PowerPoint tables in the topline
#     look. The upstream data / summary blocks are excluded from the
#     report, so only the two styled tables (and their prose) show.
#
# Run from the workspace root:
#   Rscript blockr.outline/dev/example-tables.R [port]
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

message("Open http://127.0.0.1:", port, "/")

root <- "."
deps <- c("dockViewR", "blockr.core", "blockr.dag", "blockr.dock",
          "blockr.viz", "blockr.outline")
for (d in deps) {
  pkgload::load_all(
    file.path(root, d),
    helpers = FALSE, attach_testthat = FALSE, export_all = FALSE
  )
}

board <- new_dock_board(
  blocks = c(
    data = new_dataset_block("iris", block_name = "Iris data"),
    st = blockr.viz::new_summary_table_block(
      vars = c("Sepal.Length", "Sepal.Width"), by = "Species",
      block_name = "Iris summary"
    ),
    tbl = blockr.viz::new_table_block(block_name = "Sepal measures"),
    cars = new_dataset_block("mtcars", block_name = "Cars data"),
    st2 = blockr.viz::new_summary_table_block(
      vars = "mpg", by = c("cyl", "am"), block_name = "MPG summary"
    ),
    tbl2 = blockr.viz::new_table_block(block_name = "MPG by cyl and am")
  ),
  links = links(
    from = c("data", "st", "cars", "st2"),
    to = c("st", "tbl", "st2", "tbl2")
  ),
  views = list(
    Main = c("tbl", "tbl2", "outline", "dag")
  ),
  active = "Main",
  extensions = list(
    blockr.dag::new_dag_extension(),
    blockr.outline::new_outline_extension(
      title = "Summary tables report",
      annotations = list(
        data = list(report = FALSE),
        st = list(report = FALSE),
        cars = list(report = FALSE),
        st2 = list(report = FALSE),
        tbl = list(
          description = "Sepal length and width by species.",
          report = TRUE
        ),
        tbl2 = list(
          description = "Fuel efficiency by cylinder count and transmission.",
          report = TRUE
        )
      )
    )
  )
)

serve(board)
