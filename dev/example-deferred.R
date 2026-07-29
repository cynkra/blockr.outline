# blockr.outline on a DEFERRED board (background_construction_delay = Inf,
# the cdex setting): blocks are only constructed once a view shows them, so
# the outline starts with pending rows for everything the Extra view holds.
#
# What to try:
#   * the outline shows "Evaluating…" for the Extra view's blocks,
#   * the R script / Document views carry a "waiting for R code" comment
#     for them -- except `aud`, which is excluded from the report and has
#     no reported dependent, so it is pruned from the document entirely,
#   * click Download: the reported pending blocks are demanded through
#     core's visibility channel, their code streams in, the rows resolve,
#     and the download fires once the document is complete. `aud` is never
#     constructed -- the independent excluded branch stays lazy.
#
# Run from the workspace root:
#   Rscript blockr.outline/dev/example-deferred.R [port]
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
options(blockr.background_construction_delay = Inf)

message("Open http://127.0.0.1:", port, "/")

root <- "."
deps <- c("dockViewR", "blockr.core", "blockr.dag", "blockr.dock",
          "blockr.outline")
for (d in deps) {
  pkgload::load_all(
    file.path(root, d),
    helpers = FALSE, attach_testthat = FALSE, export_all = FALSE
  )
}

board <- new_dock_board(
  blocks = c(
    data = new_dataset_block("iris", block_name = "Iris data"),
    sub = new_subset_block(block_name = "Setosa subset"),
    ex_data = new_dataset_block("mtcars", block_name = "Cars data"),
    ex_head = new_head_block(n = 5L, block_name = "Cars head"),
    aud = new_head_block(n = 3L, block_name = "QC glance")
  ),
  links = links(
    from = c("data", "ex_data", "sub"),
    to = c("sub", "ex_head", "aud")
  ),
  views = list(
    Main = c("data", "sub", "outline", "dag"),
    Extra = c("ex_data", "ex_head", "aud")
  ),
  active = "Main",
  extensions = list(
    blockr.dag::new_dag_extension(),
    blockr.outline::new_outline_extension(
      title = "Deferred board report",
      annotations = list(
        data = list(
          description = "The classic **iris** dataset.",
          report = TRUE
        ),
        ex_head = list(
          description = "First rows of the cars data.",
          report = TRUE
        ),
        aud = list(
          description = "QC only, not part of the report.",
          report = FALSE
        )
      )
    )
  )
)

serve(board)
