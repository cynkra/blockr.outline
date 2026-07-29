# The CDEx shape at full size: 80 blocks in 8 chains of 10, only chain 1
# in the Main view, the other 7 chains parked in views you may never open,
# background_construction_delay = Inf. For feeling the dormant outline
# before/after:
#
#   * AFTER (default): the outline lists chain 1 with code and 70 condensed
#     one-line rows. Startup is instant, scrolling is short, editing an
#     active block re-projects but re-highlights only 10 chunks.
#   * BEFORE: gear -> "Show code for every block". Every dormant row
#     expands -- unconstructed ones to the old "Evaluating…" placeholder,
#     which is exactly what a large deferred board used to look like.
#     Visit a few chain views first and the expanded rows carry real
#     highlighted code, the full pre-dormancy weight.
#   * Output preview: the chain ends (c1 t9, c2 t9, c3 t9) are reported;
#     flipping the eye demands their chains and shows the exhibits without
#     visiting any view.
#
# Run from the workspace root:
#   Rscript blockr.outline/dev/example-scale.R [port]
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

datasets <- c("iris", "mtcars", "airquality", "attenu",
              "ChickWeight", "quakes", "swiss", "ToothGrowth")

blocks <- list()
from <- character()
to <- character()
views <- list()
ann <- list()

for (ch in 1:8) {
  ids <- sprintf("c%d_%s", ch, c("data", paste0("t", 1:9)))

  blocks[[ids[1L]]] <- blockr.core::new_dataset_block(
    datasets[ch],
    block_name = paste0("Chain ", ch, ": ", datasets[ch])
  )
  ann[[ids[1L]]] <- list(
    description = paste0("Source data for chain ", ch, " (`",
                         datasets[ch], "`)."),
    report = FALSE
  )

  for (i in 2:10) {
    blocks[[ids[i]]] <- blockr.core::new_subset_block(
      block_name = paste0("Chain ", ch, " step ", i - 1L)
    )
    from <- c(from, ids[i - 1L])
    to <- c(to, ids[i])
    ann[[ids[i]]] <- list(
      description = paste0("Step ", i - 1L, " of chain ", ch,
                           ": narrows the data further."),
      # Only the ends of chains 1-3 land in the report; everything else
      # runs invisibly or is pruned -- the CDEx ratio.
      report = i == 10L && ch <= 3L
    )
  }

  views[[paste0("chain", ch)]] <- ids
}

views[["Main"]] <- c(views[["chain1"]], "outline", "dag")
views[["chain1"]] <- NULL
views <- views[c("Main", setdiff(names(views), "Main"))]

board <- new_dock_board(
  blocks = blocks,
  links = blockr.core::links(from = from, to = to),
  views = views,
  active = "Main",
  extensions = list(
    blockr.dag::new_dag_extension(),
    blockr.outline::new_outline_extension(
      title = "Scale report (80 blocks)",
      annotations = ann
    )
  )
)

serve(board)
