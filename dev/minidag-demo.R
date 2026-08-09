# Demo / e2e testbed for the minidag extension: a blockr.dock board that uses
# minidag INSTEAD of the blockr.dag deck to configure the workflow.
#
# The block set exercises every input arity the deck distinguishes:
#
#   d1 (dataset, 0 inputs) --> f1 (filter, 1 input) --> h1 (head, 1 input)
#   d1 + f1                --> m1 (merge, 2 named inputs x / y)
#   h1 + f1                --> r1 (rbind, variadic)
#
# From /workspace:
#   Rscript blockr.outline/dev/minidag-demo.R        (first free port, 3838:3847)
#   Rscript blockr.outline/dev/minidag-demo.R 3900   (or pin one, as an argument)
#   PORT=3839 Rscript blockr.outline/dev/minidag-demo.R       (or as an env var)

pkgload::load_all("blockr.ui", quiet = TRUE)
pkgload::load_all("blockr.core", quiet = TRUE)
pkgload::load_all("blockr.dplyr", quiet = TRUE)
pkgload::load_all("blockr.dock", quiet = TRUE)
pkgload::load_all("blockr.outline")

library(shiny)

# First FREE port in the forwarded range, so a second session (or a stale
# Rscript still holding 3838) does not kill this one with "address already in
# use". A first argument, or PORT=, pins one explicitly (any port: the 3838:3847
# range is what the devcontainer forwards, and only the sweep is bound by it).
# blockr_port() comes from the devcontainer .Rprofile and does not exist on the
# host, hence the inline probe.
port <- local({
  arg <- commandArgs(trailingOnly = TRUE)[1L]
  if (!is.na(arg) && grepl("^[0-9]+$", arg)) {
    return(as.integer(arg))
  }
  pin <- Sys.getenv("PORT", "")
  if (nzchar(pin)) {
    return(as.integer(pin))
  }
  if (exists("blockr_port")) {
    return(blockr_port())
  }
  for (p in 3838:3847) {
    con <- tryCatch(serverSocket(p), error = function(e) NULL)
    if (!is.null(con)) {
      close(con)
      return(p)
    }
  }
  stop("all forwarded ports (3838:3847) are busy")
})

options(
  blockr.tabular_display = blockr.ui::html_table_display,
  shiny.port = port,
  shiny.host = "0.0.0.0"
)

board <- new_dock_board(
  blocks = c(
    d1 = new_dataset_block("iris", block_name = "Iris data"),
    f1 = blockr.dplyr::new_filter_block(
      conditions = list(
        list(
          type = "values",
          column = "Species",
          values = list("virginica"),
          mode = "exclude"
        )
      ),
      block_name = "Two species"
    ),
    h1 = new_head_block(n = 6L, block_name = "First rows"),
    m1 = new_merge_block(by = "Species", block_name = "Self merge"),
    r1 = new_rbind_block(block_name = "Bind rows")
  ),
  links = links(
    from = c("d1", "f1", "d1", "f1", "h1", "f1"),
    to = c("f1", "h1", "m1", "m1", "r1", "r1"),
    input = c("data", "data", "x", "y", "", "")
  ),
  stacks = stacks(
    prep = new_dock_stack(
      c("d1", "f1"),
      name = "Data prep",
      color = "#2563eb"
    )
  ),
  extensions = list(
    minidag = new_minidag_extension()
  ),
  # Several views, so the deck's view list has something to organise: the
  # same block deliberately appears on more than one (h1 on Overview and
  # Detail) and the two merge/bind blocks sit on none, which is the ordinary
  # case on a real board -- they feed something without being shown.
  views = list(
    Overview = dock_view(c("minidag", "d1", "h1"), name = "Overview"),
    Detail = dock_view(c("minidag", "f1", "h1", "m1"), name = "Detail"),
    Wide = dock_view(c("minidag", "r1"), name = "Wide")
  ),
  active = "Overview"
)

cat(sprintf("\nMinidag demo: http://127.0.0.1:%d/\n\n", port))

serve(board)
