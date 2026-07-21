# Fixtures for the testServer() layer (R/ext.R :: outline_ext_srv).
#
# The extension server reads `board$board` (the committed board) and
# `board$blocks` (the constructed block servers, whose `expr` reactives feed
# the projection). blockr.core's generate_plugin_args(mode = "read") builds
# exactly that bundle from a dock board -- the block servers run, so
# board_exprs()/sections() populate for real, without a browser.

# A dock board with two parallel leaf blocks off `sub` (movable) and a
# `data + sub` stack (chapter). Enough to drive reorder, stack membership,
# rename, open and GC.
otl_dock_board <- function() {
  blockr.dock::new_dock_board(
    blocks = c(
      data  = blockr.core::new_dataset_block("iris"),
      sub   = blockr.core::new_subset_block(),
      plot  = blockr.core::new_scatter_block("Sepal.Length", "Sepal.Width"),
      audit = blockr.core::new_head_block()
    ),
    links = blockr.core::links(
      from = c("data", "sub",  "sub"),
      to   = c("sub",  "plot", "audit")
    ),
    stacks = blockr.core::stacks(
      prep = blockr.dock::new_dock_stack(c("data", "sub"), name = "Prep")
    )
  )
}

# The `board` argument a plugin server receives, in read mode (block servers
# constructed, expressions live).
otl_board_args <- function(board = otl_dock_board()) {
  blockr.core::generate_plugin_args(board, mode = "read")[["board"]]
}
