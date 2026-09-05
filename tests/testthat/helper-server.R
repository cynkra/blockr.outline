# Fixtures for the testServer() layer of the dock extensions (slides,
# report).
#
# An extension server reads `board$board` (the committed board) and
# `board$blocks` (the constructed block servers, whose `expr` reactives feed
# the projection). blockr.core's generate_plugin_args(mode = "read") builds
# exactly that bundle from a dock board -- the block servers run, so
# board_exprs() populates for real, without a browser.

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

# Board args with `plot` pending: its expr reactive reports nothing, the
# way an unconstructed / not-yet-reporting block does.
pending_plot_board <- function() {
  b <- otl_board_args()
  isolate(
    b$blocks[["plot"]]$server$expr <- reactive(req(FALSE))
  )
  b
}

# The slide builder's board bundle, with every block expression rigged to
# THROW. Anything the panel draws has to come off the board object itself
# (block names, exhibit kinds), so a panel that renders against this bundle
# is a panel that evaluates nothing -- which is the extension's central
# performance claim, and the reason it needs no visibility gate.
blind_board_args <- function() {
  b <- otl_board_args()
  isolate({
    for (id in names(b$blocks)) {
      b$blocks[[id]]$server$expr <- reactive(stop("expression evaluated"))
    }
  })
  b
}

# Take a block off a dock board the way the app does: out of every view
# first, then its links, then the block. A dock_board validates view
# membership against its blocks, so removing the block first aborts.
drop_block <- function(brd, id) {

  views <- blockr.dock::board_views(brd)
  pid <- as.character(blockr.dock::as_block_panel_id(id))

  for (v in names(views)) {
    views[[v]] <- blockr.dock::dock_view(
      setdiff(blockr.dock::view_members(views[[v]]), pid),
      name = blockr.dock::view_name(views[[v]])
    )
  }

  blockr.dock::board_views(brd) <- views

  lnks <- blockr.core::board_links(brd)
  blockr.core::board_links(brd) <- lnks[lnks$from != id & lnks$to != id]

  blks <- blockr.core::board_blocks(brd)
  blockr.core::board_blocks(brd) <- blks[setdiff(names(blks), id)]

  brd
}
