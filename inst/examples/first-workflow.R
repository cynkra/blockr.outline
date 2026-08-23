# blockr.outline -- first-workflow: an empty board, and the outline beside it.
#
# THE SIXTY-SECOND BOARD. This is the canonical minimal example for
# blockr.outline, and it is deliberately the smallest useful thing in the
# gallery: nothing is loaded, nothing is wired, and the whole point is what
# happens in the first minute. Add a dataset block, filter it, plot it. Three
# clicks, and the outline on the left fills in as you go.
#
# WHY IT EXISTS SEPARATELY FROM `empty`. The `empty` gallery demo is a blank
# dock: a clean canvas and nothing to read. This one answers the question a
# blank canvas leaves open -- "what did I just build?" -- by mounting the
# OUTLINE as a permanent left rail. The board is still blank; the difference is
# that the pipeline is legible from the first block on.
#
# THE OUTLINE IS THE PIPELINE VIEW, AND THE ONLY ONE. `blockr.dag`'s canvas
# draws the same graph with the same edges, and on a board this small it is the
# worse of the two: a two-node graph floating in a canvas says less than a
# two-row list. Two answers to one question is a worse demo than one answer, so
# the DAG extension is not loaded here at all.
#
# THE REPORT VIEW IS THE SECOND HALF OF THE CLAIM, and it is why this board is
# worth showing rather than describing. Add the three blocks as report items,
# flip a code switch, and the document that comes out is ordinary R that runs
# without blockr. On a three-block pipeline that document is about ten lines,
# which is short enough to READ ON A PROJECTOR -- something no real board's
# export will ever be. Teach "the export is canonical R" here, on a document
# people can actually read.
#
# GGPLOT IS THE PLOT BLOCK, AND blockr.viz IS NOT LOADED AT ALL. Two reasons,
# and the second is the one that matters:
#
#   * It is the plotting vocabulary this audience already has. `ggplot` with an
#     x, a y and a colour is a sentence a statistician can read off the screen;
#     an echarts chart block is a widget they have to be taught first.
#   * It keeps the exported document blockr-free. A ggplot block emits
#     `ggplot2::ggplot(...)`, which renders in any session with ggplot2. The
#     chart block emits a `blockr.viz` call, so the qmd would depend on us --
#     in the one example whose whole job is to show that it does not. On a
#     three-block board that is the difference between a claim and a caveat.
#
# Nothing is lost by dropping viz here: every block panel already renders its
# data as a paginated table (that is `blockr.tabular_display` below), so a
# three-block board needs no table block.
#
# THE BLOCK BROWSER IS CURATED, same as the `empty` gallery app. blockr.core's
# low-level blocks (subset, merge, rbind, head, csv, filebrowser, upload) are
# unregistered so the add-block menu a newcomer opens is short enough to scan.
# This is presentation, not policy: everything from dplyr and ggplot stays.
#
# Run the shipped copy (installed packages):
#   source(system.file("examples/first-workflow.R", package = "blockr.outline"))
# Run it against local source checkouts instead:
#   source("blockr.outline/dev/first-workflow.R")   # sets dev_local <- TRUE

# ---- Package loading (dual: installed vs local source) ---------------------
if (!exists("dev_local")) dev_local <- FALSE

# Named one per line, not looped: the gallery generator reads this file
# line-by-line for the demo's dependency list, so a loop variable would go on
# the website as a package called `pkg`.
blockr_pkgs <- c(
  "blockr.ui",
  "blockr.core",     # the dataset block, and the block registry
  "blockr.io",       # Import Data: a csv from a path or a URL
  "blockr.dock",     # the dock board and its views
  "blockr.dplyr",    # the filter block, step two of the sixty seconds
  "blockr.ggplot",   # the ggplot block, step three
  "blockr.session",  # project save / load / versions
  "blockr.outline"   # the outline rail and the report builder
)

for (pkg in blockr_pkgs) {
  if (dev_local) pkgload::load_all(pkg, quiet = TRUE)
  else library(pkg, character.only = TRUE)
}

# NO DATA PACKAGE. The dataset block reads from `datasets`, which ships with R,
# so `iris` and `mtcars` are one picker away and this example adds nothing to
# the install line on the website. `library(palmerpenguins)` here would put
# penguins in the same picker, at the cost of one CRAN dependency.
#
# TWO WAYS IN, AND THAT IS DELIBERATE. The INPUT category holds both the
# dataset block (pick something from `datasets`, zero typing, nothing to go
# wrong) and Import Data (a csv from a path or a URL). They serve two different
# people and the board does not have to choose:
#
#   * A visitor who opened this from the gallery picks the dataset block and is
#     plotting iris fifteen seconds later.
#   * A presenter who wants the demo to run on REAL data pastes a URL into
#     Import Data instead, and the same sixty seconds now also make the point
#     that the data came off somebody's website rather than out of our package.
#
# Same board, same three steps, different first block. Do not fork this file to
# hard-code a URL: a seeded read block is a pre-built board, which is the one
# thing this example exists not to be.

options(
  blockr.dock_is_locked = FALSE,
  blockr.tabular_display = blockr.ui::html_table_display,
  blockr.background_construction_delay = 0,
  blockr.visible_extensions = "outline"
)

if (dev_local) options(blockr.outline.execute = "in-process")

# ---- Curate the block browser ----------------------------------------------
# Keep only `dataset` and `glue` from blockr.core and drop the rest, selecting
# by the registry's `package` attribute so no other package's blocks are
# touched. Same list as the `empty` gallery app.
local({
  keep <- c("dataset_block", "glue_block")
  core <- Filter(
    function(entry) identical(attr(entry, "package"), "blockr.core"),
    blockr.core::available_blocks()
  )
  drop <- setdiff(names(core), keep)
  if (length(drop)) blockr.core::unregister_blocks(drop)
})

# ---------------------------------------------------------------- board ----
# NO BLOCKS, NO LINKS, NO STACKS. Everything on this board arrives by clicking,
# which is the entire exhibit.
board <- new_dock_board(
  extensions = list(
    blockr.outline::new_outline_extension(),
    # An empty document. Items are added in the panel: pick a block, choose
    # whether its code, its output or both go in, and write the prose between
    # them.
    blockr.outline::new_report_extension(
      title = "Untitled analysis",
      settings = list(toc = FALSE, number_sections = FALSE, warnings = FALSE)
    )
  ),
  grids = list(
    # THE LEFT RAIL IS THE WHOLE LAYOUT DECISION. The outline is pinned narrow
    # on the left and every block added lands to the right of it, so the
    # pipeline stays readable while the board grows.
    Build = dock_grid(ext("outline")),
    Report = dock_grid(
      ext("report"), ext("outline"),
      orientation = "horizontal", sizes = c(2, 1)
    )
  ),
  active = "Build"
)

serve(board, plugins = custom_plugins(manage_project()))
