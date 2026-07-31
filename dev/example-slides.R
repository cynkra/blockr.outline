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
#   * Then the Big view, which is the interesting one. Add "Air quality,
#     daily" (153 rows) to the deck and download it, then press the table
#     block's OWN PowerPoint button on the same table. The block writer
#     (blockr.viz::write_exhibit_pptx) comes back with eight slides, each
#     repeating the header and titled "(3 of 8)"; the deck comes back with
#     one slide the table runs off the bottom of. Two routines, and only
#     one of them paginates.
#   * "Ozone, month by day" (32 columns) is the control: both routes size
#     the columns to the template's content width, so that slide comes out
#     the SAME either way. The gap is height, not width -- which is why
#     write_exhibit_pptx()'s row pagination is the piece worth sharing.
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
    # `download = TRUE` on the big tables, so the block's own PowerPoint
    # button sits a click away from the deck's. Same table, two routes:
    # blockr.viz::write_exhibit_pptx() steps the font down, then pages the
    # rows over as many slides as it needs (each repeating the header,
    # titled "(3 of 8)"), while the deck's render_pptx_officer() places one
    # flextable per slide and lets a long table run off the bottom. That
    # difference is what these blocks are here to show.
    tbl_detail = blockr.viz::new_table_block(
      block_name = "Flower measurements",
      download = TRUE
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
    ),
    # Two big tables, one in each direction. TALL: 153 daily readings, which
    # the block writer pages over eight slides and the deck puts on one.
    # WIDE: the same readings pivoted to one column per day, 32 of them --
    # squeezed to the slide by BOTH routes, since the deck sets the same
    # blockr.viz.ft_fit_width the block writer does (render.R:888). It is
    # here as the control: it shows the gap is height, not width.
    # `title =` as well as `block_name =`: the block name labels the slide the
    # DECK builds, while the block writer titles its own pages from the
    # table's title -- and without one there is nothing for it to mark
    # "(3 of 8)" on.
    aq = new_dataset_block("airquality", block_name = "Air quality data"),
    tbl_long = blockr.viz::new_table_block(
      block_name = "Air quality, daily",
      title = "Air quality, daily",
      download = TRUE
    ),
    wide = blockr.dplyr::new_pivot_wider_block(
      id_cols = list("Month"),
      names_from = list("Day"),
      values_from = list("Ozone"),
      names_prefix = "d",
      block_name = "Ozone by day"
    ),
    tbl_wide = blockr.viz::new_table_block(
      block_name = "Ozone, month by day",
      title = "Ozone, month by day",
      download = TRUE
    )
  ),
  links = links(
    from = c("data", "mut1", "mut1", "summ", "summ", "aq", "aq", "wide"),
    to   = c("mut1", "tbl_detail", "summ", "tbl_summary", "chart",
             "tbl_long", "wide", "tbl_wide")
  ),
  views = list(
    Main = c("tbl_detail", "tbl_summary", "slides", "outline", "dag"),
    Big = c("tbl_long", "tbl_wide", "slides")
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
