# blockr.outline REPORT BUILDER: the middle ground, side by side with the
# two extensions it sits between.
#
# Same board as dev/example-slides.R:
#
#   data ──> mut1 ──> tbl_detail          (table 1)
#              ├────> summ ──> tbl_summary (table 2)
#              └────> chart                (a scatter)
#
# ...and three extensions over it, all on the Main view:
#
#   Outline  the whole Quarto document -- prose, chapters, output preview.
#   Slides   pick blocks, order them, download a deck. Nothing else.
#   Report   the slides interaction, plus markdown notes and the code.
#
# What this script is for:
#
#   * The Report tab opens on the builder: one ordered list of items, each
#     a block or a piece of text. Seeded with a lead paragraph, the mutate
#     block added CODE-ONLY (how the ratio is made, no table), and the
#     summary table (output only). Row order is document order; drag the
#     row number to reorder.
#   * Every block row carries the two switches: </> shows the code, the
#     eye shows the output -- the dock's eye, meaning the same thing it
#     means on a block card. Both off is legal: in the document, silent.
#   * The dots menu holds the rest: figure size on a chart row (presets +
#     document default), full width, add text above/below. The x removes.
#     Clicking the row opens the block's panel.
#   * "+ Text" appends a paragraph; click a text row to edit. Click
#     outside to apply, Esc to discard. Markdown, the outline's editor.
#   * The gear opens the document settings: block titles, default figure
#     size, toc, numbering, code folding, warnings. All of it lands in the
#     emitted YAML -- the qmd you download renders the same outside the
#     app.
#   * Flip the view switch to report.qmd. The note sits above its chunk,
#     the block title is the `##` heading (no note needed for that -- the
#     collapse property), and the code is CANONICAL R: the summarize block
#     emits dplyr, the scatter compiles to a plain ggplot2 pipeline
#     (chart_expr; an identity BAR would still fall back to static_chart --
#     see _inbox/2026-08-12-chart-expr-identity-bars.md). The display
#     tables keep blockr.viz::static_exhibit(), the sanctioned exception.
#   * Every chunk carries its block's icon in the gutter. Click one: the
#     block's panel opens in the active view, exactly like clicking a dag
#     node.
#   * report.R is the same document as a knitr spin script.
#   * Download: HTML renders the qmd (quarto), "Quarto source" / "R
#     script" hand you the text itself. No PowerPoint here: report makes
#     documents, the slides extension makes decks.
#   * Nothing is evaluated until a download (or a code view) asks: the
#     builder reads names off the board. On this deferred board the first
#     download demands the picked blocks, waits, then fires.
#
# Run from the workspace root:
#   Rscript blockr.outline/dev/example-report.R [port]
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

# Deferred by default (Inf), so the two-stage download is exercised. Set
# BLOCKR_EAGER=1 for the everything-is-live path.
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
    # A scatter, because chart_expr() covers it: the report emits a plain
    # dplyr + ggplot2 pipeline for this block, which is the whole
    # canonical-R story on one slide.
    chart = blockr.viz::new_chart_block(
      chart_type = "scatter",
      x = "Sepal.Length",
      y = "ratio1",
      block_name = "Ratio against sepal length"
    )
  ),
  links = links(
    from = c("data", "mut1", "mut1", "summ", "mut1"),
    to   = c("mut1", "tbl_detail", "summ", "tbl_summary", "chart")
  ),
  views = list(
    Main = c("tbl_detail", "tbl_summary", "report", "slides", "outline", "dag")
  ),
  active = "Main",
  extensions = list(
    blockr.dag::new_dag_extension(),
    # Seeded with the whole item surface: a lead text item, a code-only
    # block (the upstream mutate, its output off), a text item between
    # exhibits, and an output-only pick -- so a restored board
    # demonstrates every switch state.
    blockr.outline::new_report_extension(
      title = "Iris pilot",
      settings = list(toc = TRUE),
      items = list(
        list(text = "What a sepal ratio is, and why we summarize it by species."),
        list(block = "mut1", code = TRUE, output = FALSE),
        list(text = "Mean sepal ratio per species, the headline table."),
        list(block = "tbl_summary")
      )
    ),
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
