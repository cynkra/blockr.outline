# blockr.outline demo: the outline extension as a dock panel.
#
# Same five-block board as the block-docs pilot demo (including the
# deliberate split stack: "QC glance" sits in Data prep but depends on the
# Analysis subset), but descriptions, report flags and document order are
# extension state -- the blocks are plain main-API blocks, no description
# or report constructor arguments. Things to try in the "Outline" panel:
#   * click a chip or section: the block's dock panel comes to front,
#   * hover a section, hit the pencil: edit its markdown inline,
#   * flip a switch: include/exclude from the report (no board update),
#   * hover a chip, use the arrows: reorder parallel branches,
#   * R script / Document views, Render (html / pptx).
#
# Run from the workspace root:
#   Rscript blockr.outline/dev/example-outline.R [port]
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

# A report surface needs every block's expression, including blocks whose
# panels are hidden tabs. Core's deferred construction only builds blocks
# the dock reports visible, and an extension cannot request construction,
# so this demo constructs eagerly. Recorded core follow-up: an exported
# construct-on-demand hook for report-style consumers.
options(blockr.background_construction_delay = 0)

message("Open http://127.0.0.1:", port, "/")

# All-or-nothing: load_all EVERY blockr package involved (cdex-style).
root <- "."
deps <- c("dockViewR", "blockr.core", "blockr.dag", "blockr.dock",
          "blockr.dplyr", "blockr.ggplot", "blockr.outline")
for (d in deps) {
  pkgload::load_all(
    file.path(root, d),
    helpers = FALSE, attach_testthat = FALSE, export_all = FALSE
  )
}

board <- new_dock_board(
  blocks = c(
    data = new_dataset_block("iris", block_name = "Iris data"),
    filt = blockr.dplyr::new_filter_block(
      conditions = list(
        list(
          type = "values",
          column = "Species",
          values = list("setosa", "versicolor"),
          mode = "include"
        )
      ),
      block_name = "Two species"
    ),
    mut = blockr.dplyr::new_mutate_block(
      mutations = list(
        list(name = "ratio", expr = "Sepal.Length / Sepal.Width")
      ),
      block_name = "Sepal ratio"
    ),
    summ = blockr.dplyr::new_summarize_block(
      summaries = list(
        list(type = "simple", name = "avg_ratio", func = "mean", col = "ratio")
      ),
      by = list("Species"),
      block_name = "Ratio by species"
    ),
    plot = blockr.ggplot::new_ggplot_block(
      type = "point",
      x = "Sepal.Length",
      y = "Sepal.Width",
      color = "Species",
      block_name = "Sepal scatter"
    ),
    audit = new_head_block(
      n = 3L,
      direction = "tail",
      block_name = "QC glance"
    )
  ),
  links = links(
    from = c("data", "filt", "mut", "mut", "mut"),
    to = c("filt", "mut", "summ", "plot", "audit")
  ),
  # Three chapters fed by the same prepared table: the order between the
  # chart and the table chapter is a real authoring choice, so the chapter
  # grip can swap them.
  stacks = stacks(
    prep = new_dock_stack(
      c("data", "filt", "mut"),
      name = "Data prep",
      color = "#2563eb"
    ),
    charts = new_dock_stack(
      "plot",
      name = "Charts",
      color = "#d97706"
    ),
    tables = new_dock_stack(
      c("summ", "audit"),
      name = "Tables",
      color = "#7c3aed"
    )
  ),
  extensions = list(
    blockr.dag::new_dag_extension(),
    blockr.outline::new_outline_extension(
      annotations = list(
        data = list(
          description = paste(
            "The classic **iris** dataset: 150 flowers, three species,",
            "four size measurements.",
            "",
            "Used here as stand-in study data.",
            sep = "\n"
          )
        ),
        filt = list(
          description = paste(
            "Drop *virginica*: the two remaining species are the ones that",
            "overlap, which is what makes the comparison interesting."
          )
        ),
        mut = list(
          description = paste(
            "Derive the sepal **ratio**, length over width, as a single",
            "shape measure per flower."
          )
        ),
        summ = list(
          description = "Mean sepal ratio per species."
        ),
        plot = list(
          description = paste(
            "Sepal length against sepal width, coloured by species.",
            "",
            "- setosa separates cleanly",
            "- versicolor overlaps it only at the margin",
            sep = "\n"
          )
        ),
        audit = list(
          description = paste(
            "Last rows of the prepared table, as a quick QC check that the",
            "ratio landed on every row."
          ),
          report = FALSE
        )
      ),
      stack_annotations = list(
        prep = list(
          description = paste(
            "Everything the analysis consumes: the raw data, the species",
            "restriction and the derived ratio."
          )
        ),
        charts = list(description = "What we plot from the prepared table."),
        tables = list(description = "Numbers behind the chart.")
      ),
      title = "Iris pilot report"
    )
  )
)

serve(board)
