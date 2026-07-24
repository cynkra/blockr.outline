# blockr.outline as a slide-deck builder, the topline essentials without
# blockr.md: blockr.viz table blocks and the real topline flextable block,
# rendered to NATIVE, positioned pptx tables on the BMS master.
#
# Two exhibits, to show both paths:
#   * `tbl` -- a normal blockr.viz new_table_block(). It returns an annotated
#     data frame; the outline's report seam prints it through
#     blockr.viz::ft_table(), which stamps pptx_left / pptx_top (0.4 / 1.1)
#     so the deck renderer positions it.
#   * `ft` -- the genuine blockr.topline::new_flextable_block(), with its
#     clinical styling (colored header bands via col_colors, indentation,
#     bold section rows) and its own pptx_left / pptx_top. The deck renderer
#     reads those attributes and places it exactly there, so the topline
#     block drops in unchanged.
#
# pptx is built with officer (render_pptx_officer): pandoc's pptx writer
# ignores the reference template's placeholder geometry and fixes every
# table at 1in / 2in, so officer -- which honours explicit coordinates -- is
# the only way to place a table where the slide wants it. html / pdf / docx
# still render from the one quarto Document.
#
#   Rscript blockr.outline/dev/example-flextable.R [port]

port <- local({
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args)) as.integer(args[[1L]]) else
    as.integer(Sys.getenv("BLOCKR_PORT", "3838"))
})

options(shiny.port = port, shiny.host = "0.0.0.0")
options(blockr.dock_is_locked = FALSE)
options(blockr.background_construction_delay = 0)

message("Open http://127.0.0.1:", port, "/")

root <- "."
deps <- c("dockViewR", "blockr.core", "blockr.dag", "blockr.dock",
          "blockr.extra", "blockr.viz", "blockr.topline", "blockr.outline")
for (d in deps) {
  pkgload::load_all(
    file.path(root, d),
    helpers = FALSE, attach_testthat = FALSE, export_all = FALSE
  )
}

# The BMS reference template ships in blockr.topline.
bms_template <- system.file(
  "templates", "bms-template.pptx", package = "blockr.topline"
)
if (!nzchar(bms_template)) {
  bms_template <- file.path(root, "blockr.topline", "inst", "templates",
                            "bms-template.pptx")
}

# A clinical demographics frame in topline's annotated shape: `.label` is the
# row stub, `.indent` the display depth, `.bold` the section rows. The two
# data columns are the treatment arms. new_flextable_block() styles this.
clin_fn <- paste(
  "function(data) {",
  "  data.frame(",
  "    check.names = FALSE, stringsAsFactors = FALSE,",
  "    '.label' = c('Age (years)', 'Mean (SD)', 'Median',",
  "                 'Sex, n (%)', 'Female', 'Male'),",
  "    '.indent' = c(0, 1, 1, 0, 1, 1),",
  "    '.bold' = c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE),",
  "    'PBO  N=334' = c('', '52.1 (13.4)', '53', '', '180 (53.9)', '154 (46.1)'),",
  "    'DEUC 6 mg  N=336' = c('', '51.8 (12.9)', '52', '', '179 (53.3)', '157 (46.7)')",
  "  )",
  "}",
  sep = "\n"
)

board <- new_dock_board(
  blocks = c(
    iris_data = new_dataset_block("iris", block_name = "Iris data"),
    st = blockr.viz::new_summary_table_block(
      vars = c("Sepal.Length", "Sepal.Width"), by = "Species",
      block_name = "Iris summary"
    ),
    tbl = blockr.viz::new_table_block(block_name = "Summary table"),
    seed = new_dataset_block("iris", block_name = "Seed"),
    clin = blockr.extra::new_function_block(fn = clin_fn,
                                            block_name = "Demographics data"),
    ft = blockr.topline::new_flextable_block(
      col_colors = c("gray", "dark_gray", "blue"),
      first_column_label = "Characteristics",
      first_col_width = 3.2, other_cols_width = 2.6,
      pptx_left = 0.4, pptx_top = 1.1,
      block_name = "Topline flextable"
    )
  ),
  links = links(
    from = c("iris_data", "st", "seed", "clin"),
    to = c("st", "tbl", "clin", "ft")
  ),
  stacks = stacks(
    deck = new_dock_stack(c("iris_data", "st", "tbl", "seed", "clin", "ft"),
                          name = "Deck", color = "#7c3aed")
  ),
  extensions = list(
    blockr.dag::new_dag_extension(),
    blockr.outline::new_outline_extension(
      annotations = list(
        iris_data = list(report = FALSE),
        st = list(report = FALSE),
        tbl = list(
          description = "Baseline sepal measures by species (blockr.viz table block via ft_table)."
        ),
        seed = list(report = FALSE),
        clin = list(report = FALSE),
        ft = list(
          description = "Demographics, the topline flextable block: colored header bands, positioned by its pptx attributes."
        )
      ),
      stack_annotations = list(
        deck = list(description = "Summary tables in the deck.")
      ),
      title = "Flextable deck",
      template = bms_template
    )
  )
)

serve(board)
