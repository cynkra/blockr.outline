# blockr.outline as a slide-deck builder, the topline essentials without
# blockr.md: blockr.viz table blocks and topline-style flextables rendered
# to NATIVE, positioned pptx tables on the BMS master.
#
# Two exhibits, to show both paths:
#   * `tbl` -- a normal blockr.viz new_table_block(). It returns an
#     annotated data frame; the outline's report seam prints it through
#     blockr.viz::ft_table(), which stamps pptx_left / pptx_top (0.4 / 1.1)
#     so the deck renderer positions it.
#   * `ft` -- a topline-style flextable from a function block, carrying its
#     own pptx_left / pptx_top attributes. The deck renderer reads those and
#     places it exactly there -- the same positioning contract, so the old
#     topline flextable block drops in unchanged.
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
          "blockr.extra", "blockr.viz", "blockr.outline")
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

# A topline-style flextable: colored header bands, dense borders, and its own
# slide position stamped as attributes (pptx_left / pptx_top). This is what
# the topline flextable block emits; a function block stands in here.
ft_fn <- paste(
  "function(data) {",
  "  ft <- flextable::flextable(utils::head(data, 6))",
  "  ft <- flextable::font(ft, fontname = 'Trebuchet MS', part = 'all')",
  "  ft <- flextable::color(ft, color = '#444444', part = 'all')",
  "  ft <- flextable::bg(ft, bg = '#A59F9F', part = 'header')",
  "  ft <- flextable::color(ft, color = '#FFFFFF', part = 'header')",
  "  ft <- flextable::border_outer(ft,",
  "    border = officer::fp_border(color = 'black', width = 1))",
  "  ft <- flextable::autofit(ft)",
  "  attr(ft, 'pptx_left') <- 0.4",
  "  attr(ft, 'pptx_top') <- 1.1",
  "  ft",
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
    cars = new_dataset_block("mtcars", block_name = "Cars data"),
    ft = blockr.extra::new_function_block(fn = ft_fn,
                                          block_name = "Topline flextable")
  ),
  links = links(
    from = c("iris_data", "st", "cars"),
    to = c("st", "tbl", "ft")
  ),
  stacks = stacks(
    deck = new_dock_stack(c("iris_data", "st", "tbl", "cars", "ft"),
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
        cars = list(report = FALSE),
        ft = list(
          description = "A topline-style flextable, positioned by its own pptx attributes."
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
