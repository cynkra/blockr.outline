# blockr.outline + flextable smoke test.
#
# Proves the outline can drive a slide deck whose exhibits are real
# flextable tables. The table comes from a blockr.extra function block
# that returns a flextable (proper flextable blocks come later); the
# point here is only that such an exhibit survives the outline's
# export + render to a NATIVE pptx table.
#
# pptx is the interesting target: quarto's pptx path drops a flextable,
# so render_report() routes pptx through rmarkdown's officer-backed
# powerpoint_presentation instead (html/pdf stay on quarto).
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
          "blockr.extra", "blockr.outline")
for (d in deps) {
  pkgload::load_all(
    file.path(root, d),
    helpers = FALSE, attach_testthat = FALSE, export_all = FALSE
  )
}

ft_fn <- paste(
  "function(data) {",
  "  flextable::flextable(utils::head(data, 6)) |>",
  "    flextable::font(fontname = 'Trebuchet MS', part = 'all') |>",
  "    flextable::color(color = '#444444', part = 'all') |>",
  "    flextable::border_outer(",
  "      border = officer::fp_border(color = 'black', width = 1))",
  "}",
  sep = "\n"
)

board <- new_dock_board(
  blocks = c(
    data = new_dataset_block("iris", block_name = "Iris data"),
    tbl = blockr.extra::new_function_block(fn = ft_fn, block_name = "Baseline table")
  ),
  links = links(from = "data", to = "tbl"),
  stacks = stacks(
    tables = new_dock_stack(c("data", "tbl"), name = "Tables", color = "#7c3aed")
  ),
  extensions = list(
    blockr.dag::new_dag_extension(),
    blockr.outline::new_outline_extension(
      annotations = list(
        data = list(description = "The iris dataset.", report = FALSE),
        tbl = list(
          description = "Baseline table rendered as a **flextable** for the deck."
        )
      ),
      stack_annotations = list(
        tables = list(description = "Tables in the deck.")
      ),
      title = "Flextable deck test"
    )
  )
)

serve(board)
