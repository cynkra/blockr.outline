# Prove the topline attrition plot (CONSORT-style participant disposition)
# renders as an exhibit through the outline's officer pptx path, on the BMS
# master. Uses the real clinical RTF data volume shipped with the examples.
#
#   Rscript blockr.outline/dev/example-attrition.R [port]

port <- local({
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args)) as.integer(args[[1L]]) else
    as.integer(Sys.getenv("BLOCKR_PORT", "3838"))
})

options(shiny.port = port, shiny.host = "0.0.0.0")
options(blockr.dock_is_locked = FALSE)
options(blockr.background_construction_delay = 0)

# The clinical RTF volume that the read_rtf blocks resolve filenames against.
vol <- Sys.getenv("BLOCKR_TOPLINE_VOLUME", "")
if (!nzchar(vol)) {
  cand <- file.path(
    "blockr.dev", ".devcontainer", ".library", "artful",
    "extdata", "examples"
  )
  if (dir.exists(cand)) vol <- normalizePath(cand)
}
options(blockr.topline_volume = vol)
message("topline volume: ", vol)
message("Open http://127.0.0.1:", port, "/")

root <- "."
deps <- c("dockViewR", "blockr.core", "blockr.dplyr", "blockr.ui",
          "blockr.dag", "blockr.dock", "blockr.ggplot", "blockr.extra",
          "blockr.viz", "blockr.topline", "blockr.outline")
for (d in deps) {
  pkgload::load_all(
    file.path(root, d),
    helpers = FALSE, attach_testthat = FALSE, export_all = FALSE
  )
}

bms_template <- system.file(
  "templates", "bms-template.pptx", package = "blockr.topline"
)
if (!nzchar(bms_template)) {
  bms_template <- file.path(root, "blockr.topline", "inst", "templates",
                            "bms-template.pptx")
}

board <- new_dock_board(
  blocks = c(
    pretrt = blockr.topline::new_read_rtf_block(
      file = file.path(vol, "rt-ds-pretrt.rtf"),
      fix_column_shift = FALSE, indentation_to_vars = TRUE,
      block_name = "Pre-treatment disposition"
    ),
    trtwk16 = blockr.topline::new_read_rtf_block(
      file = file.path(vol, "rt-ds-trtwk16.rtf"),
      fix_column_shift = FALSE, indentation_to_vars = TRUE,
      block_name = "Week-16 disposition"
    ),
    attrition = blockr.topline::new_attrition_plot_block(
      block_name = "Participant disposition"
    )
  ),
  links = links(
    from = c("pretrt", "trtwk16"),
    to = c("attrition", "attrition"),
    input = c("x", "y")
  ),
  stacks = stacks(
    deck = new_dock_stack(c("pretrt", "trtwk16", "attrition"),
                          name = "Disposition", color = "#7c3aed")
  ),
  extensions = list(
    blockr.dag::new_dag_extension(),
    blockr.outline::new_outline_extension(
      annotations = list(
        pretrt = list(report = FALSE),
        trtwk16 = list(report = FALSE),
        attrition = list(
          description = "Participant disposition through Week 16 (topline attrition plot).",
          report = TRUE
        )
      ),
      stack_annotations = list(
        deck = list(description = "Participant disposition.")
      ),
      title = "Disposition deck",
      template = bms_template
    )
  )
)

serve(board)
