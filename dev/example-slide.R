# SLIDE BLOCK prototype (spec: _blockr.design/open/slide-block/).
#
# A board with three slide blocks next to the data they draw from:
#
#   data ──> summ ──────> slide_one   (exhibit + takeaway, 1 input)
#     └────────────┬────> slide_two   (two-up, 2 inputs: summary + detail)
#   summ ──────────┘
#   (no input) ────────> slide_txt    (bullets, zero inputs)
#
# ...plus the SLIDES EXTENSION over the same board, seeded with all three
# slide blocks and the raw summary: composed slides pass through with their
# own chrome, the plain pick gets the deck's classic one-up slide -- both
# in one download.
#
# What to try:
#   * Open a slide block: the settings fields sit above a live 16:9 preview
#     painted from the SAME inch geometry the pptx download places.
#   * Switch the layout; the text field relabels (takeaway / bullets) and
#     the capacity line says what the layout wants vs. what is linked.
#   * Download: one slide, template-styled, truncation marker and all.
#
# Run from the workspace root:
#   Rscript blockr.outline/dev/example-slide.R [port]

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

# The BMS house look, unless BLOCKR_PLAIN=1: theme_bms() carries the master
# deck (inst/templates/bms-template.pptx), the clinical header bands and the
# Trebuchet exhibit face. blockr.bms is vendored inside blockr.sandbox, the
# deploy bundle -- pkgload maps its inst/ exactly as an install would.
# apply_theme_options() is what the deck render reads too, so the slide
# block's tables and the deck's tables style through one seam.
if (!nzchar(Sys.getenv("BLOCKR_PLAIN"))) {
  pkgload::load_all(
    file.path(root, "blockr.theme"),
    helpers = FALSE, attach_testthat = FALSE, export_all = FALSE
  )
  pkgload::load_all(
    file.path(root, "blockr.sandbox", "inst", "blockr.bms"),
    helpers = FALSE, attach_testthat = FALSE, export_all = FALSE
  )
  thm <- blockr.bms::theme_bms()
  blockr.theme::apply_theme_options(thm)
  tmpl <- blockr.theme::theme_template(thm, "pptx")
  if (!is.null(tmpl) && nzchar(tmpl)) {
    options(blockr.outline.template = tmpl)
  }
  message("[theme] BMS -- deck: ", tmpl)
}

board <- new_dock_board(
  blocks = c(
    data = new_dataset_block("iris", block_name = "Iris data"),
    summ = blockr.dplyr::new_summarize_block(
      summaries = list(
        list(type = "simple", name = "mean_sepal", func = "mean",
             col = "Sepal.Length"),
        list(type = "simple", name = "mean_petal", func = "mean",
             col = "Petal.Length")
      ),
      by = list("Species"),
      block_name = "Means by species"
    ),
    slide_one = blockr.outline::new_slide_block(
      layout = "exhibit-callout",
      title = "Sepal and petal means",
      subtitle = "By species, full sample",
      footnote = "Source: iris, n = 150",
      text = paste0("**Takeaway:** *virginica* leads on both measures; ",
                    "the gap to versicolor is under one centimetre."),
      block_name = "Summary slide"
    ),
    slide_two = blockr.outline::new_slide_block(
      layout = "two-up-h",
      title = "Summary and detail",
      footnote = "Source: iris",
      block_name = "Two-up slide"
    ),
    slide_txt = blockr.outline::new_slide_block(
      layout = "bullets",
      title = "What to look at",
      text = paste("Means: virginica leads on both measures",
                   "Detail: the raw rows behind the summary",
                   "**Next:** decide whether petal width earns a slide",
                   sep = "\n"),
      block_name = "Bullet slide"
    )
  ),
  links = list(
    list(from = "data", to = "summ", input = "data"),
    list(from = "summ", to = "slide_one", input = "x"),
    list(from = "summ", to = "slide_two", input = "x"),
    list(from = "data", to = "slide_two", input = "y")
  ),
  extensions = list(
    blockr.outline::new_slides_extension(
      title = "Iris topline",
      slides = c("slide_one", "slide_txt", "summ")
    )
  )
)

serve(board)
