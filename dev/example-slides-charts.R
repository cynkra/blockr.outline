# The slide builder over a board that is mostly CHARTS, so the browser
# capture can be judged on the shapes it was built for rather than on one
# bar chart.
#
#   BLOCKR_CANVAS_CAPTURE=1 Rscript blockr.outline/dev/example-slides-charts.R [port]
#
# With the flag on, the deck's chart slides are pictures the browser drew:
# each chart is mounted offscreen at the slide's box and composed there, so a
# slide carries what the panel would show at that size, whether or not the
# panel was ever opened. Without the flag the same deck goes through
# static_chart() and you get the ggplot rebuild to compare against.
#
# What to try
#
#   * Slides tab: the deck is seeded with all four charts. Download as
#     PowerPoint.
#   * Open none of the chart panels first. The captures still come, because
#     the offscreen mount does not need the panel.
#   * Then open "Age by arm", change something in its gear (turn it
#     horizontal, switch the sort), and download again. The slide follows.
#   * Restart without BLOCKR_CANVAS_CAPTURE=1 for the ggplot deck.
#
# The arm labels are long on purpose: at slide width they are what the two
# renderers used to disagree about.

port <- local({
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args)) as.integer(args[[1L]]) else
    as.integer(Sys.getenv("BLOCKR_PORT", "3838"))
})

options(shiny.port = port, shiny.host = "0.0.0.0")
options(blockr.dock_is_locked = FALSE)
options(blockr.background_construction_delay =
          if (nzchar(Sys.getenv("BLOCKR_EAGER"))) 0 else Inf)

message("Open http://127.0.0.1:", port, "/")

for (d in c("dockViewR", "blockr.core", "blockr.dag", "blockr.dock",
            "blockr.dplyr", "blockr.viz", "blockr.outline")) {
  pkgload::load_all(file.path(".", d), helpers = FALSE,
                    attach_testthat = FALSE, export_all = FALSE)
}

# The data comes from a DATASET block plus a mutate, not new_static_block():
# a deck EVALUATES each exported block's code, and a static block emits
# `x <- get("data", envir = <environment>)`, which does not parse. Every
# slide then fails with "Deck render failed: unexpected '<'". The same trap
# the outline has (see blockr.viz/dev/parity/README.md).
board <- new_dock_board(
  blocks = c(
    data = new_dataset_block("adsl", package = "pharmaverseadam",
                             block_name = "ADSL (pharmaverseadam)"),

    # Long arm labels, which is what makes the category axis interesting:
    # a wide panel keeps them flat, a slide cannot.
    arms = blockr.dplyr::new_mutate_block(
      mutations = list(
        list(
          name = "ARM",
          expr = paste0(
            'paste0("GROUP", substr(TRT01P, 1, 4), ',
            '" ", TRT01P, " 2.0mg+Pumitamig 1500mg")'
          )
        ),
        list(name = "DURATION", expr = "as.numeric(TRTDURD)")
      ),
      block_name = "Derive arm label and BMI"
    ),

    # The chart this whole thread started from: long arm labels, vertical
    # boxplot, where the panel's box and the slide's box disagree.
    anova = blockr.viz::new_chart_block(
      chart_type = "boxplot", group = "ARM", value = "AGE",
      title = "Age by arm", download = TRUE,
      block_name = "Age by arm"
    ),

    # Colour split plus facets: several ECharts instances, a shared legend
    # band and facet labels, all of which the composer has to place.
    split = blockr.viz::new_chart_block(
      chart_type = "boxplot", group = "ARM", value = "DURATION",
      color = "SEX", facet = "SEX",
      title = "Treatment duration by arm and sex", download = TRUE,
      block_name = "Duration by arm and sex"
    ),

    # A horizontal bar, the layout the canvas defaults to.
    bars = blockr.viz::new_chart_block(
      chart_type = "bar", group = "ARM", value = ".count", func = "count",
      count_on = "axis",
      title = "Subjects per arm", download = TRUE,
      block_name = "Subjects per arm"
    ),

    # Something short-labelled, to see a chart that does NOT have to turn
    # its labels at slide width.
    race = blockr.viz::new_chart_block(
      chart_type = "bar", group = "RACE", value = "AGE", func = "mean",
      orientation = "vertical",
      title = "Mean age by race", download = TRUE,
      block_name = "Mean age by race"
    ),

    # One table, so the deck mixes both exhibit routes.
    tbl = blockr.viz::new_table_block(
      block_name = "Demographics table", title = "Demographics",
      download = TRUE
    )
  ),
  links = links(
    from = c("data", "arms", "arms", "arms", "arms", "arms"),
    to = c("arms", "anova", "split", "bars", "race", "tbl")
  ),
  views = list(
    Main = c("anova", "split", "slides", "dag"),
    More = c("bars", "race", "tbl", "slides")
  ),
  active = "Main",
  extensions = list(
    blockr.dag::new_dag_extension(),
    blockr.outline::new_slides_extension(
      title = "Study population",
      slides = c("anova", "split", "bars", "race", "tbl")
    )
  )
)

print(serve(board))
