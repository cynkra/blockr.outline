# blockr.outline showcase: a reproducible modelling workflow, authored as
# a document.
#
# The story: predict a penguin's body mass from its size and species. Take
# the Palmer penguins, tidy them, fit a linear model, read the
# coefficients, and see the fit -- each step carrying a plain-language
# description, so the outline renders the whole analysis as an explained
# report (Outline / R script / Document, one-click html/pptx/pdf).
#
# Uses the usual blockr.verse pieces:
#   * blockr.stats   model + broom (tidy / glance) blocks
#   * blockr.ggplot  the scatter -- a ggplot block, whose code IS ggplot2,
#                    so the figure reproduces in the rendered report (a
#                    blockr.viz chart would only export a data passthrough)
#   * blockr.extra   HTML table preview (blockr.html_table_preview option)
#   * blockr.session project save / load / versions (manage_project plugin)
#   * blockr.outline the narrated outline + report render
#
# Run from the workspace root:
#   Rscript blockr.outline/dev/example-penguins.R [port]

port <- local({
  a <- commandArgs(trailingOnly = TRUE)
  if (length(a)) as.integer(a[[1L]]) else
    as.integer(Sys.getenv("BLOCKR_PORT", "3838"))
})

options(
  shiny.port = port, shiny.host = "0.0.0.0",
  blockr.dock_is_locked = FALSE,
  # nice paginated / sortable HTML tables for data.frame outputs
  # (the blockr.extra table-preview glue).
  blockr.html_table_preview = TRUE,
  blockr.background_construction_delay = 0
)

message("Open http://127.0.0.1:", port, "/")

root <- "."
deps <- c(
  "dockViewR", "blockr.core", "blockr.ui", "blockr.session", "blockr.dag",
  "blockr.dock", "blockr.dplyr", "blockr.ggplot", "blockr.viz",
  "blockr.extra", "blockr.stats", "blockr.outline"
)
for (d in deps) {
  pkgload::load_all(
    file.path(root, d),
    helpers = FALSE, attach_testthat = FALSE, export_all = FALSE
  )
}

library(palmerpenguins)

# Authored in the model card's formula widget; built here from text.
mdl_formula <- blockr.stats:::parse_formula(
  "body_mass_g ~ flipper_length_mm + bill_length_mm + species"
)

board <- new_dock_board(
  blocks = c(
    peng = new_dataset_block(
      dataset = "penguins", package = "palmerpenguins",
      block_name = "Palmer penguins"
    ),
    # Data transformation: keep the complete records the model needs.
    clean = blockr.dplyr::new_filter_block(
      conditions = list(
        list(
          type = "expr",
          expr = paste(
            "!is.na(body_mass_g) & !is.na(flipper_length_mm)",
            "& !is.na(bill_length_mm)"
          )
        )
      ),
      block_name = "Complete cases"
    ),
    # A short preview stands in for the data in the report: df-print:kable
    # prints every row, so the full 342-row frame would be an endless
    # table (and a giant pptx slide). Six rows tell the story.
    peek = new_head_block(n = 6L, block_name = "First rows"),
    # Model estimation: the model block's own card carries the formula
    # widget and coefficient forest; the outline turns it into a document
    # step.
    mdl = new_model_block(
      model_type = "lm", formula = mdl_formula, block_name = "Linear model"
    ),
    # Read the model two ways: coefficient table and one-line fit stats.
    coefs = new_broom_block(
      output = "tidy", conf_int = TRUE, block_name = "Coefficients"
    ),
    fitstats = new_broom_block(
      output = "glance", block_name = "Model fit"
    ),
    # Visualise the relationship the model captures. blockr.ggplot, not a
    # blockr.viz chart: the ggplot block's expression IS ggplot2 code, so
    # the figure reproduces in the rendered document. A viz chart renders
    # through an echarts widget in its output pane -- great in the app,
    # but the exported code is only a data passthrough, so the report
    # would show the data, not the plot.
    fit = blockr.ggplot::new_ggplot_block(
      type = "point", x = "flipper_length_mm", y = "body_mass_g",
      color = "species", block_name = "Mass vs flipper length"
    )
  ),
  links = links(
    from = c("peng", "clean", "clean", "mdl", "mdl", "clean"),
    to   = c("clean", "peek", "mdl", "coefs", "fitstats", "fit")
  ),
  stacks = stacks(
    data = new_dock_stack(
      c("peng", "clean", "peek"), name = "The data", color = "#2563eb"
    ),
    model = new_dock_stack(
      c("mdl", "coefs", "fitstats"), name = "The model", color = "#7c3aed"
    ),
    fit = new_dock_stack("fit", name = "The fit", color = "#d97706")
  ),
  extensions = list(
    blockr.dag::new_dag_extension(),
    blockr.outline::new_outline_extension(
      title = "Predicting penguin body mass",
      annotations = list(
        peng = list(
          description = paste(
            "The **Palmer penguins**: 344 birds of three species, with",
            "bill, flipper and body-mass measurements. A friendlier stand-in",
            "for iris, and a natural regression target -- body mass is the",
            "thing to predict."
          ),
          report = FALSE
        ),
        clean = list(
          description = paste(
            "A handful of birds are missing a measurement. Restrict to the",
            "complete records the model needs, so every row contributes to",
            "the fit."
          ),
          report = FALSE
        ),
        peek = list(
          description = "The first few rows of the prepared data."
        ),
        mdl = list(
          description = paste(
            "Fit body mass as a linear function of **flipper length**,",
            "**bill length** and **species**. Flipper length carries most",
            "of the signal; species shifts the intercept."
          )
        ),
        coefs = list(
          description = paste(
            "The estimated coefficients with 95% confidence intervals. Each",
            "extra millimetre of flipper adds a few dozen grams; the species",
            "terms are the baseline differences that flipper length does not",
            "explain."
          )
        ),
        fitstats = list(
          description = paste(
            "One line of fit statistics -- adjusted R-squared says how much",
            "of the mass variation the model accounts for."
          )
        ),
        fit = list(
          description = paste(
            "The relationship, drawn: body mass against flipper length,",
            "coloured by species. The upward trend is the flipper effect;",
            "the colour bands are the species differences the model",
            "estimates."
          )
        )
      ),
      stack_annotations = list(
        data = list(
          description = "Where the numbers come from, and the tidy-up."
        ),
        model = list(description = "The estimate, and how well it fits."),
        fit = list(description = "The same story as a picture.")
      )
    )
  )
)

# blockr.session's manage_project plugin: save / load / version the
# workflow from the app.
serve(
  board,
  plugins = custom_plugins(manage_project())
)
