# The COMMON table path in a report: a composer table -> table block ->
# outline. This is the composer-viz pipeline (blockr.sandbox's
# composer-viz-demo) with an outline extension bolted on, to show what
# static_table means at the report seam.
#
# Pipeline:  data -> composer -> to_df -> viz (new_table_block)
#   * composer : new_function_block, composer code returning a composed_table
#   * to_df    : new_function_block calling blockr.viz::as_annotated_df()
#   * viz      : blockr.viz::new_table_block() -- the INTERACTIVE dashboard
#                table (search / sort / drill), the thing you normally use.
#
# THE SEAM: the table block's OUTPUT is an annotated data frame. In the live
# app that annotated df is drawn as the interactive widget. A report / deck
# is static -- no widget -- so the outline does NOT ship the widget; it
# applies a STATIC print function to the SAME output,
# `blockr.viz::static_exhibit`, which renders any table-shaped value as a
# styled flextable (and returns anything else untouched). Look at the R script
# / Document view: the reported chunks end in
# `blockr.viz::static_exhibit(<id>)`. That is the whole mechanism -- one
# alternative renderer on the block's output, not a different block. (Charts
# work the same way via static_chart.)
#
# Which is why the `composer` block is ALSO reported here, with nothing after
# it: static_exhibit() coerces the raw composed_table itself, so that slide is
# identical to the one built from the table block. The render block is a
# dashboard component (search / sort / drill / Excel download), not a report
# requirement.
#
#   Rscript blockr.outline/dev/example-composer-table.R [port]

port <- local({
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args)) as.integer(args[[1L]]) else
    as.integer(Sys.getenv("BLOCKR_PORT", "3838"))
})

root <- "."
deps <- c("dockViewR", "blockr.core", "blockr.dplyr", "blockr.ui",
          "blockr.dag", "blockr.dock", "blockr.dm", "blockr.extra",
          "composer", "blockr.viz", "blockr.sandbox",
          "blockr.sandbox/inst/blockr.topline", "blockr.outline")
for (d in deps) {
  pkgload::load_all(
    file.path(root, d),
    helpers = FALSE, attach_testthat = FALSE, export_all = FALSE
  )
}

options(shiny.port = port, shiny.host = "0.0.0.0")
options(blockr.dock_is_locked = FALSE)
options(blockr.background_construction_delay = 0)
options(blockr.tabular_display = blockr.ui::html_table_display)

bms_template <- file.path(root, "blockr.sandbox", "inst", "blockr.bms",
                          "inst", "templates", "bms-template.pptx")

message("Open http://127.0.0.1:", port, "/")

composer_fn <- '
function(data) {
  adsl <- data$adsl
  composer::table(
    title = "Demographic and Baseline Characteristics",
    population = "Safety Analysis Set",
    column_header_left = "Characteristic",
    data = adsl,
    denominator = composer::make_denom(adsl, pop = "SAFFL", trt = "TRT01A"),
    total_col = "overall"
  ) |>
    composer::colgroup(
      composer::by(variable = "TRT01A", levels = sort(unique(adsl$TRT01A)))
    ) |>
    composer::block_continuous(
      label = "Age (years)", variable = "AGE",
      statistic = c("N" = "{N:xxx}", "Mean (SD)" = "{mean:xx.x} ({sd:xx.xx})",
                    "Median" = "{median:xx.x}", "Min, Max" = "{min:xx}, {max:xx}")
    ) |>
    composer::block_categorical(
      label = "Sex n (%)", variable = "SEX",
      statistic = "{n:xx} ({pct:xx.x})"
    ) |>
    composer::block_categorical(
      label = "Race n (%)", variable = "RACE",
      statistic = "{n:xx} ({pct:xx.x})"
    ) |>
    composer::compose() -> tbl
  tbl
}
'

to_df_fn <- "function(data) {\n  blockr.viz::as_annotated_df(data)\n}"

board <- new_dock_board(
  blocks = c(
    data = new_dm_example_block(
      dataset = "pharmaverseadam",
      block_name = "ADaM data (pharmaverseadam)"
    ),
    composer = new_function_block(
      fn = composer_fn,
      block_name = "Composer demographics (composed_table)"
    ),
    to_df = new_function_block(
      fn = to_df_fn,
      block_name = "as_annotated_df() adapter"
    ),
    viz = new_table_block(
      excel_download = TRUE,
      block_name = "Demographics table"
    )
  ),
  links = links(
    new_link(from = "data",     to = "composer"),
    new_link(from = "composer", to = "to_df"),
    new_link(from = "to_df",    to = "viz")
  ),
  stacks = stacks(
    demog = new_dock_stack(c("data", "composer", "to_df", "viz"),
                           name = "Demographics", color = "#003C69")
  ),
  extensions = list(
    blockr.dag::new_dag_extension(),
    blockr.outline::new_outline_extension(
      annotations = list(
        data = list(report = FALSE),
        # BOTH arms are exhibits, and the deck shows them as the SAME table.
        # The composer block returns a raw composed_table and has no table
        # block after it: static_exhibit() coerces and renders it at print
        # time, so the render block is only needed for the interactive
        # dashboard table (search / sort / drill).
        composer = list(
          report = TRUE,
          description = "The composed_table straight out of the function block -- no render block in front of it."
        ),
        to_df = list(report = FALSE),
        viz = list(
          report = TRUE,
          description = "The same Table 1 through the interactive table block. Identical on the slide."
        )
      ),
      stack_annotations = list(
        demog = list(description = "A composer Table 1, rendered to the deck.")
      ),
      title = "Composer table deck",
      template = bms_template
    )
  )
)

serve(board)
