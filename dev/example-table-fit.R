# SMALLEST TABLE FONT: the board setting that decides whether a table is
# shrunk to fit one slide or carried over several.
#
# blockr.viz has always stepped the font down before it split a table. What
# was missing is a way to say how far it may go -- so every export ran at the
# 11pt default, and a table two rows too tall came back on two slides. The
# floor is now a board option, "Smallest table font"
# (blockr.viz::new_exhibit_font_option()), contributed by any table block, so
# it is in the board settings without an app naming it.
#
# One number, both axes: it is what the paginator shrinks toward before it
# carries rows onto a second slide AND before it deals columns sideways.
#
# What to try:
#
#   * Open the board settings (the cog in the top bar) and find "Smallest
#     table font" under Table options. It starts at 11pt.
#   * In the Slides panel, download the deck as PowerPoint. "Air quality,
#     first 26 days" comes back on TWO slides, and a notification names it
#     and says what it would have taken: "fits at 8pt".
#   * Set the option to 8pt and download again. The same table is now ONE
#     slide, set smaller. Nothing else about it changed.
#   * Press the same table block's own PowerPoint button. It reads the same
#     option, so the block download and the slide are the same table -- that
#     is why the setting is a board option and not a field in the deck panel.
#   * "Air quality, daily" (153 rows) is the honest failure: no legible size
#     holds it, so it is still split and the notification says so rather than
#     shrinking to something nobody can read.
#   * "Readings by day" is a summarize table, which reaches a slide as a
#     painted picture. It has the same ladder now: at 11pt it pages, at 7pt
#     it does not.
#   * "One wide row" is the other half of the fitting story: a single row
#     whose cells are much wider than the rest of their column. A column is
#     sized for its TYPICAL cells now, so that row wraps to a second line
#     instead of inflating every column and pushing the stub to its floor.
#   * The Composer view is the real clinical case: a composer demographics
#     table (17 rows, one column per arm plus the total) that splits at the
#     11pt default and comes back whole at 10pt. One point of type is the
#     whole difference between one slide and two.
#   * Watch the console. Every table that had to be broken prints one line
#     with its page count and the size that would have kept it whole.
#
# Run from the workspace root:
#   Rscript blockr.outline/dev/example-table-fit.R [port]
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

message("Open http://127.0.0.1:", port, "/")

root <- "."
# blockr.sandbox is here for one thing: `as_annotated_df.composed_table()`,
# the method that turns composer's output into the frame every blockr
# renderer and every download reads. Without it the composer block's table
# arrives as a list and the table block has nothing to draw.
deps <- c("dockViewR", "blockr.core", "blockr.dag", "blockr.dock",
          "blockr.extra", "blockr.viz", "blockr.outline", "blockr.sandbox")
for (d in deps) {
  pkgload::load_all(
    file.path(root, d),
    helpers = FALSE, attach_testthat = FALSE, export_all = FALSE
  )
}

# The composer table's code, as a Function block would hold it. It ends on
# the `compose()` result itself -- the whole composed_table, never
# `$formatted_table` -- because that object carries the rendered table AND
# the cards ARD that as_annotated_df() builds the blockr frame from.
demog_fn <- '
function(data) {
  cont <- function(tbl, label, var) {
    composer::block_continuous(
      tbl, label = label, variable = var,
      statistic = c("N" = "{N:xxx}", "Mean (SD)" = "{mean:xx.x} ({sd:xx.xx})",
                    "Median" = "{median:xx.x}", "Min, Max" = "{min:xx}, {max:xx}")
    )
  }
  cat_ <- function(tbl, label, var) {
    composer::block_categorical(tbl, label = label, variable = var,
                                statistic = "{n:xx} ({pct:xx.x})")
  }

  composer::table(
    title = "Demographic and Baseline Characteristics",
    population = "Safety Analysis Set",
    column_header_left = "Characteristic",
    data = data,
    denominator = composer::make_denom(data, pop = "SAFFL", trt = "TRT01A"),
    total_col = "overall"
  ) |>
    composer::colgroup(
      composer::by(variable = "TRT01A", levels = sort(unique(data$TRT01A)))
    ) |>
    cont("Age (years)", "AGE") |>
    cat_("Age group n (%)", "AGEGR1") |>
    cat_("Sex n (%)", "SEX") |>
    cat_("Race n (%)", "RACE") |>
    cat_("Ethnicity n (%)", "ETHNIC") |>
    cat_("Country n (%)", "COUNTRY") |>
    cat_("Treatment arm n (%)", "ARM") |>
    composer::compose()
}
'

# One row unlike the rest of its column: a cell several times wider than
# every other cell in the same column. It used to set the width for all of
# them, since a data cell never wraps, so the columns inflated and the stub
# fell to its floor. Now that cell takes a second line instead.
wide_fn <- '
function(data) {
  rows <- c(sprintf("Measure %d", 1:10), "The wide one")
  out <- data.frame(.label = rows, .indent = 1L, check.names = FALSE)
  set.seed(3)
  for (grp in c("Group A", "Group B", "Group C")) {
    v <- sprintf("%d (%.1f)", sample(10:99, length(rows), TRUE),
                 runif(length(rows), 1, 90))
    v[length(rows)] <- "a cell a good deal wider than the rest of its column"
    out[[grp]] <- v
  }
  attr(out, "label") <- "One wide row"
  out
}
'

board <- new_dock_board(
  blocks = c(
    aq = new_dataset_block("airquality", block_name = "Air quality data"),

    # The interesting one: 26 rows is a couple more than a widescreen slide
    # holds at 13pt, and comfortably inside it at 8pt. The whole setting in
    # one table.
    first26 = new_head_block(n = 26L, block_name = "First 26 days"),
    tbl_fit = blockr.viz::new_table_block(
      block_name = "Air quality, first 26 days",
      title = "Air quality, first 26 days",
      download = TRUE
    ),

    # The one no floor can save: 153 rows do not fit a slide at any size a
    # projector can carry, so it is split and says why.
    tbl_long = blockr.viz::new_table_block(
      block_name = "Air quality, daily",
      title = "Air quality, daily",
      download = TRUE
    ),

    # A painted exhibit rather than a typeset one -- its marks cannot be
    # PowerPoint text runs -- and it now reads the same floor.
    summ = blockr.viz::new_summarize_table_block(
      by = "Day",
      summaries = list(
        list(type = "simple", func = "count", show = "bar", name = "Readings"),
        list(type = "dist", col = "Temp", style = "box",
             inner = "median_q1_q3", name = "Temperature")
      ),
      block_name = "Readings by day",
      download = TRUE
    ),

    # The clinical case the request came from: a demographics table, one
    # column per arm plus the total, 17 rows -- two rows more than a slide
    # holds at 11pt. The table block takes the composed_table straight from
    # the function block and coerces it with as_annotated_df(), so what the
    # dashboard draws, what the block downloads and what the slide shows are
    # one table.
    adsl = new_dataset_block("adsl", package = "pharmaverseadam",
                             block_name = "ADSL (pharmaverseadam)"),
    demog = blockr.extra::new_function_block(
      fn = demog_fn,
      block_name = "Composer demographics"
    ),
    tbl_demog = blockr.viz::new_table_block(
      block_name = "Demographic and Baseline Characteristics",
      title = "Demographic and Baseline Characteristics",
      download = TRUE
    ),

    wide = blockr.extra::new_function_block(
      fn = wide_fn,
      block_name = "One wide row, frame"
    ),
    tbl_wide = blockr.viz::new_table_block(
      block_name = "One wide row",
      title = "One wide row",
      download = TRUE
    )
  ),
  links = links(
    from = c("aq", "first26", "aq", "aq", "adsl", "demog", "adsl",
             "wide"),
    to   = c("first26", "tbl_fit", "tbl_long", "summ", "demog", "tbl_demog",
             "wide", "tbl_wide")
  ),
  views = list(
    Composer = c("tbl_demog", "tbl_wide", "slides", "dag"),
    Main = c("tbl_fit", "slides"),
    More = c("tbl_long", "summ", "slides")
  ),
  active = "Composer",
  extensions = list(
    blockr.dag::new_dag_extension(),
    blockr.outline::new_slides_extension(
      title = "Fit study",
      slides = c("tbl_demog", "tbl_wide", "tbl_fit", "tbl_long",
                 "summ")
    )
  )
)

serve(board)
