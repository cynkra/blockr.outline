# Run the first-workflow board against LOCAL source checkouts (your latest
# uncommitted changes to any blockr package). This is the pkgload::load_all()
# counterpart of the shipped, library()-based inst/examples/first-workflow.R:
# it just flips the loader and sources it, so the two can never drift.
#
# Run from an R session at the workspace root:
#   source("blockr.outline/dev/first-workflow.R")
#
# Or headless, with a port (arg -> BLOCKR_PORT -> 3838):
#   Rscript blockr.outline/dev/first-workflow.R
#   Rscript blockr.outline/dev/first-workflow.R 3840
#
# (End users without the source checkouts run the shipped copy instead:
#   source(system.file("examples/first-workflow.R", package = "blockr.outline")))

port <- local({
  arg <- commandArgs(trailingOnly = TRUE)[1L]
  env <- Sys.getenv("BLOCKR_PORT", unset = "")
  raw <- if (!is.na(arg)) arg else if (nzchar(env)) env else "3838"
  p <- suppressWarnings(as.integer(raw))
  if (is.na(p)) stop("Not a port: ", raw, call. = FALSE)
  p
})

options(shiny.port = port, shiny.host = "0.0.0.0")

cat(sprintf("\nOpen: http://127.0.0.1:%d/\n\n", port))

dev_local <- TRUE
# print.eval = TRUE, or nothing serves under Rscript: the example ends in
# serve(), which RETURNS a shiny.appobj rather than running one, and it is the
# auto-print of that value that launches the app.
source("blockr.outline/inst/examples/first-workflow.R", print.eval = TRUE)
