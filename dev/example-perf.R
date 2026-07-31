# blockr.outline PERFORMANCE probe: a small board, fully instrumented.
#
# Five blocks, two of them tables, both in the report:
#
#   data ──> mut ──> tbl_detail          (table 1)
#             └────> summ ──> tbl_summary (table 2)
#
# The question this script exists to answer: when you click around the
# dashboard -- open a block, switch a tab, edit a value -- how much work
# does the outline actually redo?
#
# Every hot stage of the outline pipeline is traced and timed, and each
# call prints one line to the console:
#
#   [otl] outline_sections    #12   n=5    8.4ms   (total 91ms / 12 calls)
#
# Stages, cheapest question first:
#   * outline_sections  the projection (topological order + drag geometry).
#                       The O(n^2) one. Gated on panel visibility, so it
#                       should print NOTHING while the Outline tab is closed.
#   * display_sections  the listed-subset geometry.
#   * outline_code_map  syntax highlighting, active rows only.
#   * outline_tags      the full renderUI redraw. Should be RARE -- a code
#                       edit is supposed to travel as an incremental push.
#   * outline_output_map  Output-mode exhibit evaluation (heavy, on demand).
#   * highlight           downlit, the leaf under the code map.
#
# READ THE HIGHLIGHT NUMBERS WITH CARE. downlit resolves every `pkg::fun`
# in the code against the attached packages, and for a package loaded with
# pkgload::load_all() that lookup finds no installed help index and is
# never cached -- so it re-scans on every call. Measured on
# `blockr.viz::static_table(mut1)`:
#
#   packages load_all()ed (this script)   43.6 ms per highlight
#   packages installed (deployed app)      1.3 ms per highlight
#
# A 30x dev-only tax. So treat the highlight totals here as an upper
# bound: divide by ~30 for what a deployed board pays. The CALL COUNTS,
# though, are real -- they are the same in both environments, and they are
# what this probe is for.
#
# What to watch for:
#   * click a block row in the outline -> outline_tags fires ONCE
#     (activation is a skeleton change), then settles.
#   * click a dock tab / drag a panel -> ideally NOTHING prints.
#   * edit a filter value -> outline_code_map fires, outline_tags does NOT.
#   * close the Outline tab, then poke the board -> nothing prints at all.
#
# Type otl_perf() in the R console at any point for the running totals,
# otl_perf(reset = TRUE) to zero them before a measurement.
#
# Scale knob: BLOCKR_OUTLINE_N=40 lengthens the prep chain to that many
# blocks (the two tables stay at the end), to see where the projection
# starts to hurt.
#
# Run from the workspace root:
#   Rscript blockr.outline/dev/example-perf.R [port]
# Port resolution: argument, then BLOCKR_PORT, then 3838.

port <- local({
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args)) {
    as.integer(args[[1L]])
  } else {
    as.integer(Sys.getenv("BLOCKR_PORT", "3838"))
  }
})

# How many mutate steps sit between the data block and the tables. 1 gives
# the five-block board described above.
n_chain <- as.integer(Sys.getenv("BLOCKR_OUTLINE_N", "1"))

options(shiny.port = port, shiny.host = "0.0.0.0")
options(blockr.dock_is_locked = FALSE)

# Eager construction: every block reports an expression from the first
# paint, so the timings measure steady-state redraws rather than the
# staggered arrival of a deferred board. Set to Inf to probe the deferred
# path instead.
options(blockr.background_construction_delay = 0)

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

# ---------------------------------------------------------------- probe --
# Wrap the outline's hot functions in their own namespace, so the calls
# from inside the extension server go through the timer. Rebinding in the
# namespace (not the export env) is what makes internal callers see it.

otl_stats <- new.env(parent = emptyenv())

otl_perf <- function(reset = FALSE) {
  nms <- sort(ls(otl_stats))
  if (!length(nms)) {
    message("[otl] nothing recorded yet")
    return(invisible(NULL))
  }
  out <- do.call(
    rbind,
    lapply(nms, function(n) {
      s <- get(n, envir = otl_stats)
      data.frame(
        stage = n, calls = s$n,
        total_ms = round(s$total, 1),
        mean_ms = round(s$total / s$n, 2),
        max_ms = round(s$max, 1)
      )
    })
  )
  print(out, row.names = FALSE)
  if (reset) rm(list = nms, envir = otl_stats)
  invisible(out)
}

instrument <- function(pkg, fn, size = function(...) NA_integer_,
                       quiet = FALSE) {
  orig <- get(fn, envir = asNamespace(pkg))
  assign(fn, list(n = 0L, total = 0, max = 0), envir = otl_stats)

  wrapped <- function(...) {
    t0 <- Sys.time()
    on.exit({
      ms <- as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000
      s <- get(fn, envir = otl_stats)
      s$n <- s$n + 1L
      s$total <- s$total + ms
      s$max <- max(s$max, ms)
      assign(fn, s, envir = otl_stats)
      if (!quiet) {
        message(sprintf(
          "[otl] %-18s #%-4d n=%-5s %6.1fms   (total %.0fms / %d calls)",
          fn, s$n, format(tryCatch(size(...), error = function(e) NA)),
          ms, s$total, s$n
        ))
      }
    })
    orig(...)
  }

  # The wrapper must resolve `orig`, `fn`, `size` and `otl_stats` -- keep
  # its own closure, but let it find the package's internals too by leaving
  # the enclosure chained to this script's environment (which sees the
  # loaded namespaces through the search path).
  assignInNamespace(fn, wrapped, ns = pkg)
}

n_expr <- function(expressions, ...) length(expressions)
n_sects <- function(sects, ...) length(sects$ids)

instrument("blockr.outline", "outline_sections", n_expr)
instrument("blockr.outline", "display_sections", n_sects)
instrument("blockr.outline", "outline_code_map", n_sects)
instrument("blockr.outline", "outline_tags", n_sects)
instrument("blockr.outline", "outline_catalog", n_sects)
instrument("blockr.outline", "outline_output_map", n_sects)

# Per-block leaves of the code map: totals only (they fire once per block
# per redraw, so a line each would drown the log). otl_perf() shows where
# outline_code_map's time actually goes.
instrument("blockr.outline", "sect_code_html")
instrument("blockr.outline", "highlight_r_code")
instrument("blockr.outline", "sect_output")
# Down to the third-party leaf: server-side syntax highlighting is where
# the code map's time actually goes, and downlit resolves every identifier
# against the attached packages on each call.
instrument("downlit", "highlight")

# ----------------------------------------------------------------- board --

# The prep chain. One mutate at n_chain = 1 (the five-block board); more
# only to find the knee of the projection curve.
chain_ids <- paste0("mut", seq_len(n_chain))

chain <- lapply(
  seq_len(n_chain),
  function(i) {
    blockr.dplyr::new_mutate_block(
      mutations = list(
        list(name = paste0("ratio", i), expr = "Sepal.Length / Sepal.Width")
      ),
      block_name = paste("Sepal ratio", i)
    )
  }
)
names(chain) <- chain_ids

blocks <- c(
  list(data = new_dataset_block("iris", block_name = "Iris data")),
  chain,
  list(
    summ = blockr.dplyr::new_summarize_block(
      summaries = list(
        list(type = "simple", name = "avg_ratio", func = "mean",
             col = "ratio1")
      ),
      by = list("Species"),
      block_name = "Ratio by species"
    ),
    tbl_detail = blockr.viz::new_table_block(
      block_name = "Flower measurements"
    ),
    tbl_summary = blockr.viz::new_table_block(
      block_name = "Mean ratio by species"
    )
  )
)

last_chain <- chain_ids[[n_chain]]

board <- new_dock_board(
  blocks = do.call(c, blocks),
  links = links(
    from = c("data", head(chain_ids, -1L), last_chain, last_chain, "summ"),
    to = c(chain_ids[[1L]], tail(chain_ids, -1L), "tbl_detail", "summ",
           "tbl_summary")
  ),
  # Both tables plus the outline and the workflow on one view: the point of
  # the probe is what happens while several block panels are open at once.
  views = list(
    Main = c("tbl_detail", "tbl_summary", "outline", "dag")
  ),
  active = "Main",
  extensions = list(
    blockr.dag::new_dag_extension(),
    blockr.outline::new_outline_extension(
      title = "Two-table performance probe",
      annotations = list(
        tbl_detail = list(
          description = paste(
            "Every flower, with the derived sepal ratio.",
            "The first of the two report exhibits."
          ),
          report = TRUE
        ),
        tbl_summary = list(
          description = "Mean sepal ratio per species.",
          report = TRUE
        )
      )
    )
  )
)

message(
  "[otl] board: ", length(blockr.core::board_block_ids(board)),
  " blocks, chain length ", n_chain,
  " -- call otl_perf() for totals"
)

serve(board)
