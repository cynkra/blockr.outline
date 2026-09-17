# Code / document projection of a board, with per-block annotations
# (markdown description + include-in-report flag) supplied by the outline
# extension's own state rather than block attributes. Only exported
# blockr.core API is used: export_code() supplies per-block expressions,
# argument maps and expression types in a valid topological order; links
# and stacks come from the board object.

ann_description <- function(annotations, id) {
  coal(annotations[[id]][["description"]], "")
}

# Default OFF: the outline lists the report, and a board is not a report
# until someone says what belongs in it. A fresh board therefore opens on an
# empty document with the search box, rather than on 80 rows the user then
# has to exclude one by one. Boards annotated before this default flipped
# carry no `report` key for blocks that were included implicitly, so they
# reopen with an empty document as well -- adding blocks back is the search
# box, and the flag is written explicitly from then on.
ann_report <- function(annotations, id) {
  isTRUE(coal(annotations[[id]][["report"]], FALSE))
}

# Same wrapping blockr.core's exporter applies (with()/local(), bquote
# substitution for bquoted expressions), reimplemented on top of the
# exported export_code() payload. Swapping this layer for blockr.code's
# idiomatic per-block pipes is the recorded follow-up; blocks are never
# folded together (one block = one assignment, the outline's anchor).
wrap_block_expr <- function(exprs, args, types) {

  # `args` is NULL for a block with no inputs -- which happens the moment
  # an upstream block is removed. bquote(where = NULL) is defunct, so a
  # removal used to abort the whole projection (and freeze the outline on
  # its last good state). Nothing to substitute in that case anyway.
  # blockr.core::wrap_expr has the same hazard.
  if (identical(types, "bquoted") && length(args)) {
    exprs <- do.call(bquote, list(exprs, args))
  }

  if (length(args) && identical(types, "quoted")) {
    # The expression refers to its inputs by name, so the names have to
    # be bound. Folding this into a pipe is blockr.code's job.
    return(call("with", args, exprs))
  }

  # local() only earns its place around a braced block, where it keeps
  # intermediate variables from leaking. Around a single call it is pure
  # noise -- `data <- local(datasets::iris)` says nothing that
  # `data <- datasets::iris` does not.
  if (is.call(exprs) && identical(exprs[[1L]], as.name("{"))) {
    call("local", exprs)
  } else {
    exprs
  }
}

block_assignment <- function(name, value) {
  bquote(.(nme) <- .(val), list(nme = as.name(name), val = value))
}

# Kahn's algorithm re-linearization with a preference tie-break: among
# ready blocks, the earliest by `preference` wins. Any output is a valid
# topological order -- dependencies always dominate; the preference only
# spends the linearization's slack. The preference is the user-stored
# order merged with a stack-contiguity default for blocks it doesn't
# mention, so a fresh board groups by stack and user drags refine.
preferred_ordering <- function(ids, board, preference = character()) {

  lnks <- blockr.core::board_links(board)

  keep <- lnks$from %in% ids & lnks$to %in% ids
  from <- lnks$from[keep]
  to <- lnks$to[keep]

  stks <- blockr.core::board_stacks(board)
  stack_of <- setNames(rep(NA_character_, length(ids)), ids)

  for (s in names(stks)) {
    stack_of[intersect(ids, blockr.core::stack_blocks(stks[[s]]))] <- s
  }

  indeg <- setNames(integer(length(ids)), ids)

  for (t in to) {
    indeg[[t]] <- indeg[[t]] + 1L
  }

  kahn <- function(pick) {

    deg <- indeg
    out <- character()
    ready <- ids[deg == 0L]

    while (length(ready)) {

      nxt <- pick(ready, out)
      out <- c(out, nxt)
      ready <- setdiff(ready, nxt)

      for (k in to[from == nxt]) {
        deg[[k]] <- deg[[k]] - 1L
        if (deg[[k]] == 0L) {
          ready <- c(ready, k)
        }
      }
    }

    out
  }

  pos <- setNames(seq_along(ids), ids)

  # Pass 1: stack-preferring base order (contiguous chapters by default).
  base <- kahn(function(ready, out) {
    last <- if (length(out)) stack_of[[out[length(out)]]] else NA_character_
    same <- if (!is.na(last)) {
      ready[!is.na(stack_of[ready]) & stack_of[ready] == last]
    }
    if (length(same)) {
      same[which.min(pos[same])]
    } else {
      ready[which.min(pos[ready])]
    }
  })

  # Merge the stored order with the freshly computed base order: ids the
  # user has never sorted (a block just added) must keep their BASE
  # neighbourhood -- appending them to the end of the preference list
  # would rank them last and push every new block to the end of the
  # document, however it is wired.
  pref <- intersect(preference, ids)

  for (id in base) {

    if (id %in% pref) {
      next
    }

    before <- base[seq_len(match(id, base) - 1L)]
    anchor <- rev(intersect(before, pref))

    pref <- if (length(anchor)) {
      append(pref, id, after = match(anchor[[1L]], pref))
    } else {
      c(id, pref)
    }
  }

  prank <- setNames(seq_along(pref), pref)

  # Pass 2: user preference as the tie-break.
  kahn(function(ready, out) {
    ready[which.min(prank[ready])]
  })
}

# Narrow a board onto the block ids that reported an expression this flush.
#
# Rebuilds a PLAIN core board rather than mutating the one we were handed. A
# block whose expression is momentarily NULL (a ggplot block req()s while its
# upstream data settles) has to be dropped, but every container validates the
# dropped id against something it owns: core rejects links naming it, and a
# dock_board additionally rejects view memberships naming it. Both aborts
# landed in the caller's tryCatch, became req(FALSE), and froze the outline on
# its last good projection with nothing left to re-invalidate it -- so editing
# a block updated the block but never the document.
#
# A projection needs blocks, links and stacks and nothing else, so rebuilding
# those three narrowed decouples it from container validation entirely. Stack
# objects are carried over untouched, keeping their dock attributes (name,
# color) intact.
narrow_board <- function(board, known) {

  if (setequal(known, blockr.core::board_block_ids(board))) {
    return(board)
  }

  lnks <- blockr.core::board_links(board)
  stks <- blockr.core::board_stacks(board)

  narrowed <- lapply(
    setNames(nm = names(stks)),
    function(s) {
      stk <- stks[[s]]
      blockr.core::stack_blocks(stk) <- intersect(
        blockr.core::stack_blocks(stk),
        known
      )
      stk
    }
  )

  blockr.core::new_board(
    blocks = blockr.core::board_blocks(board)[known],
    links = lnks[lnks$from %in% known & lnks$to %in% known],
    # An UNSTACKED board narrows to no stacks, and new_board() rejects a
    # NULL there (as_stacks() has no NULL method), which aborted the whole
    # projection -- the very freeze this narrowing exists to prevent.
    stacks = if (length(narrowed)) {
      do.call(blockr.core::stacks, narrowed)
    } else {
      blockr.core::stacks()
    }
  )
}

outline_sections <- function(expressions, board, annotations,
                             preference = character(),
                             stack_annotations = list(),
                             geometry_cache = NULL) {

  # Only project blocks that reported an expression this flush (see the
  # defensive read in the extension server); a block mid-removal or
  # mid-relink is simply absent until it recovers.
  # Read before any subsetting: `[` drops non-standard attributes.
  pending_ids <- coal(attr(expressions, "pending"), character())

  known <- intersect(blockr.core::board_block_ids(board), names(expressions))

  if (!length(known)) {
    stop("no block expressions available")
  }

  board <- narrow_board(board, known)

  expressions <- expressions[known]

  exported <- blockr.core::export_code(expressions, board)

  exprs <- do.call(Map, c(list(wrap_block_expr), exported))

  ord <- preferred_ordering(names(exprs), board, preference)
  exprs <- exprs[ord]

  ids <- names(exprs)
  exprs <- Map(block_assignment, ids, exprs)

  blks <- blockr.core::board_blocks(board)[ids]

  stks <- blockr.core::board_stacks(board)
  stack_ids <- rep(NA_character_, length(ids))
  stack_names <- rep(NA_character_, length(ids))
  stack_colors <- character()

  for (stk_id in names(stks)) {
    hit <- ids %in% blockr.core::stack_blocks(stks[[stk_id]])
    stack_ids[hit] <- stk_id
    stack_names[hit] <- blockr.core::stack_name(stks[[stk_id]])
    stack_colors[stk_id] <- coal(
      tryCatch(
        blockr.dock::stack_color(stks[[stk_id]]),
        error = function(e) NULL
      ),
      "#2563eb"
    )
  }

  report <- lgl_ply(ids, function(i) ann_report(annotations, i))

  lnks <- blockr.core::board_links(board)
  keep <- lnks$from %in% ids & lnks$to %in% ids
  lnks <- lnks[keep, ]

  # The graph geometry (per-block reachability sweeps) is the projection's
  # one super-linear step -- ~300ms at 80 blocks against ~30ms for all of
  # the rest -- and it depends on nothing but the display order, the
  # links, the stack layout and the report flags. Those change on
  # structural edits (add / remove / relink / drag / stack / report
  # toggles), NOT when a block's expression updates -- which is what
  # invalidates the projection on every value edit. Memoise on exactly
  # those inputs: an expression-only change reuses the cached geometry
  # and pays only the cheap content phase. `geometry_cache` is an
  # environment owned by the caller (one per extension server); NULL
  # (the exporters, tests) computes fresh.
  geo_key <- list(ids = ids, links = lnks, stacks = stack_ids,
                  report = report)

  geo <- if (!is.null(geometry_cache) &&
               identical(geometry_cache$key, geo_key)) {
    geometry_cache$value
  } else {
    g <- outline_geometry(ids, lnks, stack_ids, report)
    if (!is.null(geometry_cache)) {
      geometry_cache$key <- geo_key
      geometry_cache$value <- g
    }
    g
  }

  list(
    ids = ids,
    pending = ids %in% pending_ids,
    movable = geo$movable,
    drop_lo = geo$drop_lo,
    drop_hi = geo$drop_hi,
    chap_targets = geo$chap_targets,
    code = chr_ply(
      lapply(exprs, deparse),
      paste0,
      collapse = "\n"
    ),
    names = chr_ply(blks, blockr.core::block_name),
    icons = chr_ply(seq_along(blks), function(i) block_icon_html(blks[[i]])),
    descriptions = chr_ply(ids, function(i) ann_description(annotations, i)),
    report = report,
    exported = geo$exported,
    kinds = chr_ply(blks, block_exhibit_kind),
    renderers = chr_ply(blks, block_report_renderer),
    report_calls = chr_ply(
      seq_along(blks),
      function(i) block_report_call_str(blks[[i]], ids[[i]])
    ),
    stack_ids = stack_ids,
    stack_names = stack_names,
    stack_colors = stack_colors,
    stack_descriptions = vapply(
      setNames(nm = names(stks)),
      function(s) ann_description(stack_annotations, s),
      character(1L)
    )
  )
}

# The reachability-derived half of the projection: drag affordances, drag
# ranges, chapter landing targets and the export closure. Pure in its
# arguments -- outline_sections() memoises it on exactly those (see the
# geometry_cache there). `lnks` arrives already restricted to `universe`.
#
# `universe` covers the case where `ids` is a displayed SUBSEQUENCE of the
# document (the report-only outline): reachability must walk the full
# graph, or a dependency running through a hidden block would vanish and
# the drag legality would allow orders the DAG forbids. The edge map is
# therefore built over the universe; the per-position sweeps stay over
# `ids`, the rows actually shown.
# "Is there a path from a to b" over the board's link table, as a closure so
# the successor map is built once per projection rather than per query.
dag_reaches <- function(lnks, universe) {

  kids <- split(lnks$to, factor(lnks$from, levels = universe))

  function(a, b) {
    seen <- character()
    todo <- a
    while (length(todo)) {
      cur <- todo[[1L]]
      todo <- todo[-1L]
      if (cur %in% seen) next
      seen <- c(seen, cur)
      nxt <- kids[[cur]]
      if (b %in% nxt) return(TRUE)
      todo <- c(todo, nxt)
    }
    FALSE
  }
}

# The document's evaluation closure: a block belongs to the export iff it is
# reported or some reported block depends on it. Everything else is dropped
# from the DOCUMENT entirely (not include=FALSE, whose code still runs at
# render) -- on a many-view board the independent branches would otherwise all
# evaluate for a report that shows none of them.
#
# The one piece of graph geometry a projection cannot do without: pick a table
# and the deck still has to run the blocks upstream of it. The outline's drag
# affordances are the expensive part and are computed alongside (see
# outline_geometry); the slide builder takes only this.
export_closure <- function(ids, report, reaches) {

  reported <- ids[report]

  report | lgl_ply(
    ids,
    function(a) any(lgl_ply(reported, function(b) reaches(a, b)))
  )
}

outline_geometry <- function(ids, lnks, stack_ids, report, universe = ids) {

  reaches <- dag_reaches(lnks, universe)

  # A block is reorderable iff it has slack in the DAG: it may pass its
  # displayed predecessor (which must then not be an ancestor) or its
  # successor (which must then not be a descendant). Fully pinned blocks
  # get no drag affordance -- no valid order could move them anyway.
  movable <- lgl_ply(seq_along(ids), function(i) {
    (i > 1L && !reaches(ids[i - 1L], ids[i])) ||
      (i < length(ids) && !reaches(ids[i], ids[i + 1L]))
  })

  # The evaluation closure (see export_closure). This is also what the
  # outline LISTS: one row per chunk, the reported ones with output and the
  # rest as `include: false`.
  exported <- export_closure(ids, report, reaches)

  # Legal landing range for a drag, as gap indices over the list without
  # the dragged block: it must land after its last ancestor and before its
  # first descendant. Everything in between is a valid document order.
  n <- length(ids)
  drop_lo <- integer(n)
  drop_hi <- integer(n)

  for (i in seq_len(n)) {

    rest <- ids[-i]
    anc <- which(lgl_ply(rest, reaches, ids[i]))
    des <- which(lgl_ply(
      seq_along(rest),
      function(k) reaches(ids[i], rest[k])
    ))

    drop_lo[i] <- if (length(anc)) max(anc) else 0L
    drop_hi[i] <- if (length(des)) min(des) - 1L else length(rest)
  }

  # Chapter-level slack: the same computation one level up. A run may be
  # placed before another run only if no block in it depends on a block it
  # would jump, and vice versa. Targets are expressed as the anchor block
  # of the run to precede, or "__end__" for the document end.
  run_rle <- rle(ifelse(is.na(stack_ids), "", stack_ids))
  run_starts <- cumsum(c(1L, head(run_rle$lengths, -1L)))

  chap_targets <- lapply(seq_along(run_rle$values), function(r) {

    if (!nzchar(run_rle$values[r])) {
      return(character())
    }

    idx <- seq(run_starts[r], length.out = run_rle$lengths[r])
    unit <- ids[idx]
    rest <- setdiff(ids, unit)

    anc <- vapply(
      rest,
      function(b) any(vapply(unit, function(u) reaches(b, u), logical(1L))),
      logical(1L)
    )
    des <- vapply(
      rest,
      function(b) any(vapply(unit, function(u) reaches(u, b), logical(1L))),
      logical(1L)
    )

    lo <- if (any(anc)) max(which(anc)) else 0L
    hi <- if (any(des)) min(which(des)) - 1L else length(rest)

    # Boundaries of the remaining runs, as gap indices over `rest`.
    rest_stacks <- stack_ids[match(rest, ids)]
    rest_rle <- rle(ifelse(is.na(rest_stacks), "", rest_stacks))
    rest_starts <- cumsum(c(1L, head(rest_rle$lengths, -1L)))

    out <- character()

    for (k in seq_along(rest_rle$values)) {
      gap <- rest_starts[k] - 1L
      if (gap >= lo && gap <= hi && gap != run_starts[r] - 1L) {
        out <- c(out, rest[rest_starts[k]])
      }
    }

    if (length(rest) >= lo && length(rest) <= hi &&
          length(rest) != run_starts[r] - 1L) {
      out <- c(out, "__end__")
    }

    out
  })

  list(
    movable = movable,
    exported = exported,
    drop_lo = drop_lo,
    drop_hi = drop_hi,
    chap_targets = chap_targets
  )
}

# Exhibit kind from the block's CLASS, not its result: results are gated by
# evaluation and visibility, so a runtime probe reads NULL for most blocks and
# the caption would appear only sometimes. The class is always there. A block
# that is neither plot nor data gets no prefix -- a wrong fig- would leave a
# broken cross-reference. The registry category is the ecosystem-wide answer:
# every package declares it, so a ggplot_block (class ggplot_block, not
# plot_block) still reports "plot". Class inheritance only covers blockr.core's
# own hierarchy.
block_exhibit_kind <- function(b) {

  cat <- tryCatch(
    blockr.core::block_meta_category(b),
    error = function(e) character()
  )

  if (any(cat %in% c("plot", "visualization")) || inherits(b, "plot_block")) {
    "fig"
  } else if (any(cat %in% c("data", "transform", "table")) ||
               inherits(b, c("data_block", "transform_block"))) {
    "tbl"
  } else {
    ""
  }
}

# The report renderer wrapped around a block's printed result ("" = bare
# print). The default IS the bare print: a data frame renders through the
# document's df-print:kable, a ggplot prints itself, and the emitted script
# stays the canonical R an analyst would have written, with no blockr
# package on its search path (see _inbox/2026-07-31-report-code-emits-
# expressions-not-renderer-calls.md -- reproducible, reviewable, editable).
#
# The wrap is the exception, reserved for the display-table blocks
# (table_block, summary_table_block, registry category "table") whose result
# is a bare annotated data frame: the styled table lives in the block's
# Shiny UI, so a bare print degrades to dot-columns and all. For those,
# blockr.viz::static_exhibit() restores the styled table -- flextable is the
# one engine whose knit_print emits real OpenXML tables in pptx and docx --
# and decides at RENDER time from the value, so wrapping a value that turns
# out printable can never be worse than the bare print. Figures skip the
# wrap outright.
#
# options(blockr.outline.report_renderer = "static") restores the previous
# blanket wrap (every non-figure block), for deployments whose documents
# leaned on static_exhibit's annotated-df coercion of plain results.
#
# `style` is that option's value, passed in rather than read here, because a
# DECK wants the blanket wrap unconditionally (see slide_sections): nobody
# reads a deck's qmd, so the argument for a bare variable -- the emitted
# script has to be canonical R -- does not apply to it, while the argument
# for the wrap does. The blocks that made this matter are the function and
# code blocks: their registry category is "transform", so the display-table
# test below is FALSE for them, yet their result is routinely a composer
# table. A bare print of one is not a table.
#
# Resolved defensively (same pattern as block_report_call_str): blockr.viz need
# not be installed to project the sections, and a blockr.viz older than 0.2.38
# has no static_exhibit(), so the previous class-gated static_table() wrap
# stands in. The emitted call self-qualifies; the render session loads
# blockr.viz anyway (the block's own code calls it).
block_report_renderer <- function(blk,
                                  style = getOption(
                                    "blockr.outline.report_renderer",
                                    "auto"
                                  )) {

  if (identical(block_exhibit_kind(blk), "fig")) {
    return("")
  }

  if (!requireNamespace("blockr.viz", quietly = TRUE)) {
    return("")
  }

  if (!identical(style, "static")) {

    cat <- tryCatch(
      blockr.core::block_meta_category(blk),
      error = function(e) character()
    )

    display_table <- inherits(blk, c("table_block", "summary_table_block")) ||
      any(cat %in% "table")

    if (!display_table) {
      return("")
    }
  }

  has_exhibit <- is.function(
    tryCatch(
      getExportedValue("blockr.viz", "static_exhibit"),
      error = function(e) NULL
    )
  )

  if (has_exhibit) {
    return("blockr.viz::static_exhibit")
  }

  if (inherits(blk, c("table_block", "summary_table_block"))) {
    return("blockr.viz::static_table")
  }

  ""
}

# A block-supplied report call, deparsed for the document. blockr.viz's
# report_call() generic lets a block state how its result prints -- the
# chart block emits blockr.viz::static_chart(<var>, <state...>), rebuilding the
# canvas chart as a ggplot. Resolved defensively (same pattern as
# block_icon_html): without blockr.viz, or for a block with no method, the
# simpler renderer paths below apply.
block_report_call_str <- function(blk, var) {

  if (!requireNamespace("blockr.viz", quietly = TRUE)) {
    return("")
  }

  fn <- tryCatch(
    getExportedValue("blockr.viz", "report_call"),
    error = function(e) NULL
  )

  if (!is.function(fn)) {
    return("")
  }

  cl <- tryCatch(fn(blk, var), error = function(e) NULL)

  if (is.null(cl)) {
    return("")
  }

  # blockr.viz >= 0.2.36 compiles chart state to a plain dplyr + ggplot2
  # pipeline and ships chart_code(), which formats any report call one
  # pipeline stage / layer per line (nested data-threading rendered in pipe
  # form). Older blockr.viz: plain deparse, as before.
  #
  # chart_code() is written for chart pipelines. Other report calls go
  # through it too, and it can get them wrong: the composer block's call is
  # a `{ ... }` body, which it rewrote as `.d |> as.data.frame(x) <- NULL |>
  # { ... }`, a parse error, so every composer table left the deck without a
  # slide. Its text is used only if it parses back to the same call.
  fmt <- tryCatch(
    getExportedValue("blockr.viz", "chart_code"),
    error = function(e) NULL
  )

  if (is.function(fmt)) {
    out <- tryCatch(fmt(cl), error = function(e) NULL)
    if (is.character(out) && length(out) == 1L && nzchar(out) &&
        same_call_text(out, cl)) {
      return(out)
    }
  }

  paste(deparse(cl), collapse = "\n")
}

# TRUE when `txt` parses to exactly the call `cl`. The native pipe is resolved
# at parse time, so a correct pipe-form rendering compares equal.
same_call_text <- function(txt, cl) {
  parsed <- tryCatch(
    parse(text = txt, keep.source = FALSE),
    error = function(e) NULL
  )
  length(parsed) == 1L &&
    identical(
      paste(deparse(parsed[[1L]]), collapse = "\n"),
      paste(deparse(cl), collapse = "\n")
    )
}

# The output line of a reported chunk: the picture the browser already drew
# when there is one, else the block's own report call, else the result
# variable wrapped in the block's report renderer.
#
# A chart the canvas has drawn goes into the document AS THAT PICTURE. The
# alternative is emitting code that redraws it through a second renderer,
# which is how a report came to disagree with the screen it was made from.
# The block's own code still runs above it, because downstream blocks read
# the result; only the figure is substituted.
sect_output <- function(sects, i) {

  cap <- sects$captures[[sects$ids[i]]]

  if (is.character(cap) && length(cap) == 1L && file.exists(cap)) {
    return(paste0("knitr::include_graphics(", deparse(cap), ")"))
  }

  rc <- coal(sects$report_calls[i], "")
  if (nzchar(rc)) {
    return(rc)
  }
  rndr <- coal(sects$renderers[i], "")
  if (nzchar(rndr)) {
    paste0(rndr, "(", sects$ids[i], ")")
  } else {
    sects$ids[i]
  }
}

# The registry icon exactly as the dock's block card shows it. The two
# helpers are blockr.dock internals (recorded follow-up: export them);
# resolved dynamically with a letter-tile fallback so a dock without them
# degrades instead of breaking.
# Memoised per class: the icon is registry metadata resolved from the
# block's class, stable within a process, and the double getFromNamespace +
# data-URI build was half of the projection's warm cost at 80 blocks
# (it ran per block per projection).
icon_html_cache <- new.env(parent = emptyenv())

block_icon_html <- function(blk) {

  key <- paste(class(blk), collapse = "|")
  hit <- icon_html_cache[[key]]

  if (!is.null(hit)) {
    return(hit)
  }

  val <- tryCatch(
    {
      meta <- utils::getFromNamespace("blks_metadata", "blockr.dock")(blk)
      uri <- utils::getFromNamespace("blk_icon_data_uri", "blockr.dock")(
        meta$icon, meta$color,
        mode = "inline"
      )
      as.character(uri)
    },
    error = function(e) NA_character_
  )

  icon_html_cache[[key]] <- val

  val
}

na_blank <- function(x) {
  if (length(x) != 1L || is.na(x)) "" else as.character(x)
}

# A markdown description as one plain-text line: what the condensed
# dormant row shows, and what the search menu shows under a block's name.
desc_oneline <- function(x) {

  if (!length(x) || !nzchar(x)) {
    return("")
  }

  gsub("\\s+", " ", trimws(commonmark::markdown_text(x, extensions = TRUE)))
}

# Icons, shared by the block class that drew them.
#
# `block_icon_html()` is an inline SVG -- 500 to 1400 bytes -- and it is a
# property of the block's CLASS, so a board of eighty blocks over a dozen
# types shipped the same dozen pictures eighty times. Both search catalogues
# send this table once and reference it per entry, which is most of their
# payload: 54KB down to 16KB at eighty blocks. Keys are positional, so they
# are stable for a given catalogue and mean nothing outside it.
#
# @param html Character vector of icon markup, `NA` where a block has none.
# @return `list(keys, icons)` -- one key per input (empty string for none)
#   and the distinct markup, named by key.
icon_key_table <- function(html) {

  html <- ifelse(is.na(html), "", as.character(html))
  uniq <- unique(html[nzchar(html)])

  # A board whose blocks all lack icons has nothing to key. Guarded rather
  # than left to fall through: `paste0("i", integer(0))` is "i", not
  # character(0), so the table would come back with one phantom entry.
  if (!length(uniq)) {
    return(list(keys = rep("", length(html)), icons = list()))
  }

  lookup <- stats::setNames(paste0("i", seq_along(uniq)), uniq)
  keys <- ifelse(nzchar(html), unname(lookup[html]), "")

  list(
    keys = keys,
    icons = stats::setNames(as.list(uniq), unname(lookup))
  )
}

# The search catalogue: one entry per board block, the listed ones first
# and each group in document order. The search box is a single control over
# the whole board -- a listed block is a "go to", an unlisted one an "add"
# -- so it needs the document and the pool in one payload. `runs` marks a
# block the document runs without showing: an ancestor of a reported block,
# listed as an `#| include: false` row. Switching it on only makes its
# output visible, it was going to be evaluated either way.
#
# Entries carry an icon KEY into the `icons` table beside them (see
# icon_key_table), never the markup itself.
outline_catalog <- function(sects, listed) {

  is_listed <- sects$ids %in% listed
  ord <- c(which(is_listed), which(!is_listed))

  tbl <- icon_key_table(sects$icons)

  items <- lapply(
    ord,
    function(i) {
      # `[[` throughout: names / icons / descriptions are NAMED vectors, and
      # a named element serialises as a JSON object, not a string.
      list(
        id = sects$ids[[i]],
        name = sects$names[[i]],
        icon_key = tbl$keys[[i]],
        chapter = na_blank(sects$stack_names[[i]]),
        desc = desc_oneline(sects$descriptions[[i]]),
        listed = is_listed[[i]],
        runs = isTRUE(sects$exported[[i]]) && !isTRUE(sects$report[[i]])
      )
    }
  )

  list(items = items, icons = tbl$icons)
}

# Narrow a sections projection to the ids the outline LISTS (the export
# closure, unless show-all). Like prune_sections(), but for display: the
# drag-geometry fields are recomputed on the visible subsequence rather
# than dropped, with reachability over the FULL document (`universe`) so a
# dependency running through a hidden block still pins the order. `lnks`
# is the board's link table; `cache` memoises the recomputed geometry the
# same way outline_sections() does its own.
display_sections <- function(sects, listed, lnks, cache = NULL) {

  keep <- sects$ids %in% listed

  if (all(keep)) {
    return(sects)
  }

  ids <- sects$ids[keep]

  lnks <- lnks[lnks$from %in% sects$ids & lnks$to %in% sects$ids, ]

  per_block <- c(
    "ids", "pending", "code", "names", "icons", "descriptions", "report",
    "exported", "kinds", "renderers", "report_calls", "stack_ids",
    "stack_names"
  )

  out <- sects
  for (fld in intersect(per_block, names(out))) {
    out[[fld]] <- out[[fld]][keep]
  }

  geo_key <- list(ids = ids, links = lnks, stacks = out$stack_ids,
                  report = out$report, universe = sects$ids)

  geo <- if (!is.null(cache) && identical(cache$key, geo_key)) {
    cache$value
  } else {
    g <- outline_geometry(ids, lnks, out$stack_ids, out$report,
                          universe = sects$ids)
    if (!is.null(cache)) {
      cache$key <- geo_key
      cache$value <- g
    }
    g
  }

  out$movable <- geo$movable
  out$drop_lo <- geo$drop_lo
  out$drop_hi <- geo$drop_hi
  out$chap_targets <- geo$chap_targets

  out
}

# Narrow a sections projection onto its export closure (see `exported` in
# outline_sections): the per-block vectors are subset in place, the
# stack-level maps stay whole (chapter emission only reads the stacks that
# survive in stack_ids). The drag-geometry fields (movable, drop_lo,
# drop_hi, chap_targets) index into the FULL projection and would be
# meaningless after subsetting; the exporters never read them, so they are
# dropped rather than recomputed.
prune_sections <- function(sects) {

  keep <- sects$exported

  if (is.null(keep) || all(keep)) {
    return(sects)
  }

  per_block <- c(
    "ids", "pending", "code", "names", "icons", "descriptions", "report",
    "exported", "kinds", "renderers", "report_calls", "stack_ids",
    "stack_names"
  )

  for (fld in intersect(per_block, names(sects))) {
    sects[[fld]] <- sects[[fld]][keep]
  }

  sects[c("movable", "drop_lo", "drop_hi", "chap_targets")] <- NULL

  sects
}

# The code cell of one exported section. A pending block (constructed but
# not yet reporting an expression, or not constructed at all on a deferred
# board) holds a placeholder expression that must never reach a document as
# code -- `id <- invisible(NULL)` would silently poison every dependent.
# It becomes an honest comment instead; the render path waits for pending
# blocks to resolve before running quarto (see the download flow in ext.R).
sect_export_code <- function(sects, i) {
  if (isTRUE(sects$pending[i])) {
    paste0(
      "# ", sects$ids[i], ": waiting for R code to be generated"
    )
  } else {
    sects$code[i]
  }
}

# Chapter headings: one per contiguous run of a stack, emitted only when
# the run holds at least one report-included block; repeat runs read
# "(continued)".
section_chapters <- function(sects) {

  runs <- rle(ifelse(is.na(sects$stack_ids), "", sects$stack_ids))
  starts <- cumsum(c(1L, head(runs$lengths, -1L)))

  out <- rep(NA_character_, length(sects$ids))
  seen <- character()

  for (i in seq_along(runs$values)) {

    if (!nzchar(runs$values[i])) {
      next
    }

    idx <- seq(starts[i], length.out = runs$lengths[i])

    if (any(sects$report[idx])) {
      # Anchor the heading on the first REPORTED block of the run, not
      # blindly on idx[1L]. A stack whose first block is excluded from
      # the report (report = FALSE) would otherwise pin the heading to a
      # section whose prose never renders, and the chapter title would
      # silently vanish from the output.
      anchor <- idx[which(sects$report[idx])[1L]]
      nme <- sects$stack_names[anchor]
      out[anchor] <- if (runs$values[i] %in% seen) {
        paste(nme, "(continued)")
      } else {
        nme
      }
      seen <- c(seen, runs$values[i])
    }
  }

  out
}

# Chapter intro: the stack's own description, emitted under the first
# (non-continued) heading of that stack.
chapter_intro <- function(sects, chapters, i) {

  if (is.na(chapters[i]) || grepl("\\(continued\\)$", chapters[i])) {
    return(character())
  }

  desc <- coal(sects$stack_descriptions[[sects$stack_ids[i]]], "")

  if (!nzchar(desc)) {
    return(character())
  }

  desc
}

# ---- report items: flags, settings, text -------------------------------
#
# The report extension's document model rides into the emitters as three
# optional arguments; all default NULL, and NULL reproduces the pre-item
# behaviour exactly, so the outline's and the deck's emissions are
# byte-stable.
#
#   flags     named list keyed by block id: per-item chunk visibility --
#             `code` / `output` (quarto's echo / output pair, both
#             independent) plus figure overrides (`fig_width`,
#             `fig_height`, `full_width`). A block absent from `flags`
#             was never added to the document and gets the legacy
#             `include: false` chunk.
#   items     the ordered item list (block and text entries): text items
#             are woven between the chunks, anchored to the block that
#             follows them (see weave_text_items).
#   settings  the document-wide settings list (report_settings_default());
#             emits the YAML header, embed-resources included -- the qmd
#             the panel shows must be sufficient for `quarto render`
#             outside the app, with nothing injected at render time.

section_shown <- function(flags, sects, i) {
  if (is.null(flags)) {
    sects$report[i]
  } else {
    isTRUE(flags[[sects$ids[i]]]$output)
  }
}

# The 2x2 of the two switches. Everything still evaluates -- `eval` is
# never emitted -- these only decide what the reader sees.
chunk_vis_qmd <- function(flags, sects, i) {
  if (is.null(flags)) {
    return(if (!sects$report[i]) "#| include: false")
  }
  f <- flags[[sects$ids[i]]]
  out <- isTRUE(f$output)
  code <- isTRUE(f$code)
  if (out && code) {
    return(character())
  }
  if (out) {
    return("#| echo: false")
  }
  if (code) {
    return("#| output: false")
  }
  "#| include: false"
}

chunk_fig_qmd <- function(flags, sects, i) {
  f <- if (!is.null(flags)) flags[[sects$ids[i]]]
  if (is.null(f)) {
    return(character())
  }
  c(
    if (is.numeric(f$fig_width)) paste0("#| fig-width: ", f$fig_width),
    if (is.numeric(f$fig_height)) paste0("#| fig-height: ", f$fig_height),
    if (isTRUE(f$full_width)) "#| column: page"
  )
}

# Spin mirrors: knitr chunk options on the `#+` line. `output: false` has
# no single spin equivalent, so it becomes the results/fig.show pair.
chunk_vis_spin <- function(flags, sects, i) {
  if (is.null(flags)) {
    return(if (!sects$report[i]) ", include=FALSE" else "")
  }
  f <- flags[[sects$ids[i]]]
  out <- isTRUE(f$output)
  code <- isTRUE(f$code)
  if (out && code) {
    return("")
  }
  if (out) {
    return(", echo=FALSE")
  }
  if (code) {
    return(", results=\"hide\", fig.show=\"hide\"")
  }
  ", include=FALSE"
}

chunk_fig_spin <- function(flags, sects, i) {
  f <- if (!is.null(flags)) flags[[sects$ids[i]]]
  if (is.null(f)) {
    return("")
  }
  paste0(
    c(
      if (is.numeric(f$fig_width)) paste0(", fig.width=", f$fig_width),
      if (is.numeric(f$fig_height)) paste0(", fig.height=", f$fig_height)
    ),
    collapse = ""
  )
}

# One chunk label for both emitters, so `#| label:` in the qmd and the
# `#+` name in the script are the same string.
#
# The label is deliberately NOT prefixed with the kind. A `tbl-`/`fig-`
# label makes quarto treat the output as a cross-reference FLOAT, and
# pandoc's pptx path cannot render a flextable inside that float -- the
# table silently vanishes from the slide. Dropping the prefix keeps one
# document that renders identically to html, pdf AND pptx (flextables
# included); the cost is losing @tbl-/@fig- cross-references, which slides
# do not use and reports rarely do.
chunk_label <- function(id) {
  gsub("[^a-zA-Z0-9_-]", "-", id)
}

# A block that is in the report shows its output, so it IS an exhibit: its
# title becomes the CAPTION rather than a heading -- stacks head sections,
# blocks are exhibits. The `kind` (fig/tbl) picks the caption key so the
# caption sits in the right place.
#
# Emitted as a `#|` option, which is why both emitters can share it: knitr
# reads those comments at the top of a chunk body in a spin script exactly
# as quarto reads them in a qmd.
chunk_cap <- function(block_level, sects, i, shown) {
  kind <- sects$kinds[i]
  if (!shown || !identical(block_level, "caption") || !nzchar(kind)) {
    return(character())
  }
  paste0("#| ", kind, "-cap: \"", gsub("\"", "'", sects$names[i]), "\"")
}

# `column: page` is a quarto cell option with no knitr spelling, so it
# cannot ride on the `#+` line. It rides in the chunk BODY instead: knitr
# reads `#|` option comments at the top of a chunk and hands what it does
# not recognise to quarto, which is what makes the layout survive
# `quarto render report.R`. Under rmarkdown it is an unknown option and is
# ignored, which is the same thing that happens to `column: page` there.
chunk_col_spin <- function(flags, sects, i) {
  f <- if (!is.null(flags)) flags[[sects$ids[i]]]
  if (isTRUE(f$full_width)) "#| column: page" else character()
}

# The YAML header from the settings list. Options are emitted only when
# they differ from quarto's own defaults (a toc line saying `false` is
# noise); fig size and embed-resources always (both differ), and the
# execute block whenever warnings are suppressed -- which is the report's
# default, and most of what makes a generated document read as written.
report_yaml <- function(title, s) {
  c(
    "---",
    paste0("title: \"", yaml_dq(title), "\""),
    if (isTRUE(s$toc)) "toc: true",
    if (isTRUE(s$number_sections)) "number-sections: true",
    paste0("fig-width: ", s$fig_width),
    paste0("fig-height: ", s$fig_height),
    "df-print: kable",
    report_format_yaml(s),
    if (!isTRUE(s$warnings)) {
      c("execute:", "  warning: false", "  message: false")
    },
    "---"
  )
}

# The format block, from the setting rather than from here. It used to be
# three hardcoded lines saying html, which made `format` the one YAML key
# the gear did not own -- and made the qmd on screen a document that could
# not be the one the reader asked for.
#
# The sub-keys are the format's, not the document's: `embed-resources`
# answers "a downloaded html report must be ONE file", and quarto's
# `code-fold` is html-only. Nesting either under `pdf:` writes YAML the
# renderer steps over, so a format with nothing to say gets the flat
# `format: pdf` form instead of an empty mapping (which is a YAML error,
# not a no-op).
report_format_yaml <- function(s) {

  fmt <- if (is.character(s$format) && length(s$format)) s$format else "html"

  sub <- if (identical(fmt, "html")) {
    c(
      if (isTRUE(s$embed_resources)) "    embed-resources: true",
      if (isTRUE(s$code_fold)) "    code-fold: true"
    )
  }

  if (!length(sub)) {
    return(paste0("format: ", fmt))
  }

  c("format:", paste0("  ", fmt, ":"), sub)
}

# Weave the item list's text entries between the emitted chunks. Document
# order must be a valid evaluation order, so the BLOCK order is the snapped
# one (`sects$ids`); text placement is then derived, not ordered: a text
# item anchors to the nearest FOLLOWING block item in the list (the
# trailing run to the preceding one), and is re-attached around its anchor
# in the snapped order. A drag that inverts a dependency therefore moves
# the paragraph WITH its exhibit. Anchors are computed here, never stored:
# a stored anchor can dangle, a derived one cannot. Text whose anchor block
# is not in the emission (or an all-text document) lands after the header.
weave_text_items <- function(pieces, ids, items, spin = FALSE) {

  if (is.null(items)) {
    return(list(pieces = pieces, ids = ids))
  }

  blk_at <- which(chr_ply(items, function(x) coal(x$block, "")) != "")

  before <- setNames(vector("list", length(ids)), ids)
  after <- setNames(vector("list", length(ids)), ids)
  top <- character()

  for (t in seq_along(items)) {
    txt <- items[[t]]$text
    if (is.null(txt) || !nzchar(trimws(coal(txt, "")))) {
      next
    }
    nxt <- blk_at[blk_at > t]
    prv <- blk_at[blk_at < t]
    if (length(nxt) && items[[nxt[[1L]]]]$block %in% ids) {
      id <- items[[nxt[[1L]]]]$block
      before[[id]] <- c(before[[id]], txt)
    } else if (length(prv) && items[[prv[[length(prv)]]]]$block %in% ids) {
      id <- items[[prv[[length(prv)]]]]$block
      after[[id]] <- c(after[[id]], txt)
    } else {
      top <- c(top, txt)
    }
  }

  # knitr::spin only turns `#'` lines into markdown; the plain blank line
  # between two of them is R, and leaves no blank line behind in the
  # markdown. So a text item sitting directly above a block's heading used
  # to run into it -- "The first rows. ## Head", one paragraph, no section.
  # A trailing empty `#'` is the paragraph break that survives spin, and it
  # is what makes the script and the qmd render the same document.
  fmt <- function(x) {
    if (spin) {
      paste0(c(paste0("#' ", strsplit(x, "\n")[[1L]]), "#' "), collapse = "\n")
    } else {
      x
    }
  }

  out <- new.env(parent = emptyenv())
  out$p <- character()
  out$i <- character()
  put <- function(p, i) {
    out$p <- c(out$p, p)
    out$i <- c(out$i, i)
  }

  for (txt in top) {
    put(fmt(txt), NA_character_)
  }
  for (k in seq_along(ids)) {
    for (txt in before[[ids[[k]]]]) {
      put(fmt(txt), NA_character_)
    }
    put(pieces[[k]], ids[[k]])
    for (txt in after[[ids[[k]]]]) {
      put(fmt(txt), NA_character_)
    }
  }

  list(pieces = out$p, ids = out$i)
}

# `title` / `intro` head the script as spin prose (spin has no YAML, so the
# title is a `#' #` heading). Both default off; the outline never sets them.
# `collapse = FALSE` returns the per-section pieces instead of one string,
# with attr(, "ids") naming each piece's block (NA for the header piece) --
# the report extension's gutter view consumes that, while every download
# path collapses, so the code on screen and the file written are one
# emission and cannot drift.
export_spin <- function(sects, stack_level = "#", block_level = "caption",
                        title = NULL, intro = "", collapse = TRUE,
                        flags = NULL, items = NULL, settings = NULL) {

  sects <- prune_sections(sects)

  chapters <- section_chapters(sects)

  stack_hd <- if (stack_level %in% c("#", "##")) stack_level
  block_hd <- if (block_level %in% c("#", "##", "###")) block_level

  one_section <- function(i) {

    shown <- section_shown(flags, sects, i)

    prose <- if (shown) {
      desc <- sects$descriptions[i]
      intro <- chapter_intro(sects, chapters, i)
      title_line <- if (!is.null(block_hd) && nzchar(sects$names[i])) {
        paste0("#' ", block_hd, " ", sects$names[i])
      }
      c(
        if (!is.na(chapters[i]) && !is.null(stack_hd)) {
          paste0("#' ", stack_hd, " ", chapters[i])
        },
        if (length(intro)) {
          c(paste0("#' ", strsplit(intro, "\n")[[1L]]), "#' ")
        },
        title_line,
        if (nzchar(desc)) paste0("#' ", strsplit(desc, "\n")[[1L]])
      )
    }

    header <- paste0(
      "#+ ", chunk_label(sects$ids[i]),
      chunk_vis_spin(flags, sects, i),
      chunk_fig_spin(flags, sects, i)
    )

    paste(
      c(
        prose,
        header,
        chunk_cap(block_level, sects, i, shown),
        chunk_col_spin(flags, sects, i),
        sect_export_code(sects, i),
        if (shown && !isTRUE(sects$pending[i])) sect_output(sects, i)
      ),
      collapse = "\n"
    )
  }

  # With a settings list the script opens on the same YAML front matter as
  # the qmd, spin-quoted, so the two views describe one document -- plus a
  # setup chunk saying the executable half of it again in knitr's language
  # (see spin_setup).
  header <- if (is.null(settings)) {
    spin_header(title, intro)
  } else {
    c(
      paste(paste0("#' ", report_yaml(title, settings)), collapse = "\n"),
      paste(spin_setup(settings), collapse = "\n")
    )
  }

  woven <- weave_text_items(
    chr_ply(seq_along(sects$ids), one_section),
    sects$ids,
    items,
    spin = TRUE
  )

  pieces <- c(header, woven$pieces)

  if (isTRUE(collapse)) {
    paste0(pieces, collapse = "\n\n")
  } else {
    structure(
      pieces,
      ids = c(rep(NA_character_, length(header)), woven$ids)
    )
  }
}

# knitr does not read quarto's `execute:` block, and a spin script is
# rendered by knitr (`rmarkdown::render("report.R")`), not by quarto. So
# every document-wide setting the YAML carries for quarto's benefit --
# whether warnings and messages are shown, and the default figure size --
# has to be said a second time, in knitr's own language, or the same
# settings produce two different documents. Rendered by quarto (`quarto
# render report.R` reads the same YAML the qmd does) the chunk is
# redundant; rendered by rmarkdown it is the only copy that is read.
spin_setup <- function(s) {
  opts <- c(
    paste0("fig.width = ", s$fig_width),
    paste0("fig.height = ", s$fig_height),
    if (!isTRUE(s$warnings)) c("warning = FALSE", "message = FALSE")
  )
  one <- paste0("knitr::opts_chunk$set(", paste(opts, collapse = ", "), ")")

  c(
    "#+ setup, include=FALSE",
    if (nchar(one) <= 78L) {
      one
    } else {
      c("knitr::opts_chunk$set(", paste0("  ", paste(opts, collapse = ", ")), ")")
    }
  )
}

# The spin document's header piece: title as a `#' #` heading, intro as
# spin prose under it. character(0) when neither is set, so the outline's
# spin (which sets neither) is byte-identical to before.
spin_header <- function(title, intro) {

  has_title <- !is.null(title) && nzchar(title)
  has_intro <- nzchar(coal(intro, ""))

  if (!has_title && !has_intro) {
    return(character())
  }

  paste(
    c(
      if (has_title) paste0("#' # ", title),
      if (has_title && has_intro) "#' ",
      if (has_intro) paste0("#' ", strsplit(intro, "\n")[[1L]])
    ),
    collapse = "\n"
  )
}

# `intro` is emitted as a markdown paragraph directly under the YAML block;
# `collapse = FALSE` returns the pieces + attr(, "ids") exactly as in
# export_spin (NA for the YAML and intro pieces).
export_qmd <- function(sects, title = "Board report",
                       stack_level = "#", block_level = "caption",
                       slides = FALSE, intro = "", collapse = TRUE,
                       flags = NULL, items = NULL, settings = NULL) {

  sects <- prune_sections(sects)

  chapters <- section_chapters(sects)

  # Heading levels come from the gear's Headings subsection. A stack /
  # block title can be a document heading (#, ##, ###) or, for a block,
  # the exhibit caption (default) or nothing. The block title is a
  # heading OR a caption, never both.
  stack_hd <- if (stack_level %in% c("#", "##")) stack_level
  block_hd <- if (block_level %in% c("#", "##", "###")) block_level

  # Slides: one reported block = one slide, which the document has to say
  # explicitly. A horizontal rule is pandoc's format-independent slide break
  # ("a horizontal rule always starts a new slide"), so the break does not
  # depend on the block having a title -- a heading break would put every
  # untitled exhibit on the previous slide.
  #
  # Two sections are exempt. The first, because a leading rule opens the deck
  # on an empty slide. And any section carrying a chapter heading, because a
  # heading at the slide level or above already breaks -- the rule would only
  # add the blank slide before it.
  first_reported <- which(sects$report)[1L]

  slide_break <- function(i) {
    if (!slides || !sects$report[i] || identical(i, first_reported)) {
      return(character())
    }
    if (!is.na(chapters[i]) && !is.null(stack_hd)) {
      return(character())
    }
    c("----", "")
  }

  one_section <- function(i) {

    shown <- section_shown(flags, sects, i)

    prose <- if (shown) {
      desc <- sects$descriptions[i]
      intro <- chapter_intro(sects, chapters, i)
      c(
        if (!is.na(chapters[i]) && !is.null(stack_hd)) {
          c(paste0(stack_hd, " ", chapters[i]), "")
        },
        if (length(intro)) c(intro, ""),
        if (!is.null(block_hd) && nzchar(sects$names[i])) {
          c(paste0(block_hd, " ", sects$names[i]), "")
        },
        if (nzchar(desc)) desc
      )
    }

    chunk <- c(
      "```{r}",
      paste0("#| label: ", chunk_label(sects$ids[i])),
      # Caption only when the block title is set to "caption"; a heading
      # title already carries the name, and "none" wants no title at all.
      chunk_cap(block_level, sects, i, shown),
      chunk_vis_qmd(flags, sects, i),
      chunk_fig_qmd(flags, sects, i),
      sect_export_code(sects, i),
      if (shown && !isTRUE(sects$pending[i])) sect_output(sects, i),
      "```"
    )

    paste(
      c(slide_break(i), prose, if (length(prose)) "", chunk),
      collapse = "\n"
    )
  }

  yaml <- if (is.null(settings)) {
    paste(
      c(
        "---",
        paste0("title: \"", yaml_dq(title), "\""),
        # Render plain data.frames / tibbles as kable tables rather than
        # verbatim console output, so the report reads like a document. A
        # top-level quarto option, so it holds across html / pdf / pptx.
        # Exhibits with their own print method (flextable, gt, htmlwidgets)
        # are untouched -- df-print only governs bare data frames.
        "df-print: kable",
        "---"
      ),
      collapse = "\n"
    )
  } else {
    paste(report_yaml(title, settings), collapse = "\n")
  }

  header <- c(yaml, if (nzchar(coal(intro, ""))) intro)

  woven <- weave_text_items(
    chr_ply(seq_along(sects$ids), one_section),
    sects$ids,
    items
  )

  pieces <- c(header, woven$pieces)

  if (isTRUE(collapse)) {
    paste0(pieces, collapse = "\n\n")
  } else {
    structure(
      pieces,
      ids = c(rep(NA_character_, length(header)), woven$ids)
    )
  }
}
