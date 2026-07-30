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

  if (!setequal(known, blockr.core::board_block_ids(board))) {

    # Narrow onto a PLAIN core board rather than mutating the one we were
    # handed. A block whose expression is momentarily NULL (a ggplot block
    # req()s while its upstream data settles) has to be dropped, but every
    # container validates the dropped id against something it owns: core
    # rejects links naming it, and a dock_board additionally rejects view
    # memberships naming it. Both aborts landed in the caller's tryCatch,
    # became req(FALSE), and froze the outline on its last good projection
    # with nothing left to re-invalidate it -- so editing a block updated
    # the block but never the document.
    #
    # The projection only needs blocks, links and stacks, so rebuilding
    # those three narrowed decouples it from container validation
    # entirely. Stack objects are carried over untouched, keeping their
    # dock attributes (name, color) intact.
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

    board <- blockr.core::new_board(
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
    # Exhibit kind from the block's CLASS, not its result: results are
    # gated by evaluation and visibility, so a runtime probe reads NULL
    # for most blocks and the caption would appear only sometimes. The
    # class is always there. A block that is neither plot nor data gets
    # no prefix -- a wrong fig- would leave a broken cross-reference.
    kinds = vapply(
      blks,
      function(b) {
        # The registry category is the ecosystem-wide answer: every
        # package declares it, so a ggplot_block (class ggplot_block,
        # not plot_block) still reports "plot". Class inheritance only
        # covers blockr.core's own hierarchy.
        cat <- tryCatch(
          blockr.core::block_meta_category(b),
          error = function(e) character()
        )
        if (any(cat %in% c("plot", "visualization")) ||
              inherits(b, "plot_block")) {
          "fig"
        } else if (any(cat %in% c("data", "transform", "table")) ||
                     inherits(b, c("data_block", "transform_block"))) {
          "tbl"
        } else {
          ""
        }
      },
      character(1L)
    ),
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
outline_geometry <- function(ids, lnks, stack_ids, report, universe = ids) {

  kids <- split(lnks$to, factor(lnks$from, levels = universe))

  reaches <- function(a, b) {
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

  # A block is reorderable iff it has slack in the DAG: it may pass its
  # displayed predecessor (which must then not be an ancestor) or its
  # successor (which must then not be a descendant). Fully pinned blocks
  # get no drag affordance -- no valid order could move them anyway.
  movable <- lgl_ply(seq_along(ids), function(i) {
    (i > 1L && !reaches(ids[i - 1L], ids[i])) ||
      (i < length(ids) && !reaches(ids[i], ids[i + 1L]))
  })

  # The document's evaluation closure: a block belongs to the export iff it
  # is reported or some reported block depends on it. Everything else is
  # dropped from the DOCUMENT entirely (not include=FALSE, whose code still
  # runs at render) -- on a many-view board the independent branches would
  # otherwise all evaluate for a report that shows none of them. The
  # outline view keeps showing every block; only the exporters prune.
  reported <- ids[report]

  exported <- report | lgl_ply(
    ids,
    function(a) any(lgl_ply(reported, function(b) reaches(a, b)))
  )

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

# The report renderer wrapped around a block's printed result ("" = bare
# print). The blockr.viz table blocks return a bare annotated data frame --
# their styled table lives in the block's Shiny UI, so a bare print degrades
# to df-print:kable. Wrapping the result variable in the static flextable
# renderer restores the styled table, and flextable is the one engine whose
# knit_print emits real OpenXML tables in pptx and docx (it renders in html
# and pdf too), so a single wrapper serves every format. Class check only,
# like `kinds`: blockr.viz need not be installed to project the sections;
# the emitted call self-qualifies and the render session loads blockr.viz
# anyway (the block's own code calls it).
block_report_renderer <- function(blk) {
  if (inherits(blk, c("table_block", "summary_table_block"))) {
    "blockr.viz::static_table"
  } else {
    ""
  }
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
  fmt <- tryCatch(
    getExportedValue("blockr.viz", "chart_code"),
    error = function(e) NULL
  )

  if (is.function(fmt)) {
    out <- tryCatch(fmt(cl), error = function(e) NULL)
    if (is.character(out) && length(out) == 1L && nzchar(out)) {
      return(out)
    }
  }

  paste(deparse(cl), collapse = "\n")
}

# The output line of a reported chunk: the block's own report call when it
# states one, else the result variable, wrapped in the block's report
# renderer when it has one.
sect_output <- function(sects, i) {
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

# The search catalogue: one entry per board block, the listed ones first
# and each group in document order. The search box is a single control over
# the whole board -- a listed block is a "go to", an unlisted one an "add"
# -- so it needs the document and the pool in one payload. `runs` marks a
# block the report already depends on: including it only makes its output
# visible, it was going to be evaluated either way.
outline_catalog <- function(sects, listed) {

  is_listed <- sects$ids %in% listed
  ord <- c(which(is_listed), which(!is_listed))

  lapply(
    ord,
    function(i) {
      # `[[` throughout: names / icons / descriptions are NAMED vectors, and
      # a named element serialises as a JSON object, not a string.
      list(
        id = sects$ids[[i]],
        name = sects$names[[i]],
        icon = na_blank(sects$icons[[i]]),
        chapter = na_blank(sects$stack_names[[i]]),
        desc = desc_oneline(sects$descriptions[[i]]),
        listed = is_listed[[i]],
        runs = isTRUE(sects$exported[[i]]) && !isTRUE(sects$report[[i]])
      )
    }
  )
}

# Narrow a sections projection to the ids the outline LISTS (the report
# blocks, unless show-all). Like prune_sections(), but for display: the
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

export_spin <- function(sects, stack_level = "#", block_level = "caption") {

  sects <- prune_sections(sects)

  chapters <- section_chapters(sects)

  stack_hd <- if (stack_level %in% c("#", "##")) stack_level
  # spin has no chunk-caption mechanism, so a "caption" block title
  # becomes a bold line above the output; a heading title is that
  # heading. Either way the R script stays a faithful mirror.
  block_hd <- if (block_level %in% c("#", "##", "###")) block_level

  one_section <- function(i) {

    prose <- if (sects$report[i]) {
      desc <- sects$descriptions[i]
      intro <- chapter_intro(sects, chapters, i)
      title_line <- if (!is.null(block_hd) && nzchar(sects$names[i])) {
        paste0("#' ", block_hd, " ", sects$names[i])
      } else if (identical(block_level, "caption") && nzchar(sects$names[i])) {
        paste0("#' **", sects$names[i], "**")
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
      "#+ ", sects$ids[i],
      if (!sects$report[i]) ", include=FALSE"
    )

    paste(
      c(
        prose,
        header,
        sect_export_code(sects, i),
        if (sects$report[i] && !isTRUE(sects$pending[i])) sect_output(sects, i)
      ),
      collapse = "\n"
    )
  }

  paste0(
    chr_ply(seq_along(sects$ids), one_section),
    collapse = "\n\n"
  )
}

export_qmd <- function(sects, title = "Board report",
                       stack_level = "#", block_level = "caption") {

  sects <- prune_sections(sects)

  chapters <- section_chapters(sects)

  # Heading levels come from the gear's Headings subsection. A stack /
  # block title can be a document heading (#, ##, ###) or, for a block,
  # the exhibit caption (default) or nothing. The block title is a
  # heading OR a caption, never both.
  stack_hd <- if (stack_level %in% c("#", "##")) stack_level
  block_hd <- if (block_level %in% c("#", "##", "###")) block_level

  one_section <- function(i) {

    prose <- if (sects$report[i]) {
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

    # A block that is in the report shows its output, so it IS an
    # exhibit: its title becomes the CAPTION rather than a heading --
    # stacks head sections, blocks are exhibits. The `kind` (fig/tbl)
    # picks the caption key so the caption sits in the right place.
    #
    # The label is deliberately NOT prefixed with the kind. A `tbl-`/
    # `fig-` label makes quarto treat the output as a cross-reference
    # FLOAT, and pandoc's pptx path cannot render a flextable inside that
    # float -- the table silently vanishes from the slide. Dropping the
    # prefix keeps one qmd that renders identically to html, pdf AND
    # pptx (flextables included); the cost is losing @tbl-/@fig- cross-
    # references, which slides do not use and reports rarely do.
    kind <- sects$kinds[i]
    lbl <- gsub("[^a-zA-Z0-9_-]", "-", sects$ids[i])

    # Caption only when the block title is set to "caption"; a heading
    # title already carries the name, and "none" wants no title at all.
    cap <- if (sects$report[i] && identical(block_level, "caption") &&
                 nzchar(kind)) {
      paste0("#| ", kind, "-cap: \"", gsub("\"", "'", sects$names[i]), "\"")
    }

    chunk <- c(
      "```{r}",
      paste0("#| label: ", lbl),
      cap,
      if (!sects$report[i]) "#| include: false",
      sect_export_code(sects, i),
      if (sects$report[i] && !isTRUE(sects$pending[i])) sect_output(sects, i),
      "```"
    )

    paste(c(prose, if (length(prose)) "", chunk), collapse = "\n")
  }

  yaml <- paste(
    c(
      "---",
      paste0("title: \"", gsub("\"", "\\\\\"", title), "\""),
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

  paste0(
    c(yaml, chr_ply(seq_along(sects$ids), one_section)),
    collapse = "\n\n"
  )
}
