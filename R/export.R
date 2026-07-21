# Code / document projection of a board, with per-block annotations
# (markdown description + include-in-report flag) supplied by the outline
# extension's own state rather than block attributes. Only exported
# blockr.core API is used: export_code() supplies per-block expressions,
# argument maps and expression types in a valid topological order; links
# and stacks come from the board object.

ann_description <- function(annotations, id) {
  coal(annotations[[id]][["description"]], "")
}

ann_report <- function(annotations, id) {
  isTRUE(coal(annotations[[id]][["report"]], TRUE))
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
                             stack_annotations = list()) {

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
      stacks = if (length(narrowed)) do.call(blockr.core::stacks, narrowed)
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

  # A block is reorderable iff it has slack in the DAG: it may pass its
  # displayed predecessor (which must then not be an ancestor) or its
  # successor (which must then not be a descendant). Fully pinned blocks
  # get no drag affordance -- no valid order could move them anyway.
  lnks <- blockr.core::board_links(board)
  keep <- lnks$from %in% ids & lnks$to %in% ids
  kids <- split(lnks$to[keep], factor(lnks$from[keep], levels = ids))

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

  movable <- lgl_ply(seq_along(ids), function(i) {
    (i > 1L && !reaches(ids[i - 1L], ids[i])) ||
      (i < length(ids) && !reaches(ids[i], ids[i + 1L]))
  })

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
    ids = ids,
    pending = ids %in% pending_ids,
    movable = movable,
    drop_lo = drop_lo,
    drop_hi = drop_hi,
    chap_targets = chap_targets,
    code = chr_ply(
      lapply(exprs, deparse),
      paste0,
      collapse = "\n"
    ),
    names = chr_ply(blks, blockr.core::block_name),
    icons = chr_ply(seq_along(blks), function(i) block_icon_html(blks[[i]])),
    descriptions = chr_ply(ids, function(i) ann_description(annotations, i)),
    report = lgl_ply(ids, function(i) ann_report(annotations, i)),
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

# The registry icon exactly as the dock's block card shows it. The two
# helpers are blockr.dock internals (recorded follow-up: export them);
# resolved dynamically with a letter-tile fallback so a dock without them
# degrades instead of breaking.
block_icon_html <- function(blk) {
  tryCatch(
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
      nme <- sects$stack_names[idx[1L]]
      out[idx[1L]] <- if (runs$values[i] %in% seen) {
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
      c(prose, header, sects$code[i], if (sects$report[i]) sects$ids[i]),
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
      sects$code[i],
      if (sects$report[i]) sects$ids[i],
      "```"
    )

    paste(c(prose, if (length(prose)) "", chunk), collapse = "\n")
  }

  yaml <- paste(
    c("---", paste0("title: \"", gsub("\"", "\\\\\"", title), "\""), "---"),
    collapse = "\n"
  )

  paste0(
    c(yaml, chr_ply(seq_along(sects$ids), one_section)),
    collapse = "\n\n"
  )
}
