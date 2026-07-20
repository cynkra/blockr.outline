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

  if (identical(types, "bquoted")) {
    exprs <- do.call(bquote, list(exprs, args))
  }

  if (length(args) && identical(types, "quoted")) {
    call("with", args, exprs)
  } else {
    call("local", exprs)
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

  pref <- c(intersect(preference, ids), setdiff(base, preference))
  prank <- setNames(seq_along(pref), pref)

  # Pass 2: user preference as the tie-break.
  kahn(function(ready, out) {
    ready[which.min(prank[ready])]
  })
}

outline_sections <- function(expressions, board, annotations,
                             preference = character(),
                             stack_annotations = list()) {

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

  list(
    ids = ids,
    movable = movable,
    code = chr_ply(
      lapply(exprs, deparse),
      paste0,
      collapse = "\n"
    ),
    names = chr_ply(blks, blockr.core::block_name),
    icons = chr_ply(seq_along(blks), function(i) block_icon_html(blks[[i]])),
    descriptions = chr_ply(ids, function(i) ann_description(annotations, i)),
    report = lgl_ply(ids, function(i) ann_report(annotations, i)),
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

export_spin <- function(sects) {

  chapters <- section_chapters(sects)

  one_section <- function(i) {

    prose <- if (sects$report[i]) {
      desc <- sects$descriptions[i]
      intro <- chapter_intro(sects, chapters, i)
      c(
        if (!is.na(chapters[i])) paste0("#' # ", chapters[i]),
        if (length(intro)) {
          c(paste0("#' ", strsplit(intro, "\n")[[1L]]), "#' ")
        },
        paste0("#' ## ", sects$names[i]),
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

export_qmd <- function(sects, title = "Board report") {

  chapters <- section_chapters(sects)

  one_section <- function(i) {

    prose <- if (sects$report[i]) {
      desc <- sects$descriptions[i]
      intro <- chapter_intro(sects, chapters, i)
      c(
        if (!is.na(chapters[i])) c(paste0("# ", chapters[i]), ""),
        if (length(intro)) c(intro, ""),
        paste0("## ", sects$names[i]),
        if (nzchar(desc)) c("", desc)
      )
    }

    chunk <- c(
      "```{r}",
      paste0("#| label: ", gsub("[^a-zA-Z0-9_-]", "-", sects$ids[i])),
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
