# The slide builder's half of the package: a projection and a document, both
# stripped to what a deck needs.
#
# The outline builds a DOCUMENT -- prose, chapters, code cells, an inverted
# reading order -- and the deck is one of the things it can render. This is
# the other direction: start from "these blocks, in this order, one per
# slide" and emit only that. The two share everything downstream (the
# officer pptx writer, the revealjs render, the reference-doc seam) because
# both speak the same `sects` list; they share nothing upstream, because the
# deck has no document to keep in sync.
#
# What the deck projection drops relative to outline_sections(): the drag
# geometry (movable / drop_lo / drop_hi / chap_targets -- the projection's
# one super-linear step), stacks, descriptions, and the syntax highlighting
# that the outline needs to SHOW code. What it keeps is the export closure,
# because picking a table still means running the blocks upstream of it.

# `sects` -> the projection a deck renders from.
#
# `slides` is the picked block ids IN SLIDE ORDER, which is both the picking
# and the ordering: one field, one gesture. It reaches the projection twice
# and means something different each time. As `preference` it is a tie-break
# on the topological sort, fixing the EVALUATION order (dependencies first,
# always). As `slide_ids` it is the order the slides come out in, which is
# whatever the user dragged it to.
#
# Those two orders are free to disagree, and that is the whole point. A deck
# is allowed to open on its conclusion and derive it three slides later; a
# document that computed in that order would not run. They can disagree
# because the deck emits every block's code up front, hidden, and each slide
# carries only its exhibit expression (see export_deck_qmd; the officer path
# has always worked this way).
slide_sections <- function(expressions, board, slides = character()) {

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

  ord <- preferred_ordering(names(exprs), board, intersect(slides, names(exprs)))
  exprs <- exprs[ord]

  ids <- names(exprs)
  exprs <- Map(block_assignment, ids, exprs)

  blks <- blockr.core::board_blocks(board)[ids]

  report <- ids %in% slides

  lnks <- blockr.core::board_links(board)
  lnks <- lnks[lnks$from %in% ids & lnks$to %in% ids, ]

  n <- length(ids)

  list(
    ids = ids,
    pending = ids %in% pending_ids,
    code = chr_ply(lapply(exprs, deparse), paste0, collapse = "\n"),
    names = chr_ply(blks, blockr.core::block_name),
    icons = chr_ply(seq_along(blks), function(i) block_icon_html(blks[[i]])),
    # No prose on a slide, so no descriptions -- but the field has to exist
    # and be per-block, because prune_sections() subsets it.
    descriptions = setNames(rep("", n), ids),
    report = report,
    exported = export_closure(ids, report, dag_reaches(lnks, ids)),
    kinds = chr_ply(blks, block_exhibit_kind),
    renderers = chr_ply(blks, block_report_renderer),
    report_calls = chr_ply(
      seq_along(blks),
      function(i) block_report_call_str(blks[[i]], ids[[i]])
    ),
    # A deck has no chapters. The fields stay, holding nothing: every
    # consumer downstream reads them, and NA is what "unstacked" already
    # looks like to all of them.
    stack_ids = rep(NA_character_, n),
    stack_names = rep(NA_character_, n),
    stack_colors = character(),
    stack_descriptions = character(),
    # Ids rather than indices, so the field survives prune_sections()
    # subsetting the per-block vectors underneath it.
    slide_ids = intersect(slides, ids)
  )
}

# The slide sequence as positions into `sects`, or every section in document
# order when the projection carries no slide order (the outline's, which is
# the document's own).
slide_seq <- function(sects) {

  if (is.null(sects$slide_ids)) {
    return(seq_along(sects$ids))
  }

  idx <- match(sects$slide_ids, sects$ids)
  idx[!is.na(idx)]
}

qmd_label <- function(id) {
  gsub("[^a-zA-Z0-9_-]", "-", id)
}

# The deck as a quarto document: one hidden setup chunk per block, then one
# slide per pick.
#
# The split is what buys free ordering. export_qmd() emits each block's code
# and its output TOGETHER, so document order is evaluation order and a slide
# cannot precede the block it derives from. Here the computation is done
# before the first slide and every slide is a lookup, so the slides are a
# free permutation.
#
# It also makes the deck's document simpler than the report's in the way the
# reader would expect: no code on any slide, no captions (the slide title is
# the heading), no cross-reference labels -- and therefore none of the
# `tbl-`/`fig-` float trap that makes flextables vanish from a pandoc pptx.
export_deck_qmd <- function(sects, title = "Deck") {

  sects <- prune_sections(sects)

  setup <- chr_ply(
    seq_along(sects$ids),
    function(i) {
      paste(
        c(
          "```{r}",
          paste0("#| label: setup-", qmd_label(sects$ids[i])),
          "#| include: false",
          sect_export_code(sects, i),
          "```"
        ),
        collapse = "\n"
      )
    }
  )

  # Skip a pending block rather than emit a slide that references a variable
  # nothing bound: its code is a comment (see sect_export_code), so the
  # exhibit expression would fail the render. The download flow demands the
  # closure and waits for it, so this is the belt to that braces.
  shown <- Filter(
    function(i) !isTRUE(sects$pending[i]),
    slide_seq(sects)
  )

  slides <- chr_ply(
    as.character(shown),
    function(k) {

      i <- as.integer(k)
      nm <- sects$names[[i]]

      # The heading IS the slide break, which is why an untitled block falls
      # back to a horizontal rule: pandoc breaks on either, but a heading
      # break on a block with no name would silently fold its exhibit onto
      # the previous slide.
      brk <- if (nzchar(coal(nm, ""))) paste0("## ", nm) else "----"

      paste(
        c(
          brk,
          "",
          "```{r}",
          paste0("#| label: slide-", qmd_label(sects$ids[i])),
          sect_output(sects, i),
          "```"
        ),
        collapse = "\n"
      )
    }
  )

  yaml <- paste(
    c(
      "---",
      paste0("title: \"", yaml_dq(title), "\""),
      # Render plain data.frames / tibbles as kable tables rather than
      # verbatim console output. Exhibits with their own print method
      # (flextable, gt, htmlwidgets) are untouched.
      "df-print: kable",
      # A deck shows results, never code -- the difference from the report,
      # stated once at the top instead of per chunk.
      "execute:",
      "  echo: false",
      "  warning: false",
      "  message: false",
      "---"
    ),
    collapse = "\n"
  )

  paste0(c(yaml, setup, slides), collapse = "\n\n")
}
