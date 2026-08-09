# Copy and paste, for the deck.
#
# Wire-compatible with blockr.dag's clipboard on purpose: the same
# `{object: "subboard", payload: {blocks, links, stacks}}` envelope, each part
# serialised by blockr.core's own methods. A selection copied in the DAG
# canvas pastes into the deck and back. The envelope is rebuilt here rather
# than reusing `blockr.dag:::new_subboard()` -- that class and its extraction
# helpers are internal to dag, and this package holds to exported API.
#
# `null = "null"` is not optional. Without it every NULL state field
# serialises as `{}`, comes back as an empty `list()` instead of NULL, and
# poisons the pasted block -- the failure behind blockr.dag#144, which also
# survived a save and re-propagated on the next copy. blockr.core's own
# `write_json()` has always had it.

# The blocks, the links INTERNAL to them, and any stack the selection covers
# entirely. A link with one end outside the selection is dropped: pasting it
# would wire the copy to a block the user did not copy.
minidag_subboard <- function(board, block_ids) {

  blocks <- blockr.core::board_blocks(board)
  ids <- intersect(block_ids, names(blocks))

  if (!length(ids)) {
    return(NULL)
  }

  links <- blockr.core::board_links(board)
  keep <- links$from %in% ids & links$to %in% ids

  stacks <- blockr.core::board_stacks(board)
  covered <- vapply(
    stacks,
    function(s) all(blockr.core::stack_blocks(s) %in% ids),
    logical(1)
  )

  list(
    blocks = blocks[ids],
    links = if (any(keep)) links[keep] else blockr.core::links(),
    stacks = if (any(covered)) stacks[covered] else blockr.core::stacks()
  )
}

# Block state as it stands in the running board, not as it was constructed --
# a copy has to carry what the user configured. Values arrive as reactives
# from the block servers, so they are forced here, inside the observer that
# already has a reactive context.
minidag_live_states <- function(board, block_ids) {

  running <- board$blocks
  ids <- intersect(block_ids, names(running))

  out <- lapply(
    ids,
    function(id) {
      state <- running[[id]][["server"]][["state"]]
      if (is.null(state)) {
        return(list())
      }
      lapply(state, function(v) if (shiny::is.reactive(v)) v() else v)
    }
  )

  stats::setNames(out, ids)
}

minidag_clip_json <- function(board, block_ids, states) {

  sub <- minidag_subboard(board, block_ids)

  if (is.null(sub)) {
    return(NULL)
  }

  envelope <- list(
    object = "subboard",
    payload = list(
      blocks = blockr.core::blockr_ser(sub$blocks, blocks = states),
      links = blockr.core::blockr_ser(sub$links),
      stacks = blockr.core::blockr_ser(sub$stacks)
    )
  )

  as.character(
    jsonlite::toJSON(envelope, auto_unbox = TRUE, null = "null")
  )
}

# Read a clipboard payload into an update delta, with every id remapped so a
# paste onto the board it was copied from cannot collide. Returns NULL when
# the text is not one of our envelopes -- the client already screens for that,
# but a paste is user input and this is the authoritative gate.
minidag_paste_delta <- function(board, json) {

  data <- tryCatch(
    jsonlite::fromJSON(json, simplifyDataFrame = FALSE, simplifyMatrix = FALSE),
    error = function(e) NULL
  )

  if (!is.list(data) || !identical(data[["object"]], "subboard")) {
    return(NULL)
  }

  parts <- tryCatch(
    list(
      blocks = blockr.core::blockr_deser(data$payload$blocks),
      links = blockr.core::blockr_deser(data$payload$links),
      stacks = blockr.core::blockr_deser(data$payload$stacks)
    ),
    error = function(e) {
      shiny::showNotification(
        paste("Cannot paste:", conditionMessage(e)),
        type = "error"
      )
      NULL
    }
  )

  if (is.null(parts) || !length(parts$blocks)) {
    return(NULL)
  }

  old <- names(parts$blocks)
  new <- blockr.core::rand_names(
    names(blockr.core::board_blocks(board)),
    n = length(old)
  )
  id_map <- stats::setNames(new, old)

  blocks <- blockr.core::as_blocks(
    stats::setNames(as.list(parts$blocks), unname(id_map[old]))
  )

  links <- blockr.core::links()

  if (length(parts$links)) {
    ldf <- as.data.frame(parts$links)
    ldf$from <- unname(id_map[ldf$from])
    ldf$to <- unname(id_map[ldf$to])
    # a link whose endpoint did not travel is not a link
    ldf <- ldf[!is.na(ldf$from) & !is.na(ldf$to), , drop = FALSE]
    if (nrow(ldf)) {
      links <- blockr.core::as_links(ldf[, c("from", "to", "input")])
    }
  }

  upd <- list(
    blocks = list(add = blocks),
    links = if (length(links)) list(add = links)
  )

  if (length(parts$stacks)) {
    upd$stacks <- list(add = minidag_remap_stacks(parts$stacks, id_map))
  }

  Filter(Negate(is.null), upd)
}

# A pasted stack keeps its shape and says it is a copy. Blocks deliberately
# do not: a pasted group arrives selected and adjacent, so " (copy)" on every
# member is noise, while on the group it is the one place the provenance is
# worth stating. Same convention blockr.dag settled on.
minidag_remap_stacks <- function(stacks, id_map) {

  out <- lapply(
    stacks,
    function(s) {
      members <- unname(id_map[blockr.core::stack_blocks(s)])
      members <- members[!is.na(members)]
      blockr.dock::new_dock_stack(
        blocks = members,
        name = paste0(blockr.core::stack_name(s), " (copy)"),
        color = blockr.dock::stack_color(s)
      )
    }
  )

  do.call(blockr.core::stacks, unname(out))
}
