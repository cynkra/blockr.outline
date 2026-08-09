# The block catalogue, as the deck's inline picker needs it.
#
# The picker is one surface with two states: at rest it BROWSES (every type,
# grouped by category, with its description), and once you type it RECALLS
# (flat, ranked). Both read this same list, which is why the deck needs no
# second copy of the catalogue in a pane.
#
# It is a pure function of the registry, so it is sent once when the client
# announces itself rather than on every board change: `available_blocks()`
# does not vary with the board, and instantiating every constructor to read
# its arity is the expensive part.

# Two pools, because the origin narrows the choice and nothing else about the
# gesture differs. Appending links the source INTO the new block, so a
# candidate must be able to receive one: a named input slot, or variadic
# arity, which accepts arbitrary fresh slots. Source-only blocks (arity 0, a
# dataset block) cannot be appended and drop out. Adding with no origin makes
# no link, so nothing is filtered -- this is the same rule blockr.dock's
# block browser applies via `need_inputs <- mode == "append"`, restated here
# because that helper is internal to dock.
minidag_registry <- function() {
  entries <- minidag_registry_entries()
  list(
    add = unname(entries),
    append = unname(
      Filter(
        function(m) length(m$inputs) > 0L || isTRUE(m$variadic),
        entries
      )
    )
  )
}

minidag_registry_entries <- memoise0(function() {

  registry <- blockr.core::available_blocks()

  entry <- function(i) {

    ctor <- registry[[i]]
    uid <- names(registry)[[i]]

    # Arity has to come from an instance, and a constructor with required
    # arguments (or a broken package) would otherwise take the whole deck
    # down at startup. A type we cannot instantiate is still offered; it
    # just never qualifies as an append candidate.
    blk <- tryCatch(ctor(), error = function(e) NULL)

    list(
      # the registry uid, so a commit can `create_block(type)` rather than
      # depend on the constructor's function name being importable
      type = uid,
      name = reg_attr(ctor, "name", uid),
      description = reg_attr(ctor, "description", ""),
      category = reg_attr(ctor, "category", "other"),
      package = reg_attr(ctor, "package", "local"),
      # No icon. A registry icon has no colour of its own -- the deck tints
      # one per block from board metadata -- and base64-encoding 66 of them
      # at startup buys a tile the client can draw from the category alone.
      inputs = I(as.list(
        if (is.null(blk)) character() else blockr.core::block_inputs(blk)
      )),
      variadic = !is.null(blk) && is.na(blockr.core::block_arity(blk))
    )
  }

  lapply(seq_along(registry), entry)
})

# An empty string in the registry means "not set", not "set to empty".
reg_attr <- function(entry, key, default) {
  val <- attr(entry, key, exact = TRUE)
  if (is.null(val) || (is.character(val) && !nzchar(val))) default else val
}
