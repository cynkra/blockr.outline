# Shared fixtures for the pure-function tests.
#
# outline_sections() and the exporters need three things: a blockr.core board
# (blocks + links + stacks), a named list of per-block expressions keyed by
# block id, and the extension's own annotation state. None of that requires a
# reactive context -- the expressions are supplied directly (in the running
# extension they come from the block servers, but the projection only reads
# them), so these helpers build plain objects.

# A small deterministic board: data -> sub -> head, with `data` and `sub`
# grouped into one stack. `head` is unstacked. Enough shape to exercise
# ordering, chapters and the split-stack cases without pulling in blockr.dplyr
# or blockr.ggplot.
otl_board <- function(stacks = TRUE) {

  # new_board() infers the constructor's package from the calling frame, so
  # it must be called directly (do.call() breaks that introspection).
  blocks <- c(
    data = blockr.core::new_dataset_block("iris"),
    sub  = blockr.core::new_subset_block(),
    head = blockr.core::new_head_block()
  )
  lnks <- blockr.core::links(from = c("data", "sub"), to = c("sub", "head"))

  if (stacks) {
    blockr.core::new_board(
      blocks = blocks,
      links = lnks,
      stacks = blockr.core::stacks(
        prep = blockr.core::new_stack(c("data", "sub"))
      )
    )
  } else {
    blockr.core::new_board(blocks = blocks, links = lnks)
  }
}

# Synthetic expressions matching otl_board(). `pending` sets the pending
# attribute outline_sections() reads (blocks not yet reporting a real expr).
# Drop ids from `keep` to simulate a block whose expression is momentarily
# unavailable (the narrowing branch in outline_sections).
otl_exprs <- function(keep = c("data", "sub", "head"), pending = character()) {

  all <- list(
    data = quote(datasets::iris),
    sub  = quote(subset(data, Species == "setosa")),
    head = quote(utils::head(sub, 3))
  )

  structure(all[keep], pending = pending)
}

# Annotations for a fixture board. The report flag defaults OFF in
# production (a board is not a report until someone says what belongs in
# it), so a test that means "these blocks are the document" has to say so.
# otl_ann() says it once: every id is included, and named arguments override
# individual entries.
otl_ann <- function(..., ids = c("data", "sub", "head")) {

  out <- stats::setNames(rep(list(list(report = TRUE)), length(ids)), ids)

  over <- list(...)

  for (nm in names(over)) {
    out[[nm]] <- utils::modifyList(coal(out[[nm]], list()), over[[nm]])
  }

  out
}

# A board whose two leaf blocks (`plot`, `audit`) are parallel branches off
# `sub` -- both movable, and the substrate for the tie-break ordering tests.
# `plot` is a scatter block (meta category "plot" -> kind "fig").
otl_board_parallel <- function() {

  blockr.core::new_board(
    blocks = c(
      data  = blockr.core::new_dataset_block("iris"),
      sub   = blockr.core::new_subset_block(),
      plot  = blockr.core::new_scatter_block("Sepal.Length", "Sepal.Width"),
      audit = blockr.core::new_head_block()
    ),
    links = blockr.core::links(
      from = c("data", "sub",  "sub"),
      to   = c("sub",  "plot", "audit")
    )
  )
}

otl_exprs_parallel <- function(pending = character()) {
  structure(
    list(
      data  = quote(datasets::iris),
      sub   = quote(subset(data, Species == "setosa")),
      plot  = quote(plot(sub$Sepal.Length, sub$Sepal.Width)),
      audit = quote(utils::head(sub, 3))
    ),
    pending = pending
  )
}
