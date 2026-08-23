#' Outline board extension
#'
#' A compact, list-shaped alternative to the blockr.dag workflow canvas
#' (`blockr.dag::new_dag_extension()`): blocks appear as rows in topological
#' order with a
#' git-commit-graph style rail drawn in the left gutter. Linear pipelines read
#' as a plain list; branches occupy extra rail lanes. The extension is a
#' drop-in replacement for the DAG extension in a
#' [blockr.dock::new_dock_board()]:
#'
#' - drag a block's rail dot onto another row (or dot) to connect; input
#'   arity is enforced the same way the DAG canvas does it (data blocks
#'   accept no inputs, transform blocks one, n-ary blocks fill their named
#'   slots, variadic blocks never fill up),
#' - release the drag on empty space (the left gutter works at any scroll
#'   position) to append a new block: a picker opens at the release point,
#'   browsing the catalogue by category at rest and filtering as you type.
#'   The `+` on a row appends after it. A block with no input at all -- the
#'   start of a flow -- comes from the dashed **Add a block** row at the foot of
#'   the list, where such a block sorts, or from right-clicking the empty space
#'   below it (which is also the only home for **Paste**),
#' - click a row to reveal that block's panel, double-click to rename,
#' - click a dot (or hover a rail edge) to inspect and remove connections,
#' - board stacks show as named frames; collapse them to a single row, drag a
#'   row into a frame to add that block to the stack and out of every frame to
#'   take it out again (a selection moves together),
#' - a stack the flow runs out of and back into cannot be drawn as one run of
#'   rows: those links climb the right-hand gutter as dashed arrows and the
#'   stack header offers to pull the blocks in the way into the group,
#' - each row names the views it is shown on, current view first (or reads
#'   `all views`); clicking a row shows it on the current view and clicking a
#'   view's tag drops it from that view, right-click for the rest, and the
#'   board's extensions do the same from the **Extensions** group at the foot,
#' - block eval status (waiting / unset / failed) shows as a coloured dot
#'   per row, identical in meaning to the DAG node badge.
#'
#' @section Extensions: which extension goes on which view:
#' A board's extensions are panels like any other, so each is a member of the
#' views it is placed on and absent from the rest. They are not in the DAG
#' though, so they have no lane on the rail and no place in an order derived
#' from it. They get a **Extensions** group after the last block instead, one
#' ordinary row each, naming the views the extension is on exactly as a block
#' row does, and reached by the same right-click menu.
#'
#' The group is built from the board's extension set rather than from view
#' membership, so an extension on **no** view still has a row. That is the
#' state per-view membership alone can never report, and the one a row is
#' needed to get out of.
#'

#' @section Which view a panel goes on:
#' Every row -- a block, a stack of blocks, an extension in the
#' **Extensions** group
#' -- names the views it is shown on, and right-click is where that is changed.
#' The menu carries the whole answer:
#'
#' - **All / None / Current**, three presets sharing one row. They are *states*
#'   of the ticks below rather than separate commands, so whichever one
#'   currently holds is lit. `Current` is the board's active view; the marked
#'   row in the
#'   list names it.
#' - **a tick per view**, with that view's panel count beside it, capped and
#'   scrolling because ten views is not a special board. Ticking keeps the menu
#'   open, since it is usually done more than once.
#' - **`only`** on the view row you are pointing at, which clears every other
#'   view: "send it there" in one click.
#'
#' The quick pair does not need the menu at all: **clicking a row** shows it on
#' the current view, adding it if the view does not hold it, and **clicking a
#' view's tag** drops the row from the view that tag names. That holds for the
#' extension rows too: an extension is a panel like any other, and one sitting
#' on another view -- or on no view at all -- comes to the view you are on
#' rather than sending you to it. The current view is
#' named first whenever the row is on it and tinted, because it is the one whose
#' panel you can watch go; any other view's tag does the same thing to the view
#' it points at. The `x` on hover is the mark saying the tag is clickable, not a
#' separate target.
#'
#' The `+n` count is a count, not a view, so it is not a button: the views
#' behind it are reached through the menu's checklist rather than by expanding a
#' tag list a row has no width for.
#'
#' A **stack header carries no view tag at all**. A stack has no membership of
#' its own -- a view's members are block panels -- so anything shown there could
#' only be a union over its members, which reads as a fact about the stack and
#' is not one. Expanded, the member rows say it exactly; collapsed,
#' right-clicking the header gives the per-view checklist with a tri-state
#' box, which is the honest form of the same summary.
#'
#' The menu acts on the **selection** rather than on the row under the pointer,
#' the way a file manager does: right-clicking a selected row speaks for all of
#' them, and right-clicking an unselected one collapses the selection to it
#' first. On a collapsed stack header it speaks for the stack's members.
#'
#' Below the views it carries only the operations that have no discoverable
#' gesture of their own -- rename, copy, cut, remove -- and deliberately *not*
#' connect or append: the rail dot and the row's `+` are better affordances than
#' a menu entry, and naming them here would teach the wrong gesture for the
#' thing the rail is best at.
#'
#' Clearing the outline's own tick on the view it is shown in removes the panel
#' you are clicking in, so that one box arms on the first click and commits on
#' the second. It is not a one-way door -- blockr.dock's per-view `+` (Add
#' panel) picker lists extensions alongside blocks.
#'
#' @section What the outline does not do:
#' Creating, renaming, reordering and removing views belongs to the dock's own
#' navbar, which has had all four since before this extension existed, and whose
#' new-view flow lets you pick the blocks and extensions to seed a page with.
#' The outline says which panels go where, and nothing about the pages
#' themselves.
#'
#' @section Keeping the outline clear:
#' A block added from the outline is placed by the outline: appended blocks open
#' beside the block they read from, and a block added with no origin opens
#' among the other panels. Blocks added by other routes -- the navbar's block
#' browser, the DAG canvas -- are placed by blockr.dock's
#' `determine_panel_pos()`, which drops a new panel into the last active
#' group. The outline's own group is a candidate for that unless it is named in
#' the `blockr.visible_extensions` option, which defaults to the DAG alone.
#'
#' So an app that mounts the outline and wants nothing ever stacked onto it
#' names it there, using the **mount name** it gave the extension:
#'
#' ```r
#' options(blockr.visible_extensions = c("dag", "outline"))
#'
#' new_dock_board(
#'   extensions = list(outline = new_outline_extension()),
#'   ...
#' )
#' ```
#'
#' This is a global option rather than a board one, so it is set once per
#' deployment.
#'
#' @param ... Forwarded to [blockr.dock::new_dock_extension()]
#'
#' @return A `outline_extension` object as constructed by
#' [blockr.dock::new_dock_extension()].
#'
#' @export
new_outline_extension <- function(...) {
  blockr.dock::new_dock_extension(
    outline_ext_srv,
    outline_ext_ui,
    name = "Outline",
    description = paste(
      "Compact list-shaped workflow editor: blocks as rows in topological",
      "order with a commit-graph rail for the connections. Supports the",
      "same operations as the DAG canvas (connect respecting block input",
      "arity, append, remove, stacks, status indicators) in a fraction of",
      "the space."
    ),
    class = "outline_extension",
    ...
  )
}

outline_ext_ui <- function(id, board, ...) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    outline_js_dep(),
    htmltools::div(
      id = ns("outline"),
      class = "outline",
      `data-ns` = ns("")
    )
  )
}

#' HTML dependency for the outline rail renderer
#'
#' The list+rail editor *without* the board adapter: `outlineRail.create(el,
#' adapter)` plus the stylesheet, for a host that drives it with an adapter of
#' its own. blockr.process uses this to edit a process definition -- its nodes
#' are steps and its edges are dependencies -- with the same rows, rail,
#' dots and gestures the board editor uses.
#'
#' See the header of `inst/assets/js/outline-rail.js` for the adapter
#' contract.
#'
#' @return An [htmltools::htmlDependency()] list.
#'
#' @export
outline_rail_dep <- memoise0(function() {
  htmltools::tagList(
    outline_css_dep(),
    htmlDependency(
      name = "outline-rail",
      version = pkg_version(),
      src = pkg_file("assets", "js"),
      # order is load-bearing: `outline-rail.js` reads `outlineLayout` off
      # the global
      script = c("outline-layout.js", "outline-rail.js")
    )
  )
})

outline_js_dep <- memoise0(function() {
  htmltools::tagList(
    outline_rail_dep(),
    htmlDependency(
      name = "outline",
      version = pkg_version(),
      src = pkg_file("assets", "js"),
      # the board adapter, which reads `outlineRail` off the global
      script = "outline.js"
    )
  )
})

outline_css_dep <- memoise0(function() {
  htmlDependency(
    name = "outline-css",
    version = pkg_version(),
    src = pkg_file("assets", "css"),
    stylesheet = "outline.css"
  )
})

# The full board model as one JSON-ready payload. Pushed wholesale on every
# board change: the outline is a stateless list (no user-owned positions to
# preserve), so a full re-render is both cheap and always consistent --
# no delta bookkeeping as in blockr.dag's incremental g6 proxy.
outline_payload <- function(board) {

  blocks <- blockr.core::board_blocks(board)
  links <- blockr.core::board_links(board)
  stacks <- blockr.core::board_stacks(board)

  meta <- if (length(blocks)) blockr.dock::blks_metadata(blocks)

  # The block's TYPE rather than its rendered icon. An icon is a property of
  # the type, not of the board, so it travels once with the catalogue and the
  # client looks it up -- where it used to be a base64 data URI rebuilt and
  # resent on every board change. On the CDEX board that was 93 KB of a
  # 114 KB payload, and only 21 of the 93 strings were distinct. Shiny's
  # websocket does not compress, so those were 93 KB on the wire each time a
  # filter moved. A type the catalogue does not know falls back to the
  # letter tile client-side.
  blk_entry <- function(i) {
    b <- blocks[[i]]
    list(
      id = names(blocks)[i],
      name = blockr.core::block_name(b),
      type = class(b)[[1L]],
      category = meta$category[i],
      color = meta$color[i],
      inputs = I(as.list(blockr.core::block_inputs(b))),
      variadic = is.na(blockr.core::block_arity(b))
    )
  }

  lnk_entry <- function(i) {
    list(
      id = names(links)[i],
      from = links$from[i],
      to = links$to[i],
      input = links$input[i]
    )
  }

  stk_entry <- function(id) {
    s <- stacks[[id]]
    col <- attr(s, "color")
    list(
      id = id,
      name = blockr.core::stack_name(s),
      color = if (is.character(col)) col,
      blocks = I(as.list(blockr.core::stack_blocks(s)))
    )
  }

  list(
    blocks = lapply(seq_along(blocks), blk_entry),
    links = lapply(seq_along(links), lnk_entry),
    stacks = lapply(names(stacks), stk_entry),
    views = outline_views(board),
    extensions = outline_extensions(board)
  )
}

# Views as the outline sees them: membership stated in OBJECT ids (block ids and
# extension mount names), not panel ids. The outline's rows are objects, so a
# panel id would have to be unwrapped on every comparison.
#
# `blocks` and `extensions` are two lists rather than one because the outline
# draws them in two places -- blocks in the rail, in topological order, and
# extensions
# in a group at the foot, since an extension is not in the DAG and has no place
# in an order derived from it. The membership relation is the same for both, and
# so is the control that edits it.
outline_views <- function(board) {

  # Views are a dock concept. The outline renders a plain `blockr.core` board
  # too (its rail needs blocks and links, nothing else), and there the
  # answer is "no views" rather than an error -- the client then draws no
  # membership column and no trigger at all.
  if (!blockr.dock::is_dock_board(board)) {
    return(list())
  }

  views <- blockr.dock::board_views(board)
  labels <- blockr.dock::view_names(views)
  active <- blockr.dock::active_view(views)

  # Canonical block-panel id per block on the board, so membership is
  # matched exactly rather than by unwrapping and hoping a block id and an
  # extension mount name never coincide.
  blk_ids <- names(blockr.core::board_blocks(board))
  panels <- as.character(blockr.dock::as_block_panel_id(blk_ids))

  ext_ids <- blockr.dock::dock_ext_ids(board)
  ext_panels <- as.character(blockr.dock::as_ext_panel_id(ext_ids))

  vw_entry <- function(id) {
    members <- blockr.dock::view_members(views[[id]])
    list(
      id = id,
      name = unname(labels[id]),
      active = identical(id, active),
      # Total panels on the view, which the menu's checklist shows beside each
      # name. It counts everything the view holds, not just what the outline has
      # a row for: the question it answers is "how full is that page".
      n = length(members),
      blocks = I(as.list(blk_ids[panels %in% members])),
      extensions = I(as.list(ext_ids[ext_panels %in% members]))
    )
  }

  lapply(names(views), vw_entry)
}

# The extension catalogue: one entry per extension mounted on the board, keyed
# by mount name, in mount order. This is what gives an extension a ROW --
# including one that is on no view at all, which per-view membership alone
# could never report.
#
# `self` marks the outline's own entry. It is not special-cased anywhere in the
# membership logic (its row works exactly like the others); the client only uses
# it to say "this one is the panel you are looking at" when taking it off the
# view you are on.
outline_extensions <- function(board) {

  if (!blockr.dock::is_dock_board(board)) {
    return(list())
  }

  exts <- blockr.dock::dock_extensions(board)
  me <- blockr.dock::extension_ids(board, "outline_extension")

  tool_entry <- function(id) {
    list(
      id = id,
      name = blockr.dock::extension_name(exts[[id]]),
      self = id %in% me
    )
  }

  lapply(names(exts), tool_entry)
}

# Every membership write the row menu can make, as one delta.
#
# `mode` is the whole vocabulary:
#
#   "all"   every view gains the panels it lacks
#   "none"  every view loses the panels it holds
#   "only"  `view` gains them, every other view loses them
#   "add"   `view` gains them
#   "rm"    `view` loses them
#
# The first three are the menu's presets (drawn as radios, because they are
# states of the checklist below them rather than separate commands); the last
# two are one checkbox being ticked or cleared. Blocks and extensions travel in
# the same call because membership does not distinguish them -- only the
# panel-id
# prefix does, and that is settled here.
#
# Per view, membership is re-derived from the committed board and only the
# difference is emitted, so a stale client cannot ask to add a member or remove
# a non-member; both are `validate_view_mod()` errors rather than no-ops. A view
# that needs no change is not named at all, and a delta that would change
# nothing anywhere is NULL.
outline_membership_delta <- function(board, blocks = character(),
                                     extensions = character(),
                                     mode = c("all", "none", "only", "add",
                                              "rm"),
                                     view = NULL) {

  mode <- match.arg(mode)

  if (!blockr.dock::is_dock_board(board)) {
    return(NULL)
  }

  views <- blockr.dock::board_views(board)

  blocks <- intersect(blocks, names(blockr.core::board_blocks(board)))
  extensions <- intersect(extensions, blockr.dock::dock_ext_ids(board))

  pids <- c(
    as.character(blockr.dock::as_block_panel_id(blocks)),
    as.character(blockr.dock::as_ext_panel_id(extensions))
  )

  if (!length(pids)) {
    return(NULL)
  }

  # Every mode but "all" and "none" names one view, so it has to exist.
  if (!mode %in% c("all", "none") && !isTRUE(view %in% names(views))) {
    return(NULL)
  }

  # Which views this touches, and whether each of them ends up holding the
  # panels. "add" and "rm" speak about one view and leave the others alone,
  # which is why they are not expressed as a `keep` over all of them.
  targets <- switch(
    mode,
    all = names(views),
    none = names(views),
    only = names(views),
    view
  )

  holds <- function(v) {
    switch(
      mode,
      all = TRUE,
      none = FALSE,
      only = identical(v, view),
      add = TRUE,
      rm = FALSE
    )
  }

  ops <- list()

  for (v in targets) {

    members <- blockr.dock::view_members(views[[v]])

    if (holds(v)) {
      grow <- setdiff(pids, members)
      if (length(grow)) {
        # No placement hint: an un-landed member renders through the default
        # grid and the client echo mirrors back wherever it is dropped.
        ops[[v]] <- list(
          add = stats::setNames(rep(list(list()), length(grow)), grow)
        )
      }
    } else {
      shrink <- intersect(pids, members)
      if (length(shrink)) {
        ops[[v]] <- list(rm = shrink)
      }
    }
  }

  if (!length(ops)) {
    return(NULL)
  }

  list(views = list(mod = ops))
}

# Reveal a panel in the *current* view: focus it if the view already holds it,
# otherwise add it there -- never switch to another view that happens to hold
# it (same semantics as blockr.dag's node click).
#
# Blocks and extensions differ only in how the panel id is spelled. Clicking an
# extension row is the same gesture as clicking a block row, and an extension
# on NO view has no other way back onto a page, so the click has to be able to
# mount it rather than only focus it.
outline_reveal_delta <- function(board, block = NULL, extension = NULL) {

  views <- blockr.dock::board_views(board)
  view <- blockr.dock::active_view(views)

  if (is.null(view)) {
    return(NULL)
  }

  if (length(extension)) {
    if (!all(extension %in% blockr.dock::dock_ext_ids(board))) {
      return(NULL)
    }
    pid <- as.character(blockr.dock::as_ext_panel_id(extension))
  } else {
    pid <- as.character(blockr.dock::as_block_panel_id(block))
  }

  ops <- if (pid %in% blockr.dock::view_members(views[[view]])) {
    list(select = pid)
  } else {
    list(add = stats::setNames(list(list()), pid), select = pid)
  }

  list(views = list(mod = stats::setNames(list(ops), view)))
}

# Where a block inserted from the outline should land.
#
# Without a hint, blockr.dock falls back to `determine_panel_pos()`, which
# stacks the new panel into the last active group -- and the outline's own group
# qualifies, because only panels named in the `visible_extensions` board
# option are excluded and that option defaults to the DAG alone. So a block
# added from the outline lands on top of it, hiding the extension that added it.
#
# Appending has a better answer than "not there" anyway: put it beside the
# block it reads from, which is where you are looking. With no origin there is
# nothing to sit beside, so it asks for `right` -- which dockview resolves
# against the active group, in practice landing it among the other block
# panels rather than splitting a fresh column. Either way it is not the outline.
outline_place_delta <- function(board, blk_id, from = NULL) {

  views <- blockr.dock::board_views(board)
  view <- blockr.dock::active_view(views)

  if (is.null(view)) {
    return(NULL)
  }

  pid <- as.character(blockr.dock::as_block_panel_id(blk_id))

  hint <- list(side = "right")

  if (length(from) == 1L && !is.na(from)) {

    origin <- as.character(blockr.dock::as_block_panel_id(from))

    # `near` must name a member of the view as it stands, or the delta is
    # rejected; an origin that is not on this page falls back to the right.
    if (origin %in% blockr.dock::view_members(views[[view]])) {
      hint <- list(near = origin, side = "within")
    }
  }

  list(
    mod = stats::setNames(
      list(list(add = stats::setNames(list(hint), pid), select = pid)),
      view
    )
  )
}

# Move blocks between stacks: into `stack`, or out of whichever stack holds
# them when `stack` is NULL.
#
# A block belongs to at most one stack, so joining one means leaving another,
# and both halves have to travel in ONE update -- an intermediate board where
# a block sits in two stacks does not validate. `mod` deltas are partial
# constructor arguments applied through `update_stack()`, so the reserved
# `blocks` key carries the whole new membership rather than a diff (the same
# shape blockr.dock's own stack editor commits).
outline_stack_delta <- function(board, ids, stack = NULL) {

  stacks <- blockr.core::board_stacks(board)
  ids <- intersect(ids, names(blockr.core::board_blocks(board)))

  if (!length(ids) || (!is.null(stack) && !stack %in% names(stacks))) {
    return(NULL)
  }

  mods <- list()

  for (id in names(stacks)) {
    cur <- blockr.core::stack_blocks(stacks[[id]])
    new <- if (identical(id, stack)) union(cur, ids) else setdiff(cur, ids)
    if (!setequal(new, cur)) {
      mods[[id]] <- list(blocks = new)
    }
  }

  if (!length(mods)) {
    return(NULL)
  }

  # A stack the move empties goes with it rather than lingering as a husk on
  # the board -- the same call cutting a whole stack makes. It leaves through
  # `rm`, never `mod`: `validate_mod_deltas()` runs against the post-removal
  # set, so naming an id in both is an error.
  empty <- names(mods)[!lengths(lapply(mods, `[[`, "blocks"))]
  keep <- setdiff(names(mods), empty)

  upd <- list(stacks = list())

  if (length(keep)) {
    upd$stacks$mod <- mods[keep]
  }

  if (length(empty)) {
    upd$stacks$rm <- empty
  }

  upd
}

# Group a selection into a NEW stack.
#
# A block belongs to at most one stack, so the blocks that move have to leave
# the one they were in. They cannot do it through a `mod`: blockr.core checks
# the stacks a payload ADDS against the raw membership the board already has,
# so an `add` naming a block that is still listed elsewhere is rejected even
# when a `mod` in the same update is what frees it.
#
# So a stack the selection reaches into is dropped and put back without those
# blocks, carrying its name, its colour and every other constructor argument
# across through `update_stack()` -- but under a NEW id. Re-adding the id it
# was just dropped under is a payload the DAG canvas cannot draw: it adds
# combos before it removes them, so g6 refuses the combo that still exists and
# the whole add fails, new stack included. One the move empties is dropped and
# not put back.
#
# What that costs: anything keyed by the old stack id -- a report chapter
# description in `stack_annotations`, a saved panel handle -- no longer matches
# the stack that came back.
outline_stack_new <- function(board, ids, name = "New stack") {

  ids <- intersect(ids, names(blockr.core::board_blocks(board)))

  if (length(ids) < 2L) {
    return(NULL)
  }

  stacks <- blockr.core::board_stacks(board)

  drop <- character()
  add <- list()

  for (id in names(stacks)) {

    cur <- blockr.core::stack_blocks(stacks[[id]])
    keep <- setdiff(cur, ids)

    if (setequal(keep, cur)) {
      next
    }

    drop <- c(drop, id)

    if (length(keep)) {
      # unnamed, so `stacks()` mints the id
      add <- c(
        add,
        list(blockr.core::update_stack(stacks[[id]], list(blocks = keep)))
      )
    }
  }

  add <- c(
    add, list(blockr.dock::new_dock_stack(blocks = ids, name = name))
  )

  upd <- list(stacks = list(add = do.call(blockr.core::stacks, add)))

  if (length(drop)) {
    upd$stacks$rm <- drop
  }

  upd
}

outline_ext_result <- function(board, extensions) {
  extensions[[
    blockr.dock::extension_ids(
      shiny::isolate(board$board),
      "outline_extension"
    )
  ]]
}

#' @importFrom blockr.dock extension_block_callback
#' @export
extension_block_callback.outline_extension <- function(x, ...) {
  function(id, board, update, conditions, extensions, ...,
           session = shiny::getDefaultReactiveDomain()) {

    mini <- outline_ext_result(board, extensions)

    badge <- shiny::reactive({
      errors <- sum(lengths(conditions()$error))
      status <- board$eval[[id]]
      if (is.function(status)) {
        status <- status()
      }
      list(
        spec = blockr.dock::block_status_badge(status, errors),
        status = if (errors > 0L) "failed" else status
      )
    })

    drawn <- shiny::reactiveVal(NULL)

    shiny::observeEvent(
      list(mini$ready(), badge()),
      {
        shiny::req(mini$ready())

        res <- badge()
        spec <- res$spec

        # `NA` means the block is dormant: its status is not currently
        # computed, so leave the last-known dot rather than clearing it.
        if (isTRUE(is.na(spec)) || identical(spec, drawn())) {
          return()
        }

        mini$send(
          "badge",
          if (is.null(spec)) {
            list(id = id)
          } else {
            list(
              id = id,
              color = spec$color,
              label = spec$label,
              status = res$status
            )
          }
        )

        drawn(spec)
      },
      label = paste0("outline_badge_", id)
    )

    NULL
  }
}

#' @rdname new_outline_extension
#' @usage NULL
#' @export
new_minidag_extension <- function(...) {
  .Deprecated("new_outline_extension")
  new_outline_extension(...)
}

#' @rdname outline_rail_dep
#' @usage NULL
#' @export
minidag_rail_dep <- function() {
  .Deprecated("outline_rail_dep")
  outline_rail_dep()
}
