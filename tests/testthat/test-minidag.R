test_that("minidag extension satisfies the dock extension contract", {
  ext <- new_minidag_extension()

  expect_true(blockr.dock::is_dock_extension(ext))
  expect_identical(blockr.dock::extension_id(ext), "minidag_extension")
  expect_identical(blockr.dock::extension_name(ext), "Mini deck")
  expect_no_error(blockr.dock::validate_extension(ext))

  # the type-derived key addresses the extension in views/grids
  exts <- blockr.dock::new_dock_extensions(list(ext))
  expect_named(exts, "minidag")
})

test_that("minidag_payload carries arity for every block shape", {
  board <- blockr.core::new_board(
    blocks = c(
      d1 = blockr.core::new_dataset_block("iris"),
      h1 = blockr.core::new_head_block(block_name = "First rows"),
      m1 = blockr.core::new_merge_block(by = "Species"),
      r1 = blockr.core::new_rbind_block()
    ),
    links = blockr.core::links(
      from = c("d1", "d1", "d1", "h1"),
      to = c("h1", "m1", "r1", "r1"),
      input = c("data", "x", "", "")
    ),
    stacks = blockr.core::stacks(
      prep = blockr.dock::new_dock_stack(
        c("d1", "h1"),
        name = "Prep",
        color = "#2563eb"
      )
    )
  )

  pay <- minidag_payload(board)

  expect_named(pay, c("blocks", "links", "stacks", "views"))
  expect_length(pay$blocks, 4L)
  expect_length(pay$links, 4L)
  expect_length(pay$stacks, 1L)

  blks <- stats::setNames(pay$blocks, vapply(pay$blocks, `[[`, "", "id"))

  # data block: no inputs, not variadic
  expect_length(blks$d1$inputs, 0L)
  expect_false(blks$d1$variadic)

  # transform block: exactly one named input
  expect_identical(as.character(blks$h1$inputs), "data")
  expect_false(blks$h1$variadic)

  # 2-ary block: two named slots
  expect_identical(as.character(blks$m1$inputs), c("x", "y"))
  expect_false(blks$m1$variadic)

  # variadic block: no named slots, flagged variadic
  expect_length(blks$r1$inputs, 0L)
  expect_true(blks$r1$variadic)

  expect_identical(blks$h1$name, "First rows")
  expect_true(all(vapply(pay$blocks, function(b) is.character(b$icon), NA)))

  lnk <- pay$links[[2L]]
  expect_named(lnk, c("id", "from", "to", "input"))
  expect_identical(lnk$from, "d1")
  expect_identical(lnk$to, "m1")
  expect_identical(lnk$input, "x")

  stk <- pay$stacks[[1L]]
  expect_identical(stk$id, "prep")
  expect_identical(stk$name, "Prep")
  expect_identical(stk$color, "#2563eb")
  expect_identical(as.character(stk$blocks), c("d1", "h1"))
})

test_that("minidag_payload survives an empty board", {
  pay <- minidag_payload(blockr.core::new_board())
  expect_identical(pay$blocks, list())
  expect_identical(pay$links, list())
  expect_identical(pay$stacks, list())
})

test_that("reveal delta targets the active view", {
  board <- blockr.dock::new_dock_board(
    blocks = c(d1 = blockr.core::new_dataset_block("iris"))
  )

  delta <- minidag_reveal_delta(board, "d1")

  view <- blockr.dock::active_view(blockr.dock::board_views(board))
  expect_named(delta, "views")
  expect_named(delta$views, "mod")
  expect_named(delta$views$mod, view)

  ops <- delta$views$mod[[view]]
  pid <- as.character(blockr.dock::as_block_panel_id("d1"))
  expect_identical(ops$select, pid)
})

test_that("block callback generator returns a server function", {
  cb <- extension_block_callback(new_minidag_extension())
  expect_true(is.function(cb))
  expect_true(all(
    c("id", "board", "update", "conditions", "extensions") %in%
      names(formals(cb))
  ))
})

test_that("minidag_views states membership in block ids, not panel ids", {

  board <- blockr.dock::new_dock_board(
    blocks = c(
      d1 = blockr.core::new_dataset_block("iris"),
      h1 = blockr.core::new_head_block()
    ),
    links = blockr.core::links(from = "d1", to = "h1"),
    extensions = list(mini = new_minidag_extension()),
    views = list(
      one = blockr.dock::dock_view(c("mini", "d1"), name = "One"),
      two = blockr.dock::dock_view(c("d1", "h1"), name = "Two")
    ),
    active = "two"
  )

  views <- minidag_views(board)
  expect_length(views, 2L)

  vw <- stats::setNames(views, vapply(views, `[[`, "", "id"))

  expect_identical(vw$one$name, "One")
  expect_identical(vw$two$name, "Two")

  # the extension panel is a member of `one` but has no row in the deck, so
  # it must not appear -- membership the deck cannot show, it must not offer
  # to toggle
  expect_identical(as.character(vw$one$blocks), "d1")
  expect_identical(as.character(vw$two$blocks), c("d1", "h1"))

  # `active` is a property of the collection, reported per view
  expect_false(vw$one$active)
  expect_true(vw$two$active)
})

test_that("minidag_views is empty on a board that has no views", {
  # the deck renders a plain core board too; views are a dock concept
  expect_identical(minidag_views(blockr.core::new_board()), list())
})

test_that("the registry splits into add and append pools", {

  reg <- minidag_registry()

  expect_named(reg, c("add", "append"))
  expect_true(length(reg$add) > 0L)

  # Appending links the source INTO the new block, so every candidate must be
  # able to receive one: a named input slot, or variadic arity. A source-only
  # block (a dataset block, arity 0) can be added but never appended.
  can_receive <- function(m) length(m$inputs) > 0L || isTRUE(m$variadic)
  expect_true(all(vapply(reg$append, can_receive, logical(1))))
  expect_true(length(reg$append) <= length(reg$add))

  types <- vapply(reg$add, `[[`, "", "type")
  expect_setequal(types, names(blockr.core::available_blocks()))

  # a dataset block takes no input, so it is offered for add and withheld
  # from append
  expect_true("dataset_block" %in% types)
  expect_false("dataset_block" %in% vapply(reg$append, `[[`, "", "type"))

  entry <- reg$add[[match("dataset_block", types)]]
  expect_true(nzchar(entry$name))
  expect_true(nzchar(entry$category))
  expect_true(nzchar(entry$package))
})

test_that("a block inserted from the deck lands beside its origin", {

  board <- blockr.dock::new_dock_board(
    blocks = c(
      d1 = blockr.core::new_dataset_block("iris"),
      h1 = blockr.core::new_head_block()
    ),
    links = blockr.core::links(from = "d1", to = "h1"),
    extensions = list(mini = new_minidag_extension()),
    views = list(one = blockr.dock::dock_view(c("mini", "d1", "h1")))
  )

  view <- blockr.dock::active_view(blockr.dock::board_views(board))
  pid <- as.character(blockr.dock::as_block_panel_id("new1"))

  # appended: beside the block it reads from, which is where the eye is
  delta <- minidag_place_delta(board, "new1", from = "h1")
  hint <- delta$mod[[view]]$add[[pid]]
  expect_identical(
    hint$near, as.character(blockr.dock::as_block_panel_id("h1"))
  )
  expect_identical(hint$side, "within")
  expect_identical(delta$mod[[view]]$select, pid)

  # no origin: nothing to sit beside
  bare <- minidag_place_delta(board, "new1", from = NULL)
  expect_null(bare$mod[[view]]$add[[pid]]$near)
  expect_identical(bare$mod[[view]]$add[[pid]]$side, "right")

  # an origin that is not on this page cannot be a `near` anchor -- naming it
  # would have the delta rejected outright
  off <- blockr.dock::new_dock_board(
    blocks = c(
      d1 = blockr.core::new_dataset_block("iris"),
      h1 = blockr.core::new_head_block()
    ),
    links = blockr.core::links(from = "d1", to = "h1"),
    extensions = list(mini = new_minidag_extension()),
    views = list(one = blockr.dock::dock_view(c("mini", "d1")))
  )
  elsewhere <- minidag_place_delta(off, "new1", from = "h1")
  expect_null(elsewhere$mod[["one"]]$add[[pid]]$near)
  expect_identical(elsewhere$mod[["one"]]$add[[pid]]$side, "right")
})

test_that("the clipboard round-trips a selection with fresh ids", {

  board <- blockr.dock::new_dock_board(
    blocks = c(
      a = blockr.core::new_dataset_block("iris"),
      b = blockr.core::new_head_block(n = 3L),
      c = blockr.core::new_head_block(n = 9L)
    ),
    links = blockr.core::links(
      from = c("a", "b"), to = c("b", "c"), input = c("data", "data")
    ),
    stacks = blockr.core::stacks(
      s1 = blockr.dock::new_dock_stack(c("a", "b"), name = "Prep",
                                       color = "#2563eb")
    ),
    extensions = list(mini = new_minidag_extension())
  )

  json <- minidag_clip_json(board, c("a", "b"), list())

  # blockr.dag's envelope, so a canvas copy pastes here and back
  expect_identical(jsonlite::fromJSON(json)$object, "subboard")

  delta <- minidag_paste_delta(board, json)

  expect_length(delta$blocks$add, 2L)
  expect_false(
    any(names(delta$blocks$add) %in%
          names(blockr.core::board_blocks(board)))
  )

  # the a -> b link travels because both ends were copied; b -> c does not,
  # since pasting it would wire the copy to a block nobody copied
  ld <- as.data.frame(delta$links$add)
  expect_equal(nrow(ld), 1L)
  expect_true(all(c(ld$from, ld$to) %in% names(delta$blocks$add)))

  # a stack the selection covers comes along, renamed and re-pointed
  expect_length(delta$stacks$add, 1L)
  expect_identical(
    blockr.core::stack_name(delta$stacks$add[[1L]]), "Prep (copy)"
  )
  expect_true(
    all(blockr.core::stack_blocks(delta$stacks$add[[1L]]) %in%
          names(delta$blocks$add))
  )

  # one block on its own carries no links at all
  solo <- minidag_paste_delta(board, minidag_clip_json(board, "b", list()))
  expect_null(solo$links)

  expect_null(minidag_paste_delta(board, '{"object":"nope"}'))
  expect_null(minidag_paste_delta(board, "not json at all"))
})

test_that("a NULL state field survives the clipboard as NULL", {

  # blockr.dag#144: without `null = "null"` every NULL state field becomes
  # `{}` in JSON and comes back as an empty `list()`, which poisons the
  # pasted block -- and survives a save, re-emitting on the next copy.
  board <- blockr.dock::new_dock_board(
    blocks = c(a = blockr.core::new_dataset_block("iris")),
    extensions = list(mini = new_minidag_extension())
  )

  json <- minidag_clip_json(
    board, "a", list(a = list(dataset = "iris", row_color = NULL))
  )

  expect_match(json, '"row_color":null', fixed = TRUE)
  expect_false(grepl('"row_color":{}', json, fixed = TRUE))

  back <- jsonlite::fromJSON(
    json, simplifyDataFrame = FALSE, simplifyMatrix = FALSE
  )
  expect_null(back$payload$blocks$payload$a$payload$state$row_color)
})
