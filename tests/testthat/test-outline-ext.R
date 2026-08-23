test_that("outline extension satisfies the dock extension contract", {
  ext <- new_outline_extension()

  expect_true(blockr.dock::is_dock_extension(ext))
  expect_identical(blockr.dock::extension_id(ext), "outline_extension")
  expect_identical(blockr.dock::extension_name(ext), "Outline")
  expect_no_error(blockr.dock::validate_extension(ext))

  # the type-derived key addresses the extension in views/grids
  exts <- blockr.dock::new_dock_extensions(list(ext))
  expect_named(exts, "outline")
})

test_that("outline_payload carries arity for every block shape", {
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

  pay <- outline_payload(board)

  expect_named(pay, c("blocks", "links", "stacks", "views", "extensions"))
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
  # the TYPE travels with the block, not the rendered icon: the glyph belongs
  # to the type, so it rides the catalogue once instead of every board change
  expect_true(all(vapply(pay$blocks, function(b) is.character(b$type), NA)))
  expect_true(all(vapply(pay$blocks, function(b) is.null(b$icon), NA)))

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

test_that("outline_payload survives an empty board", {
  pay <- outline_payload(blockr.core::new_board())
  expect_identical(pay$blocks, list())
  expect_identical(pay$links, list())
  expect_identical(pay$stacks, list())
})

test_that("reveal delta targets the active view", {
  board <- blockr.dock::new_dock_board(
    blocks = c(d1 = blockr.core::new_dataset_block("iris"))
  )

  delta <- outline_reveal_delta(board, "d1")

  view <- blockr.dock::active_view(blockr.dock::board_views(board))
  expect_named(delta, "views")
  expect_named(delta$views, "mod")
  expect_named(delta$views$mod, view)

  ops <- delta$views$mod[[view]]
  pid <- as.character(blockr.dock::as_block_panel_id("d1"))
  expect_identical(ops$select, pid)
})

test_that("block callback generator returns a server function", {
  cb <- extension_block_callback(new_outline_extension())
  expect_true(is.function(cb))
  expect_true(all(
    c("id", "board", "update", "conditions", "extensions") %in%
      names(formals(cb))
  ))
})

test_that("outline_views states membership in block ids, not panel ids", {

  board <- blockr.dock::new_dock_board(
    blocks = c(
      d1 = blockr.core::new_dataset_block("iris"),
      h1 = blockr.core::new_head_block()
    ),
    links = blockr.core::links(from = "d1", to = "h1"),
    extensions = list(mini = new_outline_extension()),
    views = list(
      one = blockr.dock::dock_view(c("mini", "d1"), name = "One"),
      two = blockr.dock::dock_view(c("d1", "h1"), name = "Two")
    ),
    active = "two"
  )

  views <- outline_views(board)
  expect_length(views, 2L)

  vw <- stats::setNames(views, vapply(views, `[[`, "", "id"))

  expect_identical(vw$one$name, "One")
  expect_identical(vw$two$name, "Two")

  # blocks and extensions are reported in two lists, both in OBJECT ids: the
  # outline draws them in two places (the rail, and the extensions group at the foot)
  # but the membership relation is one relation
  expect_identical(as.character(vw$one$blocks), "d1")
  expect_identical(as.character(vw$two$blocks), c("d1", "h1"))
  expect_identical(as.character(vw$one$extensions), "mini")
  expect_length(vw$two$extensions, 0L)

  # `active` is a property of the collection, reported per view
  expect_false(vw$one$active)
  expect_true(vw$two$active)

  # the panel count the menu's checklist shows: everything the view holds, not
  # just what the outline has a row for
  expect_identical(vw$one$n, 2L)
  expect_identical(vw$two$n, 2L)
})

test_that("outline_extensions gives every mounted extension a row, view or no view", {

  board <- blockr.dock::new_dock_board(
    blocks = c(d1 = blockr.core::new_dataset_block("iris")),
    extensions = list(
      outline = new_outline_extension(),
      deck = new_slides_extension()
    ),
    views = list(one = blockr.dock::dock_view(c("outline", "d1")))
  )

  extensions <- outline_extensions(board)

  # mount order, and the DECK is here despite being on no view at all: the
  # catalogue comes from the board, which is the whole reason a row can report
  # "nowhere"
  expect_identical(vapply(extensions, `[[`, "", "id"), c("outline", "deck"))
  expect_identical(extensions[[1L]]$name, "Outline")

  # `self` marks our own row, so the client can guard the one gesture that
  # removes the panel being clicked in
  expect_true(extensions[[1L]]$self)
  expect_false(extensions[[2L]]$self)

  # a plain core board carries no extensions and no views
  expect_identical(outline_extensions(blockr.core::new_board()), list())
})

test_that("outline_membership_delta covers the whole mode vocabulary", {

  board <- blockr.dock::new_dock_board(
    blocks = c(
      d1 = blockr.core::new_dataset_block("iris"),
      h1 = blockr.core::new_head_block()
    ),
    links = blockr.core::links(from = "d1", to = "h1"),
    extensions = list(
      outline = new_outline_extension(),
      deck = new_slides_extension()
    ),
    views = list(
      one = blockr.dock::dock_view(c("outline", "d1")),
      two = blockr.dock::dock_view("d1"),
      three = blockr.dock::dock_view("h1")
    )
  )

  d1 <- "block_panel-d1"
  mini <- "ext_panel-outline"

  # "all": blocks and extensions travel in the SAME call, and only the views that
  # actually lack a panel are named
  all <- outline_membership_delta(
    board, blocks = "d1", extensions = "outline", mode = "all"
  )
  expect_named(all$views$mod, c("two", "three"))
  expect_identical(names(all$views$mod$two$add), mini)
  expect_setequal(names(all$views$mod$three$add), c(d1, mini))

  # "none": every view that holds it loses it
  none <- outline_membership_delta(board, blocks = "d1", mode = "none")
  expect_named(none$views$mod, c("one", "two"))
  expect_identical(none$views$mod$one, list(rm = d1))

  # "only": the named view gains, every other loses
  only <- outline_membership_delta(
    board, blocks = "d1", mode = "only", view = "three"
  )
  expect_named(only$views$mod, c("one", "two", "three"))
  expect_identical(only$views$mod$one, list(rm = d1))
  expect_identical(names(only$views$mod$three$add), d1)

  # "add" / "rm": ONE view, the others untouched -- which is what a single
  # checkbox means, and why they are not expressed as a set over all views
  add1 <- outline_membership_delta(
    board, blocks = "d1", mode = "add", view = "three"
  )
  expect_named(add1$views$mod, "three")

  rm1 <- outline_membership_delta(
    board, extensions = "outline", mode = "rm", view = "one"
  )
  expect_named(rm1$views$mod, "one")
  expect_identical(rm1$views$mod$one, list(rm = mini))

  # nothing to do is NULL, not an empty `mod`: a stale client clicking a box
  # twice inside one round-trip must not reach `validate_view_mod()`
  expect_null(outline_membership_delta(
    board, blocks = "d1", mode = "add", view = "one"
  ))
  expect_null(outline_membership_delta(
    board, extensions = "outline", mode = "rm", view = "two"
  ))

  # unknown ids drop out; an empty set, a mode needing a view without one, and a
  # plain core board are all NULL rather than an error
  expect_null(outline_membership_delta(board, blocks = "nope", mode = "all"))
  expect_null(outline_membership_delta(board, mode = "all"))
  expect_null(outline_membership_delta(board, blocks = "d1", mode = "only"))
  expect_null(outline_membership_delta(
    board, blocks = "d1", mode = "add", view = "nope"
  ))
  expect_null(outline_membership_delta(
    blockr.core::new_board(), blocks = "d1", mode = "all"
  ))
})

test_that("a membership delta round-trips through the update lifecycle", {

  board <- blockr.dock::new_dock_board(
    blocks = c(
      d1 = blockr.core::new_dataset_block("iris"),
      h1 = blockr.core::new_head_block()
    ),
    links = blockr.core::links(from = "d1", to = "h1"),
    extensions = list(outline = new_outline_extension()),
    views = list(
      one = blockr.dock::dock_view(c("outline", "d1")),
      two = blockr.dock::dock_view("h1"),
      three = blockr.dock::dock_view("h1")
    )
  )

  commit <- function(x, upd) {
    upd <- blockr.core::augment_board_update(upd, x)
    blockr.core::validate_board_update(upd, x)
    blockr.core::apply_board_update(x, upd)
  }

  members <- function(x, v) {
    blockr.dock::view_members(blockr.dock::board_views(x)[[v]])
  }

  # one preset, a block and an extension, every view
  grown <- commit(board, outline_membership_delta(
    board, blocks = "d1", extensions = "outline", mode = "all"
  ))
  for (v in c("one", "two", "three")) {
    expect_true(all(c("block_panel-d1", "ext_panel-outline") %in%
                      members(grown, v)))
  }

  # one checkbox cleared: that view only
  ticked <- commit(grown, outline_membership_delta(
    grown, blocks = "d1", mode = "rm", view = "two"
  ))
  expect_false("block_panel-d1" %in% members(ticked, "two"))
  expect_true("block_panel-d1" %in% members(ticked, "three"))

  # and the other preset
  gone <- commit(ticked, outline_membership_delta(
    ticked, blocks = "d1", mode = "none"
  ))
  for (v in c("one", "two", "three")) {
    expect_false("block_panel-d1" %in% members(gone, v))
  }
  # the extension is untouched: "none" was stated about d1
  expect_true("ext_panel-outline" %in% members(gone, "one"))
})

test_that("the registry splits into add and append pools", {

  reg <- outline_registry()

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

test_that("a block inserted from the outline lands beside its origin", {

  board <- blockr.dock::new_dock_board(
    blocks = c(
      d1 = blockr.core::new_dataset_block("iris"),
      h1 = blockr.core::new_head_block()
    ),
    links = blockr.core::links(from = "d1", to = "h1"),
    extensions = list(mini = new_outline_extension()),
    views = list(one = blockr.dock::dock_view(c("mini", "d1", "h1")))
  )

  view <- blockr.dock::active_view(blockr.dock::board_views(board))
  pid <- as.character(blockr.dock::as_block_panel_id("new1"))

  # appended: beside the block it reads from, which is where the eye is
  delta <- outline_place_delta(board, "new1", from = "h1")
  hint <- delta$mod[[view]]$add[[pid]]
  expect_identical(
    hint$near, as.character(blockr.dock::as_block_panel_id("h1"))
  )
  expect_identical(hint$side, "within")
  expect_identical(delta$mod[[view]]$select, pid)

  # no origin: nothing to sit beside
  bare <- outline_place_delta(board, "new1", from = NULL)
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
    extensions = list(mini = new_outline_extension()),
    views = list(one = blockr.dock::dock_view(c("mini", "d1")))
  )
  elsewhere <- outline_place_delta(off, "new1", from = "h1")
  expect_null(elsewhere$mod[["one"]]$add[[pid]]$near)
  expect_identical(elsewhere$mod[["one"]]$add[[pid]]$side, "right")
})

test_that("a block dragged into a stack leaves the one it was in", {

  board <- blockr.dock::new_dock_board(
    blocks = c(
      d1 = blockr.core::new_dataset_block("iris"),
      h1 = blockr.core::new_head_block(),
      t1 = blockr.core::new_head_block(),
      t2 = blockr.core::new_head_block()
    ),
    links = blockr.core::links(
      from = c("d1", "h1", "t1"),
      to = c("h1", "t1", "t2")
    ),
    stacks = blockr.core::stacks(
      a = blockr.dock::new_dock_stack(c("d1", "h1"), name = "A"),
      b = blockr.dock::new_dock_stack(c("t1", "t2"), name = "B")
    ),
    extensions = list(mini = new_outline_extension())
  )

  # joining B means leaving A, and both halves travel in one update: a board
  # where h1 sits in two stacks does not validate
  join <- outline_stack_delta(board, "h1", "b")
  expect_identical(join$stacks$mod$a$blocks, "d1")
  expect_identical(join$stacks$mod$b$blocks, c("t1", "t2", "h1"))
  expect_null(join$stacks$rm)
  expect_no_error(
    blockr.core::validate_board(
      blockr.core::apply_board_update(board, join)
    )
  )

  # dragged out of every frame: no target stack
  leave <- outline_stack_delta(board, c("t1", "t2"))
  expect_null(leave$stacks$mod)
  expect_identical(leave$stacks$rm, "b")
  expect_no_error(
    blockr.core::validate_board(
      blockr.core::apply_board_update(board, leave)
    )
  )

  # a stack the move empties goes with it, rather than staying as a husk
  moved <- outline_stack_delta(board, c("t1", "t2"), "a")
  expect_identical(moved$stacks$mod$a$blocks, c("d1", "h1", "t1", "t2"))
  expect_identical(moved$stacks$rm, "b")

  # nothing to do, nothing sent
  expect_null(outline_stack_delta(board, "h1", "a"))
  expect_null(outline_stack_delta(board, "nosuchblock", "a"))
  expect_null(outline_stack_delta(board, "h1", "nosuchstack"))
})

test_that("grouping a selection that is stacked splits the stack", {

  board <- blockr.dock::new_dock_board(
    blocks = c(
      d1 = blockr.core::new_dataset_block("iris"),
      h1 = blockr.core::new_head_block(),
      t1 = blockr.core::new_head_block(),
      t2 = blockr.core::new_head_block()
    ),
    links = blockr.core::links(
      from = c("d1", "h1", "t1"),
      to = c("h1", "t1", "t2")
    ),
    stacks = blockr.core::stacks(
      a = blockr.dock::new_dock_stack(
        c("d1", "h1", "t1"), name = "A", color = "#2563eb"
      )
    ),
    extensions = list(mini = new_outline_extension())
  )

  # two of a stack's three rows into a new one: A is dropped and put back
  # without them, under a fresh id but with its name and colour
  split <- outline_stack_new(board, c("h1", "t1"))
  expect_identical(split$stacks$rm, "a")
  expect_length(split$stacks$add, 2L)
  expect_false("a" %in% names(split$stacks$add))

  rebuilt <- Filter(
    function(s) identical(blockr.core::stack_blocks(s), "d1"),
    as.list(split$stacks$add)
  )
  expect_length(rebuilt, 1L)
  expect_identical(attr(rebuilt[[1L]], "name"), "A")
  expect_identical(attr(rebuilt[[1L]], "color"), "#2563eb")

  fresh <- Filter(
    function(s) setequal(blockr.core::stack_blocks(s), c("h1", "t1")),
    as.list(split$stacks$add)
  )
  expect_length(fresh, 1L)
  expect_identical(attr(fresh[[1L]], "name"), "New stack")

  expect_no_error(
    blockr.core::validate_board(
      blockr.core::apply_board_update(board, split)
    )
  )

  # emptied by the move, A goes and does not come back
  whole <- outline_stack_new(board, c("d1", "h1", "t1"))
  expect_identical(whole$stacks$rm, "a")
  expect_length(whole$stacks$add, 1L)
  expect_no_error(
    blockr.core::validate_board(
      blockr.core::apply_board_update(board, whole)
    )
  )

  # blocks that were in no stack take no stack apart
  none <- outline_stack_new(
    blockr.dock::new_dock_board(
      blocks = c(
        x = blockr.core::new_dataset_block("iris"),
        y = blockr.core::new_head_block()
      )
    ),
    c("x", "y"),
    name = "Published"
  )
  expect_null(none$stacks$rm)
  expect_length(none$stacks$add, 1L)
  expect_identical(attr(none$stacks$add[[1L]], "name"), "Published")

  # a group of one is not a group
  expect_null(outline_stack_new(board, "h1"))
  expect_null(outline_stack_new(board, c("h1", "nosuchblock")))
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
    extensions = list(mini = new_outline_extension())
  )

  json <- outline_clip_json(board, c("a", "b"), list())

  # blockr.dag's envelope, so a canvas copy pastes here and back
  expect_identical(jsonlite::fromJSON(json)$object, "subboard")

  delta <- outline_paste_delta(board, json)

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
  solo <- outline_paste_delta(board, outline_clip_json(board, "b", list()))
  expect_null(solo$links)

  expect_null(outline_paste_delta(board, '{"object":"nope"}'))
  expect_null(outline_paste_delta(board, "not json at all"))
})

test_that("a NULL state field survives the clipboard as NULL", {

  # blockr.dag#144: without `null = "null"` every NULL state field becomes
  # `{}` in JSON and comes back as an empty `list()`, which poisons the
  # pasted block -- and survives a save, re-emitting on the next copy.
  board <- blockr.dock::new_dock_board(
    blocks = c(a = blockr.core::new_dataset_block("iris")),
    extensions = list(mini = new_outline_extension())
  )

  json <- outline_clip_json(
    board, "a", list(a = list(dataset = "iris", row_color = NULL))
  )

  expect_match(json, '"row_color":null', fixed = TRUE)
  expect_false(grepl('"row_color":{}', json, fixed = TRUE))

  back <- jsonlite::fromJSON(
    json, simplifyDataFrame = FALSE, simplifyMatrix = FALSE
  )
  expect_null(back$payload$blocks$payload$a$payload$state$row_color)
})
