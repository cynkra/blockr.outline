# The link model of the outline (design system, "Links in the outline"):
# removing a block drops its links, a link's input is renamed or moved through a checked `links$mod`, and the gestures reach the board
# through the extension server.

# data -> sub -> {head, merge x}, data -> rbind (1), sub -> rbind (2)
lnk_board <- function() {
  blockr.dock::new_dock_board(
    blocks = c(
      data = blockr.core::new_dataset_block("iris"),
      sub = blockr.core::new_subset_block(),
      head = blockr.core::new_head_block(),
      mrg = blockr.core::new_merge_block(by = "Species"),
      bind = blockr.core::new_rbind_block()
    ),
    links = blockr.core::links(
      from = c("data", "sub", "sub", "data", "sub"),
      to = c("sub", "head", "mrg", "bind", "bind"),
      input = c("data", "data", "x", "1", "2")
    ),
    stacks = blockr.core::stacks(
      tail = blockr.dock::new_dock_stack(c("head", "mrg"), name = "Tail")
    )
  )
}

link_id <- function(board, from, to) {
  lnks <- blockr.core::board_links(board)
  names(lnks)[lnks$from == from & lnks$to == to]
}

test_that("removing a block adds no links", {
  brd <- lnk_board()
  upd <- outline_rm_delta(brd, "sub")
  expect_identical(upd$blocks$rm, "sub")
  expect_null(upd$links)
  expect_null(upd$stacks)
})

test_that("removing every member of a stack removes the stack", {
  brd <- lnk_board()
  upd <- outline_rm_delta(brd, c("head", "mrg", "nope"))
  expect_identical(upd$blocks$rm, c("head", "mrg"))
  expect_identical(upd$stacks$rm, "tail")
  expect_null(outline_rm_delta(brd, "nope"))
})

test_that("the payload says which blocks lose their links on removal", {
  pay <- outline_payload(lnk_board())
  drops <- vapply(pay$blocks, `[[`, NA, "drops")
  names(drops) <- vapply(pay$blocks, `[[`, "", "id")
  # every block here is linked
  expect_identical(
    drops,
    c(data = TRUE, sub = TRUE, head = TRUE, mrg = TRUE, bind = TRUE)
  )

  # a block with no links at all has nothing to drop
  lone <- blockr.core::new_board(
    blocks = c(a = blockr.core::new_dataset_block("iris"))
  )
  expect_false(outline_payload(lone)$blocks[[1L]]$drops)
})

test_that("a link into a variadic block is renamed to a name not taken", {
  brd <- lnk_board()
  id <- link_id(brd, "data", "bind")

  upd <- outline_link_mod_delta(brd, id, "  left ")
  expect_identical(upd, list(links = list(mod = stats::setNames(
    list(list(input = "left")), id
  ))))

  expect_match(outline_link_mod_delta(brd, id, "2"), "already|called")
  expect_match(outline_link_mod_delta(brd, id, " "), "needs a name")
  # the same name is nothing to do; an unknown link neither
  expect_null(outline_link_mod_delta(brd, id, "1"))
  expect_null(outline_link_mod_delta(brd, "nope", "left"))
})

test_that("a link into a fixed-arity block moves only to a free input", {
  brd <- lnk_board()
  id <- link_id(brd, "sub", "mrg")

  upd <- outline_link_mod_delta(brd, id, "y")
  expect_identical(upd$links$mod[[id]], list(input = "y"))
  expect_match(outline_link_mod_delta(brd, id, "z"), "not a free input")

  # the delta goes through blockr.core as a link update
  lnks <- blockr.core::board_links(brd)
  moved <- blockr.core::update_link(lnks[[id]], upd$links$mod[[id]])
  expect_identical(moved$input, "y")
})

test_that("a new input of a variadic block gets the next free number", {
  brd <- lnk_board()
  expect_identical(outline_new_input(brd, "bind"), "3")
  expect_identical(outline_new_input(brd, "head"), "1")
})

test_that("the link gestures reach the board through the server", {
  brd <- lnk_board()
  rv <- otl_board_args(brd)
  upd <- shiny::reactiveVal()
  inserted <- shiny::reactiveVal()
  actions <- list(insert_block_action = function(x) inserted(x))

  testServer(
    outline_ext_srv,
    {
      session$setInputs(ready = TRUE)

      # remove
      session$setInputs(block_rm = list(ids = list("sub")))
      expect_identical(upd()$blocks$rm, "sub")
      expect_null(upd()$links)

      # the old single-id message still works
      session$setInputs(block_rm = list(id = "bind"))
      expect_identical(upd()$blocks$rm, "bind")
      expect_null(upd()$links)

      # rename a variadic input
      id <- link_id(brd, "data", "bind")
      session$setInputs(link_mod = list(id = id, input = "left"))
      expect_identical(upd()$links$mod[[id]], list(input = "left"))

      # a new input on a variadic block is named
      session$setInputs(link_add = list(from = "head", to = "bind", input = ""))
      expect_identical(upd()$links$add$input, "3")

      # insert: blockr.dock's action, triggered with the link id
      lid <- link_id(brd, "data", "sub")
      session$setInputs(link_insert = list(id = lid))
      expect_identical(inserted(), lid)
      session$setInputs(link_insert = list(id = "nope"))
      expect_identical(inserted(), lid)
    },
    args = list(board = rv, update = upd, actions = actions)
  )
})
