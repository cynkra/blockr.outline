# R/deck.R + R/slides.R :: the slide builder.
#
# The claim the whole extension rests on: slide order and evaluation order
# are independent. Evaluation has to follow the DAG; a deck does not, and a
# deck that cannot open on its conclusion is not a deck. These tests pin
# that separation at every layer it passes through -- the projection, the
# quarto document, the officer writer -- plus the state it serialises as.

# The fixture board is data -> sub -> head (see helper-fixtures.R), a linear
# chain: `sub` is an ancestor of `head`, so "head first, sub second" is a
# slide order no document could ever have.

test_that("the projection evaluates topologically and presents in pick order", {
  s <- slide_sections(otl_exprs(), otl_board(), slides = c("head", "sub"))

  # Evaluation order: the DAG's, always.
  expect_identical(s$ids, c("data", "sub", "head"))
  expect_lt(match("sub", s$ids), match("head", s$ids))

  # Slide order: the user's, even though it inverts the chain.
  expect_identical(s$slide_ids, c("head", "sub"))
  expect_identical(slide_seq(s), c(3L, 2L))

  # `report` is the picking, and says nothing about order.
  expect_identical(unname(s$report), c(FALSE, TRUE, TRUE))
})

test_that("picking a block exports its ancestors without making them slides", {
  s <- slide_sections(otl_exprs(), otl_board(), slides = "head")

  # `data` and `sub` have to RUN -- picking a table means running what feeds
  # it -- but neither is reported, so neither takes a slide.
  expect_identical(unname(s$exported), c(TRUE, TRUE, TRUE))
  expect_identical(unname(s$report), c(FALSE, FALSE, TRUE))
  expect_identical(s$slide_ids, "head")
})

test_that("a branch nothing picked depends on is outside the export", {
  # data -> sub -> {plot, audit}: picking only `audit` must leave `plot`
  # out of the closure entirely, so an unrelated broken branch cannot fail
  # a deck that never needed it.
  s <- slide_sections(
    otl_exprs_parallel(),
    otl_board_parallel(),
    slides = "audit"
  )

  expect_false(s$exported[s$ids == "plot"])
  expect_true(all(s$exported[s$ids %in% c("data", "sub", "audit")]))
})

test_that("an unpicked board projects with no slides at all", {
  s <- slide_sections(otl_exprs(), otl_board())

  expect_identical(s$slide_ids, character())
  expect_false(any(s$report))
  expect_false(any(s$exported))
  expect_identical(slide_seq(s), integer())
})

test_that("ids that are not on the board are ignored, in both roles", {
  # State outlives blocks: a restored board can name a block that has since
  # been removed. It must neither order nor slide.
  s <- slide_sections(otl_exprs(), otl_board(), slides = c("ghost", "head"))

  expect_identical(s$slide_ids, "head")
  expect_setequal(s$ids, c("data", "sub", "head"))
})

test_that("slide_seq falls back to document order for an outline projection", {
  # The outline's sections carry no slide_ids, and its document order IS its
  # slide order -- so the officer writer's pass 2 has to walk every section.
  s <- outline_sections(otl_exprs(), otl_board(), otl_ann())

  expect_null(s$slide_ids)
  expect_identical(slide_seq(s), seq_along(s$ids))
})

test_that("the deck document computes up front and shows results only", {
  s <- slide_sections(otl_exprs(), otl_board(), slides = c("head", "sub"))
  qmd <- export_deck_qmd(s, "Iris deck")

  expect_match(qmd, "title: \"Iris deck\"", fixed = TRUE)

  # A deck shows results, never code -- said once, in the yaml.
  expect_match(qmd, "execute:\n  echo: false", fixed = TRUE)

  # Every block's code runs before the first slide, hidden.
  for (id in c("data", "sub", "head")) {
    expect_match(qmd, paste0("#| label: setup-", id), fixed = TRUE)
  }
  expect_identical(
    length(gregexpr("#| include: false", qmd, fixed = TRUE)[[1L]]),
    3L
  )

  # ...and each slide is a lookup. This is what buys free ordering, so it is
  # the assertion that matters: the setup chunks all precede every slide.
  last_setup <- max(gregexpr("#| label: setup-", qmd, fixed = TRUE)[[1L]])
  first_slide <- min(gregexpr("#| label: slide-", qmd, fixed = TRUE)[[1L]])
  expect_lt(last_setup, first_slide)

  # The slides come out in PICK order, inverting the chain.
  expect_lt(
    regexpr("#| label: slide-head", qmd, fixed = TRUE),
    regexpr("#| label: slide-sub", qmd, fixed = TRUE)
  )

  # One slide per pick, titled by block name, and nothing for the unpicked
  # ancestor beyond its hidden setup chunk.
  expect_match(qmd, "## Head", fixed = TRUE)
  expect_match(qmd, "## Subset", fixed = TRUE)
  expect_false(grepl("## Dataset", qmd, fixed = TRUE))

  # No captions and no tbl-/fig- labels: the slide title is the heading, and
  # a cross-reference label would make pandoc treat a flextable as a float
  # and drop it from the pptx.
  expect_false(grepl("-cap:", qmd, fixed = TRUE))
})

test_that("a pending pick is skipped rather than emitted as a broken slide", {
  # sect_export_code() turns a pending block's code into a comment, so its
  # variable is never bound and a slide referencing it would fail the render.
  s <- slide_sections(
    otl_exprs(pending = "head"),
    otl_board(),
    slides = c("head", "sub")
  )

  qmd <- export_deck_qmd(s, "Deck")

  expect_false(grepl("#| label: slide-head", qmd, fixed = TRUE))
  expect_match(qmd, "#| label: slide-sub", fixed = TRUE)
})

test_that("an untitled block breaks its slide with a rule, not a heading", {
  # A heading break on a nameless block would silently fold its exhibit onto
  # the previous slide.
  s <- slide_sections(otl_exprs(), otl_board(), slides = c("sub", "head"))
  s$names[["head"]] <- ""

  qmd <- export_deck_qmd(s, "Deck")

  expect_match(qmd, "\n----\n", fixed = TRUE)
})

test_that("the deck title reaches the filename in a shape a path allows", {
  expect_identical(deck_filename("Q3 review / EU"), "q3-review-eu")
  expect_identical(deck_filename("  "), "deck")
  expect_identical(deck_filename("Iris topline"), "iris-topline")
})

test_that("the officer writer lays the slides out in pick order", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  skip_if_not_installed("blockr.viz")

  # A CHAIN, not two parallel branches: `second` derives from `first`, so no
  # legal evaluation order puts it first. Parallel blocks would prove
  # nothing here -- the pick order is also the topological tie-break, so the
  # projection would simply have reordered them (see the test below).
  board <- blockr.core::new_board(
    blocks = c(
      data = blockr.core::new_dataset_block("iris"),
      first = blockr.viz::new_table_block(block_name = "Alpha"),
      second = blockr.viz::new_table_block(block_name = "Beta")
    ),
    links = blockr.core::links(from = c("data", "first"),
                               to = c("first", "second"))
  )

  exprs <- structure(
    list(
      data = quote(datasets::iris),
      first = quote(dplyr::filter(blockr.viz::as_annotated_df(data), TRUE)),
      second = quote(dplyr::filter(blockr.viz::as_annotated_df(first), TRUE))
    ),
    pending = character()
  )

  # Picked second-then-first, which the projection cannot honour and does
  # not try to.
  s <- slide_sections(exprs, board, slides = c("second", "first"))
  expect_lt(match("first", s$ids), match("second", s$ids))

  f <- withr::local_tempfile(fileext = ".pptx")
  render_pptx_officer(s, f, "Deck", template = NULL)

  titles <- officer::pptx_summary(officer::read_pptx(f))
  titles <- titles$text[titles$text %in% c("Alpha", "Beta")]

  expect_identical(titles, c("Beta", "Alpha"))
})

test_that("a deck slide shows the same exhibit the report would", {
  # The two projections disagree about order, prose and chapters, and must
  # agree about everything that decides what an exhibit IS. Otherwise a
  # block could render one way in the report and another on a slide, and
  # every exhibit bug would need diagnosing twice.
  b <- otl_board_parallel()
  e <- otl_exprs_parallel()
  ids <- c("data", "sub", "plot", "audit")

  o <- outline_sections(e, b, otl_ann(ids = ids))
  s <- slide_sections(e, b, slides = ids)

  # Same blocks either way, but not necessarily in the same order.
  expect_setequal(o$ids, s$ids)
  at <- match(o$ids, s$ids)

  expect_identical(unname(s$kinds[at]), unname(o$kinds))
  expect_identical(unname(s$renderers[at]), unname(o$renderers))
  expect_identical(unname(s$report_calls[at]), unname(o$report_calls))

  # ...and therefore the same output expression, which is the thing the
  # officer writer and the qmd both evaluate.
  for (i in seq_along(o$ids)) {
    expect_identical(sect_output(s, at[i]), sect_output(o, i))
  }
})

test_that("the pick order steers evaluation wherever the DAG allows slack", {
  # The other half of the story. Slide order is free, but it is ALSO the
  # topological tie-break, so two parallel branches evaluate in the order
  # they were picked -- which is what keeps a re-picked deck from shuffling
  # its hidden setup chunks for no reason.
  b <- otl_board_parallel()

  a <- slide_sections(otl_exprs_parallel(), b, slides = c("plot", "audit"))
  z <- slide_sections(otl_exprs_parallel(), b, slides = c("audit", "plot"))

  expect_lt(match("plot", a$ids), match("audit", a$ids))
  expect_lt(match("audit", z$ids), match("plot", z$ids))

  # ...and `sub` precedes both either way: slack is not licence.
  expect_lt(match("sub", a$ids), match("plot", a$ids))
  expect_lt(match("sub", z$ids), match("plot", z$ids))
})

# ---- the extension server -------------------------------------------

# The slide builder's board bundle, with every block expression rigged to
# THROW. Anything the panel draws has to come off the board object itself
# (block names, exhibit kinds), so a panel that renders against this bundle
# is a panel that evaluates nothing -- which is the extension's central
# performance claim, and the reason it needs no visibility gate.
blind_board_args <- function() {
  b <- otl_board_args()
  isolate({
    for (id in names(b$blocks)) {
      b$blocks[[id]]$server$expr <- reactive(stop("expression evaluated"))
    }
  })
  b
}

# Take a block off a dock board the way the app does: out of every view
# first, then its links, then the block. A dock_board validates view
# membership against its blocks, so removing the block first aborts.
drop_block <- function(brd, id) {

  views <- blockr.dock::board_views(brd)
  pid <- as.character(blockr.dock::as_block_panel_id(id))

  for (v in names(views)) {
    views[[v]] <- blockr.dock::dock_view(
      setdiff(blockr.dock::view_members(views[[v]]), pid),
      name = blockr.dock::view_name(views[[v]])
    )
  }

  blockr.dock::board_views(brd) <- views

  lnks <- blockr.core::board_links(brd)
  blockr.core::board_links(brd) <- lnks[lnks$from != id & lnks$to != id]

  blks <- blockr.core::board_blocks(brd)
  blockr.core::board_blocks(brd) <- blks[setdiff(names(blks), id)]

  brd
}

test_that("picking, ordering and removing rewrite the one state field", {
  testServer(
    slides_ext_srv(character(), "Deck"),
    {
      session$flushReact()
      expect_identical(rv_slides(), character())

      session$setInputs(sld_add = "audit")
      session$setInputs(sld_add = "plot")
      expect_identical(rv_slides(), c("audit", "plot"))

      # A block already in the deck is not added twice.
      session$setInputs(sld_add = "audit")
      expect_identical(rv_slides(), c("audit", "plot"))

      # ...and an empty pick is ignored rather than adding a nameless row.
      session$setInputs(sld_add = "")
      expect_identical(rv_slides(), c("audit", "plot"))

      session$setInputs(sld_act = list(id = "plot", act = "up"))
      expect_identical(rv_slides(), c("plot", "audit"))

      # Moving past the end is a no-op, not an error or a duplicate.
      session$setInputs(sld_act = list(id = "plot", act = "up"))
      expect_identical(rv_slides(), c("plot", "audit"))

      session$setInputs(sld_act = list(id = "plot", act = "down"))
      expect_identical(rv_slides(), c("audit", "plot"))
      session$setInputs(sld_act = list(id = "plot", act = "down"))
      expect_identical(rv_slides(), c("audit", "plot"))

      session$setInputs(sld_act = list(id = "audit", act = "rm"))
      expect_identical(rv_slides(), "plot")
    },
    args = list(board = blind_board_args(), update = reactiveVal())
  )
})

test_that("a drag lands before or after the row it was dropped on", {
  testServer(
    slides_ext_srv(c("data", "sub", "plot"), "Deck"),
    {
      session$flushReact()

      # Dropped on the upper half of `data`: before it.
      session$setInputs(
        sld_move = list(id = "plot", target = "data", after = FALSE)
      )
      expect_identical(rv_slides(), c("plot", "data", "sub"))

      # Lower half of the last row: after it, which is the only way to reach
      # the end of the list.
      session$setInputs(
        sld_move = list(id = "plot", target = "sub", after = TRUE)
      )
      expect_identical(rv_slides(), c("data", "sub", "plot"))

      # A drop on itself changes nothing.
      session$setInputs(
        sld_move = list(id = "sub", target = "sub", after = TRUE)
      )
      expect_identical(rv_slides(), c("data", "sub", "plot"))
    },
    args = list(board = blind_board_args(), update = reactiveVal())
  )
})

test_that("the panel draws without evaluating a single block", {
  # Every expression in this bundle throws (see blind_board_args). The list
  # still has to come out with names, numbers and kinds.
  testServer(
    slides_ext_srv(c("plot", "audit"), "Deck"),
    {
      session$flushReact()

      html <- as.character(output$sld_list$html)

      expect_match(html, "data-blk=\"plot\"", fixed = TRUE)
      expect_match(html, "data-blk=\"audit\"", fixed = TRUE)
      # Slide numbers are positional, drawn from the list rather than stored.
      expect_match(html, "blockr-sld-num\">1<", fixed = TRUE)
      expect_match(html, "blockr-sld-num\">2<", fixed = TRUE)
      # ...and `plot` is a scatter block, so its kind reads off the class.
      expect_match(html, "fig", fixed = TRUE)

      # The picker offers what is not already a slide.
      expect_setequal(session$getReturned()$state$slides(), c("plot", "audit"))
    },
    args = list(board = blind_board_args(), update = reactiveVal())
  )
})

test_that("an empty deck says so rather than drawing an empty list", {
  testServer(
    slides_ext_srv(character(), "Deck"),
    {
      session$flushReact()
      expect_match(as.character(output$sld_list$html), "No slides yet")
    },
    args = list(board = blind_board_args(), update = reactiveVal())
  )
})

test_that("a removed block loses its slide", {
  testServer(
    slides_ext_srv(c("plot", "audit"), "Deck"),
    {
      session$flushReact()
      expect_identical(rv_slides(), c("plot", "audit"))

      board$board <- drop_block(board$board, "plot")
      session$flushReact()

      expect_identical(rv_slides(), "audit")
    },
    args = list(board = blind_board_args(), update = reactiveVal())
  )
})

test_that("state round-trips what the constructor was given", {
  testServer(
    slides_ext_srv(c("audit", "plot"), "Iris topline", format = "revealjs"),
    {
      session$flushReact()

      state <- session$getReturned()$state

      expect_identical(state$slides(), c("audit", "plot"))
      expect_identical(state$title(), "Iris topline")
      expect_identical(state$format(), "revealjs")
      expect_identical(state$template(), "")
    },
    args = list(board = blind_board_args(), update = reactiveVal())
  )
})

test_that("an unrecognised format falls back to PowerPoint", {
  testServer(
    slides_ext_srv(character(), "Deck", format = "keynote"),
    {
      session$flushReact()
      expect_identical(session$getReturned()$state$format(), "pptx")
    },
    args = list(board = blind_board_args(), update = reactiveVal())
  )
})

test_that("downloading demands the picked blocks and their ancestors", {
  # The two-stage download: on a deferred board a picked block may not be
  # constructed, so the click demands the export closure through core's
  # visibility channel and waits for its code.
  vis <- fake_visibility(c("data", "sub", "plot", "audit"))

  testServer(
    slides_ext_srv("plot", "Deck"),
    {
      session$flushReact()

      session$setInputs(sld_go = 1L)
      session$flushReact()

      # `plot` is pending, so it and its ancestors are demanded -- and
      # `audit`, on the branch nothing picked depends on, is NOT.
      expect_true(isolate(vis$required[["plot"]]()))
      expect_true(awaiting())
      expect_identical(isolate(vis$required[["audit"]]()), NA)
    },
    args = list(board = pending_plot_board(), update = reactiveVal(),
                visibility = vis)
  )
})

test_that("an expression survives the block going quiet after the demand", {
  # The bug this pins, found by downloading a deck in a browser and getting
  # one slide fewer than was picked, with nothing anywhere saying why.
  #
  # The wait observer withdraws its demand as soon as the closure reports
  # (restore_demanded -- the dock overloads `required` as its card-build
  # ledger, so a TRUE left behind blanks a panel later) and only THEN fires
  # the download. The block falls quiet in between, so the projection the
  # download handler builds for itself saw the block pending again -- and a
  # pending block is SKIPPED, not raised. The deck came back short, quietly.
  #
  # The expression cache is what carries the expression across that gap.
  reporting <- reactiveVal(FALSE)

  args <- otl_board_args()
  isolate(
    args$blocks[["plot"]]$server$expr <- reactive({
      req(reporting())
      quote(graphics::plot(sub))
    })
  )

  testServer(
    slides_ext_srv("plot", "Deck"),
    {
      session$flushReact()
      expect_true(sections()$pending[sections()$ids == "plot"])

      # The demand lands: the block reports.
      reporting(TRUE)
      session$flushReact()
      expect_false(sections()$pending[sections()$ids == "plot"])

      # ...and is withdrawn again before the file is built.
      reporting(FALSE)
      session$flushReact()

      s <- sections()
      expect_false(s$pending[s$ids == "plot"])
      expect_match(s$code[s$ids == "plot"], "graphics::plot")
    },
    args = list(board = args, update = reactiveVal())
  )
})

test_that("a block that leaves the board is not resurrected from the cache", {
  # The other side of the cache: it is keyed on the LIVE board, so a removed
  # block goes for good rather than coming back holding its last expression.
  testServer(
    slides_ext_srv(c("plot", "audit"), "Deck"),
    {
      session$flushReact()
      expect_true("plot" %in% sections()$ids)

      board$board <- drop_block(board$board, "plot")
      session$flushReact()

      expect_false("plot" %in% sections()$ids)
    },
    args = list(board = otl_board_args(), update = reactiveVal())
  )
})

test_that("downloading nothing is a warning, not a deck", {
  testServer(
    slides_ext_srv(character(), "Deck"),
    {
      session$flushReact()
      session$setInputs(sld_go = 1L)
      session$flushReact()

      # Never entered the demand/wait cycle: there is nothing to render.
      expect_false(awaiting())
    },
    args = list(board = blind_board_args(), update = reactiveVal())
  )
})
