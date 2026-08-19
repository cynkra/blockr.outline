# The report's emission: the item list woven into the document, the
# code/output 2x2 on every chunk, the settings YAML, and the uncollapsed
# pieces the gutter view consumes.

rpt_sects <- function(picked = "head") {
  report_sections(otl_exprs(), otl_board(stacks = FALSE), picked = picked)
}

rpt_items <- function() {
  list(
    list(text = "Why we look."),
    list(block = "sub", code = TRUE, output = FALSE),
    list(text = "The **first** rows."),
    list(block = "head")
  )
}

rpt_flags <- function() {
  report_flags(sanitize_items(rpt_items()))
}

rpt_settings <- function(...) {
  sanitize_settings(list(...))
}

test_that("the report qmd and spin render the expected documents", {
  s <- rpt_sects(picked = c("sub", "head"))

  expect_snapshot(
    cat(export_qmd(
      s, "Iris pilot", block_level = "##",
      flags = rpt_flags(), items = rpt_items(), settings = rpt_settings()
    ))
  )
  expect_snapshot(
    cat(export_spin(
      s, block_level = "##", title = "Iris pilot",
      flags = rpt_flags(), items = rpt_items(), settings = rpt_settings()
    ))
  )
})

test_that("the chunk 2x2: echo, output, include, or nothing", {
  s <- rpt_sects(picked = c("data", "sub", "head"))

  flags <- list(
    data = list(code = TRUE, output = TRUE),
    sub = list(code = TRUE, output = FALSE),
    head = list(code = FALSE, output = TRUE)
  )

  txt <- export_qmd(s, "T", block_level = "##", flags = flags)

  # Both on: no visibility option at all in data's chunk.
  data_chunk <- regmatches(txt, regexpr("```\\{r\\}\n#\\| label: data[^`]*```", txt))
  expect_no_match(data_chunk, "echo|output|include")

  # Code only: output suppressed, code visible, still evaluated.
  expect_match(txt, "#| output: false", fixed = TRUE)
  # Output only: code hidden.
  expect_match(txt, "#| echo: false", fixed = TRUE)

  # Both off = the never-added chunk.
  flags$data <- list(code = FALSE, output = FALSE)
  txt <- export_qmd(s, "T", block_level = "##", flags = flags)
  expect_match(txt, "#| label: data\n#| include: false", fixed = TRUE)
})

test_that("no heading and no output line for a block that shows no output", {
  s <- rpt_sects(picked = c("sub", "head"))

  txt <- export_qmd(
    s, "T", block_level = "##",
    flags = list(
      sub = list(code = TRUE, output = FALSE),
      head = list(code = FALSE, output = TRUE)
    )
  )

  expect_match(txt, "## Head", fixed = TRUE)
  expect_no_match(txt, "## Subset", fixed = TRUE)
  # sub's assignment is emitted, but not its bare-variable output line.
  expect_match(txt, "sub <- subset", fixed = TRUE)
  expect_no_match(txt, "```\\{r\\}[^`]*\nsub\n```")
})

test_that("figure overrides and full width emit per chunk", {
  s <- rpt_sects()

  txt <- export_qmd(
    s, "T", block_level = "##",
    flags = list(
      head = list(
        code = FALSE, output = TRUE,
        fig_width = 12, fig_height = 4, full_width = TRUE
      )
    )
  )

  expect_match(txt, "#| fig-width: 12", fixed = TRUE)
  expect_match(txt, "#| fig-height: 4", fixed = TRUE)
  expect_match(txt, "#| column: page", fixed = TRUE)

  spin <- export_spin(
    s, block_level = "##", title = "T",
    flags = list(
      head = list(
        code = FALSE, output = TRUE,
        fig_width = 12, fig_height = 4, full_width = TRUE
      )
    )
  )
  expect_match(spin, ", fig.width=12, fig.height=4", fixed = TRUE)
})

test_that("text items weave in list order and anchor to their block", {
  s <- rpt_sects(picked = c("sub", "head"))

  txt <- export_qmd(
    s, "T", block_level = "##",
    flags = rpt_flags(), items = rpt_items(), settings = rpt_settings()
  )

  # The lead paragraph precedes sub's chunk; the second text precedes
  # head's chunk; both survive in one document.
  lead_at <- regexpr("Why we look.", txt, fixed = TRUE)
  sub_at <- regexpr("#| label: sub", txt, fixed = TRUE)
  note_at <- regexpr("The **first** rows.", txt, fixed = TRUE)
  head_at <- regexpr("#| label: head", txt, fixed = TRUE)

  expect_lt(lead_at, sub_at)
  expect_lt(sub_at, note_at)
  expect_lt(note_at, head_at)
})

test_that("text anchors move with their exhibit when the order snaps", {
  s <- rpt_sects(picked = c("sub", "head"))

  # The list says head first, text before it -- but head depends on sub,
  # so the snap puts sub's chunk first. The text stays glued to head.
  items <- list(
    list(text = "About the head."),
    list(block = "head"),
    list(block = "sub", code = TRUE, output = FALSE)
  )
  flags <- report_flags(sanitize_items(items))

  txt <- export_qmd(s, "T", block_level = "##", flags = flags, items = items)

  sub_at <- regexpr("#| label: sub", txt, fixed = TRUE)
  note_at <- regexpr("About the head.", txt, fixed = TRUE)
  head_at <- regexpr("#| label: head", txt, fixed = TRUE)

  expect_lt(sub_at, note_at)
  expect_lt(note_at, head_at)
})

test_that("a trailing text item lands after the last block", {
  s <- rpt_sects()

  items <- list(
    list(block = "head"),
    list(text = "A closing word.")
  )
  txt <- export_qmd(
    s, "T", block_level = "##",
    flags = report_flags(sanitize_items(items)), items = items
  )

  expect_lt(
    regexpr("#| label: head", txt, fixed = TRUE),
    regexpr("A closing word.", txt, fixed = TRUE)
  )
})

test_that("the settings emit the YAML, defaults staying quiet", {
  s <- rpt_sects()
  flags <- list(head = list(code = FALSE, output = TRUE))

  txt <- export_qmd(
    s, "T", block_level = "##", flags = flags, settings = rpt_settings()
  )

  # Always: the document is self-contained and sized.
  expect_match(txt, "embed-resources: true", fixed = TRUE)
  expect_match(txt, "fig-width: 8", fixed = TRUE)
  expect_match(txt, "fig-height: 4.5", fixed = TRUE)
  # Warnings off is the default, and it is emitted -- suppressing them is
  # a claim the document must carry.
  expect_match(txt, "execute:\n  warning: false\n  message: false", fixed = TRUE)
  # Off-defaults that match quarto's own stay out of the YAML.
  expect_no_match(txt, "toc", fixed = TRUE)
  expect_no_match(txt, "number-sections", fixed = TRUE)
  expect_no_match(txt, "code-fold", fixed = TRUE)

  on <- export_qmd(
    s, "T", block_level = "##", flags = flags,
    settings = rpt_settings(
      toc = TRUE, number_sections = TRUE, code_fold = TRUE, warnings = TRUE
    )
  )
  expect_match(on, "toc: true", fixed = TRUE)
  expect_match(on, "number-sections: true", fixed = TRUE)
  expect_match(on, "code-fold: true", fixed = TRUE)
  expect_no_match(on, "warning: false", fixed = TRUE)

  # The spin script opens on the same front matter, spin-quoted.
  spin <- export_spin(
    s, block_level = "##", title = "T", flags = flags,
    settings = rpt_settings()
  )
  expect_match(spin, "#' ---", fixed = TRUE)
  expect_match(spin, "#'     embed-resources: true", fixed = TRUE)
})

test_that("block titles: captions and none", {
  s <- rpt_sects()
  flags <- list(head = list(code = FALSE, output = TRUE))

  cap <- export_qmd(s, "T", block_level = "caption", flags = flags)
  expect_match(cap, "#| tbl-cap:", fixed = TRUE)
  expect_no_match(cap, "## Head", fixed = TRUE)

  none <- export_qmd(s, "T", block_level = "none", flags = flags)
  expect_no_match(none, "tbl-cap", fixed = TRUE)
  expect_no_match(none, "## Head", fixed = TRUE)
})

test_that("the emitted document is canonical R", {
  skip_if_not_installed("blockr.viz", "0.2.38")

  s <- rpt_sects()
  flags <- list(head = list(code = FALSE, output = TRUE))

  for (txt in list(
    export_qmd(s, "T", block_level = "##", flags = flags),
    export_spin(s, block_level = "##", flags = flags)
  )) {
    # Plain transform blocks print bare; no blockr renderer appears.
    expect_no_match(txt, "static_exhibit", fixed = TRUE)
    expect_match(txt, "head <- utils::head(sub, 3)", fixed = TRUE)
  }
})

test_that("uncollapsed pieces join into exactly the collapsed document", {
  s <- rpt_sects(picked = c("sub", "head"))

  for (emit in list(
    function(...) export_qmd(
      s, "T", block_level = "##",
      flags = rpt_flags(), items = rpt_items(), settings = rpt_settings(), ...
    ),
    function(...) export_spin(
      s, block_level = "##", title = "T",
      flags = rpt_flags(), items = rpt_items(), settings = rpt_settings(), ...
    )
  )) {
    pieces <- emit(collapse = FALSE)
    ids <- attr(pieces, "ids")

    expect_identical(paste0(pieces, collapse = "\n\n"), emit())
    expect_length(ids, length(pieces))
    # Header and text pieces carry no block; every block piece names its
    # block, in document order.
    expect_true(anyNA(ids))
    expect_identical(ids[!is.na(ids)], s$ids)
  }
})

test_that("without flags the emitters keep the legacy behaviour", {
  s <- rpt_sects()

  txt <- export_qmd(s, "T", block_level = "##")
  # Picked block: no visibility options; ancestor: include only.
  expect_no_match(txt, "#| echo", fixed = TRUE)
  expect_no_match(txt, "#| output", fixed = TRUE)
  expect_match(txt, "#| include: false", fixed = TRUE)
  # Legacy YAML: no execute block, no embed-resources.
  expect_no_match(txt, "execute:", fixed = TRUE)
  expect_no_match(txt, "embed-resources", fixed = TRUE)
})
