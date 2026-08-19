#' Register blockr.outline blocks
#'
#' Registers the slide block with blockr so it appears in the block browser,
#' the block-adder and the assistant's block universe. Called from
#' `.onLoad()`; exported so a host can re-register after unregistering.
#'
#' @return Invisibly, the result of [blockr.core::register_blocks()].
#' @examplesIf interactive()
#' register_outline_blocks()
#' @export
#' @importFrom blockr.core register_blocks new_arg_specs new_arg_spec
#'   arg_string arg_boolean arg_enum
register_outline_blocks <- function() {
  blockr.core::register_blocks(
    "new_slide_block",
    name = "Slide",
    description = paste(
      "Compose one PowerPoint slide: pick a layout, fill its Details, link",
      "exhibits, and the block's output IS the slide at true 16:9. Downloads",
      "as pptx or html, and the slides extension picks it up as a whole",
      "slide in a deck."
    ),
    # "output": the slide is an export artifact, not a data transform -- its
    # value is a composed slide, and the block is where a deck page is made.
    category = "output",
    icon = "easel",
    arguments = slide_block_arguments(),
    guidance = slide_block_guidance(),
    package = utils::packageName(),
    overwrite = TRUE
  )
}

# Every constructor formal, because the registry IS the AI surface: an
# argument missing here is an argument the assistant cannot set.
slide_block_arguments <- function() {
  blockr.core::new_arg_specs(
    layout = blockr.core::new_arg_spec(
      paste0(
        "Slide layout, which decides the slots. \"exhibit-full\" (one ",
        "exhibit), \"two-up-h\" (two side by side), \"exhibit-callout\" ",
        "(exhibit over a tinted takeaway band), \"exhibit-notes\" (exhibit ",
        "with a notes column), \"compare-2\" (two labelled panels -- the ",
        "before/after slide, see `labels`), \"lead-exhibit\" (a paragraph ",
        "over the exhibit), \"full-bleed\" (the exhibit alone, no title or ",
        "footnote), \"bullets\" (text only, no inputs), \"section\" (a ",
        "divider, no inputs)."
      ),
      example = "exhibit-callout",
      type = blockr.core::arg_enum(names(slide_layouts()))
    ),
    title = blockr.core::new_arg_spec(
      paste0(
        "Slide title. Placed in the reference template's own title ",
        "placeholder, so it inherits the house style. Empty renders nothing."
      ),
      example = "Sepal and petal means",
      type = blockr.core::arg_string()
    ),
    subtitle = blockr.core::new_arg_spec(
      paste0(
        "One line under the title. Empty renders nothing and hands its room ",
        "to the body."
      ),
      example = "By species, full sample",
      type = blockr.core::arg_string()
    ),
    footnote = blockr.core::new_arg_spec(
      "Source line at the foot of the slide. Empty renders nothing.",
      example = "Source: iris, n = 150",
      type = blockr.core::arg_string()
    ),
    text = blockr.core::new_arg_spec(
      paste0(
        "The Details field: ONE field whatever the layout, so the words ",
        "survive a layout switch -- the takeaway, the notes, the bullet ",
        "list and the section kicker are all this. Line syntax: \"- \" ",
        "starts a bullet, \"1. \" a numbered item (consecutive items ",
        "renumber in order; a plain line or a bullet breaks the list), a ",
        "line without a marker stays plain text, and one slide mixes all ",
        "three. Inline **bold** and *italic*. Nothing else is honoured: the ",
        "accepted syntax is exactly what both the HTML preview and the pptx ",
        "can draw."
      ),
      example = "**Takeaway:** virginica leads on both measures.",
      type = blockr.core::arg_string()
    ),
    labels = blockr.core::new_arg_spec(
      paste0(
        "Panel headings for the \"compare-2\" layout, separated by \"|\" ",
        "(left | right). Ignored by every other layout."
      ),
      example = "Before | After",
      type = blockr.core::arg_string()
    ),
    paginate = blockr.core::new_arg_spec(
      paste0(
        "Single-exhibit layouts only: page a table too tall for its slot ",
        "over further slides (repeated header, title marked \"(2 of 3)\", ",
        "the slide's subtitle and footnote on every page), the way the deck ",
        "pages its tables. FALSE truncates with a visible marker instead. ",
        "Layouts with more than one content slot always truncate, whatever ",
        "this says: a second slide would duplicate the other slots."
      ),
      example = TRUE,
      type = blockr.core::arg_boolean()
    )
  )
}

slide_block_guidance <- function() {
  paste(
    "Composes ONE PowerPoint slide. Link the exhibits you want on it (the",
    "block is variadic: zero inputs is a legal text slide); the chosen",
    "layout decides how many it draws, and extra inputs are not drawn.",
    "Exhibits are typeset by the same routine the deck uses, so a slide",
    "table matches the deck's tables under a house theme.",
    "\n\nAuthored text all goes in `text` (Details) -- there is no second",
    "text argument, and switching layout keeps the words. Use `labels` only",
    "for compare-2.",
    "\n\nThe block's output is the finished slide, previewed at true 16:9",
    "from the same inch geometry the download places. To build a deck, add",
    "the slides extension and pick this block: it contributes its composed",
    "slide, while ordinary picked blocks get a default one-exhibit slide."
  )
}
