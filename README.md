# blockr.outline

A [blockr.dock](https://github.com/BristolMyersSquibb/blockr.dock) extension that
renders a blockr board as a linked outline: block chips aligned with the
generated code, per-block markdown descriptions, report include/exclude flags,
a user-sortable document order, and rendering to html, pptx or pdf.

Descriptions, report flags and document order live in the extension's state, not
in the blocks — the blocks stay plain main-API blocks, and no block constructor
gains a `description` or `report` argument.

Built on exported `blockr.core` and `blockr.dock` API only; no forks required.

## Status

Early pilot (`0.0.x`). The API may change without deprecation, and the package
has no tests or reference documentation yet.

## Installation

```r
pak::pak("cynkra/blockr.outline")
```

## Usage

Add the extension to a dock board:

```r
library(blockr.core)
library(blockr.dock)
library(blockr.outline)

board <- new_dock_board(
  blocks = c(
    data = new_dataset_block("iris", block_name = "Iris data"),
    audit = new_head_block(n = 3L, block_name = "QC glance")
  ),
  links = links(from = "data", to = "audit"),
  extensions = list(
    new_outline_extension(
      title = "Iris pilot report",
      annotations = list(
        data = list(description = "The classic **iris** dataset."),
        audit = list(description = "Quick QC check.", report = FALSE)
      )
    )
  )
)

serve(board)
```

In the "Outline" panel you can click a chip or section to bring that block's dock
panel to front, edit a section's markdown inline, flip a switch to include or
exclude a block from the report, reorder parallel branches, and switch between
the R script and document views before rendering.

A fuller demo board — five blocks across three chapters, with a deliberate split
stack — is in [`dev/example-outline.R`](dev/example-outline.R). It runs from a
workspace checkout of the blockr packages:

```sh
Rscript blockr.outline/dev/example-outline.R
```

## Bundled JavaScript

`inst/js/md-editor.js` is a pre-built [Milkdown](https://milkdown.dev) editor
bundle (MIT) copied from
[blockr.md](https://github.com/BristolMyersSquibb/blockr.md), where the
`srcjs/` sources and the esbuild step live. It cannot be rebuilt from this
repository.

## License

GPL (>= 3)
