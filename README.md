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

The markdown editor is a [Milkdown](https://milkdown.dev) (ProseMirror) bundle,
vendored here in full: sources in `srcjs/md-editor/`, dependencies pinned by
`package-lock.json`, built to `inst/js/md-editor.js`.

```sh
npm ci
npm run build
```

The built bundle is committed, so installing the R package needs no Node
toolchain. Rebuild only when you change `srcjs/` or bump a dependency.

`LICENSE.note` lists the 84 npm packages embedded in the bundle — all MIT, all
GPL-3 compatible — with their license texts reproduced verbatim under
`inst/licenses/`. Both are generated from the build itself, so they cannot
drift from what actually ships:

```sh
npm run build:meta   # build, recording which modules were bundled
npm run licenses     # regenerate LICENSE.note + inst/licenses/
```

## License

GPL (>= 3)
