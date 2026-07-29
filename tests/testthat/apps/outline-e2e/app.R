# e2e fixture for the shinytest2 browser tests. A dock board with the outline
# extension: four blocks in two chapters, seeded annotations and one block
# excluded from the report. Served with a fixed id ("board") so the extension's
# custom-message inputs resolve deterministically to `board-outline-<input>`.
#
# background_construction_delay = 0: a report surface needs every block's
# expression, including blocks whose dock panel is a hidden tab, so all blocks
# must construct eagerly (see the extension's own notes).

library(blockr.core)
library(blockr.dock)
library(blockr.outline)

options(blockr.dock_is_locked = FALSE)
options(blockr.background_construction_delay = 0)

board <- new_dock_board(
  blocks = c(
    data  = new_dataset_block("iris"),
    sub   = new_subset_block(),
    plot  = new_scatter_block("Sepal.Length", "Sepal.Width"),
    audit = new_head_block()
  ),
  links = links(
    from = c("data", "sub",  "sub"),
    to   = c("sub",  "plot", "audit")
  ),
  stacks = stacks(
    prep   = new_dock_stack(c("data", "sub"), name = "Data prep"),
    output = new_dock_stack(c("plot", "audit"), name = "Outputs")
  ),
  extensions = list(
    new_outline_extension(
      annotations = list(
        data  = list(description = "The iris dataset.", report = TRUE),
        sub   = list(description = "Setosa only.", report = TRUE),
        plot  = list(report = TRUE),
        audit = list(report = FALSE)
      ),
      stack_annotations = list(
        prep = list(description = "Everything the analysis consumes.")
      ),
      title = "Iris e2e report"
    )
  )
)

serve(board, "board")
