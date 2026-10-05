# blockr.outline 0.0.206

* Ctrl+C, Ctrl+X and Ctrl+V work in the outline when the DAG extension is on
  the same board. The DAG's shortcut handler cancelled the copy keystroke and
  pasted a second time on Ctrl+V; while the outline has the gesture, the
  keystroke no longer reaches it.
* A paste lands where you last clicked: in a stack if the click hit one of
  its rows, its header or its frame, loose otherwise. On a focused stack a
  paste always joins that stack.

# blockr.outline 0.0.197

* A chart block appears in a report or deck as the picture the browser
  drew. Without one it is left out: no output line in the report, no slide
  in the deck. This takes effect with blockr.viz 0.2.190, which drops the
  chart's ggplot report call.
