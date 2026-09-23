# ggview 0.2.3
* Added a plots collection: `plots_init()` starts one, `plots_append()` adds to
  it, `plots_get()` takes one plot back out, `plots_set()` records a change,
  `plots_reset()` forgets one, and `plots_remove()` drops a plot. It is a tibble
  with one row per plot.
* A change is stored beside the plot, never written into it. `plots_get()`
  applies the changes to the stored plot and then its canvas, so applying them
  twice is applying them once, and a reset always has something to go back to.
* Added `customizer()`, which says what a plot lets people change and what
  changing it does. `customizer_default()` covers the labels any plot has.
  A customizer may rebuild a plot rather than add to it, so it is not limited to
  what `ggplot2` can add on top. One that reaches for a variable from the
  session that wrote it is refused, because it would not survive being read
  somewhere else.
* Added `plots_params()`, which says what one plot accepts: the kind of each
  parameter, its default, and what it was changed to. With the `param_*()`
  functions — text, number, flag, colour, choice, choices and mapping — that is
  enough for a program to build a control for each parameter without knowing
  anything about plots.
* Added `save_plots()` to write a whole collection, `as.list()` to get it as a
  named list of plots, and `plots_as_content()` to shape it for bulk writers
  that take a table of objects.
* Printing a plot that has a `canvas()` no longer fails where there is no
  viewer. It draws the ordinary way instead, so a plot can be printed while
  knitting, in a plain console and under `Rscript`. The canvas cannot be
  honoured there, and `save_ggplot()` still uses it.
* `plots_append()` and `plots_set()` preview the plot they touched whenever the
  session is interactive and the RStudio viewer is there, so tuning a plot shows
  the result. `show = FALSE` turns it off for a run of several.
* Printing a collection lists it by folder, marks the plots carrying a change,
  and makes every name a link that opens that plot in the viewer.
  `plots_show()` is what those links call. `tibble::as_tibble()` gives the plain
  tibble, for when the listing is not what you want.
* Added `plots_dims()` for the default canvas size per plot type, and
  `plots_meta()` for a collection's own name and description.
* `as.gallery()` is now a generic, and a plots collection becomes a gallery
  whose folders are the slashes in its plot names.
* Added an experimental `gallery()` list for browsing nested collections of
  plots in one viewer page.

# ggview 0.2.2
* Added `...` to `ggplot_add.canvas()` method to maintain compatibility with upcoming ggplot2 generic signature changes.

# ggview 0.2.1
* CRAN release.

# ggview 0.2.0
* Implemented `canvas()` element to be used instead of running `ggview()` 
directly.
* Added zoom and pan functionality.
* Added `save_ggplot()` function to save a ggplot with canvas parameters.

# ggview 0.1.0
* Initial release.
