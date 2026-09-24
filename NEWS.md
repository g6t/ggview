# ggview 0.2.4
* Added a plots collection — `plots_init()` and friends — for keeping many plots together with
  the canvas each one is drawn at and the changes made to them. A change is stored beside the
  plot rather than written into it, and `customizer()` declares what a given plot lets people
  change, so a program can build a control for each parameter. See `vignette("plots")`.
* Printing a plot that has a `canvas()` no longer fails where there is no viewer. It draws the
  ordinary way instead, so a ggview plot can appear in a knitted document. `save_ggplot()` still
  uses the canvas.

# ggview 0.2.3
* Added an experimental `gallery()` list for browsing nested collections of plots in one viewer
  page.

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
