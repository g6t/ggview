p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()

titled <- p + ggplot2::labs(title = "Title", subtitle = "Subtitle", caption = "Caption")

quietly <- function(expr) suppressMessages(expr)

two_plots <- function() {
  quietly({
    plots <- plots_init(name = "Test bundle")
    plots <- plots_append(plots, titled, name = "Module 1/first", type = "bar", show = FALSE)
    plots_append(plots, p, name = "Module 1/second", type = "heatmap", show = FALSE)
  })
}

test_that("a new collection is empty and keeps what it was given", {
  plots <- plots_init(name = "Bundle", description = "One line")

  expect_s3_class(plots, "plots_tbl")
  expect_s3_class(plots, "tbl_df")
  expect_equal(nrow(plots), 0)
  expect_named(
    plots,
    c("id", "name", "type", "width", "height", "plot", "customizer", "values")
  )
  expect_equal(plots_meta(plots), list(name = "Bundle", description = "One line",
                                       plots = 0L, version = 1L))

  wide <- plots_init(dims = plots_dims(heatmap = c(18, 11)))
  expect_equal(plots_meta_raw(wide)$dims$heatmap, c(18, 11))

  expect_error(plots_init(dims = list(bar = c(1, 1))), "plots_dims")
  expect_error(plots_init(customizer = "nope"), "customizer")
  expect_error(plots_init(name = c("a", "b")), "single string")
})

test_that("appending stores the plot as it is, and changes beside it", {
  plots <- quietly(plots_append(plots_init(), titled, name = "first",
                                title = "Mine", show = FALSE))

  expect_equal(nrow(plots), 1)
  expect_true(nzchar(plots$id))
  expect_equal(plots$values[[1]], list(title = "Mine"))
  # The plot itself is untouched.
  expect_equal(plots$plot[[1]]$labels$title, "Title")
  expect_equal(plots_pull(plots, 1)$labels$title, "Mine")

  bare <- quietly(plots_append(plots_init(), p, name = "bare", show = FALSE))
  expect_equal(bare$values[[1]], list())

  expect_true(anyDuplicated(two_plots()$id) == 0)
})

test_that("canvas size comes from the argument, then the plot, then the type", {
  by_type <- quietly(plots_append(plots_init(), p, name = "a", type = "heatmap", show = FALSE))
  expect_equal(c(by_type$width, by_type$height), c(16, 10))

  by_default <- quietly(plots_append(plots_init(), p, name = "a", type = "nope", show = FALSE))
  expect_equal(c(by_default$width, by_default$height), c(15, 9))

  by_canvas <- quietly(plots_append(plots_init(), p + canvas(7, 3), name = "a",
                                    type = "heatmap", show = FALSE))
  expect_equal(c(by_canvas$width, by_canvas$height), c(7, 3))

  by_arg <- quietly(plots_append(plots_init(), p + canvas(7, 3), name = "a", type = "heatmap",
                                 width = 4, height = 2, show = FALSE))
  expect_equal(c(by_arg$width, by_arg$height), c(4, 2))

  custom <- plots_init(dims = plots_dims(bar = c(3, 2)))
  custom <- quietly(plots_append(custom, p, name = "a", type = "bar", show = FALSE))
  expect_equal(c(custom$width, custom$height), c(3, 2))
})

test_that("appending a name twice updates that plot in place", {
  plots <- two_plots()
  id <- plots$id[[1]]

  expect_message(
    plots <- plots_append(plots, p, name = "Module 1/first", type = "line", show = FALSE),
    "Updated"
  )
  expect_equal(nrow(plots), 2)
  expect_equal(plots$id[[1]], id)
  expect_equal(plots$type[[1]], "line")
  expect_equal(plots$name[[2]], "Module 1/second")
})

test_that("bad input stops", {
  plots <- plots_init()

  expect_error(plots_append(mtcars, p, name = "a"), "plots_tbl")
  expect_error(plots_append(plots, mtcars, name = "a"), "ggplot")
  expect_error(plots_append(plots, p, name = ""), "non-empty")
  expect_error(plots_append(plots, p, name = "a", width = -1), "positive")
  expect_error(plots_append(plots, p, name = "a", nope = 1), "no parameter")
  expect_error(plots_dims(c(1, 2)), "named")
  expect_error(plots_dims(bar = 1), "two positive numbers")
  expect_error(plots_get(plots), "empty")
  expect_error(plots_get(two_plots(), "nope"), "No plot with name")
  expect_error(plots_get(two_plots(), name = "a", id = "b"), "not both")
})

test_that("getting a plot applies its changes and its canvas", {
  plots <- quietly(plots_append(plots_init(), titled, name = "first",
                                width = 6, height = 4, show = FALSE))
  plots <- plots_set(plots, "first", title = "New title", subtitle = NA)

  got <- plots_get(plots, "first")
  expect_equal(got$labels$title, "New title")
  expect_null(got$labels$subtitle)
  expect_equal(got$labels$caption, "Caption")
  expect_equal(got$canvas$width, 6)
  expect_equal(got$canvas$height, 4)
  expect_s3_class(got, "ggview")

  # Applying twice is applying once.
  expect_equal(plots_pull(plots, 1)$labels, plots_pull(plots, 1)$labels)

  plots <- two_plots()
  expect_equal(plots_get(plots)$labels$title, plots_get(plots, "Module 1/second")$labels$title)
  expect_equal(plots_get(plots, id = plots$id[[1]])$canvas$width, plots$width[[1]])
})

test_that("setting records a change and leaves the rest alone", {
  plots <- two_plots()

  changed <- plots_set(plots, "Module 1/first", title = "T", caption = "C",
                       legend = "bottom", width = 3, height = 2)
  expect_equal(changed$values[[1]]$title, "T")
  expect_equal(changed$values[[1]]$legend, "bottom")
  # The canvas is a change on top of the size the plot was added with.
  expect_equal(c(changed$values[[1]]$width, changed$values[[1]]$height), c(3, 2))
  expect_equal(c(changed$width[[1]], changed$height[[1]]), c(12, 8))
  expect_equal(plots_get(changed, "Module 1/first")$canvas$width, 3)
  expect_equal(changed$values[[2]], list())

  # A second change adds to the first rather than replacing it.
  twice <- plots_set(changed, "Module 1/first", subtitle = "S")
  expect_named(twice$values[[1]],
               c("title", "caption", "width", "height", "legend", "subtitle"))

  expect_error(plots_set(plots, "Module 1/first", color = "red"), "no parameter")
  expect_error(plots_set(plots, "Module 1/first", legend = "sideways"), "must be one of")
  expect_error(plots_set(plots, "Module 1/first", title = 1), "single string")
  expect_error(plots_set(plots, "nope", title = "T"), "No plot with name")
  expect_error(plots_set(plots, title = "T"), "name")
})

test_that("resetting forgets changes and restores the canvas", {
  plots <- plots_set(two_plots(), "Module 1/first", title = "T", width = 3)

  one <- quietly(plots_reset(plots, "Module 1/first"))
  expect_equal(one$values[[1]], list())
  expect_equal(plots_pull(one, 1)$canvas$width, 12)
  expect_equal(plots_pull(one, 1)$labels$title, "Title")

  some <- quietly(plots_reset(plots, "Module 1/first", params = "width"))
  expect_equal(some$values[[1]]$title, "T")
  expect_equal(plots_pull(some, 1)$canvas$width, 12)

  all <- quietly(plots_reset(plots_set(plots, "Module 1/second", title = "S")))
  expect_equal(lengths(all$values), c(0L, 0L))

  # A size the script asked for is what a reset goes back to, not the type default.
  declared <- quietly(plots_append(plots_init(), p, name = "tall", type = "bar",
                                   height = 4.5, show = FALSE))
  declared <- quietly(plots_reset(plots_set(declared, "tall", height = 20), "tall"))
  expect_equal(plots_pull(declared, 1)$canvas$height, 4.5)

  expect_error(quietly(plots_reset(plots, "Module 1/first", params = "nope")),
               "Nothing to reset")
})

test_that("plots can be removed", {
  plots <- two_plots()

  one <- plots_remove(plots, "Module 1/first")
  expect_equal(nrow(one), 1)
  expect_equal(one$name, "Module 1/second")
  expect_s3_class(one, "plots_tbl")

  expect_equal(nrow(plots_remove(plots, id = plots$id)), 0)
  expect_error(plots_remove(plots, "nope"), "No plot with name")
})

test_that("params describe what a plot accepts", {
  plots <- plots_set(two_plots(), "Module 1/first", title = "T")
  params <- plots_params(plots, "Module 1/first")

  expect_s3_class(params, "plots_params")
  expect_named(params, c("title", "subtitle", "caption", "x", "y", "legend",
                         "width", "height"))
  expect_equal(params$title$type, "text")
  expect_equal(params$title$default, "Title")
  expect_equal(params$title$value, "T")
  expect_null(params$subtitle$value)
  expect_equal(params$legend$choices, c("right", "left", "top", "bottom", "none"))

  # The canvas is offered by the collection, not the customizer.
  expect_equal(params$width$type, "number")
  expect_equal(params$width$default, 12)
  expect_null(params$width$value)
  expect_equal(plots_params(plots_set(plots, "Module 1/first", width = 4),
                            "Module 1/first")$width$value, 4)
})

test_that("the canvas survives whatever unit it arrived in", {
  px <- quietly(plots_append(plots_init(), p + canvas(1200, 900, units = "px", dpi = 300),
                             name = "a", show = FALSE))
  expect_equal(c(px$width, px$height), c(4, 3))

  cm <- quietly(plots_append(plots_init(), p + canvas(2.54, 5.08, units = "cm"),
                             name = "a", show = FALSE))
  expect_equal(c(cm$width, cm$height), c(1, 2))
})

test_that("a canvas that is not usable stops at render", {
  plots <- two_plots()
  plots$width[[1]] <- NA_real_
  expect_error(plots_pull(plots, 1), "not usable")

  plots$width[[1]] <- -3
  expect_error(plots_pull(plots, 1), "not usable")
})

test_that("a name may not have an empty folder", {
  for (bad in c("a/", "/a", "a//b")) {
    expect_error(quietly(plots_append(plots_init(), p, name = bad, show = FALSE)),
                 "empty folder")
  }
})

test_that("a customizer must be able to travel", {
  expect_error(customizer("nope", function(plot) list()), "must be a function")
  expect_error(customizer(function() NULL, function(plot) list()), "first argument")

  # Nowhere to be found: it would fail wherever the customizer is read.
  missing_var <- function(plot, a = NULL) paste(plot, no_such_object)
  environment(missing_var) <- globalenv()
  expect_error(customizer(missing_var, function(plot) list()), "does not define")

  # Found only in the session that wrote it: the same problem, later.
  assign("a_session_object", "gone tomorrow", envir = globalenv())
  on.exit(rm("a_session_object", envir = globalenv()), add = TRUE)
  session_var <- function(plot, a = NULL) paste(plot, a_session_object)
  environment(session_var) <- globalenv()
  expect_error(customizer(session_var, function(plot) list()), "does not define")

  # A function it does not define is the same problem: it would not be found.
  assign("a_project_builder", function(x) x, envir = globalenv())
  on.exit(rm("a_project_builder", envir = globalenv()), add = TRUE)
  calls_global <- function(plot, a = NULL) a_project_builder(plot)
  environment(calls_global) <- globalenv()
  expect_error(customizer(calls_global, function(plot) list()), "does not define")

  # Calling a package's function, either way round, is not.
  qualified <- function(plot, a = NULL) ggplot2::labs(title = a)
  bare <- function(plot, a = NULL) labs(title = a)
  environment(qualified) <- globalenv()
  environment(bare) <- globalenv()
  expect_s3_class(customizer(qualified, function(plot) list()), "plots_customizer")
  expect_s3_class(customizer(bare, function(plot) list()), "plots_customizer")

  # A package's own objects, and things the function holds itself, are fine.
  expect_s3_class(customizer_default(), "plots_customizer")
  held <- local({
    inner <- "kept"
    customizer(function(plot, a = NULL) paste(plot, inner), function(plot) list())
  })
  expect_s3_class(held, "plots_customizer")
})

test_that("a customizer may not take over the canvas", {
  expect_error(
    customizer(function(plot, width = NULL) plot, function(plot) list()),
    "may not offer"
  )

  # And the same check again where the parameters are actually built.
  sneaky <- customizer(
    apply = function(plot, ...) plot,
    params = function(plot) list(width = param_number("Width"))
  )
  plots <- quietly(plots_append(plots_init(), p, name = "a", customizer = sneaky,
                                show = FALSE))
  expect_error(plots_params(plots, "a"), "may not offer")

  wrong <- customizer(
    apply = function(plot, a = NULL) plot,
    params = function(plot) list(a = "not a parameter")
  )
  plots <- quietly(plots_append(plots_init(), p, name = "a", customizer = wrong,
                                show = FALSE))
  expect_error(plots_params(plots, "a"), "param_text")
})

test_that("a custom customizer runs, and is checked", {
  recolor <- customizer(
    apply = function(plot, colors = NULL) {
      if (is.null(colors)) return(plot)
      suppressMessages(plot + ggplot2::scale_color_manual(values = unlist(colors)))
    },
    params = function(plot) {
      list(colors = param_mapping("Colors", keys = c("4", "6", "8"), to = "color"))
    }
  )
  dots <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, color = factor(cyl))) +
    ggplot2::geom_point()

  plots <- quietly(plots_append(plots_init(customizer = recolor), dots,
                                name = "dots", show = FALSE))
  expect_equal(names(plots_params(plots, "dots")), c("colors", "width", "height"))

  plots <- plots_set(plots, "dots", colors = c("4" = "#36c8ef"))
  expect_s3_class(plots_pull(plots, 1), "ggplot")

  expect_error(plots_set(plots, "dots", colors = c("9" = "#36c8ef")), "named after")
  expect_error(plots_set(plots, "dots", colors = c("4" = "not a color")), "mapping to color")

  # A customizer that returns something else is caught at render time.
  broken <- customizer(function(plot, a = NULL) "not a plot", function(plot) {
    list(a = param_text("A"))
  })
  oops <- quietly(plots_append(plots_init(customizer = broken), p, name = "x",
                               a = "go", show = FALSE))
  expect_error(plots_pull(oops, 1), "did not return a plot")
})

test_that("a customizer can be extended rather than replaced", {
  extended <- customizer_extend(
    customizer_default(),
    apply = function(plot, note = NULL) {
      if (is.null(note)) plot else plot + ggplot2::labs(tag = note)
    },
    params = function(plot) list(note = param_text("Note"))
  )
  plots <- quietly(plots_append(plots_init(customizer = extended), titled,
                                name = "a", show = FALSE))

  expect_true(all(c("title", "note") %in% names(plots_params(plots, "a"))))

  both <- plots_set(plots, "a", title = "New", note = "Draft")
  rendered <- plots_pull(both, 1)
  expect_equal(rendered$labels$title, "New")
  expect_equal(rendered$labels$tag, "Draft")
})

test_that("a mapping arrives whole, and can be labelled", {
  keyed <- customizer(
    apply = function(plot, colors = NULL) {
      if (!is.null(colors)) attr(plot, "seen") <- colors
      plot
    },
    params = function(plot) {
      list(colors = param_mapping("Colors", keys = c("a", "b"), to = "color",
                                  labels = c("Ours", "Theirs"),
                                  default = list(a = "#111111", b = "#222222")))
    }
  )
  plots <- quietly(plots_append(plots_init(customizer = keyed), p, name = "x",
                                show = FALSE))

  # One key changed, the rest kept.
  one <- plots_set(plots, "x", colors = c(a = "#36c8ef"))
  expect_equal(one$values[[1]]$colors, list(a = "#36c8ef", b = "#222222"))

  # And a second change builds on the first.
  two <- plots_set(one, "x", colors = c(b = "#50bd90"))
  expect_equal(two$values[[1]]$colors, list(a = "#36c8ef", b = "#50bd90"))

  expect_equal(plots_params(plots, "x")$colors$labels, c("Ours", "Theirs"))
  expect_error(param_mapping("C", keys = c("a", "b"), labels = "only one"),
               "one string per key")
})

test_that("each kind of parameter checks its value", {
  expect_error(param_check(param_text("T"), 1, "k"), "single string")
  expect_error(param_check(param_number("N", min = 2), 1, "k"), "at least")
  expect_error(param_check(param_number("N", max = 2), 3, "k"), "at most")
  expect_error(param_check(param_flag("F"), "yes", "k"), "TRUE")
  expect_error(param_check(param_color("C"), "nope", "k"), "color")
  expect_error(param_check(param_choice("C", c("a")), "b", "k"), "one of")
  expect_error(param_check(param_choices("C", c("a")), c("a", "b"), "k"), "chosen from")
  expect_error(param_check(param_mapping("M", "a"), "b", "k"), "named vector")

  expect_equal(param_check(param_text("T"), NA, "k"), NA)
  expect_null(param_check(param_text("T"), NULL, "k"))
  expect_equal(param_check(param_color("C"), "#36c8ef", "k"), "#36c8ef")
  expect_equal(param_check(param_color("C"), "red", "k"), "red")
  expect_error(param_number("N", min = "low"), "single number")
  expect_error(param_choice("C", 1), "character vector")
})

test_that("dplyr verbs keep the class and the collection's details", {
  skip_if_not_installed("dplyr")
  plots <- two_plots()

  bigger <- dplyr::mutate(plots, width = ifelse(type == "heatmap", 20, width))
  expect_s3_class(bigger, "plots_tbl")
  expect_equal(plots_meta(bigger)$name, "Test bundle")
  expect_equal(bigger$width[[2]], 20)
  expect_equal(plots_get(bigger, "Module 1/second")$canvas$width, 20)

  expect_s3_class(dplyr::filter(plots, type == "bar"), "plots_tbl")
  expect_s3_class(dplyr::arrange(plots, name), "plots_tbl")
  expect_s3_class(plots[1, ], "plots_tbl")
})

test_that("a collection turns into a list, content and files", {
  plots <- plots_set(two_plots(), "Module 1/first", title = "Changed")

  as_list <- as.list(plots)
  expect_named(as_list, c("Module 1/first", "Module 1/second"))
  expect_s3_class(as_list[[1]], "ggview")
  expect_equal(as_list[[1]]$labels$title, "Changed")

  content <- plots_as_content(plots, path = "plots")
  expect_named(content, c("object", "name", "type"))
  expect_equal(content$name[[1]], "plots/Module 1/first.png")
  expect_equal(names(content$name), unname(content$name))

  dir <- tempfile()
  files <- save_plots(plots, dir = dir, width = 4, height = 3)
  expect_equal(basename(files), c("first.png", "second.png"))
  expect_true(all(file.exists(files)))
  expect_equal(basename(dirname(files[[1]])), "Module 1")

  expect_error(save_plots(plots_init(), dir = dir), "empty")
  expect_error(plots_as_content(plots_init()), "empty")
})

test_that("a collection browses as a gallery", {
  plots <- quietly(plots_append(two_plots(), p, name = "loose", show = FALSE))

  gal <- as.gallery(plots)
  expect_s3_class(gal, "gallery")
  expect_named(gal, c("Module 1", "loose"))
  expect_named(gal[["Module 1"]], c("first", "second"))
  expect_s3_class(gal[["Module 1"]][["first"]], "ggview")

  clash <- quietly(plots_append(two_plots(), p, name = "Module 1", show = FALSE))
  expect_error(as.gallery(clash), "both a plot and a folder")
})

test_that("as_tibble gives the plain tibble", {
  plain <- tibble::as_tibble(two_plots())

  expect_s3_class(plain, "tbl_df")
  expect_false(inherits(plain, "plots_tbl"))
  expect_null(attr(plain, "meta"))
  expect_equal(nrow(plain), 2)
})

test_that("a collection survives a save and a read", {
  file <- tempfile(fileext = ".rds")
  plots <- plots_set(two_plots(), "Module 1/first", title = "Changed", width = 4)
  saveRDS(plots, file)
  back <- readRDS(file)

  expect_s3_class(back, "plots_tbl")
  expect_equal(back$id, plots$id)
  expect_equal(plots_meta(back)$name, "Test bundle")
  expect_equal(plots_pull(back, 1)$labels$title, "Changed")
  expect_equal(plots_params(back, "Module 1/first")$width$value, 4)
  expect_equal(plots_pull(quietly(plots_reset(back)), 1)$labels$title, "Title")
})

test_that("a plot prints without a viewer instead of stopping", {
  # Knitting, a plain console and Rscript all have no viewer. The canvas cannot
  # be honored there, but the plot must still draw.
  file <- tempfile(fileext = ".png")
  grDevices::png(file)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_no_error(print(p + canvas(8, 5)))
  expect_no_error(print(plots_get(two_plots(), "Module 1/first")))
})

test_that("a printed collection can be shown by its links", {
  the$printed <- NULL
  expect_error(plots_show("Module 1/first"), "No collection")

  plots <- two_plots()
  print(plots)
  expect_equal(the$printed$id, plots$id)
  expect_error(plots_show("nope"), "printed last")

  old <- options(cli.hyperlink = TRUE, cli.hyperlink_run = TRUE)
  on.exit(options(old), add = TRUE)

  # The IDE echoes what the link runs, so it runs the name, not the id.
  linked <- plots_link("first", "Module 1/first", 10)
  expect_true(grepl('plots_show("Module 1/first")', linked, fixed = TRUE))
  expect_equal(cli::ansi_nchar(linked), 10)
  expect_true(grepl('"a \\"b\\""', plots_link("a", 'a "b"', 5), fixed = TRUE))

  options(cli.hyperlink = FALSE, cli.hyperlink_run = FALSE)
  expect_equal(plots_link("first", "Module 1/first", 10), "first     ")
})

test_that("printing lists the collection and its parameters", {
  plots <- plots_set(two_plots(), "Module 1/first", title = "Changed", width = 4)
  expect_snapshot({
    print(plots_init())
    print(plots)
    print(plots_params(plots, "Module 1/first"))
    print(plots_dims(heatmap = c(18, 11)))
    print(customizer_default())
  })
})
