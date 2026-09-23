# A plots collection: one row per plot, its canvas as columns, and whatever
# somebody changed about it kept apart from the plot itself.

plots_format_version <- 1L

#' @title Start a plots collection
#' @description A plots collection keeps a set of ggplots together with the
#'   canvas each one is drawn at and the changes somebody has made to it. It is
#'   a tibble with one row per plot, so `dplyr` verbs work on it.
#'
#'   A change is never written into the plot. [plots_get()] takes the stored
#'   plot, applies the changes, then applies the canvas, so the same plot can be
#'   changed again and again, and [plots_reset()] always has something to go
#'   back to.
#'
#'   Collect plots with [plots_append()], take one out with [plots_get()],
#'   change one with [plots_set()], and write them all with [save_plots()].
#'
#' @param name A name for the collection, for whatever shows it to people.
#' @param description One line saying what the collection is.
#' @param dims Default canvas size per plot type, from [plots_dims()].
#' @param customizer What plots in this collection let people change, from
#'   [customizer()]. It is the default for [plots_append()], which can take
#'   another one for a single plot.
#'
#' @return An empty collection of class `plots_tbl`, with columns `id`, `name`,
#'   `type`, `width`, `height`, `plot`, `customizer` and `values`.
#'
#' @seealso [plots_append()], [plots_get()], [plots_set()], [plots_params()].
#'
#' @examples
#' library(ggplot2)
#' plots <- plots_init(name = "Brand tracker")
#' plots <- plots_append(
#'   plots,
#'   ggplot(mtcars, aes(wt, mpg)) + geom_point() + labs(title = "Weight vs mileage"),
#'   name = "Basics/scatter", type = "point", show = FALSE
#' )
#' plots
#'
#' # Wider heatmaps than the default, for every plot in this collection.
#' plots_init(dims = plots_dims(heatmap = c(18, 11)))
#'
#' @export
plots_init <- function(name = NULL, description = NULL, dims = plots_dims(),
                       customizer = customizer_default()) {
  check_line(name, "name")
  check_line(description, "description")
  check_dims(dims)
  check_customizer(customizer)

  new_plots_tbl(
    tibble::tibble(
      id         = character(),
      name       = character(),
      type       = character(),
      width      = double(),
      height     = double(),
      plot       = list(),
      customizer = list(),
      values     = list()
    ),
    meta = list(
      name        = name,
      description = description,
      dims        = dims,
      customizer  = customizer,
      version     = plots_format_version
    )
  )
}

#' @title Add a plot to a collection
#' @description Adds one plot to the end of a collection. The plot is stored as
#'   it is: anything given in `...` is kept beside it as a change, not written
#'   into it. Adding a name that is already in the collection updates that row
#'   in place and keeps its `id`, so re-running a chunk is safe.
#'
#' @param plots A collection, from [plots_init()].
#' @param plot A ggplot object.
#' @param name Name of the plot. It must be unique in the collection. Slashes
#'   make folders when the collection is saved or browsed, for example
#'   `"Module 1/awareness"`.
#' @param type Plot type, for example `"bar"` or `"heatmap"`. It selects the
#'   default canvas size and groups the collection for bulk re-sizing.
#' @param width,height Canvas size in inches. Each falls back to a [canvas()]
#'   already on the plot, then to the collection's default for `type`.
#' @param customizer What this plot lets people change, from [customizer()].
#'   The collection's own is used when this is `NULL`.
#' @param ... Changes to store with the plot, named after the parameters its
#'   customizer offers — `title = "…"` and so on. [plots_params()] lists them.
#' @param show Whether to preview the plot. It is shown at its true output size
#'   in the RStudio viewer, and drawn the ordinary way anywhere else.
#'
#' @return The collection, with the plot added.
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(factor(cyl))) + geom_bar() + labs(title = "Cylinders")
#'
#' plots <- plots_init()
#' plots <- plots_append(plots, p, name = "Engine/cylinders", type = "bar", show = FALSE)
#'
#' # The plot keeps its own title; the collection keeps the new one.
#' plots <- plots_append(plots, p, name = "Engine/cylinders", type = "bar",
#'                       title = "Most of these are eight-cylinder", show = FALSE)
#' plots_get(plots, "Engine/cylinders")$labels$title
#'
#' @export
plots_append <- function(plots, plot, name, type = NA_character_,
                         width = NULL, height = NULL, customizer = NULL,
                         ..., show = interactive()) {
  check_plots(plots)
  if (!inherits(plot, "ggplot")) {
    cli::cli_abort("{.arg plot} must be a {.cls ggplot} object, not {.cls {class(plot)[[1]]}}.")
  }
  check_name(name)
  check_type(type)

  meta <- plots_meta_raw(plots)
  customizer <- customizer %||% meta$customizer
  check_customizer(customizer)

  size <- plots_dims_lookup(meta$dims, type)
  width <- width %||% plot$canvas$width %||% size$width
  height <- height %||% plot$canvas$height %||% size$height

  values <- check_values(list(...), customizer, plot)

  row <- tibble::tibble(
    id         = plots_new_id(plots$id),
    name       = name,
    type       = as.character(type),
    width      = check_size(width, "width"),
    height     = check_size(height, "height"),
    plot       = list(plot),
    customizer = list(customizer),
    values     = list(values)
  )

  i <- match(name, plots$name)
  if (is.na(i)) {
    # Bare, because base `rbind()` takes the columns through `as.list()`, and a
    # collection answers that with its plots.
    out <- rbind(plots_bare(plots), row)
    verb <- "Added"
  } else {
    row$id <- plots$id[[i]]
    out <- plots
    out[i, ] <- row
    verb <- "Updated"
  }
  out <- new_plots_tbl(out, meta = meta)

  n <- nrow(out)
  cli::cli_alert_success(
    "{verb} {.val {name}} \u2014 {plots_describe(row)}. {n} plot{?s} in the collection."
  )

  if (isTRUE(show)) print(plots_pull(out, match(name, out$name)))
  out
}

#' @title Take a plot out of a collection
#' @description Returns one plot, ready to render: the stored plot with its
#'   changes applied and its canvas set. Printing the result shows it at the
#'   size it will be saved at.
#'
#'   The changes are applied to the stored plot every time, never to an
#'   already-changed one, so asking twice gives the same plot twice.
#'
#' @param plots A collection, from [plots_init()].
#' @param name Name of the plot. The last plot in the collection is returned
#'   when both `name` and `id` are omitted.
#' @param id Id of the plot, as an alternative to `name`. An id stays the same
#'   when a plot is updated, so it is the safer key for code that edits a
#'   stored collection.
#'
#' @return A ggplot object with a [canvas()].
#'
#' @seealso [as.list()] for every plot at once.
#'
#' @examples
#' library(ggplot2)
#' plots <- plots_init()
#' plots <- plots_append(plots, ggplot(mtcars, aes(wt, mpg)) + geom_point(),
#'                       name = "scatter", type = "point", show = FALSE)
#'
#' p <- plots_get(plots, "scatter")
#' p$canvas$width
#'
#' @export
plots_get <- function(plots, name = NULL, id = NULL) {
  check_plots(plots)
  i <- plots_locate(plots, name, id, .last = TRUE)
  cli::cli_alert_info("{.val {plots$name[[i]]}} \u2014 {plots_describe(plots[i, ])}.")
  plots_pull(plots, i)
}

# One plot by row number, without the message. Everything that takes several
# plots at once goes through here.
plots_pull <- function(plots, i) {
  plot <- plots$plot[[i]]
  values <- plots$values[[i]]

  if (length(values)) {
    plot <- do.call(plots$customizer[[i]]$apply, c(list(plot), values))
    if (!inherits(plot, "ggplot")) {
      cli::cli_abort(c(
        "The customizer for {.val {plots$name[[i]]}} did not return a plot.",
        "x" = "It returned {.cls {class(plot)[[1]]}}."
      ))
    }
  }
  plot + canvas(width = plots$width[[i]], height = plots$height[[i]])
}

# "bar, 15 x 9 in", for the messages.
plots_describe <- function(row) {
  size <- paste0(fmt_number(row$width), " \u00d7 ", fmt_number(row$height), " in")
  if (is.na(row$type)) size else paste0(row$type, ", ", size)
}

#' @title Change a plot in a collection
#' @description Records a change against one plot. The stored ggplot is not
#'   touched, so a change is always reversible with [plots_reset()] and never
#'   re-runs the code that built the plot.
#'
#'   Which changes a plot accepts depends on its customizer, and
#'   [plots_params()] lists them. `title`, `subtitle` and `caption` are named
#'   here because most customizers offer them; anything else is passed by name
#'   through `...`.
#'
#' @inheritParams plots_get
#' @param ... Changes named after the parameters the plot's customizer offers.
#'   `NA` takes a value away, for example `title = NA` for no title.
#' @param title,subtitle,caption Label text, for customizers that offer them.
#' @param width,height Canvas size in inches. These belong to the collection
#'   rather than the customizer, so every plot accepts them.
#' @param show Whether to preview the changed plot. It is shown at its true
#'   output size in the RStudio viewer, and drawn the ordinary way anywhere
#'   else. In a pipe of several calls each one previews, so pass `FALSE` when
#'   tuning several plots at once.
#'
#' @return The collection, with that change recorded.
#'
#' @examples
#' library(ggplot2)
#' plots <- plots_init()
#' plots <- plots_append(plots, ggplot(mtcars, aes(wt, mpg)) + geom_point(),
#'                       name = "scatter", type = "point", show = FALSE)
#'
#' plots <- plots_set(plots, "scatter",
#'                    title = "Heavier cars use more fuel",
#'                    caption = "Source: mtcars", width = 8, height = 5)
#' plots
#'
#' # Take a label away again, and then forget the change entirely.
#' plots <- plots_set(plots, "scatter", caption = NA)
#' plots <- plots_reset(plots, "scatter", params = "caption")
#'
#' @export
plots_set <- function(plots, name = NULL, id = NULL, ...,
                      title = NULL, subtitle = NULL, caption = NULL,
                      width = NULL, height = NULL, show = interactive()) {
  check_plots(plots)
  i <- plots_locate(plots, name, id)

  named <- list(title = title, subtitle = subtitle, caption = caption)
  changes <- c(named[!vapply(named, is.null, logical(1))], list(...))
  changes <- check_values(changes, plots$customizer[[i]], plots$plot[[i]])

  if (length(changes)) {
    values <- plots$values[[i]]
    values[names(changes)] <- changes
    plots$values[[i]] <- values
  }
  if (!is.null(width)) plots$width[[i]] <- check_size(width, "width")
  if (!is.null(height)) plots$height[[i]] <- check_size(height, "height")

  if (isTRUE(show)) print(plots_pull(plots, i))
  plots
}

#' @title Forget changes made to a plot
#' @description Drops the changes recorded against a plot and puts its canvas
#'   back to the default for its type. The plot itself was never changed, so
#'   what comes back is what the plot always was.
#'
#' @inheritParams plots_get
#' @param name,id Which plots to reset. Every plot in the collection when both
#'   are omitted.
#' @param params Which changes to forget, by name, for example
#'   `c("title", "width")`. All of them when `NULL`.
#'
#' @return The collection, with those changes forgotten.
#'
#' @examples
#' library(ggplot2)
#' plots <- plots_init()
#' plots <- plots_append(plots, ggplot(mtcars, aes(wt, mpg)) + geom_point(),
#'                       name = "scatter", title = "Changed", show = FALSE)
#'
#' plots_get(plots, "scatter")$labels$title
#' plots <- plots_reset(plots)
#' plots_get(plots, "scatter")$labels$title
#'
#' @export
plots_reset <- function(plots, name = NULL, id = NULL, params = NULL) {
  check_plots(plots)
  rows <- if (is.null(name) && is.null(id)) {
    seq_len(nrow(plots))
  } else {
    plots_locate_all(plots, name, id)
  }
  if (!is.null(params) && (!is.character(params) || anyNA(params))) {
    cli::cli_abort("{.arg params} must be a character vector without {.val NA}.")
  }

  dims <- plots_meta_raw(plots)$dims
  for (i in rows) {
    size <- plots_dims_lookup(dims, plots$type[[i]])
    if (is.null(params)) {
      plots$values[[i]] <- list()
      plots$width[[i]] <- size$width
      plots$height[[i]] <- size$height
    } else {
      plots$values[[i]] <- plots$values[[i]][
        setdiff(names(plots$values[[i]]), params)
      ]
      if ("width" %in% params) plots$width[[i]] <- size$width
      if ("height" %in% params) plots$height[[i]] <- size$height
    }
  }
  cli::cli_alert_success("Reset {length(rows)} plot{?s}.")
  plots
}

#' @title Remove plots from a collection
#' @description Drops one or more plots. Everything else keeps its order and
#'   its id.
#'
#' @param plots A collection, from [plots_init()].
#' @param name Names of the plots to remove.
#' @param id Ids of the plots to remove, as an alternative to `name`.
#'
#' @return The collection, without those plots.
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' plots <- plots_init()
#' plots <- plots_append(plots, p, name = "keep", show = FALSE)
#' plots <- plots_append(plots, p, name = "drop", show = FALSE)
#'
#' plots_remove(plots, "drop")
#'
#' @export
plots_remove <- function(plots, name = NULL, id = NULL) {
  check_plots(plots)
  plots[-plots_locate_all(plots, name, id), ]
}

#' @title What a plot lets people change
#' @description Lists one plot's parameters: what each one is called, what kind
#'   of value it takes, what it is by default, and what it has been changed to.
#'   This is what a program reads to offer the right control for each one, and
#'   what [plots_set()] checks a change against.
#'
#'   The canvas comes last. Every plot accepts `width` and `height` whatever
#'   its customizer offers.
#'
#' @inheritParams plots_get
#'
#' @return A list with one entry per parameter, each holding `key`, `type`,
#'   `label`, `default`, and `value` when the parameter has been changed. A
#'   `choice` also carries its `choices`, a `mapping` its `keys` and what it
#'   maps `to`, and a `number` any `min`, `max` and `step`.
#'
#' @seealso [customizer()], [plots_set()].
#'
#' @examples
#' library(ggplot2)
#' plots <- plots_init()
#' plots <- plots_append(plots, ggplot(mtcars, aes(wt, mpg)) + geom_point(),
#'                       name = "scatter", type = "point",
#'                       title = "Weight vs mileage", show = FALSE)
#'
#' plots_params(plots, "scatter")
#'
#' @export
plots_params <- function(plots, name = NULL, id = NULL) {
  check_plots(plots)
  i <- plots_locate(plots, name, id, .last = TRUE)

  params <- customizer_params(plots$customizer[[i]], plots$plot[[i]])
  values <- plots$values[[i]]
  records <- lapply(names(params), function(key) {
    param_record(key, params[[key]], values[[key]])
  })

  size <- plots_dims_lookup(plots_meta_raw(plots)$dims, plots$type[[i]])
  # A canvas is a change only when it is not the default for its type.
  changed <- function(value, default) if (isTRUE(all.equal(value, default))) NULL else value
  records <- c(records, list(
    param_record("width", param_number("Width (in)", default = size$width,
                                       min = 1, max = 60, step = 0.5),
                 changed(plots$width[[i]], size$width)),
    param_record("height", param_number("Height (in)", default = size$height,
                                        min = 1, max = 60, step = 0.5),
                 changed(plots$height[[i]], size$height))
  ))
  structure(records, names = vapply(records, function(r) r$key, character(1)),
            class = "plots_params")
}

param_record <- function(key, param, value) {
  record <- c(list(key = key), unclass(param))
  record$value <- value
  record[!vapply(record, is.null, logical(1))]
}

#' @title What a collection is
#' @description The collection's own details, rather than any one plot's: the
#'   name and description given to [plots_init()], how many plots it holds, and
#'   the version of the format it is stored in.
#'
#' @param plots A collection, from [plots_init()].
#'
#' @return A list of `name`, `description`, `plots` and `version`.
#'
#' @examples
#' plots_meta(plots_init(name = "Brand tracker", description = "Q3 wave"))
#'
#' @export
plots_meta <- function(plots) {
  check_plots(plots)
  meta <- plots_meta_raw(plots)
  list(
    name        = meta$name,
    description = meta$description,
    plots       = nrow(plots),
    version     = meta$version
  )
}

#' @title Show a plot from the collection that was printed last
#' @description Opens one plot in the viewer at its canvas size, and says
#'   nothing else. Printing a collection makes every plot name a link to this
#'   function, so clicking a name opens that plot. The links need an IDE that
#'   runs them, such as RStudio.
#'
#' @param id Name of the plot, or its id.
#'
#' @return The plot, invisibly.
#'
#' @seealso [plots_get()], which takes a plot from a collection you name.
#'
#' @examples
#' library(ggplot2)
#' plots <- plots_init()
#' plots <- plots_append(plots, ggplot(mtcars, aes(wt, mpg)) + geom_point(),
#'                       name = "scatter", show = FALSE)
#' print(plots)
#'
#' if (rstudioapi::isAvailable()) plots_show("scatter")
#'
#' @export
plots_show <- function(id) {
  plots <- the$printed
  if (is.null(plots)) {
    cli::cli_abort(c(
      "No collection has been printed yet.",
      "i" = "Print one, then click a plot name in it."
    ))
  }
  i <- match(id, plots$id)
  if (is.na(i)) i <- match(id, plots$name)
  if (is.na(i)) {
    cli::cli_abort(c(
      "{.val {id}} is not in the collection that was printed last.",
      "i" = "Print the collection again, then click a plot name in it."
    ))
  }
  invisible(print(plots_pull(plots, i)))
}

# --- the collection object ---------------------------------------------------

# The collection printed last, so the links in a printed collection have
# something to render from. A link can only carry a literal.
the <- new.env(parent = emptyenv())
the$printed <- NULL

new_plots_tbl <- function(x, meta = NULL) {
  attr(x, "meta") <- meta %||% attr(x, "meta") %||% plots_meta_default()
  class(x) <- unique(c("plots_tbl", class(x)))
  x
}

plots_meta_default <- function() {
  list(
    name = NULL, description = NULL, dims = plots_dims(),
    customizer = customizer_default(), version = plots_format_version
  )
}

plots_meta_raw <- function(x) {
  attr(x, "meta") %||% plots_meta_default()
}

plots_bare <- function(x) {
  class(x) <- setdiff(class(x), "plots_tbl")
  x
}

# Keep the class and the collection's own details through dplyr verbs.
#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.plots_tbl <- function(data, template) {
  new_plots_tbl(NextMethod(), meta = plots_meta_raw(template))
}

# `as_tibble()` is the way out of the listing: it gives the plain tibble, which
# prints as any tibble does, list-columns and all.
#' @exportS3Method tibble::as_tibble
as_tibble.plots_tbl <- function(x, ...) {
  x <- plots_bare(x)
  attr(x, "meta") <- NULL
  x
}

# Subsetting goes through a bare copy, because a tibble takes its columns with
# `lapply()`, and a collection answers that with its plots. The argument shapes
# are the ones `[.tbl_df` accepts.
#' @export
`[.plots_tbl` <- function(x, i, j, ..., drop = FALSE) {
  bare <- plots_bare(x)
  n_real_args <- nargs() - !missing(drop)
  columns_only <- n_real_args <= 2L

  out <- if (columns_only) {
    if (missing(i)) bare else bare[i]
  } else if (missing(i) && missing(j)) {
    bare[, , drop = drop]
  } else if (missing(i)) {
    bare[, j, drop = drop]
  } else if (missing(j)) {
    bare[i, , drop = drop]
  } else {
    bare[i, j, drop = drop]
  }

  if (is.data.frame(out)) new_plots_tbl(out, meta = plots_meta_raw(x)) else out
}

# An id only has to be unique within its collection, and tempfile() gives one
# without touching the session's random seed.
plots_new_id <- function(taken = character()) {
  repeat {
    id <- basename(tempfile(pattern = ""))
    if (!id %in% taken) return(id)
  }
}

# Row index of one plot. `.last = TRUE` allows both keys to be missing.
plots_locate <- function(plots, name = NULL, id = NULL, .last = FALSE,
                         call = parent.frame()) {
  if (is.null(name) && is.null(id)) {
    if (!.last) {
      cli::cli_abort("Give a {.arg name} or an {.arg id}.", call = call)
    }
    if (nrow(plots) == 0) {
      cli::cli_abort(
        c("The collection is empty.", "i" = "Add a plot with {.fn plots_append}."),
        call = call
      )
    }
    return(nrow(plots))
  }
  key <- name %||% id
  if (length(key) != 1L) {
    cli::cli_abort("Name exactly one plot, not {length(key)}.", call = call)
  }
  plots_locate_all(plots, name, id, call = call)
}

# Row indices, for the vectorised keys.
plots_locate_all <- function(plots, name = NULL, id = NULL, call = parent.frame()) {
  if (is.null(name) && is.null(id)) {
    cli::cli_abort("Give a {.arg name} or an {.arg id}.", call = call)
  }
  if (!is.null(name) && !is.null(id)) {
    cli::cli_abort("Give a {.arg name} or an {.arg id}, not both.", call = call)
  }
  arg <- if (is.null(name)) "id" else "name"
  key <- name %||% id
  if (!is.character(key) || !length(key) || anyNA(key)) {
    cli::cli_abort(
      "{.arg {arg}} must be a character vector without {.val NA}.",
      call = call
    )
  }

  i <- match(key, if (is.null(name)) plots$id else plots$name)
  if (anyNA(i)) {
    cli::cli_abort(
      c(
        "No plot with {arg} {.val {key[is.na(i)]}}.",
        "i" = "The collection holds {nrow(plots)} plot{?s}: {.code print(plots)} lists them."
      ),
      call = call
    )
  }
  i
}

# --- default canvas sizes ----------------------------------------------------

#' @title Default canvas size per plot type
#' @description The canvas size [plots_append()] gives a plot when the plot
#'   brings none of its own, and the size [plots_reset()] goes back to. Pass the
#'   result to [plots_init()] to change the defaults for one collection.
#'
#' @param ... Named sizes, each a numeric `c(width, height)` in inches, for
#'   example `heatmap = c(18, 11)`. They replace the built-in default for those
#'   types and leave the rest alone.
#' @param .default Size for a plot whose type has no entry.
#'
#' @return A named list of sizes, of class `plots_dims`.
#'
#' @examples
#' plots_dims()
#' plots_dims(heatmap = c(18, 11), bar = c(10, 6))
#'
#' @export
plots_dims <- function(..., .default = c(15, 9)) {
  defaults <- list(
    bar         = c(12, 8),
    diverging   = c(15, 8),
    dodged_bar  = c(15, 9),
    dumbbell    = c(16, 9),
    heatmap     = c(16, 10),
    line        = c(15, 9),
    stacked_bar = c(15, 9)
  )
  sizes <- list(...)
  if (length(sizes) && !all(nzchar(names(sizes) %||% ""))) {
    cli::cli_abort("Every size in {.arg ...} must be named after a plot type.")
  }
  for (type in names(sizes)) check_dim(sizes[[type]], type)
  check_dim(.default, ".default")

  out <- utils::modifyList(defaults, lapply(sizes, as.double))
  out <- out[order(names(out))]
  out[[".default"]] <- as.double(.default)
  structure(out, class = "plots_dims")
}

plots_dims_lookup <- function(dims, type) {
  size <- if (!is.na(type)) dims[[type]] else NULL
  size <- size %||% dims[[".default"]]
  list(width = size[[1]], height = size[[2]])
}

# --- validation --------------------------------------------------------------

check_plots <- function(plots, call = parent.frame()) {
  if (!inherits(plots, "plots_tbl")) {
    cli::cli_abort(
      c(
        "{.arg plots} must be a {.cls plots_tbl}, not {.cls {class(plots)[[1]]}}.",
        "i" = "Start a collection with {.fn plots_init}."
      ),
      call = call
    )
  }
  invisible(plots)
}

check_name <- function(name, call = parent.frame()) {
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
    cli::cli_abort("{.arg name} must be a single non-empty string.", call = call)
  }
  invisible(name)
}

check_line <- function(text, arg, call = parent.frame()) {
  if (is.null(text)) return(invisible(NULL))
  if (!is.character(text) || length(text) != 1L || is.na(text)) {
    cli::cli_abort("{.arg {arg}} must be a single string, or {.code NULL}.", call = call)
  }
  invisible(text)
}

check_type <- function(type, call = parent.frame()) {
  if (length(type) != 1L || !(is.character(type) || is.na(type))) {
    cli::cli_abort("{.arg type} must be a single string, or {.val NA}.", call = call)
  }
  invisible(type)
}

check_size <- function(size, arg, call = parent.frame()) {
  if (!is.numeric(size) || length(size) != 1L || is.na(size) || size <= 0) {
    cli::cli_abort("{.arg {arg}} must be a single positive number of inches.", call = call)
  }
  as.double(size)
}

check_dim <- function(size, type, call = parent.frame()) {
  if (!is.numeric(size) || length(size) != 2L || anyNA(size) || any(size <= 0)) {
    cli::cli_abort(
      "Size for {.field {type}} must be two positive numbers, {.code c(width, height)}.",
      call = call
    )
  }
  invisible(size)
}

check_dims <- function(dims, call = parent.frame()) {
  if (!inherits(dims, "plots_dims")) {
    cli::cli_abort(
      c(
        "{.arg dims} must come from {.fn plots_dims}.",
        "i" = "For example {.code plots_init(dims = plots_dims(heatmap = c(18, 11)))}."
      ),
      call = call
    )
  }
  invisible(dims)
}

# Changes, checked against the parameters the plot's customizer offers.
check_values <- function(values, customizer, plot, call = parent.frame()) {
  if (!length(values)) return(list())
  if (!length(names(values)) || !all(nzchar(names(values)))) {
    cli::cli_abort("Every change must be named after a parameter.", call = call)
  }

  params <- customizer_params(customizer, plot, call = call)
  unknown <- setdiff(names(values), names(params))
  if (length(unknown)) {
    cli::cli_abort(
      c(
        "This plot has no parameter {.val {unknown}}.",
        "i" = "It takes {.val {names(params)}}, plus {.arg width} and {.arg height}.",
        "i" = "{.code plots_params(plots, {.val {'<name>'}})} lists them in full."
      ),
      call = call
    )
  }
  for (key in names(values)) {
    values[[key]] <- param_check(params[[key]], values[[key]], key, call = call)
  }
  values
}

# --- small helpers -----------------------------------------------------------

# A label the collection can show: a single string, or NA when the plot has
# none, or has one ggplot2 does not render as plain text.
plot_label <- function(plot, slot) {
  label <- plot$labels[[slot]]
  if (is.null(label) && slot %in% c("x", "y")) label <- plot_label_built(plot, slot)
  if (is.character(label) && length(label) == 1L) label else NA_character_
}

# An axis title usually comes from the mapping, and a plot only works that out
# when it is built. Older ggplot2 versions cannot say, and report none.
plot_label_built <- function(plot, slot) {
  get_labs <- tryCatch(getExportedValue("ggplot2", "get_labs"), error = function(e) NULL)
  if (is.null(get_labs)) return(NULL)
  tryCatch(get_labs(plot)[[slot]], error = function(e) NULL)
}
