# What somebody may change about a plot, and what changing it does.

# Geometry is not customizable: the collection owns it.
plots_reserved <- c("width", "height")

#' @title Describe how a plot can be customized
#' @description A customizer is a pair of functions. `apply` takes a stored
#'   plot and the values somebody changed, and returns the changed plot.
#'   `params` takes the stored plot and returns the parameters it accepts,
#'   each one built with a `param_*()` function.
#'
#'   Keeping the two together is what lets a program offer the right control
#'   for each parameter without knowing anything about plots: [plots_params()]
#'   answers what may be changed, and [plots_get()] applies it.
#'
#'   [customizer_default()] handles the labels every plot has. Write your own
#'   for anything else — a palette, a set of categories to show, a text size.
#'   A customizer may rebuild a plot from its data rather than add to it, so
#'   what it can offer is not limited to what `ggplot2` lets you add on top.
#'
#' @param apply A function whose first argument is the plot. Its other
#'   arguments are the parameters, and each one must default to `NULL`, meaning
#'   "leave this alone". It returns a ggplot.
#' @param params A function of the plot, returning a named list of `param_*()`
#'   objects. The names are the argument names of `apply`.
#'
#' @return A customizer.
#'
#' @seealso [customizer_default()], [param_text()], [plots_params()].
#'
#' @examples
#' library(ggplot2)
#'
#' # A customizer that recolors a plot, offering one color per level.
#' recolor <- customizer(
#'   apply = function(plot, colors = NULL) {
#'     if (is.null(colors)) return(plot)
#'     suppressMessages(plot + scale_fill_manual(values = unlist(colors)))
#'   },
#'   params = function(plot) {
#'     list(colors = param_mapping("Colors", keys = c("a", "b"), to = "color"))
#'   }
#' )
#' recolor
#'
#' @export
customizer <- function(apply, params) {
  check_customizer_fun(apply, "apply")
  check_customizer_fun(params, "params")
  keys <- names(formals(apply))[-1]
  check_reserved(keys)
  structure(list(apply = apply, params = params, keys = keys), class = "plots_customizer")
}

#' @export
print.plots_customizer <- function(x, ...) {
  cli::cli_rule(left = "{.cls plots_customizer}")
  names <- x$keys
  if (identical(names, "...")) names <- "set by the plot"
  cli::cli_verbatim(paste0("  parameters: ", paste(names, collapse = ", ")))
  invisible(x)
}

#' @title The customizer every plot starts with
#' @description Offers what any plot has, whatever it draws: its title,
#'   subtitle, footnote and axis titles, the size of each of those, the size and
#'   line wrapping of the axis text, the overall text size, where the legend sits
#'   and which way it runs, and whether the gridlines show. Each one defaults to
#'   what the plot already carries, `NA` leaves it alone, and not mentioning it
#'   changes nothing.
#'
#'   The overall text size scales every text size the theme sets in points by
#'   the same ratio, so it works on a complete theme too. A specific size given
#'   with it wins. Showing a grid brings back its major lines only.
#'
#'   Sizes are changed in place, so a title drawn as a `ggtext` textbox stays a
#'   textbox. Replacing it outright is an error in ggplot2, not a silent loss.
#'
#' @return A customizer.
#'
#' @seealso [customizer()] to write one of your own.
#'
#' @examples
#' customizer_default()
#'
#' @export
customizer_default <- function() {
  customizer(apply = customize_labels, params = params_labels)
}

legend_positions <- c("right", "left", "top", "bottom", "none")
legend_directions <- c("horizontal", "vertical")

# The theme element each size parameter changes.
size_elements <- c(
  title_size = "plot.title", subtitle_size = "plot.subtitle",
  caption_size = "plot.caption", x_size = "axis.title.x",
  y_size = "axis.title.y", text_size = "text"
)

customize_labels <- function(plot,
                             title = NULL, title_size = NULL,
                             subtitle = NULL, subtitle_size = NULL,
                             caption = NULL, caption_size = NULL,
                             x = NULL, x_size = NULL,
                             y = NULL, y_size = NULL,
                             axis_text_size = NULL, axis_text_wrap = NULL,
                             text_size = NULL,
                             legend = NULL, legend_direction = NULL,
                             x_grid = NULL, y_grid = NULL) {
  labels <- list(title = title, subtitle = subtitle, caption = caption, x = x, y = y)
  labels <- labels[!vapply(labels, is.null, logical(1))]
  if (length(labels)) {
    # NA is "take this label away", which ggplot2 spells as NULL.
    labels <- lapply(labels, function(value) if (is.na(value)) NULL else value)
    plot <- plot + do.call(ggplot2::labs, labels)
  }

  # The overall size first, so a specific size given alongside it wins.
  if (given(text_size)) plot <- text_resize(plot, text_size)
  sizes <- list(title_size = title_size, subtitle_size = subtitle_size,
                caption_size = caption_size, x_size = x_size, y_size = y_size)
  for (key in names(sizes)) {
    if (given(sizes[[key]])) plot <- element_resize(plot, size_elements[[key]], sizes[[key]])
  }

  # A theme that sets both axis texts makes the parent element a no-op, so set both.
  if (given(axis_text_size)) {
    plot <- element_resize(plot, "axis.text.x", axis_text_size)
    plot <- element_resize(plot, "axis.text.y", axis_text_size)
  }
  if (given(axis_text_wrap)) {
    for (aesthetic in c("x", "y")) plot <- wrap_axis(plot, aesthetic, axis_text_wrap)
  }

  settings <- list()
  if (given(legend)) settings$legend.position <- legend
  if (given(legend_direction)) settings$legend.direction <- legend_direction
  # Showing a grid brings back its major lines only. Hiding it hides both.
  if (given(x_grid)) {
    settings$panel.grid.major.x <- grid_element(x_grid)
    if (!x_grid) settings$panel.grid.minor.x <- ggplot2::element_blank()
  }
  if (given(y_grid)) {
    settings$panel.grid.major.y <- grid_element(y_grid)
    if (!y_grid) settings$panel.grid.minor.y <- ggplot2::element_blank()
  }
  if (length(settings)) plot <- plot + do.call(ggplot2::theme, settings)

  plot
}

params_labels <- function(plot) {
  theme <- plot_theme(plot)
  size <- function(key) {
    param_number(size_labels[[key]], default = element_size(theme, size_elements[[key]]),
                 min = 4, max = 80, step = 1)
  }

  list(
    title            = param_text("Title", default = plot_label(plot, "title")),
    title_size       = size("title_size"),
    subtitle         = param_text("Subtitle", default = plot_label(plot, "subtitle")),
    subtitle_size    = size("subtitle_size"),
    caption          = param_text("Footnote", default = plot_label(plot, "caption")),
    caption_size     = size("caption_size"),
    x                = param_text("X axis title", default = plot_label(plot, "x")),
    x_size           = size("x_size"),
    y                = param_text("Y axis title", default = plot_label(plot, "y")),
    y_size           = size("y_size"),
    axis_text_size   = param_number("Axis text size",
                                    default = element_size(theme, "axis.text.x"),
                                    min = 4, max = 80, step = 1),
    axis_text_wrap   = param_number("Axis text wrap (characters)",
                                    min = 5, max = 120, step = 1),
    text_size        = size("text_size"),
    legend           = param_choice("Legend", choices = legend_positions,
                                    default = legend_position(plot)),
    legend_direction = param_choice("Legend direction", choices = legend_directions,
                                    default = theme_string(plot, "legend.direction")),
    x_grid           = param_flag("X gridlines", default = grid_shown(theme, "x")),
    y_grid           = param_flag("Y gridlines", default = grid_shown(theme, "y"))
  )
}

size_labels <- c(
  title_size = "Title size", subtitle_size = "Subtitle size",
  caption_size = "Footnote size", x_size = "X axis title size",
  y_size = "Y axis title size", text_size = "Text size"
)

# A value somebody actually asked for. `NULL` is "leave it alone" and so is `NA`
# for a setting that has no empty state, such as a legend position.
given <- function(value) !is.null(value) && !is.na(value)

# Change one theme element's size without changing what kind of element it is.
# A title drawn as a markdown textbox stays a textbox: ggplot2 refuses to merge
# two kinds of element, so replacing it outright is an error.
element_resize <- function(plot, name, size) {
  current <- plot$theme[[name]]
  element <- if (inherits(current, "element")) {
    current$size <- size
    current
  } else {
    ggplot2::element_text(size = size)
  }
  plot + do.call(ggplot2::theme, stats::setNames(list(element), name))
}

# Change the overall text size. A theme that gives an element its own size in
# points would ignore the root `text` element, so every such size is scaled by the
# same ratio. Sizes given as `rel()` follow the root on their own.
text_resize <- function(plot, size) {
  current <- element_size(plot_theme(plot), "text")
  plot <- element_resize(plot, "text", size)
  if (is.null(current) || current <= 0) return(plot)
  ratio <- size / current
  own <- vapply(plot$theme, function(element) {
    inherits(element, "element_text") && is.numeric(element$size) &&
      !inherits(element$size, "rel")
  }, logical(1))
  for (name in setdiff(names(plot$theme)[own], "text")) {
    plot <- element_resize(plot, name, plot$theme[[name]]$size * ratio)
  }
  plot
}

# Break a discrete axis's labels over several lines. A scale the plot sets itself
# is copied and keeps everything it had (position, expansion, its own labels); only
# its labels are wrapped. Without one, the plot is built once to learn whether the
# axis is discrete at all.
wrap_axis <- function(plot, aesthetic, width) {
  scale <- plot$scales$get_scales(aesthetic)
  if (is.null(scale)) {
    built <- suppressMessages(ggplot2::ggplot_build(plot))
    trained <- built$plot$scales$get_scales(aesthetic)
    if (is.null(trained) || !isTRUE(trained$is_discrete())) return(plot)
    build <- if (aesthetic == "x") ggplot2::scale_x_discrete else ggplot2::scale_y_discrete
    return(suppressMessages(plot + build(labels = wrap_labels(width))))
  }
  if (!isTRUE(scale$is_discrete())) return(plot)

  scale <- scale$clone()
  wrap <- wrap_labels(width)
  own <- scale$labels
  scale$labels <- if (is.function(own)) {
    function(x) wrap(own(x))
  } else if (is.character(own)) {
    stats::setNames(wrap(own), names(own))
  } else {
    wrap
  }
  suppressMessages(plot + scale)
}

wrap_labels <- function(width) {
  function(x) {
    vapply(
      as.character(x),
      function(label) paste(strwrap(label, width = width), collapse = "\n"),
      character(1), USE.NAMES = FALSE
    )
  }
}

# An empty line inherits the theme's own gridline styling, so a grid can be put
# back without knowing how it was drawn.
grid_element <- function(show) {
  if (isTRUE(show)) ggplot2::element_line() else ggplot2::element_blank()
}

# The plot's theme filled in from the defaults it inherits, for reading what a
# setting currently is. Older ggplot2 versions cannot say, and report nothing.
plot_theme <- function(plot) {
  tryCatch(ggplot2::complete_theme(plot$theme), error = function(e) NULL)
}

element_size <- function(theme, name) {
  if (is.null(theme)) return(NULL)
  size <- tryCatch(ggplot2::calc_element(name, theme)$size, error = function(e) NULL)
  if (is.numeric(size) && length(size) == 1L) size else NULL
}

grid_shown <- function(theme, axis) {
  if (is.null(theme)) return(NULL)
  element <- tryCatch(
    ggplot2::calc_element(paste0("panel.grid.major.", axis), theme),
    error = function(e) NULL
  )
  if (is.null(element)) NULL else !inherits(element, "element_blank")
}

# --- the parameters a customizer can offer -----------------------------------

#' @title Add parameters to a customizer
#' @description Builds a customizer that offers everything `base` offers plus
#'   whatever `apply` and `params` add. Use it to keep the labels from
#'   [customizer_default()] while adding something of your own, rather than
#'   listing them again.
#'
#'   A parameter of the same name replaces the one it shadows.
#'
#' @param base The customizer to build on, usually [customizer_default()].
#' @inheritParams customizer
#'
#' @return A customizer.
#'
#' @examples
#' library(ggplot2)
#'
#' # The default labels, plus a switch for the legend's own title.
#' customizer_extend(
#'   customizer_default(),
#'   apply = function(plot, legend_title = NULL) {
#'     if (is.null(legend_title)) plot else plot + labs(color = legend_title)
#'   },
#'   params = function(plot) list(legend_title = param_text("Legend title"))
#' )
#'
#' @export
customizer_extend <- function(base, apply, params) {
  check_customizer(base)
  added <- customizer(apply, params)

  # A value goes to whichever customizer offers it, and to the added one when both
  # do, because the added parameter shadows the base one.
  out <- customizer(
    apply = function(plot, ...) {
      values <- list(...)
      mine <- intersect(names(values), added$keys)
      plot <- do.call(base$apply, c(list(plot), values[setdiff(names(values), mine)]))
      do.call(added$apply, c(list(plot), values[mine]))
    },
    params = function(plot) {
      mine <- added$params(plot)
      theirs <- base$params(plot)
      c(theirs[setdiff(names(theirs), names(mine))], mine)
    }
  )
  out$keys <- union(base$keys, added$keys)
  out
}

#' @title Parameters a customizer offers
#' @description The kinds of parameter a [customizer()] can offer, one function
#'   per kind. A program reads the kind from [plots_params()] and shows the
#'   control that fits it: a box for text, a picker for a color, a list for a
#'   choice, one row per key for a mapping.
#'
#'   Every parameter carries a `default`, which is what the plot does without
#'   a change. It is what a reset goes back to.
#'
#' @param label What to call the parameter, for a person reading it.
#' @param default The value the plot already has. `NULL` when it has none.
#' @param choices The values allowed, for `param_choice()` and
#'   `param_choices()`.
#' @param keys The things being mapped, for `param_mapping()` — the levels of a
#'   plot's fill, say. Read them off the plot.
#' @param labels What to call each key, when the keys themselves read badly —
#'   `TRUE` and `FALSE`, say. One per key.
#' @param to What each key maps to: `"color"`, `"text"` or `"number"`.
#' @param min,max,step Bounds and increment for `param_number()`. Optional.
#'
#' @return A parameter.
#'
#' @seealso [customizer()], [plots_params()].
#'
#' @examples
#' param_text("Title", default = "Brand funnel")
#' param_number("Text size", default = 11, min = 6, max = 40)
#' param_choice("Legend", choices = c("right", "bottom", "none"))
#' param_choices("Brands to show", choices = c("Ours", "Rival A", "Rival B"))
#' param_mapping("Colors", keys = c("Ours", "Rival A"), to = "color")
#' param_mapping("Colors", keys = c("TRUE", "FALSE"), labels = c("Ours", "Others"))
#'
#' @name param
NULL

#' @rdname param
#' @export
param_text <- function(label, default = NULL) {
  new_param("text", label, default)
}

#' @rdname param
#' @export
param_number <- function(label, default = NULL, min = NULL, max = NULL, step = NULL) {
  for (bound in c("min", "max", "step")) {
    value <- switch(bound, min = min, max = max, step = step)
    if (!is.null(value) && (!is.numeric(value) || length(value) != 1L || is.na(value))) {
      cli::cli_abort("{.arg {bound}} must be a single number, or {.code NULL}.")
    }
  }
  new_param("number", label, default, min = min, max = max, step = step)
}

#' @rdname param
#' @export
param_flag <- function(label, default = NULL) {
  new_param("flag", label, default)
}

#' @rdname param
#' @export
param_choice <- function(label, choices, default = NULL) {
  new_param("choice", label, default, choices = check_choices(choices))
}

#' @rdname param
#' @export
param_choices <- function(label, choices, default = NULL) {
  new_param("choices", label, default, choices = check_choices(choices))
}

#' @rdname param
#' @export
param_color <- function(label, default = NULL) {
  new_param("color", label, default)
}

#' @rdname param
#' @export
param_mapping <- function(label, keys, to = c("color", "text", "number"),
                          labels = NULL, default = NULL) {
  keys <- check_choices(keys, "keys")
  if (!is.null(labels) && (!is.character(labels) || length(labels) != length(keys))) {
    cli::cli_abort("{.arg labels} must be one string per key, or {.code NULL}.")
  }
  # `to` rather than `value_type`, so that `param$value` on an untouched
  # parameter cannot partial-match its way to the wrong answer.
  new_param("mapping", label, default, keys = keys, labels = labels,
            to = match.arg(to))
}

new_param <- function(type, label, default, ...) {
  if (!is.character(label) || length(label) != 1L || is.na(label) || !nzchar(label)) {
    cli::cli_abort("{.arg label} must be a single non-empty string.")
  }
  param <- c(list(type = type, label = label, default = default), list(...))
  structure(param, class = "plots_param")
}

check_choices <- function(choices, arg = "choices", call = parent.frame()) {
  if (!is.character(choices) || !length(choices) || anyNA(choices)) {
    cli::cli_abort(
      "{.arg {arg}} must be a character vector without {.val NA}.",
      call = call
    )
  }
  choices
}

# --- checking a value against the parameter that accepts it ------------------

# Returns the value, or stops. `NA` is always allowed: it means "no value",
# which a customizer reads as "take this away".
param_check <- function(param, value, key, call = parent.frame()) {
  if (is.null(value)) return(NULL)
  single_na <- length(value) == 1L && !is.list(value) && is.na(value)
  if (single_na) return(value)

  # `want` arrives already formatted: cli inserts it, it does not read it again.
  bad <- function(want) {
    cli::cli_abort(
      c("{.arg {key}} must be {want}.",
        "i" = "It is {.field {param$label}}, a {.emph {param$type}} parameter."),
      call = call
    )
  }

  switch(
    param$type,
    text = if (!is.character(value) || length(value) != 1L) bad("a single string"),
    number = {
      if (!is.numeric(value) || length(value) != 1L) bad("a single number")
      if (!is.null(param$min) && value < param$min) bad(cli::format_inline("at least {param$min}"))
      if (!is.null(param$max) && value > param$max) bad(cli::format_inline("at most {param$max}"))
    },
    flag = if (!is.logical(value) || length(value) != 1L) bad(cli::format_inline("{.code TRUE} or {.code FALSE}")),
    color = if (!is_color(value)) bad(cli::format_inline("a color, such as {.val #36c8ef}")),
    choice = {
      if (!is.character(value) || length(value) != 1L) bad("a single string")
      if (!value %in% param$choices) {
        bad(cli::format_inline("one of {.val {param$choices}}"))
      }
    },
    choices = {
      if (!is.character(value)) bad("a character vector")
      unknown <- setdiff(value, param$choices)
      if (length(unknown)) bad(cli::format_inline("chosen from {.val {param$choices}}"))
    },
    mapping = {
      value <- as.list(value)
      if (!length(names(value)) || anyNA(names(value)) || !all(nzchar(names(value)))) {
        bad("a named vector or list")
      }
      unknown <- setdiff(names(value), param$keys)
      if (length(unknown)) bad(cli::format_inline("named after {.val {param$keys}}"))
      ok <- switch(
        param$to,
        color = vapply(value, is_color, logical(1)),
        number = vapply(value, function(v) is.numeric(v) && length(v) == 1L, logical(1)),
        vapply(value, function(v) is.character(v) && length(v) == 1L, logical(1))
      )
      if (!all(ok)) bad(cli::format_inline("a mapping to {param$to} values"))
    },
    cli::cli_abort("Unknown parameter type {.val {param$type}}.", call = call)
  )
  value
}

is_color <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) return(FALSE)
  !inherits(try(grDevices::col2rgb(x), silent = TRUE), "try-error")
}

# --- checks ------------------------------------------------------------------

# A customizer must survive being saved and read back somewhere else, so it may
# not reach for anything that only exists in the session that wrote it.
check_customizer_fun <- function(fun, arg, call = parent.frame()) {
  if (!is.function(fun)) {
    cli::cli_abort("{.arg {arg}} must be a function.", call = call)
  }
  if (!length(formals(fun))) {
    cli::cli_abort("{.arg {arg}} must take the plot as its first argument.", call = call)
  }
  globals <- codetools::findGlobals(fun, merge = FALSE)
  env <- environment(fun)
  where <- function(names) vapply(names, binding_of, character(1), env)
  # Only a name that lives in the writing session is a risk. A name bound nowhere is
  # usually a data column (`aes(fill = brand)`, `filter(brand == ...)`), and a
  # function bound nowhere is a package that is not attached right now.
  risky <- c(
    globals$variables[where(globals$variables) == "session"],
    globals$functions[where(globals$functions) == "session"]
  )
  if (length(risky)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} uses {length(risky)} name{?s} it does not define: {.val {risky}}.",
        "i" = "A customizer is read back in another session, where that is gone.",
        "i" = "Pass it in as a parameter, or capture it in a function that builds the
               customizer."
      ),
      call = call
    )
  }
  invisible(fun)
}

# Where a name binds, starting from a function's own environment. "local" means
# it travels with the function, in a package or in the function's own closure.
# "session" means it lives in the session that wrote the customizer and nowhere
# else. "nowhere" means it cannot be found at all right now.
binding_of <- function(name, env) {
  while (!identical(env, emptyenv())) {
    if (exists(name, envir = env, inherits = FALSE)) {
      return(if (identical(env, globalenv())) "session" else "local")
    }
    env <- parent.env(env)
  }
  "nowhere"
}

check_customizer <- function(customizer, call = parent.frame()) {
  if (!inherits(customizer, "plots_customizer")) {
    cli::cli_abort(
      c(
        "{.arg customizer} must come from {.fn customizer}.",
        "i" = "{.fn customizer_default} is the one every plot starts with."
      ),
      call = call
    )
  }
  invisible(customizer)
}

# The parameters a customizer offers for one plot, checked on the way out.
customizer_params <- function(customizer, plot, call = parent.frame()) {
  params <- customizer$params(plot)
  if (!is.list(params) || (length(params) && !length(names(params)))) {
    cli::cli_abort("A customizer's {.arg params} must return a named list.", call = call)
  }
  wrong <- !vapply(params, inherits, logical(1), "plots_param")
  if (any(wrong)) {
    cli::cli_abort(
      c("A customizer's {.arg params} must hold {.fn param_text} and friends.",
        "x" = "{.val {names(params)[wrong]}} {?is/are} something else."),
      call = call
    )
  }
  check_reserved(names(params), call = call)
  params
}

# The collection owns the canvas, so a customizer may not offer it.
check_reserved <- function(names, call = parent.frame()) {
  taken <- intersect(names, plots_reserved)
  if (length(taken)) {
    cli::cli_abort(
      c("A customizer may not offer {.val {taken}}.",
        "i" = "The collection owns the canvas: set {.arg width} and {.arg height} instead."),
      call = call
    )
  }
  invisible(names)
}

legend_position <- function(plot) theme_string(plot, "legend.position")

theme_string <- function(plot, name) {
  value <- plot$theme[[name]]
  if (is.character(value) && length(value) == 1L) value else NA_character_
}
