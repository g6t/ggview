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
#' # A customizer that recolours a plot, offering one colour per level.
#' recolour <- customizer(
#'   apply = function(plot, colours = NULL) {
#'     if (is.null(colours)) return(plot)
#'     suppressMessages(plot + scale_fill_manual(values = unlist(colours)))
#'   },
#'   params = function(plot) {
#'     list(colours = param_mapping("Colours", keys = c("a", "b"), to = "colour"))
#'   }
#' )
#' recolour
#'
#' @export
customizer <- function(apply, params) {
  check_customizer_fun(apply, "apply")
  check_customizer_fun(params, "params")
  check_reserved(names(formals(apply))[-1])
  structure(list(apply = apply, params = params), class = "plots_customizer")
}

#' @export
print.plots_customizer <- function(x, ...) {
  cli::cli_rule(left = "{.cls plots_customizer}")
  cli::cli_verbatim(paste0("  parameters: ", paste(customizer_names(x), collapse = ", ")))
  invisible(x)
}

#' @title The customizer every plot starts with
#' @description Offers the labels any plot has: its title, subtitle, footnote,
#'   axis titles, and where the legend sits. Each one defaults to what the plot
#'   already carries, `NA` removes it, and leaving it alone changes nothing.
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

customize_labels <- function(plot, title = NULL, subtitle = NULL, caption = NULL,
                             x = NULL, y = NULL, legend = NULL) {
  labels <- list(title = title, subtitle = subtitle, caption = caption, x = x, y = y)
  labels <- labels[!vapply(labels, is.null, logical(1))]
  if (length(labels)) {
    # NA is "take this label away", which ggplot2 spells as NULL.
    labels <- lapply(labels, function(value) if (is.na(value)) NULL else value)
    plot <- plot + do.call(ggplot2::labs, labels)
  }
  if (!is.null(legend) && !is.na(legend)) {
    plot <- plot + ggplot2::theme(legend.position = legend)
  }
  plot
}

params_labels <- function(plot) {
  list(
    title    = param_text("Title",         default = plot_label(plot, "title")),
    subtitle = param_text("Subtitle",      default = plot_label(plot, "subtitle")),
    caption  = param_text("Footnote",      default = plot_label(plot, "caption")),
    x        = param_text("X axis title",  default = plot_label(plot, "x")),
    y        = param_text("Y axis title",  default = plot_label(plot, "y")),
    legend   = param_choice("Legend", choices = legend_positions,
                            default = legend_position(plot))
  )
}

# --- the parameters a customizer can offer -----------------------------------

#' @title Parameters a customizer offers
#' @description The kinds of parameter a [customizer()] can offer, one function
#'   per kind. A program reads the kind from [plots_params()] and shows the
#'   control that fits it: a box for text, a picker for a colour, a list for a
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
#' @param to What each key maps to: `"colour"`, `"text"` or `"number"`.
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
#' param_mapping("Colours", keys = c("Ours", "Rival A"), to = "colour")
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
param_colour <- function(label, default = NULL) {
  new_param("colour", label, default)
}

#' @rdname param
#' @export
param_mapping <- function(label, keys, to = c("colour", "text", "number"),
                          default = NULL) {
  # `to` rather than `value_type`, so that `param$value` on an untouched
  # parameter cannot partial-match its way to the wrong answer.
  new_param("mapping", label, default, keys = check_choices(keys, "keys"),
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
    colour = if (!is_colour(value)) bad(cli::format_inline("a colour, such as {.val #36c8ef}")),
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
        colour = vapply(value, is_colour, logical(1)),
        number = vapply(value, function(v) is.numeric(v) && length(v) == 1L, logical(1)),
        vapply(value, function(v) is.character(v) && length(v) == 1L, logical(1))
      )
      if (!all(ok)) bad(cli::format_inline("a mapping to {param$to} values"))
    },
    cli::cli_abort("Unknown parameter type {.val {param$type}}.", call = call)
  )
  value
}

is_colour <- function(x) {
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
  free <- codetools::findGlobals(fun, merge = FALSE)$variables
  risky <- free[vapply(free, risky_variable, logical(1), environment(fun))]
  if (length(risky)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} uses {length(risky)} variable{?s} it does not define: {.val {risky}}.",
        "i" = "A customizer is read back in another session, where that is gone.",
        "i" = "Pass the value in as a parameter instead."
      ),
      call = call
    )
  }
  invisible(fun)
}

# A variable is a risk when it binds in the session that wrote the customizer,
# or nowhere at all. One held in a package, or captured by the function itself,
# travels with it.
risky_variable <- function(name, env) {
  while (!identical(env, emptyenv())) {
    if (exists(name, envir = env, inherits = FALSE)) {
      return(identical(env, globalenv()))
    }
    env <- parent.env(env)
  }
  TRUE
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

# Parameter names without a plot to ask about, for printing a customizer.
customizer_names <- function(customizer) {
  names(formals(customizer$apply))[-1]
}

legend_position <- function(plot) {
  position <- plot$theme$legend.position
  if (is.character(position) && length(position) == 1L) position else NA_character_
}
