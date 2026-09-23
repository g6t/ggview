# Taking a whole collection out: as a list, as files, as a gallery.

#' @title Every plot in a collection, as a list
#' @description Returns the whole collection as a named list of plots, each one
#'   ready to render: its labels and its canvas size are applied, exactly as
#'   [plots_get()] does for a single plot. The names are the plot names.
#'
#'   `lapply()` and `sapply()` take a list through `as.list()`, so on a
#'   collection they walk its plots and not its columns.
#'
#' @param x A collection, from [plots_init()].
#' @param ... Not used.
#'
#' @return A named list of ggplot objects.
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' plots <- plots_init()
#' plots <- plots_append(plots, p, name = "scatter", show = FALSE)
#' plots <- plots_append(plots, p + geom_smooth(), name = "trend", show = FALSE)
#'
#' names(as.list(plots))
#'
#' @export
as.list.plots_tbl <- function(x, ...) {
  check_plots(x)
  out <- lapply(seq_len(nrow(x)), function(i) plots_pull(x, i))
  stats::setNames(out, x$name)
}

#' @title Save every plot in a collection
#' @description Writes one file per plot, each at its own canvas size. Slashes
#'   in the plot names become folders, which are created when they are missing.
#'
#' @param plots A collection, from [plots_init()].
#' @param dir Directory to write to.
#' @param device File type, and the file extension. See [ggplot2::ggsave()].
#' @param ... Passed on to [save_ggplot()].
#'
#' @return The file paths, invisibly.
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' plots <- plots_init()
#' plots <- plots_append(plots, p, name = "Basics/scatter", width = 5, height = 4,
#'                       show = FALSE)
#'
#' files <- save_plots(plots, dir = tempfile())
#' basename(files)
#'
#' @export
save_plots <- function(plots, dir = ".", device = "png", ...) {
  check_plots(plots)
  if (!is.character(dir) || length(dir) != 1L || is.na(dir) || !nzchar(dir)) {
    cli::cli_abort("{.arg dir} must be a single non-empty string.")
  }
  if (!is.character(device) || length(device) != 1L || is.na(device)) {
    cli::cli_abort("{.arg device} must be a single string, for example {.val png}.")
  }
  if (nrow(plots) == 0) {
    cli::cli_abort(c(
      "The collection is empty.",
      "i" = "Add a plot with {.fn plots_append}."
    ))
  }

  files <- file.path(dir, paste0(plots$name, ".", device))
  for (folder in unique(dirname(files))) {
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  }

  cli::cli_progress_bar("Saving plots", total = nrow(plots))
  for (i in seq_len(nrow(plots))) {
    save_ggplot(plots_pull(plots, i), files[[i]], ...)
    cli::cli_progress_update()
  }
  cli::cli_progress_done()
  cli::cli_alert_success("Saved {nrow(plots)} plot{?s} to {.path {dir}}.")
  invisible(files)
}

#' @title A collection as a content table
#' @description Shapes a collection for bulk writers that take a table of
#'   objects rather than a list: one row per plot, holding the plot, the path to
#'   write it to, and the file type. The plot names become the paths, so slashes
#'   in them stay as folders.
#'
#' @param plots A collection, from [plots_init()].
#' @param path Folder to write into.
#' @param extension File type, and the file extension.
#'
#' @return A tibble of `object`, `name` and `type`, one row per plot. The paths
#'   are the values of `name` and its own names.
#'
#' @examples
#' library(ggplot2)
#' plots <- plots_init()
#' plots <- plots_append(plots, ggplot(mtcars, aes(wt, mpg)) + geom_point(),
#'                       name = "Basics/scatter", show = FALSE)
#'
#' plots_as_content(plots)
#'
#' @export
plots_as_content <- function(plots, path = "plots", extension = "png") {
  check_plots(plots)
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    cli::cli_abort("{.arg path} must be a single non-empty string.")
  }
  if (!is.character(extension) || length(extension) != 1L || is.na(extension) ||
      !nzchar(extension)) {
    cli::cli_abort("{.arg extension} must be a single non-empty string.")
  }
  if (nrow(plots) == 0) {
    cli::cli_abort(c(
      "The collection is empty.",
      "i" = "Add a plot with {.fn plots_append}."
    ))
  }

  # Bulk writers read the path from the names of `name`, so it carries it twice.
  full_names <- file.path(path, paste0(plots$name, ".", extension))
  tibble::tibble(
    object = unname(as.list(plots)),
    name   = stats::setNames(full_names, full_names),
    type   = extension
  )
}

#' @export
as.gallery.plots_tbl <- function(x, ...) {
  check_plots(x)
  entries <- list()
  for (i in seq_len(nrow(x))) {
    entries <- gallery_insert(
      entries,
      strsplit(x$name[[i]], "/", fixed = TRUE)[[1]],
      plots_pull(x, i)
    )
  }
  as.gallery(entries)
}

# Put one plot into a nested list, making the folders on the way.
gallery_insert <- function(entries, path, plot) {
  taken <- entries[[path[[1]]]]
  leaf <- length(path) == 1L
  if (!is.null(taken) && inherits(taken, "ggplot") != leaf) {
    cli::cli_abort(c(
      "{.val {path[[1]]}} is the name of both a plot and a folder.",
      "i" = "Rename one of them."
    ))
  }
  if (leaf) {
    entries[[path]] <- plot
    return(entries)
  }
  entries[[path[[1]]]] <- gallery_insert(taken %||% list(), path[-1], plot)
  entries
}
