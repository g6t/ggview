# Extracted from test-plots.R:582

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "ggview", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
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

# test -------------------------------------------------------------------------
plots <- plots_set(two_plots(), "Module 1/first", title = "Changed", width = 4)
expect_snapshot({
    print(plots_init())
    print(plots)
    print(plots_params(plots, "Module 1/first"))
    print(plots_dims(heatmap = c(18, 11)))
    print(customizer_default())
  })
