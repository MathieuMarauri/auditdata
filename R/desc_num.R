#'
#' This function summarises a numeric vector. It returns table with descriptive statistics
#' and plots the density function, a boxplot and the cumulative density function of the
#' vector.
#'
#' @param x a numeric vector
#' @param numeric_cutoff the minimum number of distinct values required for a numeric
#'   vector not to be coerced to a fator. -1 is the default, meaning no minimum required.
#' @param plot logical, whether to plot the graph. Defaults to TRUE.
#'
#' @details Missing values are removed at the beginning and statistics are computed on the
#'   vector without missing data.
#'
#' @return a list with 3 tables. One with summary statistics (length, number of missing,
#'   min, max, mean, range, inter_quartile range, sd), another ne with the top 5 lowest
#'   and highest values and one with some percentiles (0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9,
#'   0.95, 1). If plot is true then a plot with the empirical density, a boxplot and the
#'   cumulative mepirical density is also returned.
#'
#' @import ggplot2
#' @importFrom cowplot plot_grid
#'
desc_num <- function(x, numeric_cutoff = -1, plot = TRUE) {
  # check argument
  if (!is_numeric(x, numeric_cutoff)) 
    stop(paste0('"x" must be a numeric vector with more than ', numeric_cutoff, ' unique values'))
  # summary stats
  length <- length(x)
  x <- x[!is.na(x)]
  n_na <- length - length(x)
  percentiles <- quantile(
    x,
    probs = c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1),
    na.rm = FALSE,
    names = FALSE
  )
  range <- percentiles[9] - percentiles[1]
  range_iq <- percentiles[6] - percentiles[4]
  mean <- mean(x)
  sd <- sd(x)
  lowest <- sort(unique(x))[1:5]
  highest <- sort(unique(x), decreasing = TRUE)[1:5]
  # output
  summary_stat <- data.frame(
    Indicator = c(
      "Length", "Number of NAs", "Min", "Mean", "Max", "Range", "Inter-quartile range",
      "Standard deviation"
    ),
    Value = c(length, n_na, percentiles[1], mean, percentiles[9], range, range_iq, sd)
  )
  extreme_values <- data.frame(
    value = c(paste(lowest, collapse = ", "), paste(highest, collapse = ", "))
  )
  rownames(extreme_values) <- c("Lowest values", "Highest values")
  percentiles <- rbind(
    level = c("Min", 0.05, 0.1, 0.25, "Median", 0.75, 0.9, 0.95, "Max"),
    percentiles
  )
  rownames(percentiles) <- c("Level", "Value")
  # plots
  # histogram
  hist <- ggplot() +
    aes(x = x) +
    geom_density(fill = "grey30", alpha = .5) +
    labs(y = "Density") +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  # boxplot
  boxplot <- ggplot() +
    aes(x = 1, y = x) +
    geom_boxplot() +
    coord_flip() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    )
  # ecdf
  plot_data <- data.frame(x = x, y = ecdf(x)(x))
  ecdf <- ggplot(data = plot_data, mapping = aes(x = x, y = y)) +
    geom_line() +
    labs(
      x = "Data",
      y = "Cumulative density"
    )

  graph <- cowplot::plot_grid(hist, boxplot, ecdf,
    align = "v",
    nrow = 3,
    rel_heights = c(1 / 2, 1 / 6, 1 / 3)
  )
  if (plot) {
    print(graph)
  }
  invisible(list(
    summary_stat = summary_stat,
    extreme_values = extreme_values,
    percentiles = percentiles,
    graph = graph
  ))
}
