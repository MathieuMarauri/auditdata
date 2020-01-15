
#'
#' This function summarises a character vector. It returns table with descriptive
#' statistics and plots the frequency table.
#'
#' @param x a character vector
#' @param max_length the maximum number of different values to display in the freqeuncy
#'   table and the frequency plot
#' @param nchar the maximum number of characters to display for each value in the plot
#' @param numeric_cutoff the minimum number of distinct values required for a numeric
#'   vector not to be coerced to a fator. -1 is the default, meaning no minimum required.
#' @param plot logical, whether to plot the graph. Defaults to TRUE.
#'
#' @details Missing values are removed at the beginning and statistics are computed on the
#'   vector without missing data.
#'
#' @return a list with two tables, one with summary statistics (lenght, number of na,
#'   number of unique and number of duplicate) and a frequency table for the top most
#'   frequent max_length values. If plot is FALSE then the graph of the frequency table is
#'   also returned in the list. If x has 2 unique values then a stack bar chart is
#'   created, if it has only one then only the tables are returned.
#'
#' @import ggplot2
#'
desc_cat <- function(x, max_length = 15, nchar = 20, numeric_cutoff = -1, plot = TRUE) {
  # check argument
  if (!is_categorical(x, numeric_cutoff)) 
    stop(paste0('"x" must be a character, a factor or a numeric vector with less than ', ifelse(numeric_cutoff == -1, Inf, numeric_cutoff), ' unique values'))
  if (class(x) == "integer64") x <- as.character(x)
  # summary statistics
  length <- length(x)
  x <- x[!is.na(x)]
  n_na <- length - length(x)
  n_unique <- length(unique(x))
  if ("factor" %in% class(x)) {
    n_dup_or_lev <- length(levels(x))
    name_dup_or_lev <- "Number of levels"
  } else {
    n_dup_or_lev <- length(unique(x[duplicated(x)]))
    name_dup_or_lev <- "Number of values with duplicates"
  }
  # outputs
  summary_stat <- data.frame(
    Indicator = c(
      "Length", "Number of NAs", "Number of unique values",
      name_dup_or_lev
    ),
    Value = c(length, n_na, n_unique, n_dup_or_lev),
    stringsAsFactors = FALSE
  )
  freq_table <- freq_table(x, max_length = max_length, cum = TRUE)
  if (n_unique > 2) {
    # plots
    freq_table$value <- factor(freq_table$value,
      levels = unique(freq_table$value[order(freq_table$freq)])
    )
    levels(freq_table$value) <- paste0(
      substr(levels(freq_table$value), 0, nchar),
      ifelse(nchar(levels(freq_table$value)) > nchar, "...", "")
    )
    graph <- ggplot(data = freq_table, mapping = aes(x = value, y = freq)) +
      geom_point() +
      geom_segment(mapping = aes(xend = value, yend = 0)) +
      labs(x = "Value", y = "Frequency") +
      coord_flip()
    if (plot) {
      print(graph)
    }
    names(freq_table) <- c("Level", "Freq", "%", "Cumulative freq", "Cumulative %")
    result <- list(
      summary_stat = summary_stat,
      freq_table = freq_table,
      graph = graph
    )
  } else if (n_unique == 1) {
    summary_stat <- rbind(summary_stat, list(Indicator = "Value", Value = unique(x)))
    result <- list(summary_stat = summary_stat)
  } else if (n_unique == 2) {
    # plots
    freq_table$value <- factor(freq_table$value,
      levels = unique(freq_table$value[order(freq_table$freq)])
    )
    levels(freq_table$value) <- paste0(
      substr(levels(freq_table$value), 0, nchar),
      ifelse(nchar(levels(freq_table$value)) > nchar, "...", "")
    )
    graph <- ggplot(
      data = freq_table,
      mapping = aes(x = 1, y = percent, fill = value, label = value)
    ) +
      geom_bar(stat = "identity", width = 0.5, show.legend = FALSE) +
      geom_text(size = 3, position = position_stack(vjust = 0.5)) +
      geom_segment(
        data = NULL, mapping = aes(x = 0.7, y = 50, xend = 1.3, yend = 50),
        linetype = "dashed"
      ) +
      labs(x = "Value", y = "Frequency") +
      scale_x_continuous(limits = c(0.5, 1.5)) +
      coord_flip() +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()
      )
    result <- list(
      summary_stat = summary_stat,
      freq_table = freq_table,
      graph = graph
    )
  }
  invisible(result)
}
