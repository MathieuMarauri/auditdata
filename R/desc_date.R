#'
#' This function plots the total number of records by group. Group with no records are
#' plotted with a red point and a
#' line indicating the expected number of records by hour is also added to the graph.
#'
#' @param x a date vector
#' @param group the type of grouping, one of hour, weekday, month and year.
#' @param title logical, whether to add a title to the graph.
#'
#' @return the plot generated
#'
#' @import data.table
#' @import ggplot2
#'
plot_freq_date_group <- function(x, group, title = TRUE) {
  # # check argument
  # if (!is_date(x)) {
  #   stop('"x" must be a date.')
  # }
  group <- match.arg(group, choices = c("hour", "weekday", "month", "year"))
  date_pattern <- c("hour" = "%H", "weekday" = "%A", "month" = "%b", "year" = "%Y")
  min_year <- as.numeric(format(min(x), "%Y"))
  max_year <- as.numeric(format(max(x), "%Y"))
  factor_levels <- list(
    "hour" = c(paste0("0", 0:9), as.character(10:23)),
    "weekday" = format(seq(
      from = as.Date("2018-04-02"),
      to = as.Date("2018-04-08"),
      by = "day"
    ), "%A"),
    "month" = month.abb,
    "year" = as.character(min_year:max_year)
  )
  # aggregate the data by group and count the number of records
  plot_data <- data.table(date = format(x, date_pattern[group]))[, .(count = .N), by = date]
  # merge x and complete sequence of hours to indicate missing dates
  plot_data <- merge(
    x = plot_data,
    y = data.table(date = factor_levels[[group]]),
    by = "date",
    all = TRUE
  )
  # coerce to factor to properly order the graph
  plot_data$date <- factor(plot_data$date, levels = factor_levels[[group]], ordered = TRUE)
  # plot
  plot <- ggplot(data = plot_data[!is.na(count)], mapping = aes(x = date, y = count)) +
    geom_point() +
    geom_segment(mapping = aes(xend = date, yend = 0))
  if (nrow(plot_data[is.na(count)]) > 0) {
    plot <- plot +
      geom_point(
        data = plot_data[is.na(count)], mapping = aes(x = date, y = 0),
        color = "red"
      )
  }
  plot <- plot +
    geom_hline(yintercept = length(x) / length(levels(plot_data$date)), linetype = "dashed") +
    labs(x = stringi::stri_trans_totitle(group), y = "Number of records")
  if (group != "hour") {
    plot <- plot + coord_flip()
  }
  if (title) {
    plot <- plot + labs(
      title = "Total number of records by group. Group without data are plotted in red.",
      subtitle = "The dashed line correspond to the expected number of records by group."
    )
  }
  return(plot)
}

#'
#' This function plots the frequency of records by group of dates for every scopes of the
#' date vector.
#'
#' @param x a date vector
#'
#' @return a frequency plot by scope organized in a grid.
#'
#' @details The scope can be hourly, daily, monhly and/or yearly. If a date vector has
#'   more than two different days then it has a daily scope. The same goes for months,
#'   years and hours.
#'
#' @importFrom cowplot ggdraw draw_label plot_grid
#'
plot_freq_date <- function(x) {
  # check argument
  # if (!is_date(x)) {
  #   stop('"x" must be a date.')
  # }
  # indicators of hourly, daily, monthly and yearly data
  hourly <- length(unique(format(x, "%H"))) > 1
  daily <- length(unique(format(x, "%d"))) > 1
  monthly <- length(unique(format(x, "%B"))) > 1
  yearly <- length(unique(format(x, "%Y"))) > 1
  # plot number of records by group
  if (hourly) {
    plot_hourly <- plot_freq_date_group(x, group = "hour", title = FALSE)
  } else {
    plot_hourly <- NULL
  }
  if (daily) {
    plot_daily <- plot_freq_date_group(x, group = "weekday", title = FALSE)
  } else {
    plot_daily <- NULL
  }
  if (monthly) {
    plot_monthly <- plot_freq_date_group(x, group = "month", title = FALSE)
  } else {
    plot_monthly <- NULL
  }
  if (yearly) {
    plot_yearly <- plot_freq_date_group(x, group = "year", title = FALSE)
  } else {
    plot_yearly <- NULL
  }
  plot_list <- list(plot_hourly, plot_daily, plot_monthly, plot_yearly)
  plot_list <- plot_list[!sapply(plot_list, is.null)]
  title <- cowplot::ggdraw() +
    cowplot::draw_label("Total number of records by group.", vjust = 0)
  plots <- cowplot::plot_grid(plotlist = plot_list, ncol = 2)
  annotation <- cowplot::ggdraw() +
    cowplot::draw_label("Group without data are plotted in red. The dashed line correspond to the expected number of records.",
      size = 10
    )
  freq_plot <- cowplot::plot_grid(title, plots, annotation, rel_heights = c(0.1, 1, 0.1), ncol = 1)
  return(freq_plot)
}


#'
#' This function plots the number of records by date and displays the dates without data
#' between the min and max of x.
#'
#' @param x a date
#'
#' @return a plot of the missing dates. Depending on the scope of the date vector it can
#'   be a line chart or a calendar chart.
#'
#' @import data.table
#' @import ggplot2
#'
plot_missing_date <- function(x) {
  # check argument
  # if (!is_date(x)) {
  #   stop('"x" must be a date.')
  # }
  # indicators of hourly, daily, monthly and yearly data
  daily <- length(unique(format(x, "%d"))) > 1
  monthly <- length(unique(format(x, "%B"))) > 1
  yearly <- length(unique(format(x, "%Y"))) > 1

  # remove missing
  x <- x[!is.na(x)]

  # min
  min_date <- min(x)
  # max
  max_date <- max(x)

  # aggregate the data by day and count the number of records
  plot_data <- data.table(date = lubridate::date(x))[, .(count = .N), by = date]

  # the limits of the sequence of dates (month or year)
  if (yearly & !monthly & !daily) {
    from <- as.Date(format(min(x), "%Y-01-01"))
    to <- (seq(as.Date(format(max(x), "%Y-01-01")),
      length.out = 2,
      by = "1 year"
    ) - 1)[2]
    by <- "year"
  } else {
    from <- as.Date(format(min(x), "%Y-%m-01"))
    to <- (seq(as.Date(format(max(x), "%Y-%m-01")),
      length.out = 2,
      by = "1 month"
    ) - 1)[2]
    if (daily) {
      by <- "day"
    } else {
      by <- "month"
    }
  }

  # sequence of dates from min to max
  date_seq <- seq(
    from = from,
    to = to,
    by = by
  )

  # merge x and date_seq to indicate missing dates
  plot_data <- merge(
    x = plot_data,
    y = data.table(date = date_seq),
    by = "date",
    all = TRUE
  )

  # plot
  if (yearly) {
    plot_data[, count_shift := shift(count, 1)]
    plot_data[, group := ifelse(xor(is.na(count), is.na(count_shift)), 1, 0)]
    plot_data[, group := cumsum(group)]
    plot <- ggplot(data = plot_data, mapping = aes(x = date, y = count, group = group)) +
      geom_line()
    if (nrow(plot_data[is.na(count)]) > 0) {
      plot <- plot +
        geom_point(
          data = plot_data[is.na(count)][date > min_date & date < max_date],
          mapping = aes(x = date, y = 0),
          color = "red"
        )
    }
    plot <- plot +
      labs(
        x = "Date", y = "Number of records",
        title = "Number of records by date. Dates without data are plotted as red points."
      )
  } else if (monthly & !daily) {
    plot_data$month <- format(plot_data$date, "%B")
    plot_data$month <- factor(plot_data$month, levels = month.name, ordered = TRUE)

    # plot
    plot <- ggplot(data = plot_data[!is.na(count)], mapping = aes(x = month, y = count, group = group)) +
      geom_point() +
      geom_segment(mapping = aes(xend = month, yend = 0))
    if (nrow(plot_data[is.na(count)]) > 0) {
      plot <- plot +
        geom_point(
          data = plot_data[is.na(count)][date > min_date & date < max_date],
          mapping = aes(x = month, y = 0),
          color = "red"
        )
    }
    plot <- plot +
      coord_flip() +
      labs(
        x = "Number of recods", y = "Month",
        title = "Number of records by month. Months without data are plotted in red."
      ) +
      theme(
        strip.background = element_rect(fill = NA, color = NA),
        strip.text.x = element_text(hjust = 0, face = "bold")
      )
  } else if (daily) {
    plot_data$dow <- format(plot_data$date, "%a")
    levels_dow <- format(seq(
      from = as.Date("2018-01-01"),
      to = as.Date("2018-01-07"),
      by = "day"
    ), "%a")
    plot_data$dow <- factor(plot_data$dow,
      levels = levels_dow,
      ordered = TRUE
    )
    plot_data$dom <- format(plot_data$date, "%d")
    plot_data$woy <- as.numeric(format(plot_data$date, "%W"))
    plot_data$month <- format(plot_data$date, "%B")
    plot_data$month <- factor(plot_data$month,
      levels = month.name,
      ordered = TRUE
    )

    # plot
    plot <- ggplot(
      data = plot_data,
      mapping = aes(x = dow, y = -woy, fill = count, label = dom)
    ) +
      geom_tile(color = "gray80") +
      geom_text() +
      scale_fill_gradient(name = "Number of 
records", na.value = "transparent") +
      scale_x_discrete(position = "top") +
      facet_wrap(~month, ncol = 4, scales = "free") +
      labs(title = paste0("Number of records by day. Days without records are colored in white.")) +
      theme(
        panel.background = element_rect(fill = NA, color = NA),
        strip.background = element_rect(fill = NA, color = NA),
        strip.text.x = element_text(hjust = 0, face = "bold"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        strip.placement = "outsite"
      )
  }
  return(plot)
}

#'
#' This functions desxribes a date vector. Summary statistics are given and frequency and
#' missing plots are generated.
#'
#' @param x a date vector (can be of class Date and POSIXct)
#' @param plot logical, whether to plot the graph. Defaults to TRUE.
#'
#' @return a table with summary statistics (length, number of na, , number of unique, min
#'   and max) and if plot is true the frequency plot from plot_freq_date and a plot of the
#'   missing values from plot_missing_date.
#'
#' @import ggplot2
#'
desc_date <- function(x, plot = TRUE) {
  # check argument
  # if (!is_date(x)) {
  #   stop('"x" must be a date.')
  # }
  # summary statistics
  # length
  length <- length(x)
  # remove NAs
  x <- x[!is.na(x)]
  # number unique dates
  n_unique <- length(unique(x))
  # number of NAs
  n_na <- length - length(x)
  # min
  min_date <- min(x)
  # max
  max_date <- max(x)

  # summary output
  summary_output <- data.frame(
    Indicator = c(
      "Length", "Number of NAs", "Number of unique values", "Minimum date",
      "Maximum date"
    ),
    Value = c(length, n_unique, n_na, as.character(min_date), as.character(max_date))
  )

  # plot number of records by group
  freq_plot <- plot_freq_date(x)

  # visualize number of records and missing by date
  missing_plot <- plot_missing_date(x)

  if (plot) {
    print(freq_plot)
    print(missing_plot)
  }
  invisible(list(
    summary_stat = summary_output,
    freq_plot = freq_plot,
    missing_plot = missing_plot
  ))
}
