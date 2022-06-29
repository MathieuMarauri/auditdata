
#'
#' This function plots the missing values in a dataset. If a value is missing in a column
#' for a given row then a bar is displayed.
#'
#' @param data a data.frame
#' @param order logical, whether to order the columns and rows to display the missing
#'   values next to each other, default to FALSE.
#' @param na_type a character vector of strings that will be interpreted as NA
#'
#' @details If the order argument is set to TRUE then the order of the rows is modified
#'   and you cannot use the plot's x-axis to find a missing value in the dataset.
#'
#' @export
#' @import data.table
#' @import ggplot2
#'
audit_missing <- function(data, na_type = NULL, order = FALSE) {
  # check argument
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.")
  }
  if (!is.data.table(data)) {
    setDT(data)
  }
  if (!is.null(na_type)) {
    if (!is.character(na_type)) {
      stop("'na_type' must be a character vector.")
    }
  }

  # set vaues in na_type to NA
  if (!is.null(na_type)) {
    for (j in seq_along(data)) {
      set(data, i = which(data[[j]] %in% na_type), j = j, value = NA)
    }
  }

  # create matrix with missing data indicator
  missing_data <- as.matrix(is.na(data))

  # add number of missing values to variable name
  order_col <- apply(X = missing_data, MARGIN = 2, FUN = sum)
  colnames(missing_data) <- paste0(colnames(missing_data), " (", order_col, ")")

  if (order) {
    # reorder the columns to group the missing values
    missing_data <- missing_data[, order(order_col)]

    # reorder the rows to group the missing values
    order_row <- apply(X = missing_data, MARGIN = 1, FUN = sum)
    missing_data <- missing_data[order(-order_row), ]
  }

  # coerce to data table and add row indices for the x value of the plot
  missing_data <- as.data.table(missing_data)
  missing_data$row <- 1:nrow(missing_data)

  # melt to long format to be accepted by ggplot
  missing_data <- melt(
    data = missing_data,
    id.vars = "row",
    variable.name = "variable",
    value.name = "missing"
  )

  # plot
  row <- variable <- missing <- NULL # for CMD check
  ggplot(data = missing_data, mapping = aes(x = row, y = variable, fill = missing)) +
    geom_tile(show.legend = FALSE) +
    scale_fill_manual(values = c("TRUE" = "maroon", "FALSE" = "transparent")) +
    labs(
      x = "Row number", y = "Variable",
      title = paste0("Missing values in ", deparse(substitute(data)))
    ) +
    theme_bw()
}
