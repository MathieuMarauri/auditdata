#'
#' This function performs a global quality analysis on a table. It outputs the type, the
#' number of missing values and unique values by variable.
#'
#' @param data a data.frame.
#' @param numeric_cutoff the minimum number of distinct values required for a numeric
#'   vector not to be coerced to a fator. -1 is the default, meaning no minimum required.
#' @param na_type charcater vector with valus that should be considered NA. Default to
#'   NULL, no values other than regular NA are treated as NA.
#'
#' @return a list of two elements, a table with number of missing values and other
#'   information by variable and a named vector with the dimension of the table.
#'
#' @export
global_quality <- function(data, numeric_cutoff = -1, na_type = c("", " ", "NA", "NULL")) {
  if (is.data.table(data)) data <- as.data.table(data)
  n_cols <- ncol(data)
  n_rows <- nrow(data)
  n_unique <- nrow(unique(data))
  if (!is.null(na_type)) {
    for (j in seq_along(data)) {
      set(data, i = which(data[[j]] %in% na_type), j = j, value = NA)
    }
  }
  types <- sapply(X = data, FUN = function(x) which_type(x, numeric_cutoff))
  n_miss <- colSums(is.na(data))
  percent_miss <- 100 * n_miss / n_rows
  n_unique_values <- sapply(data, uniqueN)
  output_global <- data.table(
    names(n_miss), types, n_miss,
    as.numeric(format(percent_miss, digits = 0)),
    n_unique_values
  )
  colnames(output_global) <- c(
    "Variable", "Type", "Missing values",
    "Percentage of missing values", "Unique values"
  )
  result <- list(global = output_global, dim = c("nrow" = n_rows, "ncol" = n_cols, "unique" = n_unique))
  return(result)
}

#'
#' This function performs a quality analysis on numeric data. It outputs a table with the
#' decile values.
#'
#' @param data a data.frame
#' @param numeric_var a character vector identifying the numeric columns to analysed in
#'   data.
#'
#' @return a table with as many rows as numeric variables and one column per decile.
#'
numeric_quality <- function(data, numeric_var) {
  if (length(numeric_var) > 1) {
    output_num <- matrixStats::colQuantiles(as.matrix(data[, .SD, .SDcols = numeric_var]),
                                            probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
                                            na.rm = TRUE
    )
    output_num <- cbind.data.frame(numeric_var, output_num)
    colnames(output_num) <- c("Variable", "Min", paste0("Q", 1:9), "Max")
    output_num$Variable <- as.character(output_num$Variable)
  } else if (length(numeric_var) == 1) {
    output_num <- matrixStats::colQuantiles(as.matrix(data[, .SD, .SDcols = numeric_var]),
                                            probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
                                            na.rm = TRUE
    )
    output_num <- cbind.data.frame(numeric_var, t(output_num))
    colnames(output_num) <- c("Variable", "Min", paste0("Q", 1:9), "Max")
    output_num$Variable <- as.character(output_num$Variable)
  }
  return(output_num)
}

#'
#' This function performs a quality analysis on categorical data. It outputs a frequency
#' table by variable.
#'
#' @param data a data.frame
#' @param categorical_var a character vector identifying the categorical columns to
#'   analysed in data.
#' @param max_length the maximum number of rows in the frequency tables
#'
#' @return a list as many elements as categorical variables.
#'
categorical_quality <- function(data, categorical_var, max_length = Inf) {
  output_character <- lapply(
    X = categorical_var,
    FUN = function(name) freq_table(
      x = data[[name]],
      max_length = max_length
    )
  )
  names(output_character) <- categorical_var
  return(output_character)
}

#'
#' This function performs a quality analysis on categorical data. It outputs a frequency
#' table by variable.
#'
#' @param data a data.frame
#' @param date_var a character vector identifying the date columns to analysed in data.
#' @param max_length the maximum number of rows in the frequency tables
#'
#' @return a list with as many elements as date variables.
#'
date_quality <- function(data, date_var, max_length = Inf) {
  output_date_freq <- lapply(
    X = date_var,
    FUN = function(name) freq_table(
      x = data[[name]],
      max_length = max_length
    )
  )
  names(output_date_freq) <- date_var
  output_date_range <- lapply(
    X = date_var,
    FUN = function(name) data.frame(
      c("min", "max"),
      c(
        min(data[[name]], na.rm = TRUE),
        max(data[[name]], na.rm = TRUE)
      )
    )
  )
  names(output_date_range) <- date_var
  return(list(freq = output_date_freq, range = output_date_range))
}
