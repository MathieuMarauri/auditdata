
# global_quality <- function(data, numeric_cutoff = -1, na_type = NA) {
#   if (is.data.table(data)) data <- as.data.table(data)
#   n_cols <- ncol(data)
#   n_rows <- nrow(data)
#   n_unique <- nrow(unique(data))
#   if (!is.null(na_type)) {
#     for (j in seq_along(data)) {
#       set(data, i = which(data[[j]] %in% na_type), j = j, value = NA)
#     }
#   }
#   types <- sapply(X = data, FUN = function(x) which_type(x, numeric_cutoff))
#   n_miss <- colSums(is.na(data))
#   percent_miss <- 100 * n_miss / n_rows
#   n_unique_values <- sapply(data, uniqueN)
#   output_global <- data.table(
#     names(n_miss), types, n_miss,
#     as.numeric(format(percent_miss, digits = 0)),
#     n_unique_values
#   )
#   colnames(output_global) <- c(
#     "Variable", "Type", "Missing values",
#     "Percentage of missing values", "Unique values"
#   )
#   result <- list(global = output_global, dim = c("nrow" = n_rows, "ncol" = n_cols, "unique" = n_unique))
#   return(result)
# }


#'
#' This function analyzes the data quality of a table. It gives the dimensions of the table along
#' with the number of unique values and missing values/ It also outputs a table with the number of
#' unique values, the type and the percentage of missing values by variable.
#'
#' @param data the table to analyzed
#' @param numeric_cutoff an integer specifying the minimal number of unique values necessary for a
#'   vector not to be considered a factor. Default to -1
#' @param na_type a character vector of strings that will be interpreted as NA
#' @param return a list with two elements, one with global information (dimensions, unique values
#'   and missing values) and a table with information for each variable.
#'
#' @return a list of length 2, first element is a list containing the number of rows, columns,
#'   unique values and missing and the second one is a table containing information on each variable
#'   (type, unique values, missing).
#'
#' @import data.table
#'
#' @export
#' 
audit_global <- function(data, numeric_cutoff = -1, na_type = NULL) {
  # Arguments check
  if (!is.data.frame(data) & !is.data.table(data)) {
    stop("'data' must either be a data.frame or a data.table.")
  }
  if (!is.data.table(data)) data <- as.data.table(data)
  if (!(is.numeric(numeric_cutoff) & length(numeric_cutoff) == 1)) {
    stop("'numeric_cutoff' must be numeric of length one.")
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
  
  # global info
  n_cols <- ncol(data)
  n_rows <- nrow(data)
  n_unique <- nrow(unique(data))
  n_missing <- sum(is.na(data))
  
  # columns types
  categorical_var <- which(sapply(
    X = colnames(data),
    FUN = function(name) is_categorical(data[[name]], numeric_cutoff = numeric_cutoff)) == TRUE)
  numeric_var <- which(sapply(
    X = colnames(data),
    FUN = function(name) is_numeric(data[[name]], numeric_cutoff = numeric_cutoff)) == TRUE)
  date_var <- which(sapply(
    X = colnames(data),
    FUN = function(name) is_date(data[[name]])) == TRUE)
  
  # data quality output
  types <- rep(x = "undefined", length = n_cols)
  types[categorical_var] <- "character"
  types[numeric_var] <- "numeric"
  types[date_var] <- "date"
  n_miss <- colSums(is.na(data))
  percent_miss <- 100 * n_miss / n_rows
  n_unique_values <- sapply(data, uniqueN)
  output_global <- data.table(
    names(n_miss),
    types,
    n_miss,
    as.numeric(format(percent_miss, digits = 0)),
    n_unique_values
  )
  colnames(output_global) <- c(
    "Variable", "Type", "Missing values",
    "Percentage of missing values", "Unique values"
  )
  return(list(
    global = list(
      n_cols = n_cols, n_rows = n_rows, n_unique = n_unique,
      n_missing = n_missing
    ),
    table = output_global
  ))
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
audit_numeric <- function(data, numeric_var) {
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
audit_categorical <- function(data, categorical_var, max_length = Inf) {
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
audit_date <- function(data, date_var, max_length = Inf) {
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
