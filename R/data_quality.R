#'
#' Performs a quality audit of a table
#'
#' This function performs a quality check on a table. The number of missing values by
#' variable along with the quantiles for the numeric variables and a frequency table for
#' each categorical variable can be found in the result.
#'
#' The types are defined based on the types in the input table and on the value of other
#' arguments. 'numeric_cutoff' allows numeric variables to be classified as categorical if
#' they have less unique values than the value of 'numeric_cutoff'. Date, POSIXct and
#' POSIXlt are the only classes treated as date.
#'
#' @param data a data.frame.
#' @param numeric_cutoff the minimum number of distinct values required for a numeric
#'   vector not to be coerced to a fator. -1 is the default, meaning no minimum required.
#' @param na_type charcater vector with valus that should be considered NA. Default to
#'   NULL, no values other than regular NA are treated as NA.
#' @param max_length the maximum number of rows in the frequency tables
#' @param global_only logical, whether to return only the global summary
#'
#' @return a list with a global summary, and if available, information on numeric,
#'   categorical and date variables
#'
#' @import data.table
#' @export
data_quality <- function(data, numeric_cutoff = -1, na_type = c("", " ", "NA", "NULL"),
                         max_length = Inf, global_only = FALSE) {
  if (!is.data.frame(data)) {
    stop("'data' must be have data.frame class.")
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
  types <- sapply(X = data, FUN = function(x) which_type(x, numeric_cutoff))
  numeric_var <- names(types)[types == "numeric"]
  categorical_var <- names(types)[types == "categorical"]
  date_var <- names(types)[types == "date"]
  result <- list(global = global_quality(
    data = data,
    numeric_cutoff = numeric_cutoff,
    na_type = na_type
  ))
  if (!global_only) {
    if (length(numeric_var) > 0) {
      numeric_output <- numeric_quality(data = data, numeric_var = numeric_var)
      result <- append(result, list(numeric = numeric_output))
    }
    if (length(categorical_var) > 0) {
      categorical_output <- categorical_quality(
        data = data,
        categorical_var = categorical_var,
        max_length = max_length
      )
      result <- append(result, list(categorical = categorical_output))
    }
    if (length(date_var) > 0) {
      date_output <- date_quality(
        data = data,
        date_var = date_var,
        max_length = max_length
      )
      result <- append(result, list(date = date_output))
    }
  }
  class(result) <- append(class(result), "qualityResult")
  return(result)
}