#'
#' This function tests if a vector is of type numeric (numeric with less unique values
#' than cutoff).
#'
#' @param x a vector
#' @param numeric_cutoff the minimum number of distinct values required for a numeric
#'   vector not to be coerced to a fator. -1 is the default, meaning no minimum required.
#'
#' @return a logical
#'
is_numeric <- function(x, numeric_cutoff = -1) is.numeric(x) & uniqueN(x) > numeric_cutoff


#'
#' This function tests if a vector is of type categorial (factor, character or boolean).
#'
#' @param x a vector
#' @param numeric_cutoff the minimum number of distinct values required for a numeric
#'   vector not to be coerced to a fator. -1 is the default, meaning no minimum required.
#'
#' @return a logical
#'
is_categorical <- function(x, numeric_cutoff = -1) is.factor(x) | uniqueN(x) <= numeric_cutoff | is.logical(x) | is.character(x)


#'
#' This function tests if a vector is of type date (Date, POSIXct and POSIXlt).
#'
#' @param x a vector
#'
#' @return a logical
#'
is_date <- function(x) inherits(x, "Date") | inherits(x, "POSIXct") | inherits(x, "POSIXlt")

#'
#' This function gives the types of a vector. It relies on is_numeric and is_date.
#'
#' @param x a vector
#' @param numeric_cutoff the minimum number of distinct values required for a numeric
#'   vector not to be coerced to a fator. -1 is the default, meaning no minimum required.
#'
#' @return a character string giving the type of the input vector. Can be one of
#'   undefined, factor, numeric, logical and character
#'
which_type <- function(x, numeric_cutoff = -1) {
  result <- "undefined"
  if (is_categorical(x)) result <- "categorical"
  if (is_date(x)) result <- "date"
  if (is_numeric(x, numeric_cutoff)) result <- "numeric"
  return(result)
}