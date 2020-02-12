#'
#' This function describes a vector with summary statistics and plots.
#'
#' @param x a vector
#' @param na_type a character vector of strings that will be interpreted as NA
#' @param numeric_cutoff the minimum number of distinct values required for a numeric
#'   vector not to be coerced to a fator. -1 is the default, meaning no minimum required.
#'@param max_length the maximum number of different values to display in the freqeuncy
#'   table and the frequency plot, default to 15.
#' @param nchar maximum number of characters displayed in the plots as level values for
#'   categorical vectors. See details.
#' @param plot logical, whether to plot the graph. Defaults to TRUE.
#'
#' @details Depending on the class of the vector the functions \code{descNumeric()},
#'   \code{descCategorical()} or \code{descDate()} are called. Each one returns one table
#'   with general summary statistics such as the length of x, the number of unique values,
#'   the number of missing values. If x is categoric then a frequency table is also
#'   returned. Different plots are also generated to see. See the description of the
#'   differents functions for more information.
#'
#' @export
#'
audit_vector <- function(x, na_type = NULL, numeric_cutoff = -1, max_length = 15, nchar = 20, plot = TRUE) {
  # Set vaues in na_type to NA
  if (!is.null(na_type)) {
    x[x %in% na_type] <- NA
  }
  # Use the proper description function
  if (is_date(x)) {
    result <- audit_vector_date(x, plot = plot)
  } else if (is_numeric(x, numeric_cutoff)) {
    result <- audit_vector_numeric(x, plot = plot)
  } else if (is_categorical(x, numeric_cutoff))  {
    result <- audit_vector_categorical(x, max_length = max_length, nchar = nchar, plot = plot)
  }
  return(result)
}
