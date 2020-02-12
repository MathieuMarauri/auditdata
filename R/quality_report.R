#'
#' This function renders the data quality rmarkdown document with specific parameters.
#'
#' @param data the dataset to analyse
#' @param output_dir the directory to write the output file to, default to the current
#'   directory.
#' @param output_file name of the output file. If NULL, the default then it is
#'   'quality_report'.
#' @param na_type a character vector of strings that will be interpreted as NA
#' @param na_threshold numeric vector defining the range of values for the percentage of
#'   missing values to be colored green, orange and red. Default to green before 40
#'   percent, orange between 40 and 80 and red over 80 percent. If NULL then no colors are
#'   applied
#' @param order logical, whether to order the columns and rows to display the missing
#'   values next to each other, defautl to FALSE.
#'
#' @import data.table
#' @import knitr
#' @importFrom dplyr mutate "%>%"
#' @importFrom formattable color_bar
#' @importFrom kableExtra kable_styling cell_spec
#' @importFrom rmarkdown render
#' @export
#'
quality_report <- function(data,
                           output_dir = ".",
                           output_file = NULL,
                           na_type = NULL,
                           numeric_cutoff = -1,
                           na_threshold = c(40, 80),
                           order = FALSE) {
  # arguments check
  if (!is.data.frame(data) & !is.data.table(data)) {
    stop("'data' must either be a data.frame or a data.table.")
  }
  if (!is.data.table(data)) {
    data <- as.data.table(data)
    warning("The 'data' argument has been coerced to data.table.")
  }
  if (!is.null(na_threshold)) {
    if (!(is.numeric(na_threshold) & length(na_threshold) == 2)) {
      stop("'na_threshold' must be numeric of length 2.")
    } else if (na_threshold[1] >= na_threshold[2]) {
      stop("The first element of 'na_threshold' should be lower than the second one.")
    }
  }

  rmarkdown::render(
    input = system.file("rmarkdown/templates/quality_report.Rmd", package = "auditdata"),
    output_file = output_file,
    output_dir = output_dir,
    envir = new.env(),
    params = list(data = data, na_type = na_type, numeric_cutoff = numeric_cutoff, na_threshold = na_threshold, order = order)
  )
}
