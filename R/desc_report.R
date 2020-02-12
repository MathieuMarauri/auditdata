#'
#' This function renders the univariate exploratory analysis rmardown document with
#' specific parameters.
#'
#' @param data the dataset to analyse
#' @param min_unique the minimal number of unique values for a numeric vector to be
#'   describe as a numeric vector and not as a categorical vector. Defaults to 15.
#' @param length_out number of unique values displayed in the plots and table for
#'   categorical vectors. Defaults to 15.
#' @param nchar maximum number of characters displayed in the plots as level values for
#'   categorical vectors. See details.
#' @param output_dir the directory to write the output file to, default to the current
#'   directory.
#' @param output_file name of the output file. If NULL, the default then it is 'desc_report'.
#'
#' @import data.table
#' @import knitr
#' @importFrom magrittr "%>%"
#' @importFrom kableExtra kable_styling column_spec
#' @importFrom rmarkdown render
#' @export
#'
desc_report <- function(data,
                        numeric_cutoff = -1,
                        max_length = 15,
                        nchar = 20,
                        output_dir = ".",
                        output_file = NULL) {
  # arguments check
  if (!is.data.frame(data) & !is.data.table(data)) {
    stop("'data' must either be a data.frame or a data.table.")
  }
  if (!is.data.table(data)) {
    data <- as.data.table(data)
    warning("The 'data' argument has been coerced to data.table.")
  }

  names(data) <- make.names(names(data), unique = TRUE)

  rmarkdown::render(
    input = system.file("rmarkdown/templates/desc_report.Rmd",
      package = "explorer"
    ),
    output_dir = output_dir,
    output_file = output_file,
    envir = new.env(),
    params = list(data = data, numeric_cutoff = numeric_cutoff, max_length = max_length, nchar = nchar)
  )
}
