#'
#' Helper function for 'qualityCheck'
#'
#' This function creates a frequency table limited to 'max_length' rows.
#'
#' @param x the table containing the data,
#' @param max_length the maximum number of rows in the output.
#'
#' @return a frequency table with the top 'max_length' most frequent
#'   modalities and the associated percentage.
#' 
#' @import data.table
#'
freq_table <- function(x, max_length = Inf) {
  result <- as.data.table(x)[
    ,
    .N,
    by = x
    ][
      order(-N),
      list(
        value = x,
        freq = N,
        percent = round(100 * N / sum(N),
                        digits = 0
        )
      )
      ]
  if (nrow(result) > max_length) {
    result <- result[1:max_length, ]
  }
  return(result)
}