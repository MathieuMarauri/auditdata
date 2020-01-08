#'
#' Helper function to add custom cell
#'
#' @param wb a workbook
#' @param sheet a sheet in the workbook
#' @param row_index row index of the cell
#' @param col_index col index of the cell
#' @param value value to be added to the cell
#' @param cell_style style object to customize the cell
#' 
#' @import openxlsx
#'
add_custom_cell <- function(wb, sheet, row_index, col_index, value, cell_style) {
  writeData(
    wb = wb,
    sheet = sheet,
    x = value,
    startCol = col_index,
    startRow = row_index
  )
  addStyle(wb = wb, sheet = sheet, style = cell_style, rows = row_index, cols = col_index)
}

#'
#' Helper function to add a data frame to a sheet and styles it.
#'
#' It encapsulates writeData, addStyle and createStyle from the openxlsx package.
#'
#' @param wb a workbook
#' @param sheet a sheet in the workbook,
#' @param table the table to be added,
#' @param start_row a numeric value for the starting row,
#' @param start_column a numeric value for the starting column,
#' @param date logical. Is the first column of type date?
#' 
#' @import openxlsx
#'
add_custom_table <- function(wb, sheet, table, start_row, start_column, date = FALSE) {
  first_col_style <- createStyle(
    fontSize = 11,
    textDecoration = "bold",
    border = "LeftRight",
    borderColour = "black",
    borderStyle = "thin"
  )
  other_col_style <- createStyle(
    fontSize = 11,
    halign = "center"
  )
  colnames_style <- createStyle(
    fontSize = 11,
    textDecoration = "bold",
    halign = "center",
    border = "TopBottom",
    borderColour = "black",
    borderStyle = "thin"
  )
  date_col_style <- createStyle(
    fontSize = 11,
    textDecoration = "bold",
    border = "LeftRight",
    borderColour = "black",
    borderStyle = "thin",
    numFmt = "DATE",
    halign = "left"
  )
  n_col <- ncol(table)
  n_row <- nrow(table)
  writeData(
    wb = wb,
    sheet = sheet,
    x = table,
    startCol = start_column,
    startRow = start_row,
    colNames = TRUE,
    rowNames = FALSE,
    borders = "surrounding",
    borderColour = "black",
    borderStyle = "thin",
    keepNA = TRUE
  )
  addStyle(
    wb = wb,
    sheet = sheet,
    style = colnames_style,
    rows = start_row,
    cols = start_column:(start_column + n_col - 1),
    gridExpand = TRUE,
    stack = TRUE
  )
  addStyle(
    wb = wb,
    sheet = sheet,
    style = first_col_style,
    rows = start_row:(start_row + n_row),
    cols = start_column,
    gridExpand = TRUE,
    stack = TRUE
  )
  addStyle(
    wb = wb,
    sheet = sheet,
    style = other_col_style,
    rows = (start_row + 1):(start_row + n_row),
    cols = (start_column + 1):(start_column + n_col - 1),
    gridExpand = TRUE,
    stack = TRUE
  )
  addStyle(
    wb = wb,
    sheet = sheet,
    style = createStyle(border = "right"),
    rows = start_row,
    cols = start_column + n_col - 1,
    stack = TRUE
  )
  if (date) {
    addStyle(
      wb = wb,
      sheet = sheet,
      style = date_col_style,
      rows = start_row:(start_row + n_row),
      cols = start_column,
      gridExpand = TRUE,
      stack = TRUE
    )
  }
}