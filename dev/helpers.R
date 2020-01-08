
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
freqTable <- function(x, max_length = Inf){
  result <- as.data.table(x)[
    ,
    .N,
    by = x
    ][
      order(-N),
      list(value = x,
           freq = N,
           percent = round(100 * N / sum(N),
                           digits = 0))]
  if(nrow(result) > max_length){
    result <- result[1:max_length, ]
  }
  return(result)
}

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
#' This function tests if a vector is of type date (Date, POSIXct and POSIXlt).
#' 
#' @param x a vector
#' 
#' @return a logical
#' 
is_date <- function(x) inherits(x, 'Date') | inherits(x, 'POSIXct') | inherits(x, 'POSIXlt')

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
whatType <- function(x, numeric_cutoff = -1) {
  result <- 'undefined'
  if(is.factor(x) | uniqueN(x) <= numeric_cutoff) result <- 'factor'
  if(is.logical(x)) result <- 'logical'
  if(is.character(x)) result <- 'character'
  if(is_date(x)) result <- 'date'
  if(is_numeric(x, numeric_cutoff)) result <- 'numeric'
  return(result)
}

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
addCustomCell <- function(wb, sheet, row_index, col_index, value, cell_style){
  writeData(wb = wb,
            sheet = sheet,
            x = value,
            startCol = col_index,
            startRow = row_index)
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
addCustomTable <- function(wb, sheet, table, start_row, start_column, date = FALSE){
  first_col_style <- createStyle(fontSize = 11,
                                 textDecoration = "bold",
                                 border = "LeftRight",
                                 borderColour = "black",
                                 borderStyle = "thin")
  other_col_style <- createStyle(fontSize = 11,
                                 halign = "center")
  colnames_style <- createStyle(fontSize = 11,
                                textDecoration = "bold",
                                halign = "center",
                                border = "TopBottom",
                                borderColour = "black",
                                borderStyle = "thin")
  date_col_style <- createStyle(fontSize = 11,
                                textDecoration = "bold",
                                border = "LeftRight",
                                borderColour = "black",
                                borderStyle = "thin",
                                numFmt = "DATE",
                                halign = "left")
  n_col <- ncol(table)
  n_row <- nrow(table)
  writeData(wb = wb,
            sheet = sheet,
            x = table,
            startCol = start_column,
            startRow = start_row,
            colNames = TRUE,
            rowNames = FALSE,
            borders = "surrounding",
            borderColour = "black",
            borderStyle = "thin",
            keepNA = TRUE)
  addStyle(wb = wb,
           sheet = sheet,
           style = colnames_style,
           rows = start_row,
           cols = start_column:(start_column + n_col - 1),
           gridExpand = TRUE,
           stack = TRUE)
  addStyle(wb = wb,
           sheet = sheet,
           style = first_col_style,
           rows = start_row:(start_row + n_row),
           cols = start_column,
           gridExpand = TRUE,
           stack = TRUE)
  addStyle(wb = wb,
           sheet = sheet,
           style = other_col_style,
           rows = (start_row + 1):(start_row + n_row),
           cols = (start_column + 1):(start_column + n_col - 1),
           gridExpand = TRUE,
           stack = TRUE)
  addStyle(wb = wb,
           sheet = sheet,
           style = createStyle(border = "right"),
           rows = start_row,
           cols = start_column + n_col - 1,
           stack = TRUE)
  if(date){
    addStyle(wb = wb,
             sheet = sheet,
             style = date_col_style,
             rows = start_row:(start_row + n_row),
             cols = start_column,
             gridExpand = TRUE,
             stack = TRUE)
  }
}



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
globalQuality <- function(data, numeric_cutoff = -1, na_type = c('', ' ', 'NA', 'NULL')) {
  if(is.data.table(data)) data <- as.data.table(data)
  n_cols <- ncol(data)
  n_rows <- nrow(data)
  n_unique <- nrow(unique(data))
  if(!is.null(na_type)){
    for(j in seq_along(data)){
      set(data, i = which(data[[j]] %in% na_type), j = j, value = NA)
    }
  }
  types <- sapply(X = data, FUN = function(x) whatType(x, numeric_cutoff))
  n_miss <- colSums(is.na(data))
  percent_miss <- 100 * n_miss / n_rows
  n_unique_values <- sapply(data, uniqueN)
  output_global <- data.table(names(n_miss), types, n_miss, 
                              as.numeric(format(percent_miss, digits = 0)), 
                              n_unique_values)
  colnames(output_global) <- c("Variable", "Type", "Missing values", 
                               "Percentage of missing values", "Unique values")
  result <- list(global = output_global, dim = c('nrow' = n_rows, 'ncol' = n_cols, 'unique' = n_unique))
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
numericQuality <- function(data, numeric_var) {
  if(length(numeric_var) > 1){
    output_num <- matrixStats::colQuantiles(as.matrix(data[, .SD, .SDcols = numeric_var]), 
                                            probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), 
                                            na.rm = TRUE)
    output_num <- cbind.data.frame(numeric_var, output_num)
    colnames(output_num) <- c("Variable", "Min", paste0("Q", 1:9), "Max")
    output_num$Variable <- as.character(output_num$Variable)
  } else if(length(numeric_var) == 1){
    output_num <- matrixStats::colQuantiles(as.matrix(data[, .SD, .SDcols = numeric_var]), 
                                            probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), 
                                            na.rm = TRUE)
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
categoricalQuality <- function(data, categorical_var, max_length = Inf) {
  output_character <- lapply(X = categorical_var, 
                             FUN = function(name) freqTable(x = data[[name]], 
                                                            max_length = max_length))
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
dateQuality <- function(data, date_var, max_length = Inf) {
  output_date_freq <- lapply(X = date_var, 
                             FUN = function(name) freqTable(x = data[[name]],
                                                            max_length = max_length))
  names(output_date_freq) <- date_var
  output_date_range <- lapply(X = date_var, 
                              FUN = function(name) data.frame(c("min", "max"), 
                                                              c(min(data[[name]], na.rm = TRUE), 
                                                                max(data[[name]], na.rm = TRUE))))
  names(output_date_range) <- date_var
  return(list(freq = output_date_freq, range = output_date_range))
}

