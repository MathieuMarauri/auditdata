
#'
#' Helper function for 'qualityCheck'
#'
#' This function creates a frequency table from the variable 'name' in the table
#' 'data'. This frequency table is ordered and limited to 'length_out' rows.
#'
#' @param data the table containing the data,
#' @param name the name of the variable from wich the frequency table will be
#'   computed,
#' @param length_out the maximum number of rows in the output.
#'
#' @return a frequency table with the top 'length_out' most frequent
#'   modalities and the associated percentage.
#'
freqTable <- function(data, name, length_out = Inf){
  result <- as.data.table(data[[name]])[
    ,
    .N,
    by = data[[name]]
    ][
      order(-N),
      list(value = data,
           freq = N,
           percent = round(100 * N / sum(N),
                           digits = 0))]
  if(nrow(result) > length_out){
    result <- result[1:length_out, ]
  }
  return(result)
}



qualityCheck <- function (data, file = NULL, numeric_cutoff = -1, max_length = 100, 
                          na_type = c("", " "), na_threshold = c(40, 80), report = TRUE,
                          verbose = TRUE){
  
  if(verbose) cat("Beginning of the quality check\n")
  options(scipen = 999) # print numeric values in fixed notation unless they have more than 999 digits
  # Arguments check
  if(!is.data.frame(data) & !is.data.table(data)) 
    stop("'data' must either be a data.frame or a data.table.")
  if(!is.data.table(data)) data <- as.data.table(data)
  if(is.null(file)){
    file <- paste0(deparse(substitute(data)), "_quality_results.xlsx")
  } else{
    if(!(is.character(file) & length(file) == 1)) 
      stop("'file' must be character of length one.")
    if(!endsWith(file, ".xlsx")) 
      stop("'file' must end with '.xlsx'")
  }
  if(!(is.numeric(numeric_cutoff) & length(numeric_cutoff) == 1)) 
    stop("'numeric_cutoff' must be numeric of length one.")
  if(!(is.numeric(max_length) & length(max_length) == 1)) 
    stop("'max_length' must be numeric of length one.")
  if(!is.null(na_threshold)) if(!(is.numeric(na_threshold) & length(na_threshold) == 2)) 
    stop("'na_threshold' must be numeric of length 2.")
  if(!is.null(na_type)) if(!is.character(na_type)) 
    stop("'na_type' must be a character vector.")
  
  if(!is.null(na_type)){
    for(j in seq_along(data)){
      set(data, i = which(data[[j]] %in% na_type), j = j, value = NA)
    }
  }
  
  is_categorical <- function(x) is.factor(x) || is.character(x) || uniqueN(x) <= numeric_cutoff
  is_numeric <- function(x) is.numeric(x) & uniqueN(x) > numeric_cutoff
  is_date <- function(x) inherits(x, 'Date') | inherits(x, 'POSIXct') | inherits(x, 'POSIXlt')
  
  n_cols <- ncol(data)
  n_rows <- nrow(data)
  n_double <- nrow(unique(data))
  # columns types
  categorical_var <- which(sapply(X = colnames(data), 
                                  FUN = function(name) is_categorical(data[[name]])) == TRUE)
  numeric_var <- which(sapply(X = colnames(data),
                              FUN = function(name) is_numeric(data[[name]])) == TRUE)
  date_var <- which(sapply(X = colnames(data),
                           FUN = function(name) is_date(data[[name]])) == TRUE)
  logical_var <- which(sapply(X = colnames(data),
                              FUN = function(name) is.logical(data[[name]])) == TRUE)
  
  # summary output
  types <- rep(x = "undefined", length = n_cols)
  types[categorical_var] <- "character"
  types[numeric_var] <- "numeric"
  types[date_var] <- "date"
  types[logical_var] <- 'logical'
  n_miss <- colSums(is.na(data))
  percent_miss <- 100 * n_miss / n_rows
  n_unique_values <- sapply(data, uniqueN)
  output_global <- data.frame(names(n_miss), types, n_miss, 
                              as.numeric(format(percent_miss, digits = 0)), 
                              n_unique_values)
  colnames(output_global) <- c("Variable", "Type", "Missing values", 
                               "Percentage of missing values", "Unique values")
  
  result <- list(global = output_global)
  
  if(verbose) cat("Global summary created\n")
  
  # numeric output
  if(length(numeric_var) > 1){
    output_num <- matrixStats::colQuantiles(as.matrix(data[, .SD, .SDcols = numeric_var]), 
                                            probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), 
                                            na.rm = TRUE)
    output_num <- cbind.data.frame(names(numeric_var), output_num)
    colnames(output_num) <- c("Variable", "Min", paste0("Q", 1:9), "Max")
    result <- append(result, list(numeric = output_num))
  } else if(length(numeric_var) == 1){
    output_num <- matrixStats::colQuantiles(as.matrix(data[, .SD, .SDcols = numeric_var]), 
                                            probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), 
                                            na.rm = TRUE)
    output_num <- cbind.data.frame(names(numeric_var), t(output_num))
    colnames(output_num) <- c("Variable", "Min", paste0("Q", 1:9), "Max")
    result <- append(result, list(numeric = output_num))
  }
  
  if(verbose) cat("Numerical summary created\n")
  
  # categorical output
  if(length(categorical_var) > 0){
    output_character <- lapply(X = categorical_var, 
                               FUN = function(name) freqTable(data = data, 
                                                              name = name, 
                                                              max_length = max_length))
    result <- append(result, list(character = output_character))
  }
  
  if(verbose) cat("Categorical summary created\n")
  
  # date output
  if(length(date_var) > 0){
    output_date_freq <- lapply(X = date_var, 
                               FUN = function(name) freqTable(data = data, 
                                                              name = name, 
                                                              max_length = max_length))
    output_date_range <- lapply(X = date_var, 
                                FUN = function(name) data.frame(c("min", "max"), 
                                                                c(min(data[[name]], na.rm = TRUE), 
                                                                  max(data[[name]], na.rm = TRUE))))
    result <- append(result, list(date = output_date_freq))
  }
  
  if(verbose) cat("Date summary created\n")
  
  # Excel output
  if(report){
    # initialisation of the excel report
    workbook <- createWorkbook()
    title_style <- createStyle(fontSize = 16,
                               textDecoration = "bold")
    subtitle_style <- createStyle(fontSize = 14)
    table_title_style <- createStyle(fontSize = 11,
                                     textDecoration = "bold",
                                     border = "TopBottomLeftRight ",
                                     borderColour = "black",
                                     borderStyle = "thin",
                                     halign = "center")
    summary_sheetname <- "Summary"
    addWorksheet(wb = workbook,
                 sheetName = summary_sheetname,
                 gridLines = FALSE)
    # title
    addCustomCell(wb = workbook,
                  sheet = summary_sheetname,
                  row_index = 1,
                  col_index = 1,
                  value = "Global quality check of the table",
                  cell_style = title_style)
    # subtitle
    addCustomCell(wb = workbook,
                  sheet = summary_sheetname,
                  row_index = 2,
                  col_index = 1,
                  value = paste0("The table has ", n_cols, " columns and ", n_rows,
                                 " rows", " (", n_double, " of them are unique)"),
                  cell_style = subtitle_style)
    # summary output
    addCustomTable(wb = workbook,
                   table = output_global,
                   sheet = summary_sheetname,
                   start_row = 4,
                   start_column = 2)
    # Change column width
    for(i in 4:6){
      setColWidths(wb = workbook,
                   sheet = summary_sheetname,
                   cols = i,
                   widths = nchar(colnames(output_global)[i-1]) + 3)
    }
    setColWidths(wb = workbook,
                 sheet = summary_sheetname,
                 cols = 2,
                 widths = max(nchar(colnames(data))) + 3)
    setColWidths(wb = workbook,
                 sheet = summary_sheetname,
                 cols = 3,
                 widths = nchar("character") + 3)
    # fill cell depending on percentage value
    if(!is.null(na_threshold)){
      for(i in 1:nrow(output_global)){
        if(output_global[i, 4] > na_threshold[2]){
          # CB.setFont(cellBlock = cb, font = Font(wb = workbook, color = "red", isBold = TRUE), rowIndex = i + 1, colIndex = 4)
          addStyle(wb = workbook,
                   sheet = summary_sheetname,
                   style = createStyle(fontColour = 'red'),
                   rows = i + 4,
                   cols = 5,
                   stack = TRUE)
        } else if(output_global[i, 4] > na_threshold[1]){
          # CB.setFont(cellBlock = cb, font = Font(wb = workbook, color = "orange", isBold = TRUE), rowIndex = i + 1, colIndex = 4)
          addStyle(wb = workbook,
                   sheet = summary_sheetname,
                   style = createStyle(fontColour = 'orange'),
                   rows = i + 4,
                   cols = 5,
                   stack = TRUE)
        } else{
          # CB.setFont(cellBlock = cb, font = Font(wb = workbook, color = "forestgreen", isBold = TRUE), rowIndex = i + 1, colIndex = 4)
          addStyle(wb = workbook,
                   sheet = summary_sheetname,
                   style = createStyle(fontColour = 'forestgreen'),
                   rows = i + 4,
                   cols = 5,
                   stack = TRUE)
        }
      }
    }
    # numeric sheet
    if(length(numeric_var) > 0){
      numeric_sheetname <- "Numeric"
      addWorksheet(wb = workbook,
                   sheetName = numeric_sheetname,
                   gridLines = FALSE)
      # title
      addCustomCell(wb = workbook,
                    sheet = numeric_sheetname,
                    row_index = 1,
                    col_index = 1,
                    value = "Quantiles of the numerical variables",
                    cell_style = title_style)
      # numeric_output
      addCustomTable(wb = workbook,
                     sheet = numeric_sheetname,
                     table = output_num,
                     start_row = 3,
                     start_column = 2)
      # columns width
      setColWidths(wb = workbook,
                   sheet = numeric_sheetname,
                   cols = 2,
                   widths = max(nchar(colnames(data))) + 3)
    }
    # character sheet
    if(length(categorical_var) > 0){
      character_sheetname <- "Character"
      addWorksheet(wb = workbook,
                   sheetName = character_sheetname,
                   gridLines = FALSE)
      # title
      addCustomCell(wb = workbook,
                    sheet = character_sheetname,
                    row_index = 1,
                    col_index = 1,
                    value = "Frequences of modalities for the categorical variables",
                    cell_style = title_style)
      # subtitle
      if(max(unlist(lapply(length(categorical_var), function(index) nrow(output_character[[index]])))) == max_length){
        addCustomCell(wb = workbook,
                      sheet = character_sheetname,
                      row_index = 2,
                      col_index = 1,
                      value = paste0("The maximum number of modalities is limited to ", max_length, ". Change the parameter 'max_length' to modify this behaviour."),
                      cell_style = subtitle_style)
      }
      # character output
      liste_names <- unlist(lapply(names(categorical_var), function(name) return(c(name, rep(NA, 3)))))
      writeData(wb = workbook,
                sheet = character_sheetname,
                x = t(liste_names),
                startCol = 2,
                startRow = 4,
                colNames = FALSE,
                rowNames = FALSE,
                keepNA = FALSE)
      addStyle(wb = workbook,
               sheet = character_sheetname,
               style = table_title_style,
               rows = 4,
               cols = seq(from = 2, to = 4 * length(categorical_var), by = 4),
               gridExpand = FALSE,
               stack = TRUE)
      # Change column width
      for(i in seq(from = 1, to = length(liste_names), by = 4)){
        setColWidths(wb = workbook,
                     sheet = character_sheetname,
                     cols = i + 1,
                     widths = nchar(liste_names[i]) + 3)
      }
      
      for(index in seq_len(length(categorical_var))){
        addCustomTable(wb = workbook,
                       sheet = character_sheetname,
                       table = output_character[[index]],
                       start_row = 5,
                       start_column = (index - 1) * 4 + 2)
      }
    }
    # date sheet
    if(length(date_var) > 0){
      date_sheetname <- "Date"
      addWorksheet(wb = workbook,
                   sheetName = date_sheetname,
                   gridLines = FALSE)
      # title
      addCustomCell(wb = workbook,
                    sheet = date_sheetname,
                    row_index = 1,
                    col_index = 1,
                    value = "Frequences of modalities for the date variables",
                    cell_style = title_style)
      # subtitle
      if(max(unlist(lapply(length(date_var), function(index) nrow(output_date_freq[[index]])))) == max_length){
        addCustomCell(wb = workbook,
                      sheet = date_sheetname,
                      row_index = 2,
                      col_index = 1,
                      value = paste0("The maximum number of modalities is limited to ", max_length, ". Change the parameter 'max_length' to modify this behaviour."),
                      cell_style = subtitle_style)
      }
      # date output
      liste_names <- unlist(lapply(names(date_var), function(name) return(c(name, rep(NA, 6)))))
      writeData(wb = workbook,
                sheet = date_sheetname,
                x = t(liste_names),
                startCol = 2,
                startRow = 4,
                colNames = FALSE,
                rowNames = FALSE,
                keepNA = FALSE)
      addStyle(wb = workbook,
               sheet = date_sheetname,
               style = table_title_style,
               rows = 4,
               cols = seq(from = 2, to = 7 * length(date_var), by = 7),
               gridExpand = FALSE,
               stack = TRUE)
      # Change column width
      for(i in seq(from = 1, to = length(liste_names), by = 7)){
        setColWidths(wb = workbook,
                     sheet = date_sheetname,
                     cols = i + 1,
                     widths = nchar(liste_names[i]) + 3)
      }
      
      for(index in seq_len(length(date_var))){
        addCustomTable(wb = workbook,
                       sheet = date_sheetname,
                       table = output_date_freq[[index]],
                       start_row = 5,
                       start_column = (index - 1) * 7 + 2,
                       date = TRUE)
      }
      # add min/max
      for(index in seq_len(length(date_var))){
        writeData(wb = workbook,
                  sheet = date_sheetname,
                  x = output_date_range[[index]],
                  startCol = (index - 1) * 7 + 6,
                  startRow = 5,
                  colNames = FALSE,
                  rowNames = FALSE,
                  borders = "all")
        style <- createStyle(fontSize = 11,
                             textDecoration = "bold",
                             halign = "center")
        addStyle(wb = workbook,
                 sheet = date_sheetname,
                 style = style,
                 rows = c(5, 6),
                 cols = (index - 1) * 7 + 6,
                 gridExpand = TRUE,
                 stack = TRUE)
      }
      for(i in seq(from = 1, to = length(liste_names), by = 7)){
        setColWidths(wb = workbook,
                     sheet = date_sheetname,
                     cols = i + 6,
                     widths = nchar("00/00/0000") + 1)
      }
    }
    saveWorkbook(wb = workbook, file = file, overwrite = TRUE)
    
    if(verbose) cat(paste0('Excel report saved in "', file.path(getwd(), file), '"'))
    
  }
  
  # R output
  rownames(output_global) <- NULL
  invisible(result)
  
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
#' This function performs a global quality analysis on a table. It outputs the type, the
#' number of missing values and unique values by variable.
#'
#' @param data a data.frame.
#' 
#' @param numeric
#'   
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
  output_global <- data.frame(names(n_miss), types, n_miss, 
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
#' @param date_var a character, numeric or logical vector identifying the numeric columns
#'   to analysed in data.
#'
#' @return a table with the same number of row as the length of numeric_var and one column
#'   per decile.
#'   
numericQuality <- function(data, numeric_var) {
  if(length(numeric_var) > 1){
    output_num <- matrixStats::colQuantiles(as.matrix(data[, .SD, .SDcols = numeric_var]), 
                                            probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), 
                                            na.rm = TRUE)
    output_num <- cbind.data.frame(names(numeric_var), output_num)
    colnames(output_num) <- c("Variable", "Min", paste0("Q", 1:9), "Max")
  } else if(length(numeric_var) == 1){
    output_num <- matrixStats::colQuantiles(as.matrix(data[, .SD, .SDcols = numeric_var]), 
                                            probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), 
                                            na.rm = TRUE)
    output_num <- cbind.data.frame(names(numeric_var), t(output_num))
    colnames(output_num) <- c("Variable", "Min", paste0("Q", 1:9), "Max")
  }
  return(result)
}

#'
#' This function performs a quality analysis on categorical data. It outputs a frequency
#' table by variable.
#'
#' @param data a data.frame
#' @param date_var a character, numeric or logical vector identifying the categorical
#'   columns to analysed in data.
#' @param max_length the maximum number of rows in the frequency tables
#'
#' @return a list the length of categorical_var.
#'   
categoricalQuality <- function(data, categorical_var, max_length = Inf) {
    output_character <- lapply(X = categorical_var, 
                               FUN = function(name) freqTable(data = data, 
                                                              name = name, 
                                                              max_length = max_length))
  return(output_character)
}

#'
#' This function performs a quality analysis on categorical data. It outputs a frequency
#' table by variable.
#'
#' @param data a data.frame
#' @param date_var a character, numeric or logical vector identifying the date columns to
#'   analysed in data.
#' @param max_length the maximum number of rows in the frequency tables
#'
#' @return a list the length of categorical_var.
#'   
dateQuality <- function(data, date_var, max_length = Inf) {
    output_date_freq <- lapply(X = date_var, 
                               FUN = function(name) freqTable(data = data, 
                                                              name = name, 
                                                              max_length = max_length))
    output_date_range <- lapply(X = date_var, 
                                FUN = function(name) data.frame(c("min", "max"), 
                                                                c(min(data[[name]], na.rm = TRUE), 
                                                                  max(data[[name]], na.rm = TRUE))))
    return(list(freq = output_date_freq, range = output_date_range))
}

dataQuality <- function(data, numeric_cutoff = -1, na_type = c('', ' ', 'NA', 'NULL'), 
                        max_length = Inf) {
  if(!is.data.frame(data)) 
    stop("'data' must be have data.frame class.")
  if(!is.data.table(data)) data <- as.data.table(data)
  if(!(is.numeric(numeric_cutoff) & length(numeric_cutoff) == 1)) 
    stop("'numeric_cutoff' must be numeric of length one.")
  if(!is.null(na_type)) if(!is.character(na_type)) 
    stop("'na_type' must be a character vector.")
  types <- sapply(X = data, FUN = function(x) whatType(x, numeric_cutoff))
  numeric_var <- names(types)[types == 'numeric']
  categorical_var <- names(types)[types %in% c('factor', 'character', 'logical')]
  date_var <- names(types)[types == 'date']
  result <- list(global = globalQuality(data = data, 
                                        numeric_cutoff = numeric_cutoff, 
                                        na_type = na_type))
  if(numeric_var > 0) { 
    numeric_output <- numericQuality(data = data, numeric_var = numeric_var)
    result <- append(result, list(date = output_date_freq))
  }
  if(categorical_var > 0) {
    categorical_output <- categoricalQuality(data = data, 
                                             categorical_var = categorical_var, 
                                             max_length = max_length)
  } 
  if(date_var > 0) {
    categorical_output <- dateQuality(data = data, 
                                      date_var = date_var, 
                                      max_length = max_length)
  }
  
}

setDT(diamonds)
is.data.table(diamonds)
