
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
#'Performs a quality audit of a table
#'
#'This function performs a quality check on a table and creates an excel report if asked. The number
#'of missing values by variable along with the quantiles for the numeric variables and a frequency
#'table for each categorical variable can be found in the report.
#'
#'The excel report is composed of several sheets. A 'summary' one with information regarding missing
#'values for every variables, a 'numeric' sheet with the quantiles of all numeric varaibles, a
#''categorical' sheet with a table with unique values and their frequency for each categorical
#'variable and finally a 'date' sheet with also a frequency table for each date variable along with
#'the minimum and maximum.
#'
#'The types are defined based on the types in the input table and on the value of other arguments.
#''numeric_cutoff' allows numeric variables to be classified as categorical if they have less unique
#'values than the value of 'numeric_cutoff'.
#'
#'@param data the table to analyse,
#'@param file character, the name of the file where the report will be saved. Default to
#'  paste0(deparse(substitute(data)), "_quality_results.xlsx"),
#'@param numeric_cutoff numeric value indicating the maximum number of unique values for a numerical
#'  variable to be classified as categorical. Default to -1 meaning that no numerical variables will
#'  be treated as categorical,
#'@param length_out numeric value indicating the maximum number in the output for categorical
#'  variables,
#'@param na_type charcater vector with valus that should be considered NA. Default to NULL, no
#'  values other than regular NA are treated as NA.
#'@param na_threshold numeric vector defining the range of colors in the output for the percentage
#'  of missing values. Default to green before 40 percent, orange between 40 and 80 and red over 80
#'  percent.
#'@param report logical. Should an excel report be rendered. Default to TRUE.
#'@param verbose logical. Should messages be printed in the console. Default to TRUE.
#'
#'@return If return is TRUE then a summary table with number of missing values, percentage of
#'  missing values and number of unique values by variable is returned as a data.table.
#'
#'@export
qualityCheck <- function (data, file = NULL, numeric_cutoff = -1, length_out = 100, na_type = c("", " "), na_threshold = c(40, 80), report = TRUE,
                          verbose = TRUE){

  if(verbose) cat("Beginning of the quality check\n")
  options(scipen = 999) # print numeric values in fixed notation unless they have more than 999 digits
  # Arguments check
  if(!is.data.frame(data) & !is.data.table(data)) stop("'data' must either be a data.frame or a data.table.")
  if(!is.data.table(data)) data <- as.data.table(data)
  if(is.null(file)){
    file <- paste0(deparse(substitute(data)), "_quality_results.xlsx")
  } else{
    if(!(is.character(file) & length(file) == 1)) stop("'file' must be character of length one.")
    if(!endsWith(file, ".xlsx")) stop("'file' must end with '.xlsx'")
  }
  if(!(is.numeric(numeric_cutoff) & length(numeric_cutoff) == 1)) stop("'numeric_cutoff' must be numeric of length one.")
  if(!(is.numeric(length_out) & length(length_out) == 1)) stop("'length_out' must be numeric of length one.")
  if(!is.null(na_threshold)) if(!(is.numeric(na_threshold) & length(na_threshold) == 2)) stop("'na_threshold' must be numeric of length 2.")
  if(!is.null(na_type)) if(!is.character(na_type)) stop("'na_type' must be a character vector.")

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
  categorical_var <- which(sapply(colnames(data), function(name) is_categorical(data[[name]])) == TRUE)
  numeric_var <- which(sapply(colnames(data), function(name) is_numeric(data[[name]])) == TRUE)
  date_var <- which(sapply(colnames(data), function(name) is_date(data[[name]])) == TRUE)

  # summary output
  types <- rep(x = "undefined", length = n_cols)
  types[categorical_var] <- "character"
  types[numeric_var] <- "numeric"
  types[date_var] <- "date"
  n_miss <- colSums(is.na(data))
  percent_miss <- 100 * n_miss / n_rows
  n_unique_values <- sapply(data, uniqueN)
  output_global <- data.frame(names(n_miss), types, n_miss, as.numeric(format(percent_miss, digits = 0)), n_unique_values)
  colnames(output_global) <- c("Variable", "Type", "Missing values", "Percentage of missing values", "Unique values")

  if(verbose) cat("Global summary created\n")

  # numeric output
  if(length(numeric_var) > 1){
    output_num <- matrixStats::colQuantiles(as.matrix(data[, .SD, .SDcols = numeric_var]), probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), na.rm = TRUE)
    output_num <- cbind.data.frame(names(numeric_var), output_num)
    colnames(output_num) <- c("Variable", "Min", paste0("Q", 1:9), "Max")
  } else if(length(numeric_var) == 1){
    output_num <- matrixStats::colQuantiles(as.matrix(data[, .SD, .SDcols = numeric_var]), probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), na.rm = TRUE)
    output_num <- cbind.data.frame(names(numeric_var), t(output_num))
    colnames(output_num) <- c("Variable", "Min", paste0("Q", 1:9), "Max")
  }

  if(verbose) cat("Numerical summary created\n")

  # categorical output
  if(length(categorical_var) > 0){
    output_character <- lapply(categorical_var, function(name) freqTable(data = data, name = name, length_out = length_out))
  }

  if(verbose) cat("Categorical summary created\n")

  # date output
  if(length(date_var) > 0){
    output_date_freq <- lapply(date_var, function(name) freqTable(data = data, name = name, length_out = length_out))
    output_date_range <- lapply(date_var, function(name) data.frame(c("min", "max"), c(min(data[[name]], na.rm = TRUE), max(data[[name]], na.rm = TRUE))))
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
                  value = paste0("The table has ", n_cols, " columns and ", n_rows, " rows", " (", n_double, " of them are unique)"),
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
      if(max(unlist(lapply(length(categorical_var), function(index) nrow(output_character[[index]])))) == length_out){
        addCustomCell(wb = workbook,
                      sheet = character_sheetname,
                      row_index = 2,
                      col_index = 1,
                      value = paste0("The maximum number of modalities is limited to ", length_out, ". Change the parameter 'length_out' to modify this behaviour."),
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
      if(max(unlist(lapply(length(date_var), function(index) nrow(output_date_freq[[index]])))) == length_out){
        addCustomCell(wb = workbook,
                      sheet = date_sheetname,
                      row_index = 2,
                      col_index = 1,
                      value = paste0("The maximum number of modalities is limited to ", length_out, ". Change the parameter 'length_out' to modify this behaviour."),
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
  invisible(output_global)

}
