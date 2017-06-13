#'
#' Helper function to add titles.
#'
#' @param sheet sheet object to contain the title,
#' @param row_index numeric value indicating the row to contain the title,
#' @param title the text to use as title,
#' @param title_style style object to use for title
#'
addCustomCell <- function(sheet, row_index, col_index, title, title_style){
  rows <- createRow(sheet = sheet, rowIndex = row_index)
  sheetTitle <- createCell(row = rows, colIndex = col_index)
  setCellValue(cell = sheetTitle[[1, 1]], value = title)
  setCellStyle(cell = sheetTitle[[1, 1]], cellStyle = title_style)
}

#'
#' Helper function for 'qualityCheck'
#'
#' This function creates a frequency table from the variable 'name' in the table
#' 'data'. This frequency table is ordered and limited to 'max_unique_out' rows.
#'
#' @param data the table containing the data,
#' @param name the name of the variable from wich the frequency table will be
#'   computed,
#' @param max_unique_out the maximum number of rows in the output.
#'
#' @return a frequency table with the top 'max_unique_out' most frequent
#'   modalities and the associated percentage.
#'
freqTable <- function(data, name, max_unique_out = Inf){
  result <- as.data.table(data[[name]])[
    ,
    .N,
    by = data[[name]]
    ][
      order(-N),
      .(value = data,
        freq = N,
        percent = round(100 * N / sum(N),
                        digits = 0))]
  if(nrow(result) > max_unique_out){
    result <- result[1:max_unique_out, ]
  }
  return(result)
}

#'
#' This functions add a data frame to a sheet and styles it.
#'
#' It uses 'addDataFrame' from the xlsx package and add birder around the table.
#'
#' @param table the table to be added,
#' @param sheet the sheet where tha table will be added,
#' @param start_row a numeric value for the starting row,
#' @param start_column a numeric value for the starting column,
#' @param col_names logical, should the colnames be written,
#' @param colnames_style the style associated to the colnames,
#' @param col_style a list of CellStyle. If the name of the list element is the
#'   column number, it will be used to set the style of the column. Columns of
#'   type Date and POSIXct are styled automatically even if colSyle=NULL.
#'
addTable <- function(table, sheet, start_row, start_column, col_names, colnames_style, col_style, n_columns){
  addDataFrame(x = table,
               sheet = sheet,
               col.names = col_names,
               row.names = FALSE,
               startRow = start_row ,
               startColumn = start_column,
               colnamesStyle = colnames_style,
               colStyle = col_style,
               showNA = TRUE,
               characterNA = "NA")
  cb <- CellBlock(sheet = sheet, startRow = start_row, startColumn = start_column, noRows = nrow(table) + col_names, noColumns = n_columns, create = FALSE)
  # add borders
  CB.setBorder(cellBlock = cb, border = Border(color = "black", position = "BOTTOM", pen = "BORDER_THIN"), rowIndex = nrow(table) + col_names, colIndex = 1:n_columns)
  CB.setBorder(cellBlock = cb, border = Border(color = "black", position = "RIGHT", pen = "BORDER_THIN"), rowIndex = 1:(nrow(table) + col_names), colIndex = n_columns)
  CB.setBorder(cellBlock = cb, border = Border(color = "black", position = c("RIGHT", "LEFT"), pen = "BORDER_THIN"), rowIndex = 1, colIndex = 1)
  if(!col_names) CB.setBorder(cellBlock = cb, border = Border(color = "black", position = c("TOP"), pen = "BORDER_THIN"), rowIndex = 1, colIndex = 1:n_columns)
}

#'
#' Performs an audit of a table.
#'
#' This function performs a quality check on a table and creates an excel report
#' if asked. In the report one can find the number of missing values by variable
#' alog with the quantiles for the numeric variables and a frequency table for
#' each categorical variable. An excel report can be rendered with the different
#' values computed.
#'
#' The excel report is composed of several sheets. A 'summary' one with
#' information regarding missing values for every variables, a 'numeric' sheet
#' with the quantiles of all numeric varaibles, a 'categorical' sheet with a
#' table with unique values and their frequency for each categorical variable
#' and finally a 'date' sheet with also a frequency table for each date variable
#' along with the minimum and maximum.
#'
#' The types are defined based on the types in the input table and on the value
#' of other arguments. 'numeric_cutoff' allows numeric variables to be
#' classified as categorical if they have less unique values than the value of
#' 'numeric_cutoff'. Date variables has to be given by the user in the
#' 'date_cols' argument.
#'
#' @section warning: For now the function does not know how to deal with Date or
#'   POSIXlt/POSIXct format, please coerce to character before and use
#'   'date_cols' to specify the date format.
#'
#' @param data the table to analyse,
#' @param file character, the name of the file where the report will be saved.
#'   Default to paste0(deparse(substitute(data)), "_quality_results.xlsx"),
#' @param numeric_cutoff numeric value indicating the maximum number of unique
#'   values for a numerical variable to be classified as categorical. Default to
#'   -1 meaning that no numerical variables will be treated as categorical,
#' @param max_unique_out numeric value indicating the maximum number in the
#'   output for categorical variables,
#' @param na_type charcater vector with valus that should be considered NA.
#'   Default to NULL, no values other than regular NA are treated as NA.
#' @param na_threshold numeric vector defining the range of colors in the output
#'   for the percentage of missing values. Default to green before 40 percent,
#'   orange between 40 and 80 and red over 80 percent.
#'
#'@export
qualityCheck <- function (data, file = NULL, numeric_cutoff = -1, max_unique_out = 100, na_type = c("", " "), na_threshold = c(40, 80)){

  options(scipen = 999) # print numeric values in fixed notation unless they have more than 999 digits
  # Arguments check
  if(!is.data.table(data)) data <- as.data.table(data)
  if(is.null(file)){
    file <- paste0(deparse(substitute(data)), "_quality_results.xlsx")
  } else{
    if(!(is.character(file) & length(file) == 1)) stop("'file' must be character of length one.")
    if(!endsWith(file, ".xlsx")) stop("'file' must end with '.xlsx'")
  }
  if(!(is.numeric(numeric_cutoff) & length(numeric_cutoff) == 1)) stop("'numeric_cutoff' must be numeric of length one.")
  if(!(is.numeric(max_unique_out) & length(max_unique_out) == 1)) stop("'max_unique_out' must be numeric of length one.")
  if(!is.null(na_threshold)) if(!(is.numeric(na_threshold) & length(na_threshold) == 2)) stop("'na_threshold' must be numeric of length 2.")
  if(!is.null(na_type)) if(!is.character(na_type)) stop("'na_type' must be a character vector.")

  if(!is.null(na_type)){
    for(j in seq_along(data)){
      set(data, i = which(data[[j]] %in% na_type), j = j, value = NA)
    }
  }

  is.date <- function(x) inherits(x, 'Date') | inherits(x, 'POSIXct') | inherits(x, 'POSIXlt')

  n_cols <- ncol(data)
  n_rows <- nrow(data)
  n_double <- nrow(unique(data))
  # columns types
  categorical_var <- which(sapply(colnames(data), function(name) is.factor(data[[name]]) || is.character(data[[name]]) || uniqueN(data[[name]]) <= numeric_cutoff) == TRUE)
  numeric_var <- which(sapply(colnames(data), function(name) is.numeric(data[[name]]) & uniqueN(data[[name]]) > numeric_cutoff) == TRUE)
  date_var <- which(sapply(colnames(data), function(name) is.date(data[[name]])) == TRUE)

  # summary output
  types <- rep(x = "undefined", length = n_cols)
  types[categorical_var] <- "character"
  types[numeric_var] <- "numeric"
  types[date_var] <- "date"
  n_miss <- colSums(is.na(data))
  percent_miss <- 100 * n_miss / n_rows
  n_unique_values <- t(data[, lapply(.SD, uniqueN)])
  output_global <- cbind.data.frame(names(n_miss), types, n_miss, as.numeric(format(percent_miss, digits = 0)), n_unique_values)
  colnames(output_global) <- c("Variables", "Type", "Missing values", "Percentage of missing values", "Unique values")

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

  # categorical output
  if(length(categorical_var) > 0){
    output_character <- lapply(categorical_var, function(name) freqTable(data = data, name = name, max_unique_out = max_unique_out))
  }

  # date output
  if(length(date_var) > 0){
    output_date_freq <- lapply(date_var, function(name) freqTable(data = data, name = name, max_unique_out = max_unique_out))
    output_date_range <- lapply(date_var, function(name) data.frame(c("min", "max"), c(min(data[[name]], na.rm = TRUE), max(data[[name]], na.rm = TRUE))))
  }

  # initialisation of the excel report
  workbook <- createWorkbook(type = "xlsx")
  # creation of some cell styles
  title_style <- CellStyle(wb = workbook) +
    Font(wb = workbook,  heightInPoints = 16, isBold = TRUE, underline = 0)
  subtitle_style <- CellStyle(wb = workbook) +
    Font(workbook,  heightInPoints = 14, isItalic = FALSE, isBold = FALSE)
  table_rownames_style <- CellStyle(wb = workbook) +
    Font(wb = workbook, isBold = TRUE) +
    Alignment(wrapText = TRUE, horizontal = "ALIGN_LEFT") +
    Border(color = "black", position = c("LEFT", "RIGHT"), pen = "BORDER_THIN")
  table_colnames_style <- CellStyle(wb = workbook) +
    Font(wb = workbook, isBold = TRUE) +
    Alignment(wrapText = TRUE, horizontal = "ALIGN_CENTER") +
    Border(color = "black", position = c("TOP", "BOTTOM"), pen = "BORDER_THIN")
  first_col_style <- CellStyle(wb = workbook) +
    Font(wb = workbook, isBold = TRUE) +
    Border(color = "black", position = c("RIGHT", "LEFT"), pen = "BORDER_THIN")
  other_col_style <- CellStyle(wb = workbook) +
    Alignment(wrapText = TRUE, horizontal = "ALIGN_CENTER")
  date_col_style <- CellStyle(wb = workbook) +
    Alignment(wrapText = TRUE, horizontal = "ALIGN_CENTER") +
    Font(wb = workbook, isBold = TRUE) +
    DataFormat(x = "dd-mm-yyyy") +
    Border(color = "black", position = c("RIGHT", "LEFT"), pen = "BORDER_THIN")
  table_title_style <- CellStyle(wb = workbook) +
    Font(wb = workbook, isBold = TRUE) +
    Alignment(wrapText = TRUE, horizontal = "ALIGN_CENTER") +
    Border(color = "black", position = c("TOP", "BOTTOM", "RIGHT", "LEFT"), pen = "BORDER_THIN")
  # creation of the different sheets
  summary_sheet <- createSheet(wb = workbook, sheetName = "Summary")
  # title
  addCustomCell(summary_sheet,
                row_index = 1,
                col_index = 1,
                title = "Global quality check of the table",
                title_style = title_style)
  # subtitle
  addCustomCell(summary_sheet,
                row_index = 2,
                col_index = 1,
                title = paste0("The table has ", n_cols, " columns and ", n_rows, " rows", " (", n_double, " of them are unique)"),
                title_style = subtitle_style)
  # summary output
  col_style <- append(list(`1` = first_col_style), rep(list(other_col_style), times = 4))
  names(col_style) <- 1:5
  addTable(table = output_global,
           sheet = summary_sheet,
           start_row = 4,
           start_column = 2,
           col_names = TRUE,
           colnames_style = table_colnames_style,
           col_style = col_style,
           n_columns = 5)
  # Change column width
  cb <- CellBlock(sheet = summary_sheet, startRow = 4, startColumn = 2, noRows = n_cols + 1, noColumns = 4, create = FALSE)
  for(i in 4:6){
    setColumnWidth(sheet = summary_sheet,
                   colIndex = i,
                   colWidth = nchar(colnames(output_global)[i-1]) + 3)
  }
  setColumnWidth(sheet = summary_sheet,
                 colIndex = 2,
                 colWidth = max(nchar(colnames(data))) + 3)
  setColumnWidth(sheet = summary_sheet,
                 colIndex = 3,
                 colWidth = nchar("character") + 3)
  # fill cell depending on percentage value
  if(!is.null(na_threshold)){
    for(i in 1:nrow(output_global)){
      if(output_global[i, 4] > na_threshold[2]){
        CB.setFont(cellBlock = cb, font = Font(wb = workbook, color = "red", isBold = TRUE), rowIndex = i + 1, colIndex = 4)
      } else if(output_global[i, 4] > na_threshold[1]){
        CB.setFont(cellBlock = cb, font = Font(wb = workbook, color = "orange", isBold = TRUE), rowIndex = i + 1, colIndex = 4)
      } else{
        CB.setFont(cellBlock = cb, font = Font(wb = workbook, color = "forestgreen", isBold = TRUE), rowIndex = i + 1, colIndex = 4)
      }
    }
  }
  # numeric sheet
  if(length(numeric_var) > 0){
    numeric_sheet <- createSheet(workbook, sheetName = "Numeric")
    # title
    addCustomCell(numeric_sheet,
                  row_index = 1,
                  col_index = 1,
                  title = "Quantiles of the numerical variables",
                  title_style = title_style)
    # numeric_output
    col_style <- append(list(first_col_style), rep(list(other_col_style), times = 11))
    names(col_style) <- 1:12
    addDataFrame(x = output_num,
                 sheet = numeric_sheet,
                 startRow = 3,
                 startColumn = 2,
                 col.names = TRUE,
                 row.names = FALSE,
                 colnamesStyle = table_colnames_style,
                 showNA = FALSE,
                 colStyle = col_style)
    # add borders
    cb <- CellBlock(sheet = numeric_sheet, startRow = 3, startColumn = 2, noRows = length(numeric_var) + 1, noColumns = 12, create = FALSE)
    CB.setBorder(cellBlock = cb, border = Border(color = "black", position = "BOTTOM", pen = "BORDER_THIN"), rowIndex = length(numeric_var) + 1, colIndex = 1:12)
    CB.setBorder(cellBlock = cb, border = Border(color = "black", position = "RIGHT", pen = "BORDER_THIN"), rowIndex = 1:(length(numeric_var) + 1), colIndex = 12)
    CB.setBorder(cellBlock = cb, border = Border(color = "black", position = c("RIGHT", "LEFT"), pen = "BORDER_THIN"), rowIndex = 1, colIndex = 1)
    # columns width
    setColumnWidth(sheet = numeric_sheet,
                   colIndex = 2,
                   colWidth = max(nchar(colnames(data))) + 3)
  }
  # character sheet
  if(length(categorical_var) > 0){
    character_sheet <- createSheet(wb = workbook, sheetName = "Character")
    # title
    addCustomCell(character_sheet,
                  row_index = 1,
                  col_index = 1,
                  title = "Frequences of modalities for the categorical variables",
                  title_style = title_style)
    # subtitle
    if(max(unlist(lapply(length(categorical_var), function(index) nrow(output_character[[index]])))) == max_unique_out){
      addCustomCell(character_sheet,
                    row_index = 2,
                    col_index = 1,
                    title = paste0("The maximum number of modalities is limited to ", max_unique_out, ". Change the parameter 'max_unique_out' to modify this behaviour."),
                    title_style = subtitle_style)
    }
    # character output
    liste_names <- unlist(lapply(names(categorical_var), function(name) return(c(name, rep(NA, 3)))))
    col_style <- rep(list(table_title_style), times = length(categorical_var))
    names(col_style) <- seq(from = 1, to = length(liste_names), by = 4)
    addDataFrame(x = t(liste_names),
                 sheet = character_sheet,
                 col.names = FALSE,
                 row.names = FALSE,
                 startRow = 4,
                 startColumn = 2,
                 colStyle = col_style,
                 showNA = TRUE)
    # Change column width
    for(i in seq(from = 1, to = length(liste_names), by = 4)){
      setColumnWidth(sheet = character_sheet,
                     colIndex = i + 1,
                     colWidth = nchar(liste_names[i]) + 3)
    }
    col_style <- append(list(`1` = first_col_style), rep(list(other_col_style), times = 2))
    names(col_style) <- 1:3
    for(index in seq_len(length(categorical_var))){
      addTable(table = output_character[[index]],
               sheet = character_sheet,
               start_row = 5,
               start_column = (index - 1) * 4 + 2,
               col_names = TRUE,
               colnames_style = table_colnames_style,
               col_style = col_style,
               n_columns = 3)
    }
  }
  # date sheet
  if(length(date_var) > 0){
    date_sheet <- createSheet(wb = workbook, sheetName = "Date")
    # title
    addCustomCell(date_sheet,
                  row_index = 1,
                  col_index = 1,
                  title = "Frequences of modalities for the date variables",
                  title_style = title_style)
    # subtitle
    addCustomCell(date_sheet,
                  row_index = 2,
                  col_index = 1,
                  title = paste0("The maximum number of modalities is limited to ", max_unique_out, ". Change the parameter 'max_unique_out' to modify this behaviour."),
                  title_style = subtitle_style)
    # date output
    liste_names <- unlist(lapply(names(date_var), function(name) return(c(name, rep(NA, 6)))))
    col_style <- rep(list(table_title_style), times = length(date_var))
    names(col_style) <- seq(from = 1, to = length(liste_names), by = 7)
    addDataFrame(x = t(liste_names),
                 sheet = date_sheet,
                 col.names = FALSE,
                 row.names = FALSE,
                 startRow = 4,
                 startColumn = 2,
                 colStyle = col_style,
                 showNA = FALSE)
    # Change column width
    for(i in seq(from = 1, to = length(liste_names), by = 7)){
      setColumnWidth(sheet = date_sheet,
                     colIndex = i + 1,
                     colWidth = nchar(liste_names[i]) + 3)
    }
    # add frequences
    col_style <- append(list(`1` = date_col_style), rep(list(other_col_style), times = 2))
    names(col_style) <- 1:3
    for(index in seq_len(length(date_var))){
      addTable(table = output_date_freq[[index]],
               sheet = date_sheet,
               start_row = 5,
               start_column = (index - 1) * 7 + 2,
               col_names = TRUE,
               colnames_style = table_colnames_style,
               col_style = col_style,
               n_columns = 3)
    }
    # add min/max
    for(index in seq_len(length(date_var))){
      addTable(table = output_date_range[[index]],
               sheet = date_sheet,
               start_row = 5,
               start_column = (index - 1) * 7 + 6,
               col_names = FALSE,
               col_style = list(`1` = first_col_style, `2` = date_col_style),
               n_columns = 2)
    }
    for(i in seq(from = 1, to = length(liste_names), by = 7)){
      setColumnWidth(sheet = date_sheet,
                     colIndex = i + 6,
                     colWidth = nchar("00-00-0000") + 3)
    }
  }
  saveWorkbook(wb = workbook, file = file)
}
