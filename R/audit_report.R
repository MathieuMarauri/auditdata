
#'
#' This function creates a data quality report containing global information and optionnaly
#' univariate information. It can be either an html, an excel or both files.
#' 
#' @param data the dataset to analyse
#' @param output_file name of the output file. If NULL, the default then it is 'quality_report'.
#' @param quality_res an object with class qualityResult obtained with \code{data_quality()}
#' @param global_only logical, should only the global data quality be rendered?
#' @param na_type a character vector of strings that will be interpreted as NA
#' @param output_dir the directory to write the output file to, default to the current directory.
#' @param numeric_cutoff the minimum number of distinct values required for a numeric vector not to
#'   be coerced to a fator. -1 is the default, meaning no minimum required.
#' @param na_threshold numeric vector defining the range of values for the percentage of missing
#'   values to be colored green, orange and red. Default to green before 40 percent, orange between
#'   40 and 80 and red over 80 percent. If NULL then no colors are applied
#' @param max_length the maximum number of rows in the frequency tables. Default to Inf, all the values.
#' @param nchar maximum number of characters displayed in the plots as level values for
#'   categorical vectors.
#' @param order logical, whether to order the columns and rows to display the missing values next to
#'   each other, defautl to FALSE.
#' @param verbose logical, should information messages be printed in the console? default to TRUE.
#' 
#' @examples
#' \donttest{
#' data(iris)
#' 
#' # html report with only global information
#' audit_report(iris, "iris.html", global_only = TRUE)
#' 
#' # excel report with only global information
#' audit_report(iris, "iris.xlsx", global_only = TRUE)
#' 
#' # complete html report
#' audit_report(iris, "iris.html")
#' 
#' # complete excel report
#' audit_report(iris, "iris.xlsx")
#' }
#'
#' @import data.table
#' @import knitr
#' @importFrom magrittr "%>%"
#' @importFrom formattable color_bar
#' @importFrom kableExtra kable_styling cell_spec column_spec scroll_box
#' @importFrom rmarkdown render
#' @import openxlsx
#'
#' @export
audit_report <- function(data,
                         output_file,
                         quality_res = NULL,
                         global_only = FALSE,
                         na_type = NULL,
                         output_dir = NULL,
                         numeric_cutoff = -1,
                         na_threshold = c(40, 80),
                         max_length = Inf,
                         nchar = 20,
                         order = FALSE,
                         verbose = TRUE) {
  if (endsWith(output_file, ".html")) {
    audit_report_html(data = data, output_dir = output_dir, output_file = output_file, na_type = na_type, 
                      numeric_cutoff = numeric_cutoff, na_threshold = na_threshold, 
                      max_length = max_length, nchar = nchar, order = order, global_only = global_only)
  } else if (endsWith(output_file, ".xlsx")) {
    audit_report_excel(data = data, quality_res = quality_res, output_file = output_file, numeric_cutoff = numeric_cutoff, na_type = na_type, 
                       max_length = max_length, global_only = global_only, na_threshold = na_threshold, verbose = verbose)
  } else {
    message("The 'output_file' provided did not end with .xlsx or .html")
  }
}



#'
#' This function creates the data quality html file with global data quality and optionnaly the
#' univaraite exploratory analysis.
#'
#' @param data the dataset to analyse
#' @param output_dir the directory to write the output file to, default to the current directory.
#' @param output_file name of the output file. If NULL, the default then it is 'quality_report'.
#' @param na_type a character vector of strings that will be interpreted as NA
#' @param numeric_cutoff the minimum number of distinct values required for a numeric vector not to
#'   be coerced to a fator. -1 is the default, meaning no minimum required.
#' @param na_threshold numeric vector defining the range of values for the percentage of missing
#'   values to be colored green, orange and red. Default to green before 40 percent, orange between
#'   40 and 80 and red over 80 percent. If NULL then no colors are applied
#' @param max_length the maximum number of rows in the frequency tables. Default to 15.
#' @param nchar maximum number of characters displayed in the plots as level values for
#'   categorical vectors.
#' @param order logical, whether to order the columns and rows to display the missing values next to
#'   each other, defautl to FALSE.
#' @param global_only logical, should only the global data quality be rendered?
#'
#' @examples
#' \donttest{
#' data(iris)
#' audit_report_html(iris, "iris.html", global_only = TRUE)
#' audit_report_html(iris, "iris.html")
#' }
#'
#' @import data.table
#' @import knitr
#' @importFrom magrittr "%>%"
#' @importFrom formattable color_bar
#' @importFrom kableExtra kable_styling cell_spec column_spec scroll_box
#' @importFrom rmarkdown render
#'
#' @export
audit_report_html <- function(data,
                              output_dir = NULL,
                              output_file = NULL,
                              na_type = NULL,
                              numeric_cutoff = -1,
                              na_threshold = c(40, 80),
                              max_length = Inf,
                              nchar = 20,
                              order = FALSE,
                              global_only = FALSE) {
  # arguments check
  if (!is.data.frame(data) & !is.data.table(data)) {
    stop("'data' must either be a data.frame or a data.table.")
  }
  if (!is.data.table(data)) {
    data <- as.data.table(data)
  }
  if (!is.null(na_threshold)) {
    if (!(is.numeric(na_threshold) & length(na_threshold) == 2)) {
      stop("'na_threshold' must be numeric of length 2.")
    } else if (na_threshold[1] >= na_threshold[2]) {
      stop("The first element of 'na_threshold' should be lower than the second one.")
    }
  }
  if (!is.null(output_file)) {
    if (!(is.character(output_file) & length(output_file) == 1 & endsWith(output_file, ".html"))) {
      stop("'output_file' should have an html extension.")
    }
  }
  
  if (is.null(output_file) & is.null(output_dir)) {
    output_file <- "audit_report_global.html"
    output_dir <- "."
  } else if (is.null(output_dir)) {
    output_dir <- stringi::stri_replace_first_regex(output_file, "(?<=/).[^/]*$", "") %>% 
      stringi::stri_replace_first_regex("/$", "")
  } else if (is.null(output_file)) {
    output_file <- "audit_report_global.html"
  }
  
  rmarkdown::render(
    input = system.file("rmarkdown/templates/audit_report_global.Rmd", package = "auditdata"),
    output_file = output_file,
    output_dir = output_dir,
    envir = new.env(),
    params = list(data = data, na_type = na_type, numeric_cutoff = numeric_cutoff, na_threshold = na_threshold, 
                  max_length = max_length, nchar = nchar, order = order, global_only = global_only)
  )
}


#'
#' Performs a quality audit of a table
#'
#' This function builds an excel report based on the result of a quality check. It renders
#' an excel report with predefined styles using the openxlsx package.
#'
#' If quality_res is provided, data, numeric_cutoff, na_type and max_length are ignored.
#'
#' @param data a data.frame
#' @param quality_res an object with class qualityResult obtained with \code{data_quality()}
#' @param output_file output file name
#' @param numeric_cutoff the minimum number of distinct values required for a numeric
#'   vector not to be coerced to a fator. -1 is the default, meaning no minimum required.
#' @param na_type charcater vector with valus that should be considered NA. Default to
#'   NULL, no values other than regular NA are treated as NA.
#' @param max_length the maximum number of rows in the frequency tables
#' @param global_only logical, whether to return only the global summary
#' @param na_threshold numeric vector of length 2 defining the range of colors in the
#'   output for the percentage of missing values. Default to c(40, 80).
#' @param verbose logical, should information messages be printed in the console? default to TRUE.
#'
#' @return invisible, a list with a global summary, and if available, information on numeric,
#'   categorical and date variables
#'   
#' @examples
#' data(mtcars)
#' audit_report_excel(mtcars, output_file = "mtcars.xlsx")
#' 
#' data(iris)
#' audit_report_excel(mtcars, output_file = "iris.xlsx")
#'
#' @import openxlsx
#'
#' @export
audit_report_excel <- function(data = NULL, quality_res = NULL, output_file = NULL, numeric_cutoff = -1, na_type = NULL, 
                               max_length = Inf, global_only = FALSE, na_threshold = c(40, 80), verbose = TRUE) {
  if (is.null(data) & is.null(quality_res)) {
    stop("One of data and quality_res should be provided.")
  }
  if (!is.null(data) & !is.null(quality_res) & class(quality_res)[2] == "qualityResult") {
    warning("Both data and quality_res provided, report is build with quality_res.")
  }
  if (!is.null(data) & !is.null(quality_res) & class(quality_res)[2] != "qualityResult") {
    warning('Both data and quality_res provided, report is build with data as quality_res does not have class "qualityResult"')
  }
  if (is.null(data) & !is.null(quality_res) & class(quality_res)[2] != "qualityResult") {
    stop('quality_res is not of class "qualityResult", make sure it comes from dataQuality.')
  }
  if (!is.null(quality_res) & class(quality_res)[2] == "qualityResult" & length(quality_res) == 1 & !global_only) {
    warning("The report is build with only the global view as quality_res only have one element. Make sure qualitty_res is obtained with global_only set to FALSE.")
  }
  if (is.null(output_file)) {
    output_file <- "quality_results.xlsx"
  } else {
    if (!(is.character(output_file) & length(output_file) == 1 & endsWith(output_file, ".xlsx"))) {
      stop("'output_file' should have an xlsx extension.")
    }
  }
  if (!is.null(na_threshold)) {
    if (!(is.numeric(na_threshold) & length(na_threshold) == 2)) {
      stop("'na_threshold' must be numeric of length 2.")
    }
  }
  if (!is.null(data) & (is.null(quality_res) | (!is.null(quality_res) & class(quality_res)[2] != "qualityResult"))) {
    quality_res <- data_quality(
      data = data,
      numeric_cutoff = numeric_cutoff,
      na_type = na_type,
      max_length = max_length,
      global_only = global_only
    )
  }
  output_global <- quality_res$global$table
  n_cols <- quality_res$global$global$n_cols
  n_rows <- quality_res$global$global$n_rows
  n_unique <- quality_res$global$global$n_unique
  
  
  workbook <- createWorkbook()
  title_style <- createStyle(
    fontSize = 16,
    textDecoration = "bold"
  )
  subtitle_style <- createStyle(fontSize = 14)
  table_title_style <- createStyle(
    fontSize = 11,
    textDecoration = "bold",
    border = "TopBottomLeftRight ",
    borderColour = "black",
    borderStyle = "thin",
    halign = "center"
  )
  summary_sheetname <- "Summary"
  addWorksheet(
    wb = workbook,
    sheetName = summary_sheetname,
    gridLines = FALSE
  )
  add_custom_cell(
    wb = workbook,
    sheet = summary_sheetname,
    row_index = 1,
    col_index = 1,
    value = "Global quality check of the table",
    cell_style = title_style
  )
  add_custom_cell(
    wb = workbook,
    sheet = summary_sheetname,
    row_index = 2,
    col_index = 1,
    value = paste0(
      "The table has ", n_cols, " columns and ", n_rows,
      " rows", " (", n_unique, " of them are unique)"
    ),
    cell_style = subtitle_style
  )
  add_custom_table(
    wb = workbook,
    table = output_global,
    sheet = summary_sheetname,
    start_row = 4,
    start_column = 2
  )
  for (i in 4:6) {
    setColWidths(
      wb = workbook,
      sheet = summary_sheetname,
      cols = i,
      widths = nchar(colnames(output_global)[i - 1]) + 3
    )
  }
  setColWidths(
    wb = workbook,
    sheet = summary_sheetname,
    cols = 2,
    widths = max(nchar(output_global[["Variable"]])) + 3
  )
  setColWidths(
    wb = workbook,
    sheet = summary_sheetname,
    cols = 3,
    widths = max(nchar(output_global[["Type"]])) + 3
  )
  if (!is.null(na_threshold)) {
    for (i in 1:nrow(output_global)) {
      if (output_global[i, 4] > na_threshold[2]) {
        addStyle(
          wb = workbook,
          sheet = summary_sheetname,
          style = createStyle(fontColour = "red"),
          rows = i + 4,
          cols = 5,
          stack = TRUE
        )
      } else if (output_global[i, 4] > na_threshold[1]) {
        addStyle(
          wb = workbook,
          sheet = summary_sheetname,
          style = createStyle(fontColour = "orange"),
          rows = i + 4,
          cols = 5,
          stack = TRUE
        )
      } else {
        addStyle(
          wb = workbook,
          sheet = summary_sheetname,
          style = createStyle(fontColour = "forestgreen"),
          rows = i + 4,
          cols = 5,
          stack = TRUE
        )
      }
    }
  }
  if (verbose) cat("Global summary created\n")
  if (!global_only) {
    if ("numeric" %in% names(quality_res)) {
      output_num <- quality_res$numeric
      numeric_sheetname <- "Numeric"
      addWorksheet(
        wb = workbook,
        sheetName = numeric_sheetname,
        gridLines = FALSE
      )
      # title
      add_custom_cell(
        wb = workbook,
        sheet = numeric_sheetname,
        row_index = 1,
        col_index = 1,
        value = "Quantiles of the numerical variables",
        cell_style = title_style
      )
      # numeric_output
      add_custom_table(
        wb = workbook,
        sheet = numeric_sheetname,
        table = output_num,
        start_row = 3,
        start_column = 2
      )
      # columns width
      setColWidths(
        wb = workbook,
        sheet = numeric_sheetname,
        cols = 2,
        widths = max(nchar(output_num[, 1])) + 3
      )
      if (verbose) cat("Numeric summary created\n")
    }
    if ("categorical" %in% names(quality_res)) {
      output_character <- quality_res$categorical
      character_sheetname <- "Character"
      addWorksheet(
        wb = workbook,
        sheetName = character_sheetname,
        gridLines = FALSE
      )
      add_custom_cell(
        wb = workbook,
        sheet = character_sheetname,
        row_index = 1,
        col_index = 1,
        value = "Frequences of modalities for the categorical variables",
        cell_style = title_style
      )
      if (max(sapply(X = output_character, FUN = function(x) nrow(x))) == max_length) {
        add_custom_cell(
          wb = workbook,
          sheet = character_sheetname,
          row_index = 2,
          col_index = 1,
          value = paste0("The maximum number of levels displayed is limited to ", max_length, "."),
          cell_style = subtitle_style
        )
      }
      list_names <- lapply(
        X = names(output_character),
        FUN = function(name) return(c(name, rep(NA, 3)))
      )
      list_names <- unlist(list_names)
      writeData(
        wb = workbook,
        sheet = character_sheetname,
        x = t(list_names),
        startCol = 2,
        startRow = 4,
        colNames = FALSE,
        rowNames = FALSE,
        keepNA = FALSE
      )
      addStyle(
        wb = workbook,
        sheet = character_sheetname,
        style = table_title_style,
        rows = 4,
        cols = seq(from = 2, to = 4 * length(output_character), by = 4),
        gridExpand = FALSE,
        stack = TRUE
      )
      for (i in seq(from = 1, to = length(list_names), by = 4)) {
        setColWidths(
          wb = workbook,
          sheet = character_sheetname,
          cols = i + 1,
          widths = nchar(list_names[i]) + 3
        )
      }
      for (index in seq_len(length(output_character))) {
        add_custom_table(
          wb = workbook,
          sheet = character_sheetname,
          table = output_character[[index]],
          start_row = 5,
          start_column = (index - 1) * 4 + 2
        )
      }
      if (verbose) cat("Categorical summary created\n")
    }
    if ("date" %in% names(quality_res)) {
      output_date_freq <- quality_res$date$freq
      output_date_range <- quality_res$date$range
      date_sheetname <- "Date"
      addWorksheet(
        wb = workbook,
        sheetName = date_sheetname,
        gridLines = FALSE
      )
      # title
      add_custom_cell(
        wb = workbook,
        sheet = date_sheetname,
        row_index = 1,
        col_index = 1,
        value = "Frequences of modalities for the date variables",
        cell_style = title_style
      )
      # subtitle
      if (max(sapply(X = output_date_freq, FUN = function(x) nrow(x))) == max_length) {
        add_custom_cell(
          wb = workbook,
          sheet = date_sheetname,
          row_index = 2,
          col_index = 1,
          value = paste0("The maximum number of levels displayed is limited to ", max_length, "."),
          cell_style = subtitle_style
        )
      }
      # date output
      list_names <- lapply(
        X = names(output_date_freq),
        FUN = function(name) return(c(name, rep(NA, 6)))
      )
      list_names <- unlist(list_names)
      writeData(
        wb = workbook,
        sheet = date_sheetname,
        x = t(list_names),
        startCol = 2,
        startRow = 4,
        colNames = FALSE,
        rowNames = FALSE,
        keepNA = FALSE
      )
      addStyle(
        wb = workbook,
        sheet = date_sheetname,
        style = table_title_style,
        rows = 4,
        cols = seq(from = 2, to = 7 * length(output_date_freq), by = 7),
        gridExpand = FALSE,
        stack = TRUE
      )
      # Change column width
      for (i in seq(from = 1, to = length(list_names), by = 7)) {
        setColWidths(
          wb = workbook,
          sheet = date_sheetname,
          cols = i + 1,
          widths = nchar(list_names[i]) + 3
        )
      }
      
      for (index in seq_len(length(output_date_freq))) {
        add_custom_table(
          wb = workbook,
          sheet = date_sheetname,
          table = output_date_freq[[index]],
          start_row = 5,
          start_column = (index - 1) * 7 + 2,
          date = TRUE
        )
      }
      # add min/max
      for (index in seq_len(length(output_date_freq))) {
        writeData(
          wb = workbook,
          sheet = date_sheetname,
          x = output_date_range[[index]],
          startCol = (index - 1) * 7 + 6,
          startRow = 5,
          colNames = FALSE,
          rowNames = FALSE,
          borders = "all"
        )
        style <- createStyle(
          fontSize = 11,
          textDecoration = "bold",
          halign = "center"
        )
        addStyle(
          wb = workbook,
          sheet = date_sheetname,
          style = style,
          rows = c(5, 6),
          cols = (index - 1) * 7 + 6,
          gridExpand = TRUE,
          stack = TRUE
        )
      }
      for (i in seq(from = 2, to = length(list_names), by = 7)) {
        setColWidths(
          wb = workbook,
          sheet = date_sheetname,
          cols = c(i, i + 5),
          widths = nchar("00/00/0000") + 2
        )
      }
      if (verbose) cat("Date summary created\n")
    }
  }
  saveWorkbook(wb = workbook, file = output_file, overwrite = TRUE)
  invisible(quality_res)
}
