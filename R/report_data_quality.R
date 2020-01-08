
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
#' @param file output file name
#' @param numeric_cutoff the minimum number of distinct values required for a numeric
#'   vector not to be coerced to a fator. -1 is the default, meaning no minimum required.
#' @param na_type charcater vector with valus that should be considered NA. Default to
#'   NULL, no values other than regular NA are treated as NA.
#' @param max_length the maximum number of rows in the frequency tables
#' @param global_only logical, whether to return only the global summary
#' @param na_threshold numeric vector of length 2 defining the range of colors in the
#'   output for the percentage of missing values. Default to c(40, 80).
#'
#' @return invisible, a list with a global summary, and if available, information on numeric,
#'   categorical and date variables
#'
#' @import openxlsx
#'
#' @export
report_data_quality <- function(data = NULL,
                                quality_res = NULL,
                                file = NULL,
                                numeric_cutoff = -1,
                                na_type = NA,
                                max_length = Inf,
                                global_only = FALSE,
                                na_threshold = c(40, 80)) {
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
  if (is.null(file)) {
    file <- "quality_results.xlsx"
  } else {
    if (!(is.character(file) & length(file) == 1)) {
      stop("file must be a character of length one.")
    }
    if (!endsWith(file, ".xlsx")) {
      file <- paste0(file, ".xlsx")
      warning("file does end with .xlsx, it was added.")
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
  output_global <- quality_res$global$global
  n_cols <- quality_res$global$dim["ncol"]
  n_rows <- quality_res$global$dim["nrow"]
  n_unique <- quality_res$global$dim["unique"]
  
  
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
      liste_names <- sapply(
        X = names(output_character),
        FUN = function(name) return(c(name, rep(NA, 3)))
      )
      writeData(
        wb = workbook,
        sheet = character_sheetname,
        x = t(unlist(lapply(X = names(output_character), FUN = function(name) return(c(name, rep(NA, 3)))))),
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
      for (i in seq(from = 1, to = length(liste_names), by = 4)) {
        setColWidths(
          wb = workbook,
          sheet = character_sheetname,
          cols = i + 1,
          widths = nchar(liste_names[i]) + 3
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
      if (max(
        sapply(
          X = output_date_freq,
          FUN = function(x) nrow(x)
        )
      ) == max_length) {
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
      liste_names <- sapply(
        X = names(output_date_freq),
        FUN = function(name) return(c(name, rep(NA, 6)))
      )
      writeData(
        wb = workbook,
        sheet = date_sheetname,
        x = t(liste_names),
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
      for (i in seq(from = 1, to = length(liste_names), by = 7)) {
        setColWidths(
          wb = workbook,
          sheet = date_sheetname,
          cols = i + 1,
          widths = nchar(liste_names[i]) + 3
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
      for (i in seq(from = 1, to = length(liste_names), by = 7)) {
        setColWidths(
          wb = workbook,
          sheet = date_sheetname,
          cols = i + 6,
          widths = nchar("00/00/0000") + 1
        )
      }
    }
  }
  saveWorkbook(wb = workbook, file = file, overwrite = TRUE)
  invisible(quality_res)
}
