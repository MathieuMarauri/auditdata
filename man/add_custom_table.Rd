% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_custom_cell.R
\name{add_custom_table}
\alias{add_custom_table}
\title{Helper function to add a data frame to a sheet and styles it.}
\usage{
add_custom_table(wb, sheet, table, start_row, start_column, date = FALSE)
}
\arguments{
\item{wb}{a workbook}

\item{sheet}{a sheet in the workbook,}

\item{table}{the table to be added,}

\item{start_row}{a numeric value for the starting row,}

\item{start_column}{a numeric value for the starting column,}

\item{date}{logical. Is the first column of type date?}
}
\description{
It encapsulates writeData, addStyle and createStyle from the openxlsx package.
}
