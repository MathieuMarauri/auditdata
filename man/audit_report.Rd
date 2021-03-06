% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/audit_report.R
\name{audit_report}
\alias{audit_report}
\title{This function creates a data quality report containing global information and optionnaly
univariate information. It can be either an html, an excel or both files.}
\usage{
audit_report(
  data,
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
  verbose = TRUE
)
}
\arguments{
\item{data}{the dataset to analyse}

\item{output_file}{name of the output file. If NULL, the default then it is 'quality_report'.}

\item{quality_res}{an object with class qualityResult obtained with \code{data_quality()}}

\item{global_only}{logical, should only the global data quality be rendered?}

\item{na_type}{a character vector of strings that will be interpreted as NA}

\item{output_dir}{the directory to write the output file to, default to the current directory.}

\item{numeric_cutoff}{the minimum number of distinct values required for a numeric vector not to
be coerced to a fator. -1 is the default, meaning no minimum required.}

\item{na_threshold}{numeric vector defining the range of values for the percentage of missing
values to be colored green, orange and red. Default to green before 40 percent, orange between
40 and 80 and red over 80 percent. If NULL then no colors are applied}

\item{max_length}{the maximum number of rows in the frequency tables. Default to Inf, all the values.}

\item{nchar}{maximum number of characters displayed in the plots as level values for
categorical vectors.}

\item{order}{logical, whether to order the columns and rows to display the missing values next to
each other, defautl to FALSE.}

\item{verbose}{logical, should information messages be printed in the console? default to TRUE.}
}
\description{
This function creates a data quality report containing global information and optionnaly
univariate information. It can be either an html, an excel or both files.
}
\examples{
\donttest{
data(iris)

# html report with only global information
audit_report(iris, "iris.html", global_only = TRUE)

# excel report with only global information
audit_report(iris, "iris.xlsx", global_only = TRUE)

# complete html report
audit_report(iris, "iris.html")

# complete excel report
audit_report(iris, "iris.xlsx")
}

}
