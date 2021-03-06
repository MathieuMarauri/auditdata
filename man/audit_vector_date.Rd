% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/desc_date.R
\name{audit_vector_date}
\alias{audit_vector_date}
\title{This functions desxribes a date vector. Summary statistics are given and frequency and
missing plots are generated.}
\usage{
audit_vector_date(x, plot = TRUE)
}
\arguments{
\item{x}{a date vector (can be of class Date and POSIXct)}

\item{plot}{logical, whether to plot the graph. Defaults to TRUE.}
}
\value{
a table with summary statistics (length, number of na, , number of unique, min
  and max) and if plot is true the frequency plot from plot_freq_date and a plot of the
  missing values from plot_missing_date.
}
\description{
This functions desxribes a date vector. Summary statistics are given and frequency and
missing plots are generated.
}
