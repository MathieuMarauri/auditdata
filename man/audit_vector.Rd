% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/describe.R
\name{audit_vector}
\alias{audit_vector}
\title{This function describes a vector with summary statistics and plots.}
\usage{
audit_vector(
  x,
  na_type = NULL,
  numeric_cutoff = -1,
  max_length = 15,
  nchar = 20,
  plot = TRUE
)
}
\arguments{
\item{x}{a vector}

\item{na_type}{a character vector of strings that will be interpreted as NA}

\item{numeric_cutoff}{the minimum number of distinct values required for a numeric
vector not to be coerced to a fator. -1 is the default, meaning no minimum required.}

\item{max_length}{the maximum number of different values to display in the freqeuncy
table and the frequency plot, default to 15.}

\item{nchar}{maximum number of characters displayed in the plots as level values for
categorical vectors. See details.}

\item{plot}{logical, whether to plot the graph. Defaults to TRUE.}
}
\description{
This function describes a vector with summary statistics and plots.
}
\details{
Depending on the class of the vector the functions \code{descNumeric()},
  \code{descCategorical()} or \code{descDate()} are called. Each one returns one table
  with general summary statistics such as the length of x, the number of unique values,
  the number of missing values. If x is categoric then a frequency table is also
  returned. Different plots are also generated to see. See the description of the
  differents functions for more information.
}
