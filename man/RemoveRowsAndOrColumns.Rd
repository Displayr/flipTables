% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/removerowsandorcolumns.R
\name{RemoveRowsAndOrColumns}
\alias{RemoveRowsAndOrColumns}
\title{Remove Rows and Columns From Tabular Data By Names}
\usage{
RemoveRowsAndOrColumns(
  x,
  row.names.to.remove = c("NET", "Total", "SUM"),
  column.names.to.remove = c("NET", "Total", "SUM"),
  split = "[;,]"
)
}
\arguments{
\item{x}{Data to remove rows and columns from; can be a matrix or data.frame or
a list of matrices and data.frames.}

\item{row.names.to.remove}{A vector or comma-separated string containing the
row labels to remove.}

\item{column.names.to.remove}{A vector or comma-separated string containing the
column labels to remove.}

\item{split}{Character delimiter to split \code{row.names.to.remove}
and \code{col.names.to.remove} on. Default is to split on either of
\code{","} or \code{";"}. Assumed to be a regular expression; see \code{\link{strsplit}}.}
}
\value{
\code{x} with the rows specified in \code{row.names.to.remove} and
columns specified in \code{column.names.to.remove} removed.
}
\description{
Given tabular data and character vectors or delimited strings of row and column
names, produces a table with the specified rows and columns removed. Where a
list is supplied, \code{column.names.to.remove} are applied to the list.
}
\details{
Trailing spaces are removed and lower/upper case is ignored.
}
