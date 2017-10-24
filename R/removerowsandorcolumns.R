
#' Remove Rows and Columns From Tabular Data By Names
#'
#' Given tabular data and character vectors or delimited strings of row and column
#' names, produces a table with the specified rows and columns removed.
#' @param x Data to remove rows and columns from; can be a matrix or data.frame or
#' a list of matrices and data.frames.
#' @param row.names.to.remove A vector or comma-separated string containing the
#' row labels to remove.
#' @param column.names.to.remove A vector or comma-separated string containing the
#' column labels to remove.
#' @param split Character delimiter to split \code{row.names.to.remove}
#' and \code{col.names.to.remove} on. Default is to split on either of
#' \code{","} or \code{";"}. Assumed to be a regular expression; see \code{\link{strsplit}}.
#' @details Trailing spaces are removed and lower/upper case is ignored.
#' @return \code{x} with the rows specified in \code{row.names.to.remove} and
#' columns specified in \code{column.names.to.remove} removed.
#' @importFrom flipFormat ConvertCommaSeparatedStringToVector
#' @importFrom flipU CopyAttributes
#' @export
RemoveRowsAndOrColumns <- function(x,
                                   row.names.to.remove = c("NET", "Total", "SUM"),
                                   column.names.to.remove = c("NET", "Total", "SUM"),
                                   split = "[;,]")
{
    if (is.null(row.names.to.remove) && is.null(column.names.to.remove))
        return(x)

    if(inherits(x, "list"))
        return(lapply(x, RemoveRowsAndOrColumns, row.names.to.remove = row.names.to.remove,
                      column.names.to.remove = column.names.to.remove, split = split))

    if (is.null(dim(x)))
        return(RemoveByName(x, list(row.names.to.remove,
                                    column.names.to.remove), sep = split))

    ind <- RetainedRowsAndOrColumns(x = x, row.names.to.remove = row.names.to.remove,
                                column.names.to.remove = column.names.to.remove,
                                split = split)

    if (length(ind[[1]]) == 0 || length(ind[[2]]) == 0)
        stop ("Removing rows/columns gives empty input matrix\n")

    CopyAttributes(x[ind[[1]], ind[[2]], drop = FALSE], x)
}

#' Determine indices to be kept in a table
#'
#' For given vectors or delimited strings of row and column names, deterimnes the
#' indices in tabular data that does \emph{not} match the specified names.
#' @param x The data that is being analyzed.
#' @param row.names.to.remove A character vector or delimited string containing the
#' row labels to remove.
#' @param column.names.to.remove A character vector or delimited string containing
#' the column labels to remove.
#' @param split Character delimiter to split \code{row.names.to.remove}
#' and \code{col.names.to.remove} on. Default is to split on either of
#' \code{","} or \code{";"}. Assumed to be a regular expression; see
#' \code{\link{strsplit}}.
#' @details Trailing spaces are removed and lower/upper case is ignored.
#' @return A list of 2 vectors, indices of rows and columns from a table that
#' do not match names in \code{row.names.to.remove} and
#' \code{column.names.to.remove}, respectively.
#' @export
RetainedRowsAndOrColumns <- function(x,
                                   row.names.to.remove = c("NET", "Total", "SUM"),
                                   column.names.to.remove = c("NET", "Total", "SUM"),
                                   split = "[;,]")
{
    ind <- list(retained.rows = 1:nrow(x), retained.cols = 1:ncol(x))
    for (i in 1:2)
    {
        tmpname <- switch(i, rownames(x), colnames(x))
        tmpstring <- switch(i, row.names.to.remove, column.names.to.remove)

        if (!is.null(tmpname) && !is.null(tmpstring))
        {
            tmpname <- tolower(trimws(tmpname))

            # No splitting if empty delimiter is given (vector is expected)
            if (split != "")
                tmpstring <- ConvertCommaSeparatedStringToVector(tmpstring, split)
            tmpstring <- tolower(tmpstring)
            ind[[i]] <- which(!tmpname %in% tmpstring)
        }
    }
    return(ind)
}
