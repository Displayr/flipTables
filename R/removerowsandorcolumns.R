
#' Remove Rows and Columns From Tabular Data By Names
#'
#' Given tabular data and character vectors or delimited strings of row and column
#' names, produces a table with the specified rows and columns removed. Where a
#' list is supplied, \code{column.names.to.remove} are applied to the list.
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
#' @importFrom flipU CopyAttributes
#' @importFrom flipU RemoveAt
#' @export
RemoveRowsAndOrColumns <- function(x,
                                   row.names.to.remove = c("NET", "Total", "SUM"),
                                   column.names.to.remove = c("NET", "Total", "SUM"),
                                   split = "[;,]")
{
    ## If nothing is to be removed, return the original object.
    if (is.null(row.names.to.remove) && is.null(column.names.to.remove))
        return(x)

    if (inherits(x, "list"))
    {
        # Removing the elements from the list.
        if (!is.null(nm <- names(x)))
            x <- RemoveAt(x, at = column.names.to.remove, ignore.case = TRUE, split = split)

        # Applying remove to the elements int the list.
        return(lapply(x, RemoveRowsAndOrColumns, row.names.to.remove = row.names.to.remove,
                      column.names.to.remove = column.names.to.remove, split = split))
    }

    # Vectors and 1D arrays
    if (is.null(dim(x)) || is.array(x) && length(dim(x)) == 1)
        return(RemoveAt(x, row.names.to.remove, split = split))

    out <- RemoveAt(x, list(row.names.to.remove, column.names.to.remove), 1:2, TRUE, split)
    if (length(out) == 0 || prod(dim(out)) == 0)
    {
        if (dim(out)[1] == 0)
            stop("Removing row '", paste(rownames(x), collapse = "', '"), "' gives empty input matrix\n")
        if (dim(out)[2] == 0)
            stop("Removing column '", paste(colnames(x), collapse = "', '"), "' gives empty input matrix\n")
    }
    if (!inherits(x, "QTable"))
        out <- CopyAttributes(out, x)
    out
}
