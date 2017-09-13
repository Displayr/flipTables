#' Create a BasicTable Object
#'
#' Creates tables used by Displayr charting functions
#' @param x \strong{named} matrix or vector
#' @param by vector over which y will be aggregated. Must have the same
#' number of elements as y; see \code{\link[stats]{aggregate}}
#' @param date optional vector of dates, which if supplied, will be
#'     used for the row names of the returned table; must be unique
#'     and have the same length or number of rows of \code{x}; will
#'     overwrite any existing row names present in \code{x}
#' @param row.names.to.remove character vector of row labels
#'     specifying rows to remove from the returned table
#' @param col.names.to.remove character vector of column labels
#'     specifying columns to remove from the returned table
#' @param logical; if \code{TRUE} the table will be transposed before
#'     being returned00
#' @importFrom flipTransformations ParseEnteredData
#' @return An object of class \code{BasicTable} - a named matrix or vector
BasicTable <- function(x, by = NULL, date = NULL,
                       row.names.to.remove = NULL,
                       col.names.to.remove = NULL,
                       transpose = FALSE)
{
    if (is.character(x))  # user entered raw data
        x <- flipTransformations::ParseEnteredData(x, want.data.frame = FALSE, want.col.names = TRUE,
                                                   want.row.names = TRUE)
    if (!is.null(date))
        x <- tidyDataForStacking(x, date)

    dim.x <- dim(x)
    dim.names <- dimnames(x)
    if (length(dim.x) != 2)
    {
        if (length(dim.x) == 3 & !is.null(dim.names))
        {
            x <- x[, , 1]
            warning(gettextf("The analysis has been performed on the first statistic in the table (%s).",
                           dim.names[[3]][1]))
            if (is.character(x[1,1]))
                x <- matrix(as.numeric(x), nrow(x), dimnames = dimnames(x))
        }
        else
        {
            stop("This analysis requires a two-dimensional table (i.e., a table with one ",
                 "set of row headings, one set of columns headings, and one statistic in each cell.")
        }
    }
    x <- SetDimNames(x)
    if (is.null(dim.names))
    {
        dimnames(x) <- list(Rows = 1:nrow(x), Columns = 1:ncol(x))
    }
    else
    {
        x <- RemoveRowsAndOrColumns(x, row.names.to.remove, col.names.to.remove)
    }
    class(x) <- c("BasicTable", if (is.null(dim(x))) "numeric" else "matrix")
    x
}

#' Create Dimnames for a BasicTable
#'
#' Adds names to a vector or row and column names to a matrix
#' @param x numeric vector or matrix
#' @return x with updated names
#' @noRd
#' @keywords internal
SetDimNames <- function(x, date = NULL){
    dims <- dim(x)
    if (is.null(dims) && is.null(names(x)))
        return(setNames(x, seq_along(x)))

    dim.names <- dimnames(x)
    rnames <- if (!is.null(dim.names[[1L]]))
                  dim.names[[1L]]
              else
                  paste0("Row ", seq_len(dims[1L]))
    cnames <- if (!is.null(dim.names[[2L]]))
                  dim.names[[2L]]
              else
                  paste0("Col ", seq_len(dims[2L]))
    structure(x, dimnames = list(rnames, cnames))
}

