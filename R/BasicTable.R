#' Create a BasicTable Object
#'
#' Creates tables used by Displayr charting functions
#' @param x a \strong{named} numeric matrix or vector
#' @param by vector over which y will be aggregated. Must have the
#'     same number of elements as y; see
#'     \code{\link[stats]{aggregate}}
#' @param date optional vector of dates, which if supplied, will be
#'     used for the row names of the returned table; must be unique
#'     and have the same length or number of rows of \code{x}; will
#'     overwrite any existing row names present in \code{x}
#' @param as.binary logical; if \code{TRUE}, unordered factors in
#'     \code{x} are represented as dummy variables; otherwise, they
#'     are represented as sequential integers
#' @param row.names.to.remove character vector of row labels
#'     specifying rows to remove from the returned table
#' @param col.names.to.remove character vector of column labels
#'     specifying columns to remove from the returned table
#' @param transpose logical; if \code{TRUE} the table will be
#'     transposed before being returned
#' @details If \code{x} is not a numeric vector or matrix, an attempt
#'     will be made to coerce it to one using \code{\link{AsBasicTable}}.
#' @importFrom flipTransformations RemoveRowsAndOrColumns
#' @seealso \code{\link{AsBasicTable}}
#' @return An object of class \code{BasicTable} - a \strong{named}
#'     matrix or vector
#' @export
BasicTable <- function(x, by = NULL, date = NULL,
                       as.binary = FALSE, row.names.to.remove = NULL,
                       col.names.to.remove = NULL,
                       transpose = FALSE)
{
    ## if not given a numeric vector or matrix, try to coerce to one
    if (!is.numeric(x) || is.null(dims <- dim(x)) || length(dims) > 2 || IsQTable(x))
        x <- AsBasicTable(x, as.binary = as.binary)

    ## Handle by arg

    x <- setDimNames(x, date)
    x <- RemoveRowsAndOrColumns(x, row.names.to.remove, col.names.to.remove)


    ## Handle transpose
    if (transpose)
        x <- t(x)

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
#' @importFrom stats setNames
setDimNames <- function(x, date = NULL){
    dims <- dim(x)
    if (is.null(dims) || length(dims) == 1L)
    {  # 2nd condition needed for 1D array case
        if (is.null(names(x)))
            names(x) <- seq_along(x)
        return(x)
    }
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

