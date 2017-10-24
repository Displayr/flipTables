#' Create a Tidy Table or Vector
#'
#' Creates tables that are ensured to be in a tidy format.  Output is
#' guaranteed to always have row and column names and there is special
#' handling of arrays and tables from \code{Q}.
#' @param x Numeric matrix or vector; should be named, but if names
#' are not present they will be created in the output
#' @param date Optional vector of dates, which if supplied, will be
#'     used for the row names of the returned table; must be unique
#'     and have the same length or number of rows of \code{x}; will
#'     overwrite any existing row names present in \code{x}; the
#'     default, \code{FALSE}, means that no dates are present in the
#'     data; a value of \code{NULL} indicates that dates are contained
#'     in x.
#' @param row.names.to.remove Character vector or delimited string
#' of row labels specifying rows to remove from the returned table.
#' @param col.names.to.remove Character vector or delimited string of column
#' labels specifying columns to remove from the returned table
#' @param transpose logical; if \code{TRUE} the table will be
#'     transposed before being returned
#' @param split Character delimiter to split \code{row.names.to.remove}
#' and \code{col.names.to.remove} on. Default is to split on either of
#' \code{","} or \code{";"}.  Assumed to be a regular expression; see
#' \code{\link{strsplit}}.
#' @details If \code{x} is not a numeric vector or matrix, an attempt
#'     will be made to coerce it to one using
#'     \code{\link{AsTidyTabularData}}.
#'
#' If a named vector is created from \code{x}, then
#' \code{row.names.to.remove} and \code{col.names.to.remove} will be
#' combined (using \code{\link[base]{union}}) to determine entries to remove.
#' @seealso \code{\link{AsTidyTabularData}}
#' @note If \code{transpose == TRUE}, then the table is transposed
#' \emph{before} rows and columns are removed, so
#' \code{row.names.to.remove} should be specified according to the
#' rownames of the \emph{transposed} table, as should
#' \code{col.names.to.remove}
#' @return A \strong{named} matrix or vector; a tidy version of \code{x}.
#' @export
TidyTabularData <- function(x, date = FALSE,
                       row.names.to.remove = NULL,
                       col.names.to.remove = NULL,
                       transpose = FALSE, split = "[;,]")
{
    if (!isFALSE(date))
        x <- processDates(x, date)

    ## if not given a numeric vector or matrix or dates, try to coerce to one
    if (!is.numeric(x) || !(is.null(dim(x)) || length(dim(x)) == 2L) || isQTable(x))
        x <- AsTidyTabularData(x)

    x <- setDimNames(x)
    ## Handle transpose
    ## transpose before removal of rows and columns
    ##   for flipDimensionReduction::CorrespondenceAnalysis !!
    if (transpose)
        x <- t(x)

    x <- RemoveRowsAndOrColumns(x, row.names.to.remove, col.names.to.remove,
                                split)

    class(x) <- if (is.null(dim(x)) || length(dim(x)) == 1L)
                    "numeric"
                else "matrix"
    x
}

#' Check if an object is FALSE
#'
#' Similar to \code{identical(x, FALSE)} except in a few cases
#' @param x any object
#' @note Taken from \code{base::isFALSE} which is only available for
#' R >= 3.5.0
#' @noRd
isFALSE <- function(x)
              is.logical(x) && length(x) == 1L && !is.na(x) && !x

#' Create Dimnames for a BasicTable
#'
#' Adds names to a vector or row and column names to a matrix
#' @param x numeric vector or matrix
#' @return x with updated names
#' @noRd
#' @keywords internal
#' @importFrom stats setNames
setDimNames <- function(x)
{
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
    ## ensure dimname names are preserved
    ## structure(x, dimnames = a_list)  fails for data.frames
    dimnames(x) <- setNames(list(rnames, cnames), names(dim.names))
    x
}

#' Add dates to a BasicTable
#'
#' Processes user-supplied dates and adds them to the names
#' @param x list, vector, array, matrix, or table, possibly containing
#'     dates
#' @param date vector of dates; a value of \code{NULL} (the default)
#'     indicates that the dates are contained in \code{x}
#' @return a numeric vector with dates for names
#' @noRd
processDates <- function(x, date = NULL)
{
    if(is.null(date))
    {
        if(is.list(x))
        {
            if (length(x) == 1)
            {
                date <- rownames(x)
                x <- x[[1]]
            } else
            {
                date <- x[[1]]
                x <- x[[2]]
            }
        }
        else if (is.vector(x))
        {
            date <- names(x)
        }
        else if (is.array(x) & length(dim(x)) == 1)
        {
            date <- names(x)
        }
        else
        {
            if(is.table(x) | is.matrix(x) | is.array(x))
            {
                if (length(dim(x)) == 2)
                {
                    if(nrow(x) > ncol(x))
                        x <- t(x)
                    if (nrow(x) == 1)
                    {
                        date <- colnames(x)
                        x <- as.vector(x)
                    }
                    else
                    {
                        date <- x[1, ]
                        x <- x[2, ]
                    }
                }
            }
            else
            {
                stop("Input data is in the wrong format.")
            }
        }
    }
    if (anyDuplicated(date))
        stop("Duplicate dates. Dates should be unique.")
   x <- as.numeric(x)
   names(x) <- date
   x
}


