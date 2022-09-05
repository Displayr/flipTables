#' Hide Empty Entries From a Table Or Vector
#'
#' Searches for empty rows or columns in a table and returns a copy
#' with those entries removed
#' @param x matrix; vector; or list
#' @param is.percent logical; indicating whether the supplied data are
#'     percentages; specifying a value of \code{NULL} (the default)
#'     means that the function will check for an attribute of \code{x}
#'     called "statistic" to check for percentages
#' @return \code{x} with any empty rows or columns removed; or in the
#'     case \code{x} is vector, \code{x} with any empty entries
#'     removed
#' @importFrom flipU CopyAttributes
#' @seealso \code{GetNonEmptyRowsAndColumns}
#' @examples
#' x <- matrix(nrow = 2, ncol = 2)
#' x[1, ] <- 1
#' HideEmptyRowsAndColumns(x)
#' x[2, ] <- 0
#' HideEmptyRowsAndColumns(x, is.percent = TRUE)
#' @export
HideEmptyRowsAndColumns <- function(x, is.percent = NULL)
{
    if (is.null(dim(x)) || is.array(x) && length(dim(x)) == 1)
    {
        idx <- GetNonEmptyElements(x, FALSE, is.percent)
        if (!length(idx))
            stop("Hiding empty elements gives empty input vector.")
        y <- x[idx, drop  = FALSE]
        # Subscripting QTables (verbs:::`[.QTable`) already updates attributes
        if (!inherits(x, "qTable"))
            y <- CopyAttributes(y, x)
        return(y)
    }
    idx <- GetNonEmptyRowsAndColumns(x, FALSE, is.percent = is.percent)
    if (!length(idx[[1L]]) || !length(idx[[2L]]))
        stop ("Hiding empty rows/columns gives empty input matrix.")
    extractArray(x, idx[[1L]], idx[[2L]])
}

#' Get Names of Empty Entries From a Table Or Vector
#'
#' Searches for empty rows or columns in a table and returns a copy
#' with those entries removed
#' @param x matrix; vector; or list
#' @param use.names logical; if \code{TRUE}
#' @param is.percent logical; indicating whether the supplied data are
#'     percentages; specifying a value of \code{NULL} (the default)
#'     means that the function will check for an attribute of \code{x}
#'     called "statistic" to check for percentages
#' @return Two-element list containing the non-empty indices
#' @export
GetNonEmptyRowsAndColumns <- function(x, use.names = TRUE, is.percent = NULL)
{
    if (is.character(x))
        out <- list(which(apply(x, 1, function(x) any(nzchar(x)))),
                    which(apply(x, 2, function(x) any(nzchar(x)))))
    else
    {
        if (is.null(is.percent))
            is.percent <- !is.null(attr(x, "statistic")) && grepl("%", attr(x, "statistic"))
        isNonEmptyVec <- function(x)
                                  !all(is.na(x) | (is.percent & x == 0))
        out <- list(which(apply(x, 1, isNonEmptyVec)),
                    which(apply(x, 2, isNonEmptyVec)))
    }
    if (use.names)
        return(list(names(out[[1L]]), names(out[[2L]])))
    else
        return(out)
}

#' Hide Empty Rows From a Table
#'
#' Searches for empty rows in a table and returns a copy
#' with those entries removed
#' @param x matrix; vector; or list
#' @param remove.zeros logical; indicating whether zeros are removed.
#'     If \code{FALSE}, then only \code{NAs} are removed.
#'     Specifying a value of \code{NULL}
#'     means that the function will check for an attribute of \code{x}
#'     called "statistic" to check for percentages
#' @param first.stat.only Only examines the first statistic when
#'     determining whether the row/column is empty.
#' @export
HideEmptyRows <- function(x, remove.zeros = TRUE, first.stat.only = TRUE)
{
    if (is.null(dim(x)) || is.array(x) && length(dim(x)) == 1)
    {
        idx <- GetNonEmptyElements(x, FALSE, remove.zeros)
        if (!length(idx))
            stop("Hiding empty elements gives empty input vector.")
        y <- x[idx, drop  = FALSE]
        # Subscripting QTables (verbs:::`[.QTable`) already updates attributes
        if (!inherits(x, "qTable"))
            y <- CopyAttributes(y, x)
        return(y)
    }
    idx <- getNonEmptyIndices(x, 1, FALSE, remove.zeros, first.stat.only)
    if (!length(idx))
        stop ("Hiding empty rows gives empty input matrix.")
    extractArray(x, row.index = idx)
}

#' Hide Empty Columns From a Table
#'
#' Searches for empty columns in a table and returns a copy
#' with those entries removed
#' @inherit HideEmptyRows
#' @export
HideEmptyColumns <- function(x, remove.zeros = TRUE, first.stat.only = TRUE)
{
    if (is.null(dim(x)) || is.array(x) && length(dim(x)) == 1)
    {
        idx <- GetNonEmptyElements(x, FALSE, remove.zeros)
        if (!length(idx))
            stop("Hiding empty elements gives empty input vector.")
        return(x)
    }
    idx <- getNonEmptyIndices(x, 2, FALSE, is.percent = remove.zeros, first.stat.only)
    if (!length(idx))
        stop ("Hiding empty columns gives empty input matrix.")
    extractArray(x, col.index = idx)
}

# Similar to GetNonEmptyRowsAndColumns but only looks in 1 direction
#' @importFrom flipU IsQTable
getNonEmptyIndices <- function(x, margin = 1, use.names = TRUE, is.percent = NULL, first.stat.only = TRUE)
{
    if (first.stat.only && IsQTable(x) && length(dim(x)) > 1)
       x <- GetFirstStat(x)
    if (is.character(x))
        out <- which(apply(x, margin, function(x) any(nzchar(x))))
    else
    {
        if (is.null(is.percent))
            is.percent <- !is.null(attr(x, "statistic")) && grepl("%", attr(x, "statistic"))
        isNonEmptyVec <- function(x)
                                  !all(is.na(x) | (is.percent & x == 0))
        out <- if (length(dim(x)) == 0)   which(apply(as.matrix(x), margin, isNonEmptyVec))
               else                       which(apply(x, margin, isNonEmptyVec))
    }
    if (use.names)
        return(names(out[[1L]]))
    else
        return(out)
}

#' Get Names of Empty Entries From a Table Or Vector
#'
#' Searches for empty rows or columns in a table and returns a copy
#' with those entries removed
#' @param x matrix; vector; or list
#' @param use.names logical; if \code{TRUE}, the nams of the non-empty
#'     entries are returned; otherwise, the numeric positions of the
#'     non-empty elements are returned
#' @param is.percent logical; indicating whether the supplied data are
#'     percentages; specifying a value of \code{NULL} (the default)
#'     means that the function will check for an attribute of \code{x}
#'     called "statistic" to check for percentages
#' @return a vector containing the non-empty indices
#' @export
GetNonEmptyElements <- function(x, use.names = TRUE, is.percent = NULL)
{
    if (is.character(x))
        idx <- which(nzchar(x))
    else
    {
        if (is.null(is.percent))
            is.percent <- !is.null(attr(x, "statistic")) && grepl("%", attr(x, "statistic"))
        idx <- if (inherits(x, "list"))
                      !vapply(x, is.null, FALSE)
                 else
                     which(!is.na(x)  & !(is.percent & x == 0))
    }
    if (use.names)
        return(names(idx))
    else
        return(idx)
}
