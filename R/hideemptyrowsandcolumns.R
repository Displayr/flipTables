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
        return(CopyAttributes(x[idx, drop  = FALSE], x))
    }
    idx <- GetNonEmptyRowsAndColumns(x, FALSE, is.percent = is.percent)
    if (!length(idx[[1L]]) || !length(idx[[2L]]))
        stop ("Hiding empty rows/columns gives empty input matrix.")
    CopyAttributes(x[idx[[1L]], idx[[2L]], drop = FALSE], x)
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
        out <- list(which(apply(x, 1, function(x) all(nzchar(x)))),
                    which(apply(x, 2, function(x) all(nzchar(x)))))
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