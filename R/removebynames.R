#' Remove indices from a vector by name
#'
#' Remove elements from a vector while preserving attributes
#' @param x Named vector, data.frame, or list to remove entries from,
#' or a list of these; see the details.
#' @param rnames Either 1) a character, vector giving entry names to remove; 2) a single
#' string containing comma or semi-colon separated names to remove, 3) a list where each
#' element is either 1) or 2).
#' @return \code{x} with entries specified in \code{rnames} removed
#' they will be combined using \code{\link{union}}.
#' @note An error is thrown if removal would result in an empty vector.
#' @details In the case when \code{x} inherits from class \code{"list"}, it is first
#' checked whether any of \code{rnames} appears in the names of the elements of
#' \code{x}; if they do, then elements will be removed from each element of \code{x}.
#' If the the check finds no matches in any element, then an attempt is made to remove
#' entire elements of \code{x} whose names appear in \code{rnames}.
#' @importFrom flipU CopyAttributes
#' @examples
#' x <- c(a = 1, b = 2, c = 3, d = 4, e = 5)
#' RemoveByName(x, "a")
#' RemoveByName(x, "a; b")
#' RemoveByName(x, list(c("a", "b"), " c,  d"))
#' @export
RemoveByName <- function(x, rnames)
{
    if (!length(rnames) || !nzchar(rnames))
        return(x)

    rnames <- if(is.list(rnames))
                  unique(unlist(lapply(rnames, sepNames)))  # Reduce("union", lapply(rnames, sepNames))
              else
                  sepNames(rnames)

    ## deal with possible comma or semi-colon separated names
    rnames <- sepNames(rnames)

    if (inherits(x, "list"))
    {  ## try to guess if user wants names removed from
       ## each element of the list or if they want elements
        ## removed from the list
        checkNames <- function(x)
            any(names(x) %in% rnames)

        if (any(vapply(x, checkNames, FALSE)))
            return(lapply(x, RemoveByName, rnames))
        ## else elements will be removed from x below
    }

    ## need to check this after handling list x case
    xnames <- names(x)
    if (is.null(xnames))
        return(x)

    if (all(xnames %in% rnames))
        stop("Removing entries gives empty vector")

    CopyAttributes(x[setdiff(xnames, rnames)], x)
}

#' @noRd
sepNames <- function(rnames)
{
    sep <- if (length(rnames) == 1)
               gsub("[^;,]", "", rnames)
    if (!is.null(sep) && nzchar(sep))
        rnames <- trimws(strsplit(rnames, sep)[[1L]])
    rnames
}
