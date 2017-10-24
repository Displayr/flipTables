#' rbindAndCbindWithLabels
#'
#' Takes a sequence of vectors or tables and merges them, similar to rbind and cbind, except that it matches
#' based on names. If there are no names, matching is based upon indices.
#' @param ... The tables or vectors to be merged.
#' @param rows If TRUE, binds by rows. Otherwise by columns.
#' @param keep.all If \code{"TRUE"}, even non-matching rows/columns are retained. Ignored and treated as
#' \code{"TRUE"} if matching is based upon indices.
rbindAndCbindWithLabels <- function(..., rows, keep.all)
{
    # rbind does not work on matrices with different ncols. However we use rbind to extract rownames.
    # So temporarily set all ncols to 1 for matrices. Vica versa for cbind.
    tables <- list(...)
    tables <- lapply(tables, oneDimensionalArrayToVector)
    vectors <- unlist(lapply(tables, is.vector))
    if (rows)
        tables[!vectors] <- lapply(tables[!vectors], '[', , 1, drop = FALSE)
    else
        tables[!vectors] <- lapply(tables[!vectors], '[', 1, , drop = FALSE)
    bind <- if (rows) rbind else cbind
    # reset names of vectors which have been lost and are used as rownames.
    object.names <- as.character(substitute(list(...)))[-1L]
    named.objects <- sapply(object.names, exists)
    names(tables)[named.objects] <- object.names[named.objects]
    bind.tables <- suppressWarnings(do.call(bind, tables))

    tables <- list(...)   # reset to full size matrices
    tables <- lapply(tables, oneDimensionalArrayToVector)
    tables <- tables[!sapply(tables, is.null)]

    if(is.list(tables[[1]]) && !is.data.frame(tables[[1]]))
        return(rbindAndCbindWithLabels(tables[[1]], rows = rows, keep.all = keep.all))

    merged <- MergeTables(tables, direction = if (rows) "Up-and-down" else "Side-by-side", nonmatching = if (keep.all) "Keep all" else "Matching only")

    if (rows)
    {
        rownames(merged) <- rownames(bind.tables)
    }
    else
    {
        colnames(merged) <- colnames(bind.tables)
    }
    merged
}



#' Rbind
#'
#' Takes a sequence of vectors or tables and merges them, similar to \code{\link{rbind}}, except that it matches
#' based on names. If there are no names, matching is based upon indices.
#' @param ... The tables or vectors to be merged.
#' @param keep.all If \code{"TRUE"}, even non-matching rows are retained. Ignored and treated as
#' \code{"TRUE"} if matching is based upon indices.
#' @export
Rbind <- function(..., keep.all = TRUE)
{
    rbindAndCbindWithLabels(..., rows = TRUE, keep.all = keep.all)
}


#' Cbind
#'
#' Takes a sequence of vectors or tables and merges them, similar to \code{\link{cbind}}, except that it matches
#' based on names. If there are no names, matching is based upon indices.
#' @param ... The tables or vectors to be merged.
#' @param keep.all If \code{"TRUE"}, even non-matching rows are retained. Ignored and treated as
#' \code{"TRUE"} if matching is based upon indices.
#' @export
Cbind <- function(..., keep.all = TRUE)
{
    rbindAndCbindWithLabels(..., rows = FALSE, keep.all = keep.all)
}

# Converts array with 1 dimension to an equivalent vector, preserving names.
# Else returns object unchanged
#' @importFrom methods is
oneDimensionalArrayToVector <- function(x) {
    if (!is(x, "array") || length(dim(x)) != 1)
        return(x)
    array.names <- names(x)
    x <- as.vector(x)
    names(x) <- array.names
    return(x)
}
