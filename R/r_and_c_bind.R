#' rbindAndCbindWithLabels
#'
#' Takes a sequence of vectors or tables and merges them, similar to rbind and cbind, except that it matches
#' based on names.
#' @param ... The tables or vectors to be merged.
#' @param rows If TRUE, binds by rows. Otherwise by columns.
#' @param keep.all If TRUE, even non-matching rows are retained.
rbindAndCbindWithLabels <- function(..., rows, keep.all)
{
    bind <- if (rows) rbind else cbind
    bind.tables <- suppressWarnings(bind(...))
    tables <- list(...)
    if(is.list(tables[[1]]))
        return(rbindAndCbinWithLabels(tables[[1]]))
    if (is.null(tables[[1]]))
    {
        tables[[1]] <- NULL
        return(bind.tables)
    }
    if (!(any(c("names", "dimnames") %in% names(attributes(tables[[1]])))))
    {
        warning("As the first table contains no names, names have been ignored in the matching.")
        return(bind(...))
    }
    merged <- MergeTables(tables, direction = if (rows) "Up-and-down" else "Side-by-side", nonmatching = if (keep.all) "Keep all" else "Matching only")
    if (rows)
    {
        if(is.vector(merged) || nrow(merged) == 1)
            return(merged)
        rownames(merged) <- rownames(bind.tables)

    }
    else
    {
        if(is.vector(merged) || ncol(merged) == 1)
            return(merged)
        colnames(merged) <- colnames(bind.tables)
    }
    merged
}



#' RBind
#'
#' Takes a sequence of vectors or tables and merges them, similar to \code{\link{rbind}}, except that it matches
#' based on names.
#' @param ... The tables or vectors to be merged.
#' @param keep.all If TRUE, even non-matching rows are retained.
#' @export
Rbind <- function(..., keep.all = TRUE)
{
    rbindAndCbindWithLabels(..., rows = TRUE, keep.all = keep.all)
}


#' Cbind
#'
#' Takes a sequence of vectors or tables and merges them, similar to \code{\link{rbind}}, except that it matches
#' based on names.
#' @param ... The tables or vectors to be merged.
#' @param keep.all If TRUE, even non-matching rows are retained.
#' @export
Cbind <- function(..., keep.all = TRUE)
{
    rbindAndCbindWithLabels(..., rows = FALSE, keep.all = keep.all)
}
