#' RBind
#'
#' Takes a sequence of vectors or tables and merges them, similar to rbind, except that it matches
#' based on names.
#' @param ... The tables or vectors to be merged.
#' @export
Rbind <- function(...)
{
    rbind.tables <- suppressWarnings(rbind(...))
    tables <- list(...)
    if(is.list(tables[[1]]))
        return(Rbind(tables[[1]]))
    if (is.null(tables[[1]]))
    {
        tables[[1]] <- NULL
        return(rbind.tables)
    }
    if (!(any(c("names", "dimnames") %in% names(attributes(tables[[1]])))))
    {
        warning("As the first table contains no names, names have been ignored in the matching.")
        return(rbind(...))
    }
    merged <- MergeTables(tables, direction = "Up-and-down", nonmatching = "Keep all")
    if(is.vector(merged) || nrow(merged) == 1)
        return(merged)
    rownames(merged) <- rownames(rbind.tables)
    merged
}
