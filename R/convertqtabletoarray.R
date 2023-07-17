#' Converts a Q Table into an array
#' @return The output is always a 3d array, so that 1-column vector
#' is converted to an array with 3 dimensions (but last two have
#' a length of 1). This makes it easy to identify the statistics.
#' @param x A Q table which is expected to have the attributes
#' typically associated with tables, e.g. "questions", "name"
#' and "statistic" (if there is only one statistic).
#' @export
ConvertQTableToArray <- function(x)
{
    # This is possibly valid output
    # e.g. crosstab: nominal multi grid x nominal has 4 dimensions
    # But what to do with it?
    if (length(dim(x)) >= 3)
        return(x)

    # 1-column table with multiple statistics in 2nd dimension
    if (length(dim(x)) == 2 && is.null(attr(x, "statistic")))
    {
        dn <- dimnames(x)
        dn <- c(dn[1], "", dn[2])
        x <- array(x, dim = sapply(dn, length), dimnames = dn)
        return(x)

    } else
    {
        dn <- dimnames(x)
        if (is.null(dimnames(x)))
        {
            dn <- list()
            dn[[1]] <- names(x)
        }
        if (length(dn) == 1)
            dn[[2]] <- ""
        dn[[3]] <- paste0("", attr(x, "statistic"))
        x <- array(x, dim = sapply(dn, length), dimnames = dn)
    }
    return(x)
}

#' @param x The object to be returned
#' @param y The object to copy attributes from, if a QTable, the attributes are not copied to x
#' @noRd
copyAttributesIfNotQTable <- function(x, y)
{
    if (inherits(y, "QTable")) return(x)
    CopyAttributes(x, y)
}
