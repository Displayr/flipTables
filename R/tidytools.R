#' \code{SelectRows}
#' @description Select rows from a matrix or dataframe.
#' @param x Matrix or dataframe from which rows will be extracted
#' @param select A string containing a comma seperated list of the
#'  names or indices of the rows to be selected.
#' @importFrom flipTransformations TextAsVector
#' @export
SelectRows <- function(x, select = "")
{
    if (!checkIsTable(x))
        return(x)
    if (sum(nchar(select), na.rm = TRUE) == 0)
        return(x)

    s.vec <- TextAsVector(select)
    s.ind <- matchNameOrIndex(s.vec, rownames(x, do.NULL = FALSE, prefix = ""))
    s.na <- which(is.na(s.ind))
    if (length(s.na) > 0)
        warning("Table does not contain rows '", 
             paste(s.vec[s.na], collapse = "','"), "'.")
   
    return(x[s.ind[which(!is.na(s.ind))],])
}

#' \code{SelectColumns}
#' @description Select rows from a matrix or dataframe.
#' @param x Matrix or dataframe from which columns will be extracted
#' @param select A string containing a comma seperated list of the
#'  names or indices of the columns to be selected.
#' @importFrom flipTransformations TextAsVector
#' @export
SelectColumns <- function(x, select = "")
{
    if (!checkIsTable(x))
        return(x)
    if (sum(nchar(select), na.rm = TRUE) == 0)
        return(x)

    s.vec <- TextAsVector(select)
    s.ind <- matchNameOrIndex(s.vec, colnames(x, do.NULL = FALSE, prefix = ""))
    s.na <- which(is.na(s.ind))
    if (length(s.na) > 0)
        warning("Table does not contain rows '", 
             paste(s.vec[s.na], collapse = "','"), "'.")
   
    return(x[,s.ind[which(!is.na(s.ind))]])
}

#' @param p.list Vector of patterns to match
#' @param x Vector of names
#' @return list of indices mapping p.list to x.
#'      Unmatched entries in p.list are set to NA
#' @noRd
matchNameOrIndex <- function(p.list, x)
{
    # Looking for exact string-to-string match
    ind <- match(p.list, x)
    
    # Try to convert unmatched entries to index numbers
    ind.na <- which(is.na(ind))
    if (length(ind.na))
        ind[ind.na] <- suppressWarnings(as.numeric(p.list[ind.na]))
    ind[ind < 1 | ind > length(x)] <- NA

    return(ind)
}
 

checkIsTable <- function(x)
{
    res <- length(dim(x)) == 2
    if (!res)
        warning("Input data should be a 2-dimensional matrix or dataframe.")
    return(res)
}

#' \code{SortRows}
#' @description Sorts the rows of the table based on the values in the specified column
#' @details This function differs from the QScript in a number of ways.
#' 1) Sorting does not respect spans.
#' 2) The default column in based on 'Column n' in 'Statistics - Below'. This is
#'    not available to R. Instead we pick the column with the largest sum. If there
#'    is a tie between columns, we will pick the rightmost column
#' 3) There is no parameter for 'Never exclude from sort'. This is because we require
#'    that entries in 'Exclude from sort' must match the rowname exactly, instead of
#'    a partial match used in the QScript
#' @param x Input matrix or dataframe which is being sorted. Values must be numeric.
#' @param decreasing Order to sort values.
#' @param column The column to 
#' @param rows.to.exclude A string containing a comma-separated list of rows
#'    (either by name or index) which should not be sorted. These rows
#'    will remain at the bottom of the output table
#' @export
SortRows <- function(x,
                 decreasing = FALSE,
                 column = NULL, # integer, otherwise largest column
                 rows.to.exclude = "NET, Sum, Total")
{
    x <- as.matrix(x)
    if (!is.numeric(x))
        stop("Sorting cannot be applied to text tables.")

    if (sum(nchar(column), na.rm = TRUE) == 0)
    {
        tmp.sum <- colSums(x)
        col.ind <- (ncol(x) + 1) - which.max(rev(tmp.sum))

    } else
    {
        col.ind <- matchNameOrIndex(column[1], colnames(x, do.NULL = FALSE, prefix = ""))
        if (length(col.ind) == 0)
            stop("Column '", column, "' was not found in the table.")
        if (length(column) > 1)
            warning("Only column '", column[1], "' was used to sort the table.")
    }
    rows.excl <- matchNameOrIndex(TextAsVector(rows.to.exclude),
                                  rownames(x, do.NULL = FALSE, prefix = ""))
    rows.excl <- rows.excl[!is.na(rows.excl)]
    rows.incl <- setdiff(1:nrow(x), rows.excl)
    ord.ind <- order(x[rows.incl, col.ind], decreasing = decreasing)
    return(x[c(rows.incl[ord.ind], rows.excl),])
}

checkTableDim <- function(x, min.nrow = 0, min.ncol = 0)
{
    res <- TRUE
    res <- res && (nrow(x) > min.nrow)
    if (!res)
        warning("Input table must have at least ", min.nrow,  " rows.")
    res <- res && (ncol(x) > min.ncol)
    if (!res)
        warning("Input table must have at least ", min.ncol, " columns.")
    return(res)
}

#' LastKColumns
#' @description Extract last K columns from table
#' @param x Input matrix or dataframe
#' @param K Number of columns to retain.
#' @importFrom flipU CopyAttributes
#' @export
LastKColumns <- function(x, K)
{
    if (!checkIsTable(x))
        return(x)
    if (!checkTableDim(x, min.nrow = K))
        return(x)

    CopyAttributes(x[, ncol(x)-((K:1)-1)], x)  
}

LastKRows <- function(x, K)
{
    if (!checkIsTable(x))
        return(x)
    if (!checkTableDim(x, min.nrow = K))
        return(x)

    CopyAttributes(x[, nrow(x)-((K:1)-1)], x)  
}

#This involves NO sorting, just take the first K rows
#TopKAndSpecifiedRows <- function(x, K, select = NULL)

# Automatic ordering uses correspondence analysis ca::ca
