#' \code{SelectRows}
#' @description Select rows from a matrix or dataframe.
#' @param x Matrix or dataframe from which rows will be extracted
#' @param select A string containing a comma seperated list of the
#'  names or indices of the rows to be selected.
#' @importFrom flipTransformations TextAsVector
#' @export
SelectRows <- function(x, select = NULL)
{
    if (!checkIsTable(x))
        return(x)
    if (sum(nchar(select), na.rm = TRUE) == 0)
        return(x)

    sel.ind <- getMatchIndex(select, rownames(x, do.NULL = FALSE, prefix = ""))
    if (isTableWithStats(x))
        res <- x[sel.ind, , ]
    else
        res <- x[sel.ind, ]

    return(res)
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

    sel.ind <- getMatchIndex(select, colnames(x, do.NULL = FALSE, prefix = ""))
    if (isTableWithStats(x))
        res <- x[, sel.ind, ]
    else
        res <- x[,sel.ind]

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
    if (!checkIsTable(x))
        return(x)
    if ((isTableWithStats(x) && !is.numeric(x[,,1])) || !is.numeric(x))
        stop("Sorting cannot be applied to non-numeric tables.")

    # Finding the column to sort on
    if (!is.null(column))
    {
        col.ind <- matchNameOrIndex(column[1], colnames(x, do.NULL = FALSE, prefix = ""))
        if (length(col.ind) == 0)
            stop("Column '", column, "' was not found in the table.")
        if (length(column) > 1)
            warning("Only column '", column[1], "' was used to sort the table.")
    
    } else if (isTableWithStats(x) && "Column n" %in% dimnames(x)[[3]])
    {
        d.ind <- which(dimnames(x)[[3]] == "Column n")
        col.ind <- which.max(x[1, ,d.ind])

    } else
    {
        col.ind <- NCOL(x)
    }

    rows.excl <- matchNameOrIndex(TextAsVector(rows.to.exclude),
                                  rownames(x, do.NULL = FALSE, prefix = ""))
    rows.excl <- rows.excl[!is.na(rows.excl)]
    rows.incl <- setdiff(1:nrow(x), rows.excl)
    ord.ind <- if (isTableWithStats(x)) order(x[rows.incl, col.ind, 1], decreasing = decreasing)
               else order(x[rows.incl, col.ind], decreasing = decreasing)
    ind <- c(rows.incl[ord.ind], rows.excl)

    if (isTableWithStats(x))
        return(x[ind,,])
    else
        return(x[ind,])
}

# This is a wrapper for matchNameOrIndex
# It will break up the pattern from a commma-separated list to a vector
# It will also check for unmatched entries and give warnings
getMatchIndex <- function(pattern, x, type = "rows")
{
    sel.vec <- TextAsVector(pattern)
    sel.ind <- matchNameOrIndex(sel.vec, x)
    sel.na <- which(is.na(sel.ind))
    if (length(sel.na) > 0)
        warning("Table does not contain ", type, " '",
            paste(sel.vec[sel.na], collapse = "','"), "'.")
    return(sel.ind[which(!is.na(sel.ind))])
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
 
isTableWithStats <- function(x)
{
    if (class(x) == "array" && length(dim(x)) == 3)
        return(TRUE)
    return(FALSE)
}

checkIsTable <- function(x)
{
    if (isTableWithStats(x))
        return(TRUE)

    res <- length(dim(x)) == 2
    if (!res)
        warning("Input data should be a 2-dimensional matrix or dataframe.")
    return(res)
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

#' HideOutputsWithSmallSampleSizes
#' @description Throws an error if any of the 'Base n' values in the table are too small
#' @param x Input table, which must contain the 'Base n' statistic.
#' @param min.size Minimum sample size required.
HideOutputsWithSmallSampleSizes <- function(x, min.size = 30)
{
    if (!isTableWithStats(x) && "Base n" %in% dimnames(x)[[3]])
        stop("Table does not have 'Base n'")

    d.ind <- which(dimnames(x)[[3]] == "Base n")
    if (any(x[,,d.ind] < min.size, na.rm = TRUE))
        stop("Output not shown because it is based on less than ", min.size, " observations.")
    else
        return(x)
}

HideRowsAndColumnsWithSmallSampleSizes <- function(x, min.size = 30)
{
    if (!isTableWithStats(x) && ("Column n" %in% dimnames(x)[[3]] || "Base n" %in% dimnames(x)[[3]]))
        stop("Table does not have 'Column n' or 'Base n'")
    
    d.ind <- which(dimnames(x)[[3]] == "Column n")
    if (length(d.ind) == 0)
        d.ind <- which(dimnames(x)[[3]] == "Base n")

    # Search rows and columns
    col.rm <- c()
    row.rm <- c()
    for (i in 1:ncol(x))
    {
        if (all(x[,i,d.ind] < min.size))
            col.rm <- c(col.rm, i)
    }
    for (i in 1:nrow(x))
    {
        if (all(x[i, ,d.ind] < min.size))
            row.rm <- c(row.rm, i)
    }

    if (length(col.rm) > 0)
    {
        warning("Columns ", paste(col.rm, collapse=","), " have sample size less than ", min.size, " and have been removed.")
        x <- x[,-col.rm,]
    }
    if (all(dim(x) > 0) && length(row.rm) > 0)
    {
        warning("Rows ", paste(row.rm, collapse=","), " have sample size less than ", min.size, " and have been removed.")
        x <- x[-row.rm, ,]
    }
    x
}


#' \code{TopKAndSpecifiedRows}
#' @description Returns the first K rows (no sorting), and additional rows listed
#' @param x Input table
#' @param K Number of rows from the top of the table to return.
#' @param select Optional string containing comma separated list of names 
#'    or indices of the rows to be selected
TopKAndSpecifiedRows <- function(x, K, select = NULL)
{
    if (!checkIsTable(x))
        return(x)
    
    sel.ind <- getMatchIndex(select, rownames(x, do.NULL = FALSE, prefix = ""))
    sel.ind <- c(1:K, setdiff(sel.ind, 1:K))
    
    if (isTableWithStats(x))
        return(x[sel.ind, , ])
    else
        return(x[sel.ind,])
}
    

# Automatic ordering uses correspondence analysis ca::ca
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

    sel.ind <- ncol(x) - ((K:1)-1)
    if (isTableWithStats(x))
        return(CopyAttributes(x[,sel.ind,], x))
    else
        return(CopyAttributes(x[,sel.ind], x))
}

# Automatic ordering uses correspondence analysis ca::ca
#' LastKRows
#' @description Extract last K rows from table
#' @param x Input matrix or dataframe
#' @param K Number of rows to retain.
#' @importFrom flipU CopyAttributes
#' @export
LastKRows <- function(x, K)
{
    if (!checkIsTable(x))
        return(x)
    if (!checkTableDim(x, min.nrow = K))
        return(x)
   
    sel.ind <- nrow(x)-((K:1)-1)
    if (isTableWithStats(x))
        return(CopyAttributes(x[sel.ind, ,], x))
    else
        return(CopyAttributes(x[sel.ind,], x)) 
}


