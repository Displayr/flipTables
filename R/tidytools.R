# Function to extract rows/columns from array
# It handles both 2d matrices and 3d arrays
# Will always try to copy attributes
extractArray <- function(x, row.index = 1:nrow(x), col.index = 1:ncol(x))
{
    if (isTableWithStats(x))
        res <- x[row.index, col.index,, drop = FALSE]
    else
        res <- x[row.index, col.index, drop = FALSE]
    return(CopyAttributes(res, x))
}

# Converts vector or 1-d array into matrix
# so that the other functions for tidying tables can be used
# note that no checking is done
convertToMatrix <- function(x)
{
    res <- as.matrix(x)
    return(CopyAttributes(res, x))
}

#' Reverse rows of a table
#' @description Reverse order of rows in matrix, dataframe or array
#' @param x Input table which may be a matrix, dataframe or array.
#'    Vectors or 1-d arrays will be converted into a matrix with 1 column
#' @export
ReverseRows <- function(x)
{
    if (length(dim(x)) < 2)
        x <- convertToMatrix(x)
    n <- nrow(x)
    extractArray(x, row.index = n:1)
}

#' Reverse columns of a table
#' @description Reverse order of columns in matrix, dataframe or array
#' @param x Input table which may be a matrix, dataframe or array.
#'    Vectors or 1-d arrays will be ignored
#' @export
ReverseColumns <- function(x)
{
    if (length(dim(x)) < 2)
        x <- return(x)
    n <- ncol(x)
    extractArray(x, col.index = n:1)
}

#' Select rows from a table
#' @description Function to select row from a table, either by rownames
#'   or as range from the top or bottom.
#' @param x Matrix or dataframe from which rows will be extracted
#' @param select A string containing a comma seperated list of the
#'  names or indices of the rows to be selected.
#' @param first.k If a number greater than zero is supplied,
#'   The first \code{first.k} rows will also be selected
#' @param last.k If a number greater than zero is supplied,
#'   The last \code{last.k} rows will also be selected
#' @importFrom flipTransformations TextAsVector
#' @importFrom flipU CopyAttributes
#' @export
SelectRows <- function (x, select = NULL, first.k = NA, last.k = NA)
{
    if (sum(c(nchar(select), first.k, last.k), na.rm = TRUE) == 0)
        return(x)

    ind <- indexSelected(x, "row", select, first.k, last.k)
    extractArray(x, row.index = ind)
}

#' Select columns from a table
#' @description Function to select column from a table, either by colnames
#'   or as range from the top or bottom.
#' @param x Matrix or dataframe from which columns will be extracted
#' @param select A string containing a comma seperated list of the
#'  names or indices of the columns to be selected.
#' @param first.k If a number greater than zero is supplied,
#'   The first \code{first.k} columns will also be selected
#' @param last.k If a number greater than zero is supplied,
#'   The last \code{last.k} columns will also be selected
#' @export
SelectColumns <- function (x, select = NULL, first.k = NA, last.k = NA)
{
    if (sum(c(nchar(select), first.k, last.k), na.rm = TRUE) == 0)
        return(x)

    ind <- indexSelected(x, "column", select, first.k, last.k)
    extractArray(x, col.index = ind)
}

indexSelected <- function(x, dim = "row", select = NULL, first.k = NA, last.k = NA)
{
    if (length(dim(x)) < 2)
        x <- convertToMatrix(x)
    if (!checkIsTable(x))
        return(x)

    sel.ind <- NULL
    dim.names <- if (dim == "column") colnames(x, do.NULL = FALSE, prefix = "")
                 else                 rownames(x, do.NULL = FALSE, prefix = "")
    max.dim <- if (dim == "column") ncol(x)
               else                 nrow(x)
    min.dim <- max(0, first.k, last.k, na.rm = TRUE)
    if (max.dim < min.dim)
        stop("Input table has less than ", min.dim, " ", dim, "s.")

    if (sum(nchar(select), na.rm = TRUE) > 0)
        sel.ind <- getMatchIndex(select, dim.names)
    if (sum(first.k, na.rm = TRUE) > 0)
        sel.ind <- c(1:first.k, sel.ind)
    if (sum(last.k, na.rm = TRUE) > 0)
        sel.ind <- c(sel.ind, max.dim -((last.k:1)-1))
    return(sel.ind)
}

indexSortedByValues <- function(x,
                         values,
                         decreasing = FALSE,
                         exclude = "NET, SUM, Total",
                         dim = "row")
{
    max.dim <- if (dim == "column") ncol(x)
               else                 nrow(x)
    dim.names <- if (dim == "column") colnames(x, do.NULL = FALSE, prefix = "")
                 else                 rownames(x, do.NULL = FALSE, prefix = "")

    ind.excl <- getMatchIndex(exclude, dim.names, dim, warn = FALSE)
    ind.incl <- setdiff(1:max.dim, ind.excl)
    ord.ind <- order(values[ind.incl], decreasing = decreasing)
    return(c(ord.ind, ind.excl))
}

#' Automatically order the rows by correspondence analysis of the table
#' @description Rows of the table are ordered according to the row-coordinates
#'    given by correspondence analysis of the table (1st dimension)
#' @param x Input matrix
#' @importFrom ca ca
#' @export
AutoOrderRows <- function(x)
{
    if (!checkIsTable(x))
        return(x)
    tmp <- if (isTableWithStats(x)) ca(x[,,1]) else ca(x)
    ord <- order(tmp$rowcoord[,1])
    extractArray(x, row.index = ord)
}

#' Automatically order the columns by correspondence analysis of the table
#' @description Columns of the table are ordered according to the column-coordinates
#'    given by correspondence analysis of the table (1st dimension)
#' @param x Input matrix
#' @importFrom ca ca
#' @export
AutoOrderColumns <- function(x)
{
    if (!checkIsTable(x))
        return(x)
    tmp <- if (isTableWithStats(x)) ca(x[,,1]) else ca(x)
    ord <- order(tmp$colcoord[,1])
    extractArray(x, col.index = ord)
}

#' Sort rows of a table
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
#'  Vectors and 1-d arrays will be converted to a matrix.
#' @param decreasing Order to sort values.
#' @param column The column to sort by. If none is specified and the 'Column n'
#'  statistic is present in the table, it will use the column with the largest
#'  value of 'Column n'. Otherwise it will pick the right-most column.
#' @param exclude A string containing a comma-separated list of rows
#'    (either by name or index) which should not be sorted. These rows
#'    will remain at the bottom of the output table
#' @export
SortRows <- function(x,
                 decreasing = FALSE,
                 column = NULL, # integer, otherwise largest column
                 exclude = "NET, SUM, Total")
{
    if (length(dim(x)) < 2)
        x <- convertToMatrix(x)
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
        col.ind <- ncol(x)
    }
    ind <- indexSortedByValues(x,
                 values = if (isTableWithStats(x)) x[,col.ind,1] else x[,col.ind],
                 decreasing, exclude, "row")
    extractArray(x, row.index = ind)
}

#' Sort columns of a table
#' @description Sorts the columns of the table based on the values in the specified column
#' @param x Input matrix or dataframe which is being sorted. Values must be numeric.
#'      Vectors and 1-d arrays will be ignored.
#' @param decreasing Order to sort values.
#' @param row The row to sort by. If none is specified and the 'Row n'
#'  statistic is present in the table, it will use the row with the largest
#'  value of 'Row n'. Otherwise it will pick the bottom row.
#' @param exclude A string containing a comma-separated list of columns
#'    (either by name or index) which should not be sorted. These columns
#'    will remain at the end of the output table
#' @export
SortColumns <- function(x,
                 decreasing = FALSE,
                 row = NULL, # integer, otherwise largest column
                 exclude = "NET, SUM, Total")
{
    if (length(dim(x)) < 2)
        return(x)
    if (!checkIsTable(x))
        return(x)
    if ((isTableWithStats(x) && !is.numeric(x[,,1])) || !is.numeric(x))
        stop("Sorting cannot be applied to non-numeric tables.")

    # Finding the row to sort on
    if (!is.null(row))
    {
        row.ind <- matchNameOrIndex(row[1],
            rownames(x, do.NULL = FALSE, prefix = ""))
        if (length(row.ind) == 0)
            stop("Row '", row, "' was not found in the table.")
        if (length(row) > 1)
            warning("Only row '", row[1], "' was used to sort the table.")

    } else if (isTableWithStats(x) && "Row n" %in% dimnames(x)[[3]])
    {
        d.ind <- which(dimnames(x)[[3]] == "Row n")
        row.ind <- which.max(x[,1,d.ind])

    } else
    {
        row.ind <- nrow(x)
    }

    ind <- indexSortedByValues(x,
                 values = if (isTableWithStats(x)) x[row.ind,,1] else x[row.ind,],
                 decreasing, exclude, "column")
    extractArray(x, col.index = ind)
}

# This is a wrapper for matchNameOrIndex
# It will break up the pattern from a commma-separated list to a vector
# It will also check for unmatched entries and give warnings
getMatchIndex <- function(pattern, x, dim = "row", warn = TRUE)
{
    sel.vec <- TextAsVector(pattern)
    sel.ind <- matchNameOrIndex(sel.vec, x)
    sel.na <- which(is.na(sel.ind))
    if (warn && length(sel.na) > 0)
        warning("Table does not contain ", dim, "s '",
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


#' Throws error if table has small sample size
#' @description Throws an error if any of the 'Base n' values in the table are too small
#' @param x Input table, which must contain the 'Base n' statistic.
#' @param min.size Minimum sample size required.
#' @export
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

#' Removes rows and columns with small sample sizes
#' @description Using the 'Column n' (preferred) or 'Column n' statistic
#'   this funcion will remove any rows/column from \code{x}, where all
#'   are smaller than \code{min.size}. If any rows/columns are removed
#'   then warnings will be given.
#' @param x Input table, which must contain the 'Column n' or 'Base n' statistic.
#' @param min.size Minimum sample size required.
#' @export
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
        x <- extractArray(x, col.index = -col.rm)
    }
    if (all(dim(x) > 0) && length(row.rm) > 0)
    {
        warning("Rows ", paste(row.rm, collapse=","), " have sample size less than ", min.size, " and have been removed.")
        x <- extractArray(x, row.index = -row.rm)
    }
    x
}
