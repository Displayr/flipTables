# Function to extract rows/columns from array
# It handles both 2d matrices and 3d arrays
# Will always try to copy attributes
extractArray <- function(x, row.index = 1:nrow(x), col.index = 1:ncol(x), keep.all.stats = TRUE)
{
    if (isTableWithStats(x) && keep.all.stats)
        res <- x[row.index, col.index,, drop = FALSE]
    else if (isTableWithStats(x) && !keep.all.stats)
    {
        warning("Only the first statistic '", dimnames(x)[[3]][1], "' used.")
        res <- x[row.index, col.index, 1, drop = FALSE]
        attr(res, "statistic") <- dimnames(x)[[3]][1]
    }
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

# Converts 1-d Q Table + statistics in to 3d array
convertTo3dQTable <- function(x)
{
    if (!isQTable(x))           # ignore if not Q Table
       return(x)
    if (isTableWithStats(x))
        return(x)               # no further conversion for 2d table

    dims <- dim(x)
    n.dim <- length(dims)
    dim.names <- dimnames(x)
    stat <- attr(x, "statistic")
    has.only.one.stat <- !is.null(stat)

    if (!has.only.one.stat && n.dim > 1)
    {
        res <- array(x, c(dims[1], 1, dims[2]))
        dimnames(res) <- list(dim.names[[1]], NULL, dim.names[[2]])
        return(CopyAttributes(res, x))
    }
    return(x)
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
    if (length(dim(x)) < 2)
        x <- convertToMatrix(x)

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
    if (length(dim(x)) < 2)
        return(x)

    ind <- indexSelected(x, "column", select, first.k, last.k)
    extractArray(x, col.index = ind)
}


#' Select entries from a table
#' @description Function to select entries from a table using
#' either the row/column name or indices
#' @param x Matrix or dataframe from which values will be extracted.
#' @param row A string containing a comma seperated list of the
#' name or indices of the rows to be selected
#' @param column A string containing a comma seperated list of the
#' name or indices of the column to be selected, If \code{x} is a vector
#' or 1-dimensional array, \code{column} will be ignored.
#' @param return.single.value Logical; If this true, the function will
#' always return a single numeric value. If multiple cells are selected,
#' the entries are summed. If no entries are selected a
#' value of zero is returned.
#' @export
SelectEntry <- function (x, row, column = NULL, return.single.value = FALSE)
{
    indRow <- indexSelected(x, "row", as.character(row))

    # If data is already percentages in Qtable then divide by 100
    # Note that R outputs and pasted data will already be in decimals
    stat <- attr(x, "statistic")
    qst <- attr(x, "questions")
    dnm <- dimnames(x)
    is.pct <- (!is.null(stat) && !is.null(qst) && grepl("%$", stat)) ||
              (length(dnm) > 2 && grepl("%$", dnm[[3]][1]))
    if (is.pct)
        x <- x / 100

    if (length(dim(x)) < 2)
	{
        #cat("column:", column, "\n")
		if (length(column) > 0 && sum(nchar(column)) > 0 && sum(as.numeric(column), na.rm = TRUE) != 1)
			warning("Column ", column, " ignored for 1-dimensional table")
        res <- x[indRow]

	} else
    {
		indCol <- indexSelected(x, "column", as.character(column))
        if (sum(nchar(column), na.rm = TRUE) == 0)
        {
            warning("First column was returned as no column was specified")
            indCol <- 1
        }
        res <- extractArray(x, row.index = indRow, col.index = indCol, keep.all.stats = FALSE)
        if (!is.null(attr(res, "statistic")) && grepl("%$", attr(res, "statistic")))
            is.pct <- TRUE
    }
    if (return.single.value && is.numeric(res))
        res <- sum(res, na.rm = TRUE)
    if (is.pct)
        attr(res, "statistic") <- "%"
    return(res)
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
        sel.ind <- getMatchIndex(select, dim.names, dim = dim)
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
    ord.ind <- ind.incl[order(values[ind.incl], decreasing = decreasing)]
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

    # Finding the column to sort on
    col.ind <- NULL
    if (!is.null(column))
    {
        col.ind <- matchNameOrIndex(column[1], colnames(x, do.NULL = FALSE, prefix = ""))
        if (length(col.ind) == 0)
            stop("Column '", column, "' was not found in the table.")
        if (length(column) > 1)
            warning("Only column '", column[1], "' was used to sort the table.")

    }
    if (length(col.ind) != 1 || !is.finite(col.ind))
        col.ind <- ncol(x)
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

    # Finding the row to sort on
    row.ind <- NULL
    if (!is.null(row))
    {
        row.ind <- matchNameOrIndex(row[1],
            rownames(x, do.NULL = FALSE, prefix = ""))
        if (length(row.ind) == 0)
            stop("Row '", row, "' was not found in the table.")
        if (length(row) > 1)
            warning("Only row '", row[1], "' was used to sort the table.")

    }
    if (length(row.ind) != 1 || !is.finite(row.ind))
        row.ind <- nrow(x)

    ind <- indexSortedByValues(x,
                 values = if (isTableWithStats(x)) x[row.ind,,1] else x[row.ind,],
                 decreasing, exclude, "column")
    extractArray(x, col.index = ind)
}

# This is a wrapper for matchNameOrIndex
# It will break up the pattern from a commma-separated list to a vector
# It will also check for unmatched entries and give warnings
# warn = FALSE is used by indexSortedValues when no error/warning is required
getMatchIndex <- function(pattern, x, dim = "row", warn = TRUE)
{
    pattern <- as.character(pattern)
    sel.vec <- if (length(pattern) > 1) pattern else TextAsVector(pattern)
    sel.ind <- matchNameOrIndex(sel.vec, x, strip.zeros = FALSE)
    sel.na <- which(is.na(sel.ind))
    warning.msg <- ""
    if (length(sel.na) > 0)
        warning.msg <- paste0("Table does not contain ", dim, if (length(sel.na) > 1) "s" else "",  " '",
            paste(sel.vec[sel.na], collapse = "','"), "'.")
    sel.ind[sel.ind == 0] <- NA
    sel.ind <- sel.ind[which(!is.na(sel.ind))]
    if (warn && nchar(warning.msg) > 0 && length(sel.ind) == 0)
        stop(warning.msg)
    else if (warn && nchar(warning.msg))
        warning(warning.msg)
    return(sel.ind)
}

#' @param p.list Vector of patterns to match
#' @param x Vector of names
#' @param strip.zeros If false, then zero-values in the return vector indicate that
#'    some entries in \code{p.list} ambiguously match multiple names. This information
#'    is useful for writing warning messages (e.g. in \code{getMatchIndex}.
#'    If true, the zeros are converted to NAs.
#' @return list of indices mapping p.list to x.
#'      Unmatched entries in p.list are set to NA
#'		Ambiguous patterns are preferentially treated as indices
#' @importFrom flipU TrimWhitespace
#' @importFrom stringi stri_reverse
#' @noRd
matchNameOrIndex <- function(p.list, x, strip.zeros = TRUE)
{
    # Looking for string-to-string match
    p.list <- TrimWhitespace(p.list)
    x <- TrimWhitespace(x)
    ind.as.name <- charmatch(p.list, x)
    retry <- which(!is.finite(ind.as.name))
    ind.as.name[retry] <- charmatch(stri_reverse(p.list[retry]), stri_reverse(x))

    # Give warnings if pattern can be used as both an index (numeric) or a name
    ind <- suppressWarnings(as.numeric(p.list))
    ind[ind < 1 | ind > length(x)] <- NA
    ambig.pos <- which(!is.na(ind) & !is.na(ind.as.name) & ind != ind.as.name)
    for (ii in ambig.pos)
    {
        # Check for an exact match
        if (p.list[ii] %in% x)
            warning("'", p.list[ii], "' treated as an index. ",
             "To select entry with name '", p.list[ii], "' use index ", ind.as.name[ii], "\n")
    }

	# Patterns are treated as indices where possible
	pos.as.name <- is.na(ind)
	ind[pos.as.name] <- ind.as.name[pos.as.name]

    # Give warnings from duplicate matches in charmatch if relevant
    dup.match <- which(ind.as.name == 0 & pos.as.name & nchar(p.list) > 0)
    if (length(dup.match) > 0)
    {
        warning.msg <- ""
        for (ii in dup.match)
        {
            tmp.match <- grep(p.list[ii], x, value = TRUE, fixed = TRUE)
            warning.msg <- paste0(warning.msg, "'", p.list[ii], "' matched multiple values ambiguously: '",
                paste(tmp.match, collapse = "', '"), "'.\n")
        }
        if (any(is.finite(ind) & ind > 0))
            warning(warning.msg)
        else
            stop(warning.msg)
    }
    if (strip.zeros)
        ind[ind == 0] <- NA
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
    x <- convertTo3dQTable(x)
    if (!isTableWithStats(x) || !any(c("Base n", "Sample Size") %in% dimnames(x)[[3]]))
        stop("Table does not have 'Sample Size' or 'Base n'")

    d.ind <- which(dimnames(x)[[3]] %in% c("Base n", "Sample Size"))
    if (any(x[,,d.ind[1]] < min.size, na.rm = TRUE))
        stop("Output not shown because it is based on less than ", min.size, " observations.")
    else
        return(x)
}

#' Removes rows with small sample sizes
#' @description Using the 'Column n' (preferred) or 'Column n' statistic
#'   this funcion will remove any rows from \code{x}, where all
#'   are smaller than \code{min.size}. If any rows/columns are removed
#'   then warnings will be given.
#' @param x Input table, which must contain the 'Column n' or 'Base n' statistic.
#' @param min.size Minimum sample size required.
#' @export
HideRowsWithSmallSampleSizes <- function(x, min.size = 30)
{
    x <- convertTo3dQTable(x)
    size.names <- c("Column Sample Size", "Column n", "Sample Size", "Base n")
    if (!isTableWithStats(x) || !any(size.names %in% dimnames(x)[[3]]))
        stop("Table must have at least one of the following statistics: '",
             paste(size.names, collapse = "', '", sep=""), "'.")

    j <- 1
    d.ind <- NULL
    while (length(d.ind) == 0)
    {
        d.ind <- which(dimnames(x)[[3]] == size.names[j])
        j <- j + 1
    }

    # Search rows
    row.rm <- c()
    for (i in 1:nrow(x))
    {
        if (all(x[i, ,d.ind] < min.size))
            row.rm <- c(row.rm, i)
    }
    if (all(dim(x) > 0) && length(row.rm) > 0)
    {
        warning("Rows ", paste(row.rm, collapse=","), " have sample size less than ", min.size, " and have been removed.")
        x <- extractArray(x, row.index = -row.rm)
    }
    x
}

#' Removes columns with small sample sizes
#' @description Using the 'Column n' (preferred) or 'Column n' statistic
#'   this funcion will remove any rows from \code{x}, where all
#'   are smaller than \code{min.size}. If any rows/columns are removed
#'   then warnings will be given.
#' @param x Input table, which must contain the 'Column n' or 'Base n' statistic.
#' @param min.size Minimum sample size required.
#' @export
HideColumnsWithSmallSampleSizes <- function(x, min.size = 30)
{
    x <- convertTo3dQTable(x)
    size.names <- c("Column Sample Size", "Column n", "Sample Size", "Base n")
    if (!isTableWithStats(x) || !any(size.names %in% dimnames(x)[[3]]))
        stop("Table must have at least one of the following statistics: '",
             paste(size.names, collapse = "', '", sep=""), "'.")

    j <- 1
    d.ind <- NULL
    while (length(d.ind) == 0)
    {
        d.ind <- which(dimnames(x)[[3]] == size.names[j])
        j <- j + 1
    }

    # Search rows and columns
    col.rm <- c()
    for (i in 1:ncol(x))
    {
        if (all(x[,i,d.ind] < min.size))
            col.rm <- c(col.rm, i)
    }
    if (length(col.rm) > 0)
    {
        warning("Columns ", paste(col.rm, collapse=","), " have sample size less than ", min.size, " and have been removed.")
        x <- extractArray(x, col.index = -col.rm)
    }
    x
}
