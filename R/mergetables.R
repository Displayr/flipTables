#' MergeTables
#'
#' Merge two or more tables. Bind tables together either by rows (similar to \code{\link{rbind}}) or by
#' columns (similar to \code{\link{cbind}}). Unlike those functions, this will
#' attempt to match cases based on the row or column names (similar to
#' \code{\link{merge}}).
#'
#' @param tables A list of tables to merge
#' @param direction \code{"Side-by-side"} is similar to
#'     \code{\link{cbind}}.  \code{"Up-and-down"} is similar to
#'     \code{\link{rbind}}.
#' @param nonmatching How to handle non-matching row or column
#'     names. These are similar to the \code{all.*} arguments in
#'     \code{\link{merge}}.  \code{MergeTables} supports 2 options:
#'     \code{"Keep all"} (like \code{all = TRUE}) and
#'     \code{"Matching only"} (like \code{all = FALSE}).
#'     \code{Merge2Tables} supports these and a further 2 options:
#'     \code{"Keep all from first table"} (like \code{all.x = TRUE})
#'     and \code{"Keep all from second table"} (like \code{all.y =
#'     TRUE}).
#' @param override.row.names Either a character vector or a single
#'     string to be parsed by
#'     \code{\link{ConvertCommaSeparatedStringToVector}}, which will
#'     override/replace the default row names of the output table.
#' @param override.column.names Either a character vector or a single
#'     string to be parsed by
#'     \code{\link{ConvertCommaSeparatedStringToVector}}, which will
#'     override/replace the default column names of the output table.
#' @details If any table has no names for matching, matching is
#'     performed based on the index order with the output retaining
#'     the names from any table that does have them.  In this case the
#'     number of columns of the output (for \code{"Up-and-down"}) is
#'     the maximum of the numbers of columns of the inputs.
#' @export
MergeTables <- function(tables, direction = c("Side-by-side", "Up-and-down"),
                        nonmatching = c("Keep all", "Matching only"),
                        override.row.names = "",
                        override.column.names = "")
{
    direction <- match.arg(direction)
    nonmatching <- match.arg(nonmatching)

    ## Checking for column names
    if (!is.null(names(tables)))
    {
        dims <- lapply(tables, dim)
        ## DS-3041: fix rbind'ing when all inputs are vectors
        if (direction == "Up-and-down" && all(vapply(dims, length, 0L) < 2))
        {
            for (i in seq_along(tables))
            {
                tables[[i]] <- t(tables[[i]])
                rownames(tables[[i]]) <- names(tables)[i]
            }
        }else
        {
            for (i in 1:length(tables))
            {
                if (length(dims[[i]]) < 2)
                    tables[[i]] <- as.matrix(tables[[i]])
                if (is.null(colnames(tables[[i]])) && ncol(tables[[i]]) == 1)
                    colnames(tables[[i]]) <- names(tables)[i]
            }
        }
    }

    merged <- NULL
    if (length(tables) == 1)
    {
        merged <- to.matrix(tables[[1]], direction)
    }
    else if (length(tables) == 2)
    {
        merged <- Merge2Tables(tables[[1]], tables[[2]], direction = direction,
                               nonmatching = nonmatching)
    }
    else
    {
        if (direction == "Up-and-down")
            tmp.names <- unlist(lapply(tables, function(x){rownames(x)}))
        else
            tmp.names <- unlist(lapply(tables, function(x){attr(x, "statistic")}))

        merged <- Merge2Tables(tables[[1]],
            Recall(tables[-1], direction = direction, nonmatching = nonmatching),
            direction = direction, nonmatching = nonmatching,
            disambig.names = tmp.names[which(duplicated(tmp.names))])
    }

    if (length(override.row.names) > 1 || nzchar(override.row.names))
        merged <- replaceDimNames(merged, override.row.names, "Row")

    if (length(override.column.names) > 1 || nzchar(override.column.names))
        merged <- replaceDimNames(merged, override.column.names, "Column")

    return(merged)
}

#' @describeIn MergeTables Merge two tables.
#' @param left,right The tables to merge. These should be vectors, matrices or
#'   arrays. If the array has 3 dimensions, the first 'plane' of the third
#'   dimension is kept, the others are dropped. It is an error to have more than
#'   3 dimensions in the array.
#' @param disambig.names Optional vector of column names that should be disambiguated
#'   using the table name
#' @importFrom flipU IsRServer
#' @export
Merge2Tables <- function(left, right, direction = c("Side-by-side", "Up-and-down"),
    nonmatching = c("Keep all", "Keep all from first table", "Keep all from second table", "Matching only"),
    disambig.names = NULL)
{
    left.name <- deparse(substitute(left))
    right.name <- deparse(substitute(right))
    left.table.name <- ""
    right.table.name <- ""
    if (!is.null(attr(left, "name", exact = TRUE)))
        left.table.name <- left.name <- attr(left, "name")
    if (!is.null(attr(right, "name", exact = TRUE)))
        right.table.name <- right.name <- attr(right, "name")

    left <- to.matrix(left, direction)
    right <- to.matrix(right, direction)
    direction <- match.arg(direction)
    nonmatching <- match.arg(nonmatching)

    if (length(dim(left)) > 3 || length(dim(right)) > 3)
        stop("One of the input tables has more than 3 dimensions.")

    if (length(dim(left)) == 3) {
        warning("'", left.name, "' contains multiple statistics. Only using the first statistic.")
        left <- left[, , 1]
    }
    if (length(dim(right)) == 3) {
        warning("'", right.name, "' contains multiple statistics. Only using the first statistic.")
        right <- right[, , 1]
    }

    .makeMatrix <- function(x, statistic) {
        if ((is.null(dim(x)) || length(dim(x)) == 1) &&
            (is.numeric(x) || is.character(x)))
        {
            x <- as.matrix(x)
            colnames(x) <- statistic
        }
        return(x)
    }
    left <- .makeMatrix(left, attr(left, "statistic"))
    right <- .makeMatrix(right, attr(right, "statistic"))

    if (direction == "Up-and-down")
    {
        left <- t(left)
        right <- t(right)
    }

    ## DS-3147 weird newline/space chars from copy/paste errors
    .fixWhitespaceInTableNames <- function(table)
    {
        out <- table
        dn <- dimnames(out)
        .fixWSinVecNames <- function(dimname){
            dimname <- stringr::str_trim(dimname)
            dimname <- gsub("\\s+", " ", dimname)
            dimname
        }
        if (!length(dn))
        {
            if (!is.null(names(out)))
                return(setNames(out, .fixWSinVecNames(out)))
            else
                return(out)
        }

        dn <- lapply(dn, .fixWSinVecNames)
        dimnames(out) <- dn
        out
    }
    left <- .fixWhitespaceInTableNames(left)
    right <- .fixWhitespaceInTableNames(right)

    # If either left or right does not have rownames then rows are merged in index order.
    if (is.null(rownames(left)) || is.null(rownames(right))) {

        dir <- if (direction == "Up-and-down") "column" else "row"

        warning(paste("There are no matching", dir, "names. Merging is based on",
            dir, "index order."))
        max.rows <- max(NROW(left), NROW(right))
        left <- pad.rows(left, max.rows)
        right <- pad.rows(right, max.rows)

        bind.as.matrix <- is.bindable(left, right)
        if (direction == "Up-and-down" || bind.as.matrix)
            merged <- cbind(left, right)
        else
            merged <- data.frame(left, right, stringsAsFactors = FALSE)

        if (direction == "Up-and-down")
            merged <- t(merged)
        return(merged)
    }

    if (nonmatching == "Matching only" &&
        length(intersect(rownames(left), rownames(right))) == 0)
    {
        if (direction == "Side-by-side")
        {
            type <- "rows"
            other.direction <- "up-and-down"
        }
        else
        {
            type <- "columns"
            other.direction <- "side-by-side"
        }
        stop("Can not find any matching ", type, ". Perhaps you meant to join ", other.direction, "?")
    }
    left.NAs <- which(is.na(rownames(left)))
    if (length(left.NAs) > 0)
        stop(ngettext(length(left.NAs), "Row", "Rows"), " ", paste(left.NAs, collapse = ", "),
             " in '", left.name, "' ",
             ngettext(length(left.NAs), "has missing name. ", "have missing names. "),
             "Please give the affected rows a unique name before rerunning Merge Tables.")
    right.NAs <- which(is.na(rownames(right)))
    if (length(right.NAs) > 0)
        stop(ngettext(length(right.NAs), "Row", "Rows"), " ", paste(right.NAs, collapse = ", "),
             " in '", right.name, "' ",
             ngettext(length(right.NAs), "has missing name. ", "have missing names. "),
             "Please give the affected rows a unique name before rerunning Merge Tables.")

    .checkDupNames <- function(row.names, tb.name)
    {
        dup.names <- unique(row.names[which(duplicated(row.names))])
        if (length(dup.names) == 0)
            return (0)
        dup.pos <- rep("", length(dup.names))
        for (i in 1:length(dup.names))
            dup.pos[i] <- paste(which(row.names == dup.names[i]), collapse = ", ")
        stop("Duplicated rownames (",
            paste(sprintf("'%s' in rows %s", dup.names, dup.pos), collapse = ";"), ") in '", tb.name,
            "'. Merge duplicated rows or remove duplicated rows before rerunning Merge Tables.")
    }
    .checkDupNames(rownames(left), left.name)
    .checkDupNames(rownames(right), right.name)

    all.x <- all.y <- FALSE
    if (nonmatching %in% c("Keep all from first table", "Keep all"))
    {
        all.x <- TRUE
    }
    if (nonmatching %in% c("Keep all from second table", "Keep all"))
    {
        all.y <- TRUE
    }

    indL <- which(colnames(left) %in% c(disambig.names, colnames(right)))
    indR <- which(colnames(right) %in% c(disambig.names, colnames(left)))
    if (length(indL) > 0)
    {
        ## Don't warn in Q/Displayr since user can now override row/column names
        ## using GUI controls in Combine Tables
        if (!any(nzchar(left.table.name)) && !IsRServer())
            warning("Assign name to ", left.name,
                    " by setting 'attr(", left.name,  ", \"name\") <- name'")
        colnames(left)[indL] <- paste0(left.name, " - ", colnames(left)[indL])
    }
    # Disambiguation is only added if right.table.name defined
    # Otherwise the recursion in MergeTables will peform this multiple times
    if (length(indR) > 0 && right.table.name != "")
        colnames(right)[indR] <- paste0(right.table.name, " - ", colnames(right)[indR])
    merged <- merge(as.data.frame(left, stringsAsFactors = FALSE),
                    as.data.frame(right, stringsAsFactors = FALSE),
                    by = "row.names", all.x = all.x, all.y = all.y)
    rownames(merged) <- merged$Row.names
    merged[["Row.names"]] <- NULL

    # Rearrange rows to match the input as closely as possible
    if (nonmatching == "Matching only")
    {
        index <- match(intersect(rownames(left), rownames(right)), rownames(merged))
        merged <- merged[index, ]
    }
    else if (nonmatching == "Keep all from first table")
    {
        index <- match(rownames(left), rownames(merged))
        merged <- merged[index, ]
    }
    else if (nonmatching == "Keep all from second table")
    {
        index <- match(rownames(right), rownames(merged))
        merged <- merged[index, ]
    }
    else
    {
        new.row.names <- mergeNames(rownames(left), rownames(right))
        index <- match(new.row.names, rownames(merged))
        merged <- merged[index, ]

        # NET should always be last
        if ("NET" %in% rownames(merged))
        {
            pos <- match("NET", rownames(merged))
            rownums <- seq(along = rownames(merged))
            rownums <- c(rownums[-pos], pos)
            merged <- merged[rownums, ]
        }
    }

    if (direction == "Up-and-down")
    {
        merged <- t(merged)
    }
    if (direction == "Up-and-down" || is.bindable(left, right))
        merged <- as.matrix(merged)
    return(merged)
}


mergeNames <- function(left, right)
{
    matches <- match(right, left)

    # If no names match, just combine left and right
    if (all(is.na(matches)))
        return(c(left, right))

    min.match <- min(matches, na.rm = TRUE)
    max.match <- max(matches, length(left), na.rm = TRUE)

    # If all the names match we don't need to do anything
    if (!any(is.na(matches)))
        return(left)

    min.along <- max.along <- rep(NA, length(matches))
    cur.min <- 0
    reset <- TRUE

    for (i in seq(along = matches))
    {
        if (is.na(matches[i]))
        {
            min.along[i] <- cur.min
            reset <- TRUE
        }
        else
        {
            if (reset)
                cur.min <- matches[i]
            else
                cur.min <- min(cur.min, matches[i])

            min.along[i] <- cur.min
            reset <- FALSE
        }
    }

    cur.max <- max.match + 1
    reset <- TRUE

    for (i in rev(seq(along = matches)))
    {
        if (is.na(matches[i]))
        {
            max.along[i] <- cur.max
            reset <- TRUE
        }
        else
        {
            if (reset)
                cur.max <- matches[i]
            else
                cur.max <- max(cur.max, matches[i])

            max.along[i] <- cur.max
            reset <- FALSE
        }
    }

    # Treat NAs at the beginning and end differently
    first.match <- min(which(!is.na(matches)))
    if (first.match != 1)
    {
        matches[1:(first.match - 1)] <- seq(from = min.match - 0.9, to = min.match - 0.1,
            length.out = first.match - 1)
    }

    last.match <- max(which(!is.na(matches)))
    len <- length(matches)
    if (last.match != len)
    {
        if (matches[last.match] == max.match)
            max.match <- max.match + 1

        matches[(last.match + 1):len] <- seq(from = matches[last.match] + 0.1, to = max.match - 0.1,
            length.out = len - last.match)
    }

    lengths <- rle(!is.na(matches))$lengths
    denom <- rep(lengths, lengths) + 1
    num <- unlist(lapply(lengths, function(x) seq(from = 1, to = x)))
    diff.along <- max.along - min.along

    new.order <- data.frame(
        order = c(seq(along = left), ifelse(is.na(matches), min.along + diff.along * num / denom, matches)),
        name = c(left, right),
        stringsAsFactors = FALSE
    )
    new.order <- new.order[!duplicated(new.order$name), ]
    new.order$name[order(new.order$order)]
}

# Converts a vector to a matrix, if required.
# Note that factor and dates are returned unaltered
to.matrix <- function(x, direction)
{
    if (!is.vector(x))
        return(x)
    x <- as.matrix(x)
    if (direction == "Up-and-down")
        return(t(x))
    x
}

# Returns a logical indicating whether x and y
# can be merged together as a matrix
is.bindable <- function(x, y)
{
    if (is.numeric(x) && is.numeric(y))
        return(TRUE)
    if (is.character(x) && is.character(y))
        return(TRUE)
    else
        return(FALSE)
}

# Pad with empty rows in preparation for cbind of data.frame
# padNAs is used to ensure that factors and dates retain
# their class
pad.rows <- function(x, n)
{
    if (NROW(x) >= n)
        return(x)

    add.n <- n - NROW(x)
    return(padNAs(x, add.n = add.n))
}

#' Updates the row or column names of a table using a comma-separated list
#' of names.
#' @details tbl.names can have length less than the number of rows/columns in
#' the table to only replace the first 1, 2, ..., length(tbl.names) row/column labels. This
#' matches the behaviour of the formatting options for autofit tables in Displayr
#' @importFrom flipU ConvertCommaSeparatedStringToVector IsRServer
#' @noRd
replaceDimNames <- function(tbl, tbl.names, dim.name)
{
    idx <- ifelse(dim.name == "Row", 1, 2)
    dim.length <- dim(tbl)[idx]
    if (length(tbl.names) == 1L && dim.length != 1L)
        tbl.names <- ConvertCommaSeparatedStringToVector(tbl.names)

    names.length <- length(tbl.names)
    if (names.length < dim.length)
    {
        tnames <- dimnames(tbl)[[idx]]
        tnames[seq_len(names.length)] <- tbl.names
        tbl.names <- tnames
    }else if(names.length > dim.length)
    {
        tbl.names <- tbl.names[seq_len(dim.length)]
    }
    dimnames(tbl)[[idx]] <- tbl.names
    return(tbl)
}
