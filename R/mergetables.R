#' MergeTables
#'
#' Merge two or more tables. Bind tables together either by rows (similar to \code{\link{rbind}}) or by
#' columns (similar to \code{\link{cbind}}). Unlike those functions, this will
#' attempt to match cases based on the row or column names (similar to
#' \code{\link{merge}}).
#'
#' @param tables A list of tables to merge
#' @param direction \code{"Side-by-side"} is similar to \code{\link{cbind}}.
#'   \code{"Up-and-down"} is similar to \code{\link{rbind}}.
#' @param nonmatching How to handle non-matching row or column names. These are
#'   similar to the \code{all.*} arguments in \code{\link{merge}}.
#'   \code{MergeTables} supports 2 options: \code{"Keep all"} (like \code{all =
#'   TRUE}) and \code{"Matching only"} (like \code{all = FALSE}).
#'   \code{Merge2Tables} supports these and a further 2 options: \code{"Keep all
#'   from first table"} (like \code{all.x = TRUE}) and \code{"Keep all from
#'   second table"} (like \code{all.y = TRUE}).
#' @details If any table has no names for matching, matching is performed based on the index
#'   order with the output retaining the names from any table that does have them.
#'   In this case the number of columns of the output (for \code{"Up-and-down"}) is the maximum
#'   of the numbers of columns of the inputs.
#' @export
MergeTables <- function(tables, direction = c("Side-by-side", "Up-and-down"),
    nonmatching = c("Keep all", "Matching only"))
{
    direction <- match.arg(direction)
    nonmatching <- match.arg(nonmatching)

    merged <- NULL
    if (length(tables) == 1)
    {
        merged <- to.matrix(tables[[1]], direction)
    }
    else if (length(tables) == 2)
    {
        merged <- Merge2Tables(tables[[1]], tables[[2]], direction = direction, nonmatching = nonmatching)
    }
    else
    {
        tmp.names <- unlist(lapply(tables, function(x){attr(x, "statistic")}))
        merged <- Merge2Tables(tables[[1]],
            Recall(tables[-1], direction = direction, nonmatching = nonmatching),
            direction = direction, nonmatching = nonmatching,
            disambig.names = tmp.names[which(duplicated(tmp.names))])
    }

    merged
}

#' @describeIn MergeTables Merge two tables.
#' @inheritParams MergeTables
#' @param left,right The tables to merge. These should be vectors, matrices or
#'   arrays. If the array has 3 dimensions, the first 'plane' of the third
#'   dimension is kept, the others are dropped. It is an error to have more than
#'   3 dimensions in the array.
#' @param disambig.names Optional vector of column names that should be disambiguated
#'   using the table name
#' @export
Merge2Tables <- function(left, right, direction = c("Side-by-side", "Up-and-down"),
    nonmatching = c("Keep all", "Keep all from first table", "Keep all from second table", "Matching only"),
    disambig.names = NULL)
{
    left.name <- deparse(substitute(left))
    right.name <- deparse(substitute(right))
    right.table.name <- ""
    if (!is.null(attr(left, "name")))
        left.name <- attr(left, "name")
    if (!is.null(attr(right, "name")))
        right.table.name <- right.name <- attr(right, "name")


    #cat("line 64\n")
    #cat("left:", left.name, "; right:", right.name, "\n")
    #print(colnames(right))
    #cat("line 71\n")
    #print(colnames(right))
    #cat(right.table.name, "\n")
    #print(str(right))
    #cat("line 66\n")

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
        x <- as.matrix(x)
        colnames(x) <- statistic
        x
    }
    if (is.null(dim(left)) || length(dim(left)) == 1) {
        left <- .makeMatrix(left, attr(left, "statistic"))
    }
    if (is.null(dim(right)) || length(dim(right)) == 1) {
        right <- .makeMatrix(right, attr(right, "statistic"))
    }

    if (direction == "Up-and-down")
    {
        left <- t(left)
        right <- t(right)
    }

    rownames(left)  <- stringr::str_trim(rownames(left))
    rownames(right) <- stringr::str_trim(rownames(right))

    # If either left or right does not have rownames then rows are merged in index order.
    if (is.null(rownames(left)) || is.null(rownames(right))) {

        x <- if (direction == "Up-and-down") "column" else "row"

        warning(paste("There are no matching", x, "names. Merging is based on",
            x, "index order."))
        max.rows <- max(nrow(left), nrow(right))

        # pad left or right or neither with rows of NAs at bottom
        left.padding <- matrix(nrow = max.rows - nrow(left), ncol = ncol(left))
        right.padding <- matrix(nrow = max.rows - nrow(right), ncol = ncol(right))
        left <- rbind(left, left.padding)
        right <- rbind(right, right.padding)

        merged <- cbind(left, right)
        rownames(merged) <- c(rownames(left), rownames(right))[seq(max.rows)]
        if (direction == "Up-and-down")
            merged <- t(merged)
        return(merged)
    }

    if (length(intersect(rownames(left), rownames(right))) == 0)
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

    if (any(is.na(rownames(left))) || any(is.na(rownames(right))))
    {
        stop("Objects to be merged must both have a unique set of names without NAs, however NAs have been found.")
    }

    if (length(unique(rownames(left))) != nrow(left) || length(unique(rownames(right))) != nrow(right))
    {
        stop("Objects to be merged must both have a unique set of names without NAs, however duplicate names have been found.")
    }

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
        colnames(left)[indL] <- paste0(left.name, " - ", colnames(left)[indL])
    if (length(indR) > 0 && right.table.name != "")
        colnames(right)[indR] <- paste0(right.name, " - ", colnames(right)[indR])
    merged <- merge(left, right, by = "row.names", all.x = all.x, all.y = all.y)
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

    as.matrix(merged)
}


mergeNames <- function(left, right)
{
    matches <- match(right, left)
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


to.matrix <- function(x, direction) # Converts a vector to a matrix, if required.
{
    if (!is.vector(x))
        return(x)
    x <- as.matrix(x)
    if (direction == "Up-and-down")
        return(t(x))
    x
}
