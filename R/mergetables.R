#' Merge tables
#'
#' Bind tables together either by rows (similar to \code{\link{rbind}}) or by
#' columns (similar to \code{\link{cbind}}). Unlike those functions, this will
#' attempt to match cases based on the row or column names (similar to
#' \code{\link{merge}}).
#'
#' @param tables A list of tables to merge
#' @param joinby \code{"Join columns"} is similar to \code{\link{cbind}}.
#'   \code{"Join rows"} is similar to \code{\link{rbind}}.
#' @param nonmatching How to handle non-matching row or column names. These are
#'   similar to the \code{all.*} arguments in \code{\link{merge}}.
#'   \code{MergeTables} supports 2 options: \code{"Matching only"} (like
#'   \code{all = FALSE}) and \code{"Keep all"} (like \code{all = TRUE}).
#'   \code{Merge2Tables} supports these and a further 2 options: \code{"Keep all
#'   from first table"} (like \code{all.x = TRUE}) and \code{"Keep all from
#'   second table"} (like \code{all.y = TRUE}).
#' @export
MergeTables <- function(tables, joinby = c("Join columns", "Join rows"),
    nonmatching = c("Matching only", "Keep all"))
{
    joinby <- match.arg(joinby)
    nonmatching <- match.arg(nonmatching)

    merged <- NULL
    if (length(tables) == 1)
    {
        merged <- tables[[1]]
    }
    else if (length(tables) == 2)
    {
        merged <- Merge2Tables(tables[[1]], tables[[2]], joinby = joinby, nonmatching = nonmatching)
    }
    else
    {
        merged <- Merge2Tables(tables[[1]],
            Recall(tables[-1], joinby = joinby, nonmatching = nonmatching),
            joinby = joinby, nonmatching = nonmatching)
    }

    merged
}

#' @describeIn MergeTables Merge 2 tables.
#' @inheritParams MergeTables
#' @param left,right The tables to merge. These should be vectors, matrices or
#'   arrays. If the array has 3 dimensions, the first 'plane' of the third
#'   dimension is kept, the others are dropped. It is an error to have more than
#'   3 dimensions in the array.
#' @export
Merge2Tables <- function(left, right, joinby = c("Join columns", "Join rows"),
    nonmatching = c("Matching only", "Keep all from first table", "Keep all from second table", "Keep all"))
{
    joinby <- match.arg(joinby)
    nonmatching <- match.arg(nonmatching)

    if (length(dim(left)) > 3 || length(dim(right)) > 3)
        stop("One of the input tables has more than 3 dimensions.")

    if (length(dim(left)) == 3) {
        warning("'", deparse(substitute(left)), "' contains multiple statistics. Only using the first statistic.")
        left <- left[, , 1]
    }
    if (length(dim(right)) == 3) {
        warning("'", deparse(substitute(right)), "' contains multiple statistics. Only using the first statistic.")
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

    if (joinby == "Join rows")
    {
        left <- t(left)
        right <- t(right)
    }

    rownames(left)  <- stringr::str_trim(rownames(left))
    rownames(right) <- stringr::str_trim(rownames(right))

    if (length(intersect(rownames(left), rownames(right))) == 0)
    {
        type <- ifelse(joinby == "Join columns", "rows", "columns")
        stop("Can not find any matching ", type, ". Perhaps you meant to join by ", type, "?")
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

    merged <- merge(left, right, by = "row.names", all.x = all.x, all.y = all.y)
    rownames(merged) <- merged$Row.names
    merged[["Row.names"]] <- NULL

    # Rearrange rows to match the input as closely as possible
    if (nonmatching %in% c("Matching only", "Keep all from first table"))
    {
        index <- match(rownames(merged), rownames(left))
        merged <- merged[index, ]
    }
    else if (nonmatching == "Keep all from second table")
    {
        index <- match(rownames(merged), rownames(right))
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

    if (joinby == "Join rows")
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
    if (matches[last.match] == max.match)
        max.match <- max.match + 1
    if (last.match != len)
    {
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
