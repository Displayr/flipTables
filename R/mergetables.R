#' Merge tables
#'
#' Bind tables together either by rows (similar to \code{\link{rbind}})
#' or by columns (similar to \code{\link{cbind}}). Unlike those
#' functions, this will attempt to match cases based on the row or column
#' names (similar to \code{\link{merge}}).
#'
#' @param left,right The tables to merge. These should be vectors, matrices
#'      or arrays. If the array has 3 dimensions, the first 'plane' of the
#'      third dimension is kept, the others are dropped. It is an error to
#'      have more than 3 dimensions in the array.
#' @param joinby 'Join columns' is similar to \code{\link{cbind}}.
#'      'Join rows' is similar to \code{\link{rbind}}.
#' @param nonmatching How to handle non-matching row or column names.
#'      These are similar to the \code{all.*} arguments in
#'      \code{\link{merge}}.
#' @export
MergeTables <- function(left, right, joinby = c("Join columns", "Join rows"),
    nonmatching = c("Matching only", "Keep all from first table", "Keep all from second table", "Keep all"))
{
    joinby <- match.arg(joinby)
    nonmatching <- match.arg(nonmatching)

    if (length(dim(left)) > 3 || length(dim(right)) > 3)
        stop("One of the input tables has more than 3 dimensions.")

    if (length(dim(left)) == 3) {
        warning("The first table contains multiple statistics. Only using the first statistic.")
        left <- left[, , 1]
    }
    if (length(dim(right)) == 3) {
        warning("The second table contains multiple statistics. Only using the first statistic.")
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

    if (joinby == "Join rows")
    {
        merged <- t(merged)
    }

    as.matrix(merged)
}
