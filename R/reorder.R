#' Reorder
#'
#' Reorders the rows or columns of a table.
#'
#' @param x A \code{\link{matrix}} or \code{\link{data.fram}} A list of tables to merge
#' @param rows One of  \code{"Descending"}, which orders the rows from top to bottom
#' based on \code{FUN}, \code{"Ascending"}, or \code{"None"}.
#' @param columns Same as \code{"rows"}.
#' @param FUN A function to be applied to the rows and/or columns as the
#' basis for the re-odering. Defaults to \code{mean}.
#' @param fudge The value of fudge * i, where i is the row or column number is
#' added to each cell. This ensure that ties are broken (provided that the values of
#' the data are not very small, and that they are broken in a consistent and likely
#' orderly way (e.g., if all the numbers in the table are the same, the table
#' is not reordered).
#' @export
Reorder <- function(x, rows = "Descending", columns = "Descending", FUN = mean, fudge = 1e-12)
{
    .order <- function(x, MARGIN)
    {
        values <- apply(x, MARGIN, FUN, na.rm = TRUE)
        values <- values + fudge * (length(values):1)
        order(values, decreasing = if (MARGIN == 1) rows == "Descending" else columns == "Descending" )
    }
    if (rows != "None")
        z <- x[.order(x, 1), ]
    if (columns != "None")
        z <- x[, .order(x, 2)]
    x
}
