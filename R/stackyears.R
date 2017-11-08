#' StackYears
#'
#' Stacks years to make seasonal patterns and growth easier to see.
#'
#' @param x A \code{\link{vector}}, \code{\link{list}}, \code{\link{table}}.
#' or \code{\link{data.frame}}.
#' @param date An optional vector containing dates. Where not provided,
#' the dates are assumed to be in the row or column names, first row or column,
#' or first element of \code{x}.
#' @param n.years The number of years to stack. Older data is ignored.
#' @param calendar If true, the years are calendar. Otherwise, the end at the final
#' time point. When this is done, some years can have different numbers of
#' values (e.g., weeks).
#' @param period.number If TRUE, period numbers instead of dates are returned in the column names.
#' @param transpose If TRUE, the result is transposed.
#' @return A \code{\link{matrix}}, with the column names containing the dates or time periods,
#' and the rows the years.
#' @importFrom lubridate year years interval duration
#' @importFrom flipTime AsDate
#' @export
StackYears <- function(x, date = NULL, n.years = NULL, calendar = TRUE,
                       period.number = FALSE, transpose = FALSE)
{
    x <- TidyTabularData(x, date)
    date <- AsDate(names(x), on.parse.failure = "silent")
    latest.year <- year(max(date))
    year.index <- if (calendar)
        latest.year - year(date) + 1
    else
        floor(interval(date, max(date)) / duration(1, "years")) + 1
    number.years <- max(year.index)
    if (is.null(n.years))
        n.years <- number.years
    else if (n.years > number.years)
    {
        n.years <- number.years
        warning("Specified number of years exceeds the number in the data")
    }
    if (number.years <= 2)
        warning("This function require at least one whole year of data.")
    year.table <- table(year.index)
    #year.table <- year.table[length(year.table):1]
    n.periods <- max(year.table)
    date.diff = date[2] - date[1]
    colnm <- if(period.number) 1:n.periods
        else
            {
                first.2 <- date[year.index == 1][1:2]
                seq(first.2[1], by = date.diff, length.out = n.periods)
            }
    rnm <- latest.year:(latest.year - n.years + 1)
    result <- matrix(NA, nrow = n.years, ncol = n.periods,
        dimnames = list(Year = rnm, Date = as.character(colnm)))
    for (i in 1:(n.years - 1))
        result[i, 1:year.table[i]] <- x[year.index == i]
    if (year.table[n.years] + 1 >= n.periods) # Checking to see if last year is incomplete.
        result[n.years, 1:year.table[n.years]] <- x[year.index == n.years]
    else
        result[n.years, (n.periods - year.table[n.years]+ 1):n.periods] <- x[year.index == n.years]
    if (transpose)
        result <- t(result)
    result
}

