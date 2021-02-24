#' Coerce an R Object To a Tidy Table
#'
#' Coerces R objects to a tidy tabular format.  Recognized inputs include
#' tables from \code{Q}, arrays, matrices, data.frames, and pasted
#' tables (character matrices).  Output will always be named and always
#' be either a vector or matrix.
#' @param x  An object to be coerced to a tidy table.
#' @param ... Additional arguments passed to
#'     \code{\link[flipTransformations]{ParseUserEnteredTable}} in the
#'     case that \code{x} has character entries.
#' @return A \strong{named} matrix or
#'     vector; a tidy version of \code{x}
#' @importFrom utils modifyList
#' @importFrom flipTransformations AsNumeric ParseUserEnteredTable ProcessQVariables
#' @importFrom stats setNames
#' @details \code{factors} will automatically be converted to numeric
#'     variables (with a warning) using
#'     \code{\link[flipTransformations]{AsNumeric}}.
#' @seealso \code{\link[flipTransformations]{AsNumeric}}
#' @export
AsTidyTabularData <- function(x, ...)
{
    old.attrs <- attributes(x)
    old.attrs <- old.attrs[!names(old.attrs) %in% c("dimnames", "dim", "row.names",
                                                    "names", "class")]
    if (isQTable(x))
    {
        x <- qTableToTidyTable(x)
        if (!is.null(attr(x, "statistic")))
            old.attrs$statistic <- attr(x, "statistic")  # update if dropped extra stats
        if (length(dim(x)) == 1L)  # convert 1D array to named vector
            x <- setNames(as.numeric(x), dimnames(x)[[1L]])
    }else if (is.character(x))
    {  # assume raw user-entered table
        x <- as.matrix(x)  # ParseEnteredData fails for vectors (since user-entered
                           ## data is always a matrix
        dots <- list(...)
        dots$raw.matrix <- x
        if (is.null(dots$want.data.frame))
            dots$want.data.frame <- FALSE
        x <- do.call("ParseUserEnteredTable", dots)
        x <- AsNumeric(x)
    }else if (is.data.frame(x))
    {
        ## convert data.frame default row names to tidy table default Row 1, Row 2, etc.
        #if (identical(attr(x, "row.names"), seq_len(nrow(x))))
        #    row.names(x) <- paste0("Row ", seq_len(nrow(x)))
        x <- ProcessQVariables(x)
        x <- as.matrix(AsNumeric(x, binary = FALSE, remove.first = FALSE))
    }else if (is.factor(x))
    {
        tmp.names <- names(x)
        if (is.null(tmp.names))
            tmp.names <- paste0("Row ", seq_along(x))
        x <- as.matrix(AsNumeric(data.frame(x, row.names = tmp.names),
                       binary = FALSE, remove.first = FALSE))
    }else if (is.numeric(x))
    {  # make sure has dimnames and is <= 2D
        dims <- dim(x)
        n.dim <- length(dims)

        if (n.dim > 4)
        {
            stop("Cannot coerce an array of greater than four dimensions to a tidy table.")
        }else if (n.dim == 4 || n.dim == 3)
        {   # Make sure there are sensible dimnames, a statistic attribute,
            #  and flatten as if a QTable
            x <- createArrayNames(x)
            if (is.null(old.attrs$statistic))
                old.attrs$statistic <- "UNKNOWN"
            x <- do.call(paste0("flatten", n.dim, "DQTable"), list(x))
        }else if (n.dim == 1L)  # convert 1D array to named vector
            x <- setNames(as.numeric(x), dimnames(x)[[1L]])
    }else if (inherits(x, "CorrelationMatrix"))
    {
        corr.mat <- x$cor
        attr(corr.mat, "statistic") <- "Correlation"
        return(corr.mat)
    }else
    {
        classes <- paste(class(x), collapse = ", ")
        stop(gettextf("Cannot coerce object of type (%s) to a  tidy table.",
                      sQuote(classes)))
    }

    #x <- setDimNames(x)  # set labels
    if (length(old.attrs) && length(attributes(x)))
        attributes(x) <- modifyList(old.attrs, attributes(x))
    if (is.null(dim(x)))
        x <- AsNumeric(x)
    else if (length(dim(x)) == 1L)
        class(x) <- "numeric"
    else if (!is.data.frame(x))
        class(x) <- "matrix"
    x
}

#' Check if an object is a QTable
#'
#' Looks for attributes \code{"name"},
#' and \code{"questions"} to determine if the object is a
#' table created by \code{Q}
#' @param x an R object
#' @return logical; is \code{x} a \code{QTable}
#' @note An attribute \code{"statistic"} is not guaranteed to be present, as
#' the names of the statistics computed may be present in the dimnames
#' @keywords internal
#' @noRd
isQTable <- function(x)
{
    all(c("questions", "name") %in% names(attributes(x)))
}

## Unused and Incomplete!!!
## .valid.stats <- c("Average", "Standard Deviation", "Minimum", "5th Percentile", "25th Percentile",
##                   "Median", "75th Percentile", "95th Percentile", "Maximum", "Mode",
##                   "Trimmed Average", "Interquartile Range", "Sum", "% Share", "Column Sample Size",
##                   "Sample Size", "Missing Count", "Effective Sample Size",
##                   "Weighted Column Sample Size", "Weighted Sample Size", "t-Statistic",
##                   "d.f.", "z-Statistic", "Standard Error", "p", "Corrected p",
##                   "Multiple Comparison Adjustment", "Not Duplicate", "Column Names",
##                   "Columns Compared", "Column Comparisons")

#' Converts a QTable to a tidy table
#' @param x a QTable (an array with attributes \code{"name"}, \code{"statistic"},
#' and \code{"questions"} and named dimensions)
#' @details If a Qtable contains multiple statistics, only the first will be used to
#' create the basic table
#' @return a matrix or vector with the same attributes (aside from possibly modified
#' dimensions and \code{dimnames}
#' @noRd
#' @keywords internal
qTableToTidyTable <- function(x)
{
   stopifnot(isQTable(x))
   dims <- dim(x)
   n.dim <- length(dims)
   dim.names <- dimnames(x)
   stat <- attr(x, "statistic")
   has.only.one.stat <- !is.null(stat)

   if (!has.only.one.stat && n.dim > 1)
   {
       stat <- dim.names[[n.dim]][1]
       warning(gettextf("Multiple statistics detected; only the first in the table (%s) will be used.",
                           dQuote(stat)))
       x <- GetFirstStat(x)
       dims <- dim(x)
       n.dim <- length(dims)
       dim.names <- dimnames(x)
   }

   out <- if (length(dims) == 4L)  # Two group variables
              flatten4DQTable(x)
          else if (length(dims) == 3L)
              flatten3DQTable(x)
          else
              x
   ## convert to numeric
   if (is.character(out))
   {
       if (all(out == "" | is.na(out) | !is.na(suppressWarnings(as.numeric(out)))))
           storage.mode(out) <- "numeric"
       else
           warning("the supplied QTable contains character entries which could not be converted to numeric")
   }
   if (!has.only.one.stat)  # need to update statistic attribute
       attr(out, "statistic") <- stat
   out
}

#' Get First Statistic From A QTable
#'
#' Drops any statistics from the array beyond the first
#' @param x a QTable
#' @return an array; the first statistic in the QTable; i.e. \code{x} with
#' only the first element of the last dimension returned
#' @details the statistics are assumed to be
#' contained in the last dimension of the array
#' @section Warning the subsetting into the array will results in length-1
#' dimensions of \code{x} will be dropped in the returned array
#' @noRd
#' @keywords internal
GetFirstStat <- function(x)
{
    # QTables with this attribute always have only 1 statistic
    if (!is.null(attr(x, "statistic")))
        return(x)
    text <- paste0("x[", paste(rep(",", length(dim(x))-1), collapse = ""), 1, "]")
    eval(parse(text = text))
}

#' Flatten a 4D QTable to a matrix
#'
#' Takes a four-dimensional QTable (array) and flattens
#' it two a matrix.  The dimensions are combined in a way
#' that mimics what is done in \code{Q}.
#' @param a a four dimensional array with all dimensions named.
#' @return a matrix with rownames and column names of dimension
#' \eqn{d1*d3 \times d2*d4} where \code{a} is \eqn{d1\times d2\times d3\times d4}
#' @examples
#'    ta <- array(1:24, dim = 2:4)
#'    dimnames(ta) <- list(c("one", "two"), letters[1:3], LETTERS[1:4])
#'    out <- flipTables:::flatten3DQTable(ta)
#'    all.equal(out["one: B", "c"], ta["one", "c", "B"])
#' @noRd
#' @keywords internal
flatten3DQTable <- function(a)
{
    ## Loop for 3D case
    ## d1 <- dim(a)[1]
    ## d2 <- dim(a)[2]
    ## d3 <- dim(a)[3]
    ## m <- matrix(0, d1*d3, d2)
    ## for ( i in 1:d1){
    ##   for (j in 1:d2){
    ##       for (k in 1:d3){
    ##          m[(i-1)*d3 + k, j] = a[i, j, k]
    ##       }
    ##   }
    ## }
    ## m

    dnames <- dimnames(a)
    ## Weird use of t below is to replicate Q behaviour
    ## The code:
    ##   matrix(a, nrow = d1*d3, ncol = d3)
    ## would not result in same output as Q (the first dim
    ## is populated first/varies fastest in R, in Q the third
    ## dim is populated first/varies fastest)
    out <- apply(a, 2L, t)
    dimnames(out) <- list(combineNames(dnames[c(3, 1)]), dnames[[2]])
    out
}

#' Flatten a 4D QTable to a matrix
#'
#' Takes a four-dimensional QTable (array) and flattens
#' it two a matrix.  The dimensions are combined in a way
#' that mimics what is done in \code{Q}.
#' @param a a four dimensional array with all dimensions named.
#' @return a matrix with rownames and column names of dimension
#' \eqn{d1*d3 \times d2*d4} where \code{a} is \eqn{d1\times d2\times d3\times d4}
#' @examples
#'    ta <- array(1:120, dim = 2:5)
#'    dimnames(ta) <- list(c("one", "two"), letters[1:3], LETTERS[1:4], paste0("d", 1:5))
#'    out <- flipTables:::flatten4DQTable(ta)
#'    all.equal(out["C: two", "a: d3"], ta["two", "a", "C", "d3"])
#' @keywords internal
#' @noRd
flatten4DQTable <- function(a)
{
    dnames <- dimnames(a)
    ## Commented out code below results in valid 2D table, but
    ##  with wrong dimensions combined compared to Q output
    ## out <- apply(a, 2:3, t)
    ## out <- apply(aperm(out, c(2, 1, 3)), 2, t)
    ## dimnames(out) <- list(combineNames(dnames[c(3, 2)]),
    ##                       combineNames(dnames[c(4, 1)]))
    out <- apply(a, c(2, 4), c)
    tnames <- combineNames(dnames[c(3, 1)], flip = TRUE)
    out <- t(apply(out, 1, t))
    dimnames(out) <- list(combineNames(dnames[c(3, 1)], flip = TRUE),
                          combineNames(dnames[c(4, 2)], flip = FALSE))

    out
}

#' Combine two lists of variable names
#'
#' Uses \code{\link[base]{expand.grid}} to create new
#' variable names from a two combined \code{QTable} dimensions
#' @param name.list list of length 2 containing character vectors
#' of possibly differing lengths; e.g. the result of \code{dimnames} on a
#' two-dimensional array
#' @return character vector of new names of length
#' \code{length(name.list[[1]])*length(name.list[[2]])}
#' @noRd
#' @keywords internal
combineNames <- function(name.list, flip = FALSE)
{
    if (flip)
        name.list <- rev(name.list)
    name.grid <- expand.grid(name.list)
    paste(name.grid$Var2, name.grid$Var1, sep = ": ")
}

#' Tries to create sensible row and column names if collapsing a
#' non-QTable 3D or 4D array
#' @param x a three or four dimensional array
#' @return a three or four dimensional array with all dimensions named
#' @seealso \code{\link{provideDimnames}}
#' @noRd
#' @keywords internal
createArrayNames <- function(x)
{
    dim.names <- dimnames(x)
    n.dim <- length(dim(x))
    null.idx <- if (is.null(dim.names))
                    seq_len(n.dim)
                else
                    which((vapply(dim.names, is.null, logical(1L))))
    if (!length(null.idx))
        return(x)
    warning(gettextf("dimnames are missing for some dimensions of %s; creating some before proceeding",
                     deparse(substitute(x))), call. = FALSE)
    base <- as.list(paste0("Dim", seq_len(n.dim)))
    provideDimnames(x, base = base, sep = "_", unique = TRUE)
}
