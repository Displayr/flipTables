#' Pad with missing values
#'
#' @description Add entries to vectors or rows to matrices
#'      or data frames filled with NAs.
#' @param x object to pad
#' @param add.n Number of empty entries or rows to add
#' @export
padNAs <- function(x, add.n)
{
    UseMethod("padNAs")
}

#' @export
padNAs.default <- function(x, add.n)
{
    return(c(x, rep(NA, add.n)))
}

#' @export
padNAs.factor <- function(x, add.n)
{
    new.x <- c(x, rep(NA, add.n)) # converts factor to numeric
    new.x <- factor(new.x, labels = levels(x))
    return(new.x)
}

#' @export
padNAs.matrix <- function(x, add.n)
{
    m.pad <- matrix(NA, nrow = add.n, ncol = NCOL(x))
    return(rbind(x, m.pad))
}

#' @export
padNAs.data.frame <- function(x, add.n)
{
    new.x <- as.data.frame(lapply(x, padNAs, add.n = add.n),
            stringsAsFactors = FALSE)
    if (!is.null(rownames(x)))
        rownames(new.x)[1:nrow(x)] <- rownames(x)
    return(new.x)
}


