context("QTable to TidyTabularData")

## Load QTables for testing
## R CMD check finds ./inst/tests/QTable.R where it gets installed, i.e. in ./tests
## However, testing this file with testthat functions in an interactive session or on
## travis-CI requires specifying the file path as ./inst/tests/QTables.R
## devtools shim for system.file seems to fail on travis-CI with devtools::test()
## because our travis.yml wasnt first calling devtools::load_all()
findInstDirFile <- function(pkg, inst.sub.dir, file)
{
    file.path(system.file(inst.sub.dir, package = pkg, mustWork = TRUE), file)
}
source(findInstDirFile("flipTables", "tests", "QTables.R"))

test_that("IsQTable works",
{
    expect_false(IsQTable(matrix(0, 2, 2)))
    expect_false(IsQTable(array(0, dim = c(2, 2))))
})

test_that("5D case",
{
    expect_true(IsQTable(q5))
    expect_warning(out <- AsTidyTabularData(q5), "\\bAverage\\b", perl = TRUE)
    dims <- dim(q5)
    dn.orig <- dimnames(q5)
    dn.new <- dimnames(out)
    expect_equal(dim(out), c(dims[1L]*dims[3L], dims[4L]*dims[2L]))
    ## labels
    expect_equal(dn.new[[1L]][3], paste0(dn.orig[[3]][1], ": ", dn.orig[[1]][3]))
    expect_equal(dn.new[[2L]][2], paste0(dn.orig[[4]][1], ": ", dn.orig[[2]][2]))
})

test_that("4D table, pick any grid x multi cat with multiple statistics",
{
    expect_true(IsQTable(q4.ms))
    expect_warning(out <- AsTidyTabularData(q4.ms), "Column [%]")
    dims <- dim(q4.ms)
    expect_equal(dim(out), c(dims[1L]*dims[3L], dims[2L]))
    expect_equal(out["Diet Coke: NET", "Traditional"], q4.ms["Diet Coke", "Traditional", "NET", "Column %"])
    ## Picked correct statistic and updated attribute
    expect_equal(attr(out, "statistic"), dimnames(q4.ms)[[4]][1])
})

test_that("Flattening 4D table",
{
    ta <- array(1:120, dim = 2:5)
    dimnames(ta) <- list(c("one", "two"), letters[1:3], LETTERS[1:4], paste0("d", 1:5))
    dims <- dim(ta)
    out <- flipTables:::flatten4DQTable(ta)
    expect_equal(dim(out), c(dims[1] * dims[3], dims[2] * dims[4]))
    expect_equal(out["C: two", "a: d3"], ta["two", "a", "C", "d3"])
    expect_equal(out["A: one", "c: d5"], ta["one", "c", "A", "d5"])
    expect_equal(out["D: two", "b: d2"], ta["two", "b", "D", "d2"])
    expect_true(IsQTable(q5))

})


test_that("4D Qtable numeric grid x cat. grid only one statistic",
{
    expect_true(IsQTable(q4.os))
    expect_silent(out <- AsTidyTabularData(q4.os))
    expect_equal(attr(out, "name"), attr(q4.os, "name"))
    expect_equal(attr(out, "statistic"), attr(q4.os, "statistic"))
})

test_that("3D number grid by number multi, one statistic",
{
    expect_true(IsQTable(q3.os))
    expect_silent(out <- AsTidyTabularData(q3.os))
})

test_that("3D: categorical by pick1 multi, one statistic",
{
    expect_true(IsQTable(q3.os2))
    expect_silent(out <- AsTidyTabularData(q3.os2))
    expect_length(dim(out), 2L)
})

test_that("3D: categorical x numeric, multi stat.",
{
    expect_true(IsQTable(q3.ms))
    expect_warning(out <- AsTidyTabularData(q3.ms))
    expect_length(dim(out), 2L)
})

test_that("3D: grid, multi stat",
{
    expect_true(IsQTable(q3.ms2))
    expect_warning(out <- AsTidyTabularData(q3.ms2))
})

test_that("3D: multi stat. character entries",
{
    expect_warning(out <- AsTidyTabularData(q3.ms3))
    expect_length(dim(out), 2L)

    ## first stat. can't be converted to numeric
    x <- q3.ms3
    x[, , 1] <- x[, , 2]
    expect_warning(out <- AsTidyTabularData(x), regexp = "QTable contains character entries",
                   all = FALSE)
    expect_is(out[1, 1], "character")

    ## empty chars are converted to NA with no warning
    x <- q3.ms3
    x[1:3, 1, 1] <- ""
    expect_warning(out <- AsTidyTabularData(x), "^Multiple statistics", all = TRUE)
    expect_equal(sum(is.na(out)), 3L)

    ## single statistic table with characters
    x <- q3.ms3
    attr(x, "statistic") <- "mean"
    expect_warning(out <- AsTidyTabularData(x), regexp = "QTable contains character entries",
                   all = FALSE)
    expect_equal(dim(out), c(dim(q3.ms3)[1] * dim(q3.ms3)[3], dim(q3.ms3)[2]))
})

test_that("2D: qdate x cat, 1 stat",
{
    expect_true(IsQTable(q2.os))
    expect_silent(out <- AsTidyTabularData(q2.os))
})


test_that("2D: multi grid, 1 stat",
{
    expect_true(IsQTable(q2.os2))
    expect_silent(out <- AsTidyTabularData(q2.os2))
})

test_that("2D: numeric x cat, 1 stat",
{
    expect_true(IsQTable(q2.os3))
    expect_silent(out <- AsTidyTabularData(q2.os3))

})

test_that("2D: multi cat, multi stat",
{
    expect_true(IsQTable(q2.ms))
    expect_warning(out <- AsTidyTabularData(q2.ms))
})

test_that("1D: number multi",
{
    expect_true(IsQTable(q1.os))
    expect_silent(out <- AsTidyTabularData(q1.os))
})

test_that("1D: number",
{
    expect_true(IsQTable(q1.os2))
    expect_silent(out <- AsTidyTabularData(q1.os2))
})

test_that("1D: setDimNames is okay with 1D array",
{
    q1 <- structure(c(42.625, 11.125, 17.875, 9, 2.5, 14.875, 0.75,
                        1.25, 100), .Dim = 9L, statistic = "%", .Dimnames = list(
                            c("Coca-Cola", "Diet Coke", "Coke Zero", "Pepsi ", "Diet Pepsi",
                              "Pepsi Max", "Dislike all cola", "Don't care", "NET")), name = "Preferred cola",
                        questions = c("Preferred cola",
                                      "SUMMARY"))
    expect_true(IsQTable(q1))
    expect_silent(out <- AsTidyTabularData(q1))
})
