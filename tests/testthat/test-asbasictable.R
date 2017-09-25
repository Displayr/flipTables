context("AsBasicTable")

test_that("Works with 3D array",
{
    a <- array(1:24, dim = 2:4)
    dimnames(a) <- list(NULL, NULL, LETTERS[1:4])
    a <- provideDimnames(a, base = list("D1", "D2", "D3"), sep = "_")
})

test_that("Works with 4D array",
{
    a <- array(1:120, dim = 2:5)
    dimnames(a) <- list(NULL, letters[1:3], NULL, LETTERS[1:5])
    expect_warning(AsBasicTable(a))
    expect_equal(attr(AsBasicTable(a), "statistic"), "UNKNOWN")
})

test_that("AsBasicTable: perserves attributes",
{

})

test_that("AsBasicTable: works with QDates",
{

})

test_that("AsBasicTable: as labels to vector",
{

})

test_that("AsBasicTable: errors for unknown inputs",
{

})

test_that("AsBasicTable: works for matrices",
{

})
