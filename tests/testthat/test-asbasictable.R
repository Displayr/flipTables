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

test_that("AsBasicTable: 1D array becomes named vector",
{
    formTables <- list(structure(c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12), .Names = c("Blueberry",
                      "Cherry", "Apple", "Boston Cream", "Other", "Vanilla Cream")),
                        structure(c(42.625, 11.125, 17.875, 9, 2.5, 14.875, 0.75,
                        1.25, 100), .Dim = 9L, statistic = "%", .Dimnames = list(
                            c("Coca-Cola", "Diet Coke", "Coke Zero", "Pepsi ", "Diet Pepsi",
                              "Pepsi Max", "Dislike all cola", "Don't care", "NET")), name = "Preferred cola",
                        questions = c("Preferred cola",
                                      "SUMMARY")))
    out <- AsBasicTable(formTables[[2L]])
    expect_null(dim(out))
    expect_true(inherits(out, "numeric"))
    expect_equal(names(out), c("Coca-Cola", "Diet Coke", "Coke Zero", "Pepsi ", "Diet Pepsi",
                               "Pepsi Max", "Dislike all cola", "Don't care", "NET"))
    expect_equal(attr(out, "statistic"), "%")
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
