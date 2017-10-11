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
    expect_equal(attr(suppressWarnings(AsBasicTable(a)), "statistic"), "UNKNOWN")
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
    out <- AsBasicTable(matrix(1:4, 2, 2))
    expect_equal(dim(out), c(2, 2))
    expect_equal(colnames(out), paste0("Col ", 1:2))
})


test_that("AsBasicTable: converts 1D array to numeric",
{
    out <- AsBasicTable(array(1, dim = 1))
    expect_null(dim(out))
    expect_equal(names(out), "1")
})

test_that("AsBasicTable: preserves dimname names",
{
    xd <- array(runif(9), dim = c(3, 3, 3),
                dimnames = list(A = c("a","a","a"),
                                B = c("a","a","a"), C = c("a","a","a")))
    attr(xd, "name") <- "My so cool table from Q"
    attr(xd, "questions") <- "What's the meaning of life?"
    expect_warning(out <- AsBasicTable(xd), "^Multiple statistic")
    expect_equal(names(dimnames(out)), c("A",  "B"))
})

test_that("AsBasicTable: data.frame inputs",
{
    df <- data.frame(1, row.names = "a")
    out <- AsBasicTable(df)
    expect_is(out, "matrix")
    expect_equal(rownames(out), rownames(df))

    df <- data.frame(x = 1:10, y = as.factor(rep(1:2, 5)))
    out <- AsBasicTable(df)
    expect_equal(rownames(out), paste0("Row ", seq_len(nrow(df))))
    expect_equal(ncol(out), 3)
    expect_equal(colnames(out), c("x", paste0("y.", 1:nlevels(df$y))))
    expect_true(all(out[, -1] == 0 | out[, -1] == 1))
})

test_that("AsBasicTable: factor input",
{
    f <- as.factor(rep(1:3, each = 2))
    out <- AsBasicTable(f)
    expect_is(out, "matrix")
    expect_equal(rownames(out), paste0("Row ", seq_along(f)))

    expect_equal(ncol(out), nlevels(f))
    expect_equal(colnames(out), paste0("x.", 1:nlevels(f)))
    expect_true(all(out == 0 | out == 1))
})
