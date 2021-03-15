context("AsTidyTabularData")

test_that("Works with 3D array",
{
    a <- array(1:24, dim = 2:4)
    dimnames(a) <- list(NULL, NULL, LETTERS[1:4])
    expect_error(a <- provideDimnames(a, base = list("D1", "D2", "D3"), sep = "_"), NA)
})

test_that("Works with 4D array",
{
    a <- array(1:120, dim = 2:5)
    dimnames(a) <- list(NULL, letters[1:3], NULL, LETTERS[1:5])
    expect_warning(AsTidyTabularData(a))
    expect_equal(attr(suppressWarnings(AsTidyTabularData(a)), "statistic"), "UNKNOWN")
})

test_that("AsTidyTabularData: 1D array becomes named vector",
{
    formTables <- list(structure(c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12), .Names = c("Blueberry",
                      "Cherry", "Apple", "Boston Cream", "Other", "Vanilla Cream")),
                        structure(c(42.625, 11.125, 17.875, 9, 2.5, 14.875, 0.75,
                        1.25, 100), .Dim = 9L, statistic = "%", .Dimnames = list(
                            c("Coca-Cola", "Diet Coke", "Coke Zero", "Pepsi ", "Diet Pepsi",
                              "Pepsi Max", "Dislike all cola", "Don't care", "NET")), name = "Preferred cola",
                        questions = c("Preferred cola",
                                      "SUMMARY")))
    out <- AsTidyTabularData(formTables[[2L]])
    expect_null(dim(out))
    expect_true(inherits(out, "numeric"))
    expect_equal(names(out), c("Coca-Cola", "Diet Coke", "Coke Zero", "Pepsi ", "Diet Pepsi",
                               "Pepsi Max", "Dislike all cola", "Don't care", "NET"))
    expect_equal(attr(out, "statistic"), "%")
})

test_that("AsTidyTabularData: works with QDates",
{
    dat <- structure(list(foo = structure(c(1, 2), name = "UniqueID", label = "Unique Identifier",
                                          question = "Unique Identifier",
                                          questiontype = "Number"),
                    bar = structure(c(3L, 5L),
                    .Label = c("19-Dec-11-01-Jan-12",
                                      "02-Jan-12-15-Jan-12", "16-Jan-12-29-Jan-12", "30-Jan-12-12-Feb-12",
                                      "13-Feb-12-26-Feb-12", "27-Feb-12-11-Mar-12", "12-Mar-12-25-Mar-12",
                                      "26-Mar-12-08-Apr-12", "09-Apr-12-22-Apr-12", "23-Apr-12-06-May-12",
                                      "07-May-12-20-May-12", "21-May-12-03-Jun-12", "04-Jun-12-17-Jun-12",
                                      "18-Jun-12-01-Jul-12", "02-Jul-12-15-Jul-12", "16-Jul-12-29-Jul-12",
                                      "30-Jul-12-12-Aug-12", "13-Aug-12-26-Aug-12", "27-Aug-12-09-Sep-12",
                                      "10-Sep-12-23-Sep-12", "24-Sep-12-07-Oct-12", "08-Oct-12-21-Oct-12",
                                      "22-Oct-12-04-Nov-12", "05-Nov-12-18-Nov-12", "19-Nov-12-02-Dec-12",
                                      "03-Dec-12-16-Dec-12", "17-Dec-12-30-Dec-12"), class = c("ordered",
                                                                                               "factor"), name = "date",
                    label = "Interview Date", question = "Interview Date", questiontype = "Date")),
               .Names = c("foo", "bar"), row.names = c(NA, 2L), class = "data.frame")
    expect_warning(out <- AsTidyTabularData(dat), "converted to numeric")
    expect_is(out, "matrix")
    expect_equal(dim(out), dim(dat))
})

test_that("AsTidyTabularData: as labels to vector",
{
    x <- rbind(letters[1:3], 1:3)
    out <- AsTidyTabularData(x)
    expect_equal(colnames(out), x[1, ])
    expect_equal(out, as.numeric(x[2, ]), check.attributes = FALSE)

    x <- t(x)
    out <- AsTidyTabularData(x)
    expect_equal(rownames(out), x[, 1])
    expect_equal(out, as.numeric(x[, 2]), check.attributes = FALSE)
})

test_that("AsTidyTabularData: errors for unknown inputs",
{
    expect_error(AsTidyTabularData(as.Date("2017-01-01")),
                 "^Cannot coerce object of type")
})

test_that("AsTidyTabularData: works for matrices",
{
    out <- AsTidyTabularData(matrix(1:4, 2, 2))
    expect_equal(dim(out), c(2, 2))
    #expect_equal(colnames(out), paste0("Col ", 1:2))
})


test_that("AsTidyTabularData: converts 1D array to numeric",
{
    out <- AsTidyTabularData(array(1, dim = 1))
    expect_null(dim(out))
    #expect_equal(names(out), "1")
})

test_that("AsTidyTabularData: preserves dimname names",
{
    xd <- array(runif(9), dim = c(3, 3, 3),
                dimnames = list(A = c("a","a","a"),
                                B = c("a","a","a"), C = c("a","a","a")))
    attr(xd, "name") <- "My so cool table from Q"
    attr(xd, "questions") <- "What's the meaning of life?"
    expect_warning(out <- AsTidyTabularData(xd), "^Multiple statistic")
    expect_equal(names(dimnames(out)), c("A",  "B"))
})

test_that("AsTidyTabularData: data.frame inputs",
{
    df <- data.frame(1, row.names = "a")
    out <- AsTidyTabularData(df)
    expect_is(out, "matrix")
    expect_equal(rownames(out), rownames(df))

    df <- data.frame(x = 1:10, y = as.factor(rep(1:2, 5)))
    out <- suppressWarnings(AsTidyTabularData(df))
    #expect_equal(rownames(out), paste0("Row ", seq_len(nrow(df))))
    expect_equal(ncol(out), 2)
    expect_equal(colnames(out), c("x", "y"))

})

test_that("AsTidyTabularData: factor input",
{
    f <- as.factor(rep(1:3, each = 2))
    out <- suppressWarnings(AsTidyTabularData(f))
    expect_is(out, "matrix")
    expect_equal(rownames(out), paste0("Row ", seq_along(f)))

    expect_equal(ncol(out), 1)
    expect_equal(colnames(out), paste0("x"))
    #expect_true(all(out == 0 | out == 1))
})

test_that("DS-3240: CorrelationMatrix input returns corr. matrix",
{
    cm <- structure(list(cor = structure(c(1, 0, 0.15, -0.08, -0.12, 0,
    1, 0.19, 0.2, 0.1, 0.15, 0.19, 1, 0.09, 0.18, -0.08, 0.2, 0.09,
    1, 0.07, -0.12, 0.1, 0.18, 0.07, 1), .Dim = c(5L, 5L), .Dimnames = list(
    c("x", "y", "z", "u", "v"), c("x", "y", "z", "u", "v"))),
    t = structure(c(Inf, 0.02, 0.86, -0.58, -1.03, 0.02, Inf,
    1.31, 1.46, 0.7, 0.86, 1.31, Inf, 0.61, 1.53, -0.58, 1.46,
    0.61, Inf, 0.43, -1.03, 0.7, 1.53, 0.43, Inf), .Dim = c(5L,
    5L), .Dimnames = list(c("x", "y", "z", "u", "v"), c("x",
    "y", "z", "u", "v"))), p = structure(c(0, 0.99, 0.4, 0.57,
    0.31, 0.99, 0, 0.2, 0.15, 0.49, 0.4, 0.2, 0, 0.54, 0.13,
    0.57, 0.15, 0.54, 0, 0.67, 0.31, 0.49, 0.13, 0.67, 0), .Dim = c(5L,
    5L), .Dimnames = list(c("x", "y", "z", "u", "v"), c("x",
    "y", "z", "u", "v"))), standard.errors = structure(c(0, 0.11,
    0.17, 0.13, 0.12, 0.11, 0, 0.14, 0.14, 0.14, 0.17, 0.14,
    0, 0.15, 0.12, 0.13, 0.14, 0.15, 0, 0.17, 0.12, 0.14, 0.12,
    0.17, 0), .Dim = c(5L, 5L), .Dimnames = list(c("x", "y",
    "z", "u", "v"), c("x", "y", "z", "u", "v"))), colors.min.value = -1,
    colors.max.value = 1, show.cell.values = "Automatic", row.labels = "Yes",
    column.labels = "Yes", colors = "RdBu"), class = "CorrelationMatrix",
    ChartData = structure(c(1, 0, 0.15, -0.08, -0.12, 0, 1, 0.19, 0.2, 0.1, 0.15,
    0.19, 1, 0.09, 0.18, -0.08, 0.2, 0.09, 1, 0.07, -0.12, 0.1, 0.18, 0.07, 1),
    .Dim = c(5L, 5L), .Dimnames = list(c("x", "y", "z", "u", "v"),
    c("x", "y", "z", "u", "v")), assigned.rownames = TRUE))
    expect_equal(out <- AsTidyTabularData(cm), cm$cor, check.attributes = FALSE)
    expect_equal(attr(out, "statistic"), "Correlation")
})
