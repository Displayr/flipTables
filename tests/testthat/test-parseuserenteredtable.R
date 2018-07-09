context("ParseUserEnteredData")
## Unit tests taken from flipTransformations/tests/testthat/test-dataentry.R
## and flipTransformations/tests/testthat/test-ParseUserEnteredTable.R
## with flipTransformations @commit a0b7e63
## Tests modified to account for TidyTabularData always creating row and column names,
## whereas ParseUserEnteredData does not

test_that("label in 1x1 cell",
{
    m <- matrix("1", 3, 3)
    m[1, ] <- c("%", LETTERS[1:2])
    m[2:3, 1] <- letters[1:2]
    out <- TidyTabularData(m)
    expect_equal(dim(out), c(2, 2))
    expect_equal(rownames(out), letters[1:2])
    expect_equal(colnames(out), LETTERS[1:2])
})

test_that("no label in 1x1 cell",
{
    m <- matrix("1", 3, 3)
    m[1, 1] <- ""
    m[1, 2:3] <- LETTERS[1:2]
    m[2:3, 1] <- letters[1:2]
    out <- TidyTabularData(m)
    expect_equal(rownames(out), letters[1:2])
    expect_equal(colnames(out), LETTERS[1:2])
    expect_equal(dim(out), c(2, 2))
    expect_true(is.numeric(out))
})


test_that("DS-1471: TB comment",
{
    m <- matrix(as.character(sample(9, 5*9, replace = TRUE)), 5, 9)
    m[1, 1] <- ""
    m[2:nrow(m), 1] <- LETTERS[1:(nrow(m)-1)]
    #m[2:3, 1] <- letters[1:2]
    out <- TidyTabularData(m)
    expect_equal(colnames(out), m[1, -1])
    expect_true(is.numeric(out))
})

test_that("DS-1471: TB comment transposed",
{
    m <- matrix(as.character(sample(9, 5*9, replace = TRUE)), 5, 9)
    m[1, 1] <- ""
    m[2:nrow(m), 1] <- LETTERS[1:(nrow(m)-1)]
    m <- t(m)
    #m[2:3, 1] <- letters[1:2]
    out <- TidyTabularData(m)
    expect_equal(colnames(out), m[1, -1])
    expect_equal(colnames(out), m[1, -1])
    expect_true(is.numeric(out))
})

test_that("row names, no column names",
{
    m <- matrix(as.character(sample(9, 5*9, replace = TRUE)), 5, 9)
    m[1:nrow(m), 1] <- LETTERS[1:nrow(m)]

    out <- TidyTabularData(m)
    expect_equal(colnames(out), paste0("Col ", seq_len(ncol(m) - 1L)))
    expect_equal(rownames(out), m[, 1])
})

test_that("column names, no row names",
{
    m <- matrix(as.character(sample(9, 5*9, replace = TRUE)), 5, 9)
    # m[1, 1] <- ""
    m[1:nrow(m), 1] <- LETTERS[1:nrow(m)]
    m <- t(m)
    #m[2:3, 1] <- letters[1:2]
    out <- TidyTabularData(m)
    expect_equal(rownames(out), paste0("Row ", seq_len(nrow(m) - 1L)))
    expect_equal(colnames(out), m[1, ])
})

test_that("row vector",
{
    n.row <- 1
    n.col <- 10
    m <- matrix(rep("1", n.row*n.col), n.row, n.col)
    out <- TidyTabularData(m)
    expect_equal(out, structure(rep.int(1, n.col), names = seq_len(n.col)))
})

test_that("named row vector",
{
    n.col <- 10
    m <- rbind(letters[1:n.col], rep("1", n.col))

    out <- TidyTabularData(m)
    expect_equal(names(out), m[1, ])
    expect_equal(out, as.numeric(m[2, ]), check.attributes = FALSE)
})

test_that("column vector",
{
    n.row <- 10
    n.col <- 1
    m <- matrix(rep("1", n.row*n.col), n.row, n.col)
    out <- TidyTabularData(m)
    expect_equal(out, structure(rep.int(1, n.row), names = seq_len(n.row)))
})

test_that("named column vector",
{
    n.col <- 6
    m <- cbind(letters[1:n.col], rep("1", n.col))

    out <- TidyTabularData(m)
    expect_equal(names(out), m[, 1])
    expect_equal(drop(out), as.numeric(m[, 2]), check.attributes = FALSE)
})

test_that("2 x 3",
{
    n.row <- 2
    n.col <- 3
    m <- matrix(as.character(seq_len(n.row*n.col)), n.row, n.col)
    m[1:nrow(m), 1] <- LETTERS[1:nrow(m)]
    m[1, 2:ncol(m)] <- letters[2:ncol(m)]
    m <- t(m)
    out <- TidyTabularData(m)
    expect_equal(colnames(out), m[1,])
})

test_that("Missing data",
          {
              n.row <- 3
              n.col <- 4
              m <- matrix(as.character(seq_len(n.row * n.col)), n.row, n.col)
              m[2, ] <- c("invalid", "NA", "NaN", "-")
              out <- TidyTabularData(m)
              expect_equal(sum(out, na.rm = TRUE), 52)
          })

## start old tests for flipTransformations/R/dataentry.R
## taken from flipTransformations/tests/testthat/test-TidyTabularData.R
test_that("numeric matrix without names", {
    raw.matrix <- structure(c("", "", "", "", "", "", "", "", "1", "", "999", "-111",
                              "", "", "3", "2", "3.14", "", "", "", "", "6", "", "7"), .Dim = c(6L, 4L))
    expect_equal(TidyTabularData(raw.matrix),
                 structure(c(1, NA, 999, -111, 3, 2, 3.14, NA, NA, 6, NA, 7), .Dim = c(4L, 3L),
                           dimnames = list(paste0("Row ", 1:4), paste0("Col ", 1:3))))
})

test_that("numeric vector without names", {
    raw.matrix <- structure(c("", "", "", "", "", "", "", "", "", "", "", "", "1",
                              "2", "3", "", "5", "6"), .Dim = c(9L, 2L))
    expect_equal(TidyTabularData(raw.matrix),
                 structure(c(1, 2, 3, NA, 5, 6), names = seq_len(6)))
})

test_that("numeric vector with names", {
    raw.matrix <- structure(c("one", "two", "three", "", "five", "six", "1",
                              "2", "3", "", "5", "6"), .Dim = c(6L, 2L))
    expect_equal(TidyTabularData(raw.matrix), structure(c(1, 2, 3, NA, 5, 6),
                                                         .Names = c("one", "two", "three", "", "five", "six")))
})

test_that("numeric matrix with row names", {
    raw.matrix <- structure(c("a", "b", "c", "d", "e", "f", "g", "h", "i", "1",
                "2", "5", "2", "5", "7", "3", "3", "1", "5", "2", "2", "4", "6",
                "7", "4", "3", "2"), .Dim = c(9L, 3L))
    expect_equal(TidyTabularData(raw.matrix),
                 structure(c(1, 2, 5, 2, 5, 7, 3, 3, 1, 5, 2, 2, 4, 6, 7, 4, 3, 2),
                           .Dim = c(9L, 2L),
                           .Dimnames = list(c("a", "b", "c", "d", "e", "f", "g", "h", "i"),
                                            paste0("Col ", 1:2L))))
})

test_that("numeric matrix with column names", {
    raw.matrix <- structure(c("x", "1", "2", "5", "2", "5", "7", "3", "3", "1",
                              "y", "5", "2", "2", "4", "6", "7", "4", "3", "2"), .Dim = c(10L, 2L))
    expect_equal(TidyTabularData(raw.matrix),
                 structure(c(1, 2, 5, 2, 5, 7, 3, 3, 1, 5, 2, 2, 4, 6, 7, 4, 3,
                             2), .Dim = c(9L, 2L),
                           .Dimnames = list(paste0("Row ", seq_len(nrow(raw.matrix) - 1)),
                                            c("x", "y"))))
})

test_that("numeric matrix with names", {
    raw.matrix <- structure(c("", "", "", "", "", "Height", "Weight", "Strength",
                "Australia", "8", "8", "7", "USA", "7", "10", "10", "Denmark",
                "10", "4", "2"), .Dim = 4:5)
    expect_equal(TidyTabularData(raw.matrix), structure(c(8, 8, 7, 7, 10, 10, 10, 4, 2), .Dim = c(3L, 3L), .Dimnames = list(
        c("Height", "Weight", "Strength"), c("Australia", "USA", "Denmark"))))
})

test_that("numeric matrix with names and titles", {
    raw.matrix <- structure(c("", "", "Product", "", "", "", "", "", "", "", "",
                              "", "Coke", "Diet Coke", "Coke Zero", "Pepsi", "Diet Pepsi",
                              "Pepsi Max", "None of these", "NET", "Attribute", "Feminine",
                              "0.064220183", "0.574923547", "0.22324159", "0.085626911", "0.605504587",
                              "0.100917431", "0.097859327", "1", "", "Health-conscious", "0.018348624",
                              "0.587155963", "0.550458716", "0.021406728", "0.577981651", "0.308868502",
                              "0.174311927", "1", "", "Innocent", "0.091743119", "0.229357798",
                              "0.128440367", "0.097859327", "0.434250765", "0.073394495", "0.29969419",
                              "1", "", "Older", "0.651376147", "0.217125382", "0.04587156",
                              "0.379204893", "0.091743119", "0.064220183", "0.085626911", "1",
                              "", "Open to new experiences", "0.226299694", "0.091743119",
                              "0.519877676", "0.155963303", "0.162079511", "0.504587156", "0.119266055",
                              "1", "", "Rebellious", "0.262996942", "0.04587156", "0.314984709",
                              "0.177370031", "0.039755352", "0.44648318", "0.159021407", "1",
                              "", "Sleepy", "0.091743119", "0.235474006", "0.091743119", "0.143730887",
                              "0.296636086", "0.064220183", "0.388379205", "1", "", "Traditional",
                              "0.923547401", "0.146788991", "0.03058104", "0.5382263", "0.033639144",
                              "0.039755352", "0.027522936", "1", "", "Weight-conscious", "0.006116208",
                              "0.764525994", "0.645259939", "0", "0.764525994", "0.406727829",
                              "0.055045872", "1", "", "NET", "0.981651376", "0.923547401",
                              "0.908256881", "0.788990826", "0.951070336", "0.868501529", "0.574923547",
                              "1"), .Dim = c(10L, 12L))
    expect_equal(TidyTabularData(raw.matrix),
                 structure(c(0.064220183, 0.574923547, 0.22324159, 0.085626911,
                            0.605504587, 0.100917431, 0.097859327, 1, 0.018348624, 0.587155963,
                            0.550458716, 0.021406728, 0.577981651, 0.308868502, 0.174311927,
                            1, 0.091743119, 0.229357798, 0.128440367, 0.097859327, 0.434250765,
                            0.073394495, 0.29969419, 1, 0.651376147, 0.217125382, 0.04587156,
                            0.379204893, 0.091743119, 0.064220183, 0.085626911, 1, 0.226299694,
                            0.091743119, 0.519877676, 0.155963303, 0.162079511, 0.504587156,
                            0.119266055, 1, 0.262996942, 0.04587156, 0.314984709, 0.177370031,
                            0.039755352, 0.44648318, 0.159021407, 1, 0.091743119, 0.235474006,
                            0.091743119, 0.143730887, 0.296636086, 0.064220183, 0.388379205,
                            1, 0.923547401, 0.146788991, 0.03058104, 0.5382263, 0.033639144,
                            0.039755352, 0.027522936, 1, 0.006116208, 0.764525994, 0.645259939,
                            0, 0.764525994, 0.406727829, 0.055045872, 1, 0.981651376, 0.923547401,
                            0.908256881, 0.788990826, 0.951070336, 0.868501529, 0.574923547, 1),
                           .Dim = c(8L, 10L),
                           .Dimnames = list(c("Coke", "Diet Coke", "Coke Zero", "Pepsi", "Diet Pepsi", "Pepsi Max",
                                              "None of these", "NET"),
                                            c("Feminine", "Health-conscious", "Innocent", "Older", "Open to new experiences",
                                              "Rebellious", "Sleepy", "Traditional", "Weight-conscious", "NET")),
                           row.column.names = c("Product", "Attribute")))
})


test_that("DS-1558: all character table, row/col labels",
{
    x <- structure(c("", "21 July", "5/8 Aug", "22 Aug", "1st Sep", "12/14 Sep",
    "", "Chris", "Immediate contact for new Displayr users who appear in Slack.",
    "Immediate contact for new Displayr users who appear in Slack.",
    "Immediate contact for new Displayr users who appear in Slack.",
    "Review long-resolution tickets", "Immediate contact for new Displayr users who appear in Slack.",
    "", "Matt E", "Follow up with 10 Q non users in our region. Continue with Phrase express",
    "Follow up with lapsed Displayr users - 3+ uses, not used for 7 days or more, restric.",
    "Follow up with lapsed Displayr users - 3+ uses, not used for 7 days or more, restrict to o.",
    "Monitor pending in Zendesk, makes notes for areas for improvement.",
    "Follow up with 10 Q non users in our region.", "Review 10 tickets with longest resolution.",
    "Tim", "Contact people in app when first sign up and if notice they are seen \"just now\". Follow up.",
    "Contact people in app when first sign up and if notice they are seen \"just now\". Follow",
    "", "Contact people in app when first sign up and if notice they are seen \"just now\". Triallist research.",
    "Contact people in app when first sign up and if notice they are seen \"just now\". Triallist research.",
    "Review 10 Displayr users, active last two weeks, who have had 5 or more uses or ",
    "Neal", "", "Contact 10 Q non-users.", "Contact 10 Q non-users.",
    "Contact 10 Q non-users.", "", "Research + Contact 5 US Displayr users. Review 5 tic.",
    "Matt S", "Monitor new Displayr users in UK daytime and approach with in-app messages",
    "Monitor new Displayr users in UK daytime and approach with in-app messages",
    "", "", "", "Record tickets with large number of replies and classify reasons. Review 5 .",
    "Nigel", "Contact 5-10 EU / UK Displayr users", "Contact 20 EU / UK Displayr users",
    "", "", "Contact 5 EU / UK Displayr users", "Contact 5 EU / UK."
  ), .Dim = c(7L, 7L))

    out <- suppressWarnings(TidyTabularData(x))
    expect_equal(dim(out), c(6, 6))
    expect_equal(colnames(out), x[1, -1])
    expect_equal(rownames(out), x[-1, 1])
})

test_that("data frame", {
    raw.matrix <- structure(c("num", "1", "2", "", "4", "2", "", "5", "", "23",
                              "", "2", "3.14", "5", "", "6", "date", "1-Feb-16", "2-Feb-16",
                              "3-Feb-16", "4-Feb-16", "5-Feb-16", "6-Feb-16", "7-Feb-16", "8-Feb-16",
                              "9-Feb-16", "10-Feb-16", "11-Feb-16", "12-Feb-16", "13-Feb-16",
                              "14-Feb-16", "15-Feb-16", "char", "a", "b", "b", "a", "f", "f",
                              "c", "f", "c", "c", "c", "f", "f", "f", "c"), .Dim = c(16L, 3L))
    expect_silent(out <- TidyTabularData(raw.matrix, want.data.frame = TRUE,
                                          want.factors = TRUE))
    expect_equal(out,
                 structure(list(num = c(1, 2, NA, 4, 2, NA, 5, NA, 23, NA, 2, 3.14, 5, NA, 6),
                                date = structure(c(1454284800, 1454371200, 1454457600,
                                                   1454544000, 1454630400, 1454716800, 1454803200, 1454889600, 1454976000,
                                                   1455062400, 1455148800, 1455235200, 1455321600, 1455408000, 1455494400),
                                                 class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                                char = structure(c(1L, 2L, 2L, 1L, 4L, 4L, 3L, 4L, 3L, 3L, 3L, 4L, 4L, 4L, 3L),
                                                 .Label = c("a","b", "c", "f"), class = "factor")),
                           .Names = c("num", "date", "char"), row.names = as.character(1L:15L),
                           class = "data.frame"))
})

test_that("data frame no factors", {
    raw.matrix <- structure(c("num", "1", "2", "", "4", "2", "", "5", "", "23",
                              "", "2", "3.14", "5", "", "6", "date", "1-Feb-2016", "2-Feb-2016",
                              "3-Feb-2016", "4-Feb-2016", "5-Feb-2016", "6-Feb-2016", "7-Feb-2016", "8-Feb-2016",
                              "9-Feb-2016", "10-Feb-2016", "11-Feb-2016", "12-Feb-2016", "13-Feb-2016",
                              "14-Feb-2016", "15-Feb-2016", "char", "a", "b", "b", "a", "f", "f",
                              "c", "f", "c", "c", "c", "f", "f", "f", "c"), .Dim = c(16L, 3L))
    expect_equal(TidyTabularData(raw.matrix, want.data.frame = TRUE, want.factors = FALSE),
                 structure(list(num = c(1, 2, NA, 4, 2, NA, 5, NA, 23, NA, 2, 3.14, 5, NA, 6),
                                date = structure(c(1454284800, 1454371200, 1454457600,
                                                1454544000, 1454630400, 1454716800, 1454803200, 1454889600, 1454976000,
                                                1455062400, 1455148800, 1455235200, 1455321600, 1455408000, 1455494400),
                                                class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                                char = c("a", "b", "b", "a", "f", "f", "c", "f", "c", "c", "c", "f", "f", "f", "c")),
                           .Names = c("num", "date", "char"), row.names = as.character(1L:15L), class = "data.frame"))
})

test_that("data frame row names", {
    raw.matrix <- structure(c("", "row 1", "row 2", "row 3", "row 4", "row 5",
                              "row 6", "row 7", "row 8", "row 9", "row 10", "row 11", "row 12",
                              "row 13", "row 14", "row 15", "num", "1", "2", "", "4", "2",
                              "", "5", "", "23", "", "2", "3.14", "5", "", "6", "date", "1-Feb-2016",
                              "2-Feb-2016", "3-Feb-2016", "4-Feb-2016", "5-Feb-2016", "6-Feb-2016", "7-Feb-2016",
                              "8-Feb-2016", "9-Feb-2016", "10-Feb-2016", "11-Feb-2016", "12-Feb-2016",
                              "13-Feb-2016", "14-Feb-2016", "15-Feb-2016", "char", "a", "b", "b",
                              "a", "f", "f", "c", "f", "c", "c", "c", "f", "f", "f", "c"), .Dim = c(16L, 4L))
    expect_equal(TidyTabularData(raw.matrix, want.data.frame = TRUE,
                                 want.row.names = TRUE, want.factors = TRUE),
                 structure(list(num = c(1, 2, NA, 4, 2, NA, 5, NA, 23, NA, 2, 3.14, 5, NA, 6),
                                date = structure(c(1454284800, 1454371200, 1454457600,
                                                1454544000, 1454630400, 1454716800, 1454803200, 1454889600, 1454976000,
                                                1455062400, 1455148800, 1455235200, 1455321600, 1455408000, 1455494400),
                                                class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                                char = structure(c(1L, 2L, 2L, 1L, 4L, 4L, 3L, 4L, 3L, 3L, 3L, 4L, 4L, 4L, 3L),
                                                 .Label = c("a", "b", "c", "f"), class = "factor")),
                           .Names = c("num", "date", "char"),
                           row.names = c("row 1", "row 2", "row 3", "row 4", "row 5", "row 6", "row 7", "row 8",
                                         "row 9", "row 10", "row 11", "row 12", "row 13", "row 14", "row 15"),
                           class = "data.frame"))
})

test_that("data frame no names", {
    raw.matrix <- structure(c("1", "2", "", "4", "2", "", "5", "", "23", "", "2",
                "3.14", "5", "", "6", "1-Feb-16", "2-Feb-16", "3-Feb-16", "4-Feb-16",
                "5-Feb-16", "6-Feb-16", "7-Feb-16", "8-Feb-16", "9-Feb-16", "10-Feb-16",
                "11-Feb-16", "12-Feb-16", "13-Feb-16", "14-Feb-16", "15-Feb-16",
                "a", "b", "b", "a", "f", "f", "c", "f", "c", "c", "c", "f", "f",
                "f", "c"), .Dim = c(15L, 3L))
    expect_silent(out <- TidyTabularData(raw.matrix, want.data.frame = TRUE,
                                           want.col.names = FALSE, want.factors = TRUE))
    expect_equal(out,
                 structure(list(X1 = c(1, 2, NA, 4, 2, NA, 5, NA, 23, NA, 2, 3.14, 5, NA, 6),
                                X2 = structure(c(1454284800, 1454371200, 1454457600,
                                               1454544000, 1454630400, 1454716800, 1454803200, 1454889600, 1454976000,
                                               1455062400, 1455148800, 1455235200, 1455321600, 1455408000, 1455494400
                                       ), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                                X3 = structure(c(1L, 2L, 2L, 1L, 4L, 4L, 3L, 4L, 3L, 3L, 3L, 4L, 4L, 4L, 3L),
                                               .Label = c("a", "b", "c", "f"), class = "factor")),
                           .Names = c("X1", "X2", "X3"), row.names = as.character(1L:15L), class = "data.frame"))
})

test_that("data frame blank names", {
    raw.matrix <- structure(c("V1", "2", "", "4", "2", "", "5", "", "23", "", "2",
                              "3.14", "5", "", "6", "", "2-Feb-16", "3-Feb-16", "4-Feb-16",
                              "5-Feb-16", "6-Feb-16", "7-Feb-16", "8-Feb-16", "9-Feb-16", "10-Feb-16",
                              "11-Feb-16", "12-Feb-16", "13-Feb-16", "14-Feb-16", "15-Feb-16",
                              "V3", "b", "b", "a", "f", "f", "c", "f", "c", "c", "c", "f",
                              "f", "f", "c"), .Dim = c(15L, 3L))
    expect_warning(TidyTabularData(raw.matrix, want.data.frame = TRUE),
                   "Some variables have been assigned blank names.")
})

test_that("data frame duplicate names", {
    raw.matrix <- structure(c("V1", "2", "", "4", "2", "", "5", "", "23", "", "2",
                              "3.14", "5", "", "6", "V1", "2-Feb-16", "3-Feb-16", "4-Feb-16",
                              "5-Feb-16", "6-Feb-16", "7-Feb-16", "8-Feb-16", "9-Feb-16", "10-Feb-16",
                              "11-Feb-16", "12-Feb-16", "13-Feb-16", "14-Feb-16", "15-Feb-16",
                              "V3", "b", "b", "a", "f", "f", "c", "f", "c", "c", "c", "f",
                              "f", "f", "c"), .Dim = c(15L, 3L))
    expect_warning(TidyTabularData(raw.matrix, want.data.frame = TRUE),
                   "Some variables share the same name.")
})

test_that("Warnings can be toggled on/off",
{
    x <- matrix(c("", "r1", "r2", "c1", "a", "b", "c1", "c", "d"), 3, 3)
    expect_warning(TidyTabularData(x, warn = TRUE), "Some variables share the same name")
    expect_silent(TidyTabularData(x, warn = FALSE))
})
