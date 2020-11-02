context("Merging tables")

direction <- "Side-by-side"

left <- matrix(1:12, nrow = 3, ncol = 4, dimnames = list(letters[1:3], LETTERS[1:4]))
right <- matrix(21:29, nrow = 3, ncol = 3, dimnames = list(letters[c(1, 2, 4)], LETTERS[5:7]))
left.multistat <- array(1:24, dim = c(3, 4, 2),
    dimnames = list(letters[1:3], LETTERS[1:4], paste("Statistic", 1:2)))
right.multistat <- array(21:37, dim = c(3, 3, 2),
    dimnames = list(letters[c(1, 2, 4)], LETTERS[5:7], paste("Statistic", 1:2)))
too.many.dims <- array(1:48, dim = c(3, 4, 2, 2))

matching.only <- matrix(c(1, 2, 4, 5, 7, 8, 10, 11, 21, 22, 24, 25, 27, 28),
    nrow = 2, ncol = 7, dimnames = list(letters[1:2], LETTERS[1:7]))
keep.first <- matrix(c(1:12, 21, 22, NA, 24, 25, NA, 27, 28, NA),
    nrow = 3, ncol = 7, dimnames = list(rownames(left), LETTERS[1:7]))
keep.second <- matrix(c(1, 2, NA, 4, 5, NA, 7, 8, NA, 10, 11, NA, 21:29),
    nrow = 3, ncol = 7, dimnames = list(rownames(right), LETTERS[1:7]))
keep.all <- matrix(c(1:2, NA, 3:5, NA, 6:8, NA, 9:11, NA, 12,
    21, 22, 23, NA, 24, 25, 26, NA, 27, 28, 29, NA),
    nrow = 4, ncol = 7, dimnames = list(c("a", "b", "d", "c"), LETTERS[1:7]))

test_that("Merge side-by-side",
{
    expect_equal(Merge2Tables(left, right, direction, "Matching only"), matching.only)
    expect_equal(Merge2Tables(left, right, direction, "Keep all from first table"), keep.first)
    expect_equal(Merge2Tables(left, right, direction, "Keep all from second table"), keep.second)
    expect_equal(Merge2Tables(left, right, direction, "Keep all"), keep.all)

    expect_equal(MergeTables(list(left, right), direction, "Matching only"), matching.only)
    expect_equal(MergeTables(list(left, right), direction, "Keep all"), keep.all)
})

test_that("Merge side-by-side, multiple statistics",
{
    expect_warning(Merge2Tables(left.multistat, right, direction, "Matching only"),
        "'left.multistat' contains multiple statistics.")
#     expect_warning(MergeTables(list(left.multistat, right), direction, "Matching only"),
#         "'tables[[1]]' contains multiple statistics.")

    expect_equal(suppressWarnings(Merge2Tables(left.multistat, right, direction, "Matching only")),
        matching.only)
    expect_equal(suppressWarnings(Merge2Tables(left.multistat, right, direction, "Keep all from first table")),
        keep.first)
    expect_equal(suppressWarnings(Merge2Tables(left.multistat, right, direction, "Keep all from second table")),
        keep.second)
    expect_equal(suppressWarnings(Merge2Tables(left.multistat, right, direction, "Keep all")),
        keep.all)

    expect_equal(suppressWarnings(MergeTables(list(left.multistat, right), direction, "Matching only")),
        matching.only)
    expect_equal(suppressWarnings(MergeTables(list(left.multistat, right), direction, "Keep all")),
        keep.all)

    expect_warning(Merge2Tables(left, right.multistat, direction, "Matching only"),
        "'right.multistat' contains multiple statistics.")
#     expect_warning(MergeTables(list(left, right.multistat), direction, "Matching only"),
#         "'tables[[2]]' contains multiple statistics.")

    expect_equal(suppressWarnings(Merge2Tables(left, right.multistat, direction, "Matching only")),
        matching.only)
    expect_equal(suppressWarnings(Merge2Tables(left, right.multistat, direction, "Keep all from first table")),
        keep.first)
    expect_equal(suppressWarnings(Merge2Tables(left, right.multistat, direction, "Keep all from second table")),
        keep.second)
    expect_equal(suppressWarnings(Merge2Tables(left, right.multistat, direction, "Keep all")),
        keep.all)

    expect_equal(suppressWarnings(MergeTables(list(left, right.multistat), direction, "Matching only")),
        matching.only)
    expect_equal(suppressWarnings(MergeTables(list(left, right.multistat), direction, "Keep all")),
        keep.all)
})

test_that("Merge side-by-side, too many dimensions",
{
    expect_error(Merge2Tables(too.many.dims, right, direction, "Matching only"),
        "One of the input tables has more than 3 dimensions.")
})

test_that("Merge up-and-down inappropriately",
{
    expect_error(Merge2Tables(left, right, "Up-and-down", "Matching only"),
        "Can not find any matching columns.")
})


direction <- "Up-and-down"

left <- matrix(1:12, nrow = 3, ncol = 4, dimnames = list(letters[1:3], LETTERS[1:4]))
right <- matrix(21:29, nrow = 3, ncol = 3, dimnames = list(letters[4:6], LETTERS[c(1, 2, 5)]))
# left.multistat is the same as previous
right.multistat <- array(21:37, dim = c(3, 3, 2),
    dimnames = list(letters[4:6], LETTERS[c(1, 2, 5)], paste("Statistic", 1:2)))

matching.only <- matrix(c(1:3, 21:23, 4:6, 24:26),
    nrow = 6, ncol = 2, dimnames = list(letters[1:6], LETTERS[1:2]))
keep.first <- matrix(c(1:3, 21:23, 4:6, 24:26, 7:9, rep(NA, 3), 10:12, rep(NA, 3)),
    nrow = 6, ncol = 4, dimnames = list(letters[1:6], colnames(left)))
keep.second <- matrix(c(1:3, 21:23, 4:6, 24:26, rep(NA, 3), 27:29),
    nrow = 6, ncol = 3, dimnames = list(letters[1:6], colnames(right)))
keep.all <- matrix(c(1:3, 21:23, 4:6, 24:26, rep(NA, 3), 27:29, 7:9, rep(NA, 3), 10:12, rep(NA, 3)),
    nrow = 6, ncol = 5, dimnames = list(letters[1:6], c("A", "B", "E", "C", "D")))

test_that("Merge up-and-down",
{
    expect_equal(Merge2Tables(left, right, direction, "Matching only"), matching.only)
    expect_equal(Merge2Tables(left, right, direction, "Keep all from first table"), keep.first)
    expect_equal(Merge2Tables(left, right, direction, "Keep all from second table"), keep.second)
    expect_equal(Merge2Tables(left, right, direction, "Keep all"), keep.all)

    expect_equal(MergeTables(list(left, right), direction, "Matching only"), matching.only)
    expect_equal(MergeTables(list(left, right), direction, "Keep all"), keep.all)
})

test_that("Merge side-by-side inappropriately",
{
    expect_error(Merge2Tables(left, right, "Side-by-side", "Matching only"),
        "Can not find any matching rows.")
})

test_that("Merge without matching rows (side-by-side)",
{
    # Keep all
    tbl <- structure(c(1L, 2L, 3L, NA, NA, NA, 4L, 5L, 6L, NA, NA, NA, 7L,
                       8L, 9L, NA, NA, NA, 10L, 11L, 12L, NA, NA, NA, NA, NA, NA, 21L,
                       22L, 23L, NA, NA, NA, 24L, 25L, 26L, NA, NA, NA, 27L, 28L, 29L
                        ), .Dim = 6:7, .Dimnames = list(c("a", "b", "c", "d", "e", "f"
                        ), c("left - A", "left - B", "C", "D", "A", "B", "E")))
    expect_equal(suppressWarnings(Merge2Tables(left, right,
                                               "Side-by-side", "Keep all")), tbl)

    # Keep all from first table
    tbl <- structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA), .Dim = c(3L, 7L),
                     .Dimnames = list(c("a", "b", "c"),
                                      c("left - A", "left - B", "C", "D", "A",
                                               "B", "E")))
    expect_equal(suppressWarnings(Merge2Tables(left, right, "Side-by-side",
                                               "Keep all from first table")), tbl)

    # Keep all from second table
    tbl <- structure(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 21L,
                       22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L), .Dim = c(3L, 7L),
                     .Dimnames = list(c("d", "e", "f"),
                                      c("left - A", "left - B", "C", "D", "A",
                                               "B", "E")))
    expect_equal(suppressWarnings(Merge2Tables(left, right, "Side-by-side",
                                               "Keep all from second table")), tbl)
})

test_that("Merge without matching rows (up-and-down)",
{
    left2 <- t(left)
    right2 <- t(right)

    # Keep all
    tbl <- structure(c(1L, 4L, 7L, 10L, NA, NA, NA, 2L, 5L, 8L, 11L, NA,
                       NA, NA, 3L, 6L, 9L, 12L, NA, NA, NA, NA, NA, NA, NA, 21L, 24L,
                       27L, NA, NA, NA, NA, 22L, 25L, 28L, NA, NA, NA, NA, 23L, 26L,
                       29L), .Dim = 7:6,
                     .Dimnames = list(c("left2 - A", "left2 - B", "C", "D", "A", "B", "E"),
                                      c("a", "b", "c", "d", "e", "f")))
    expect_equal(suppressWarnings(Merge2Tables(left2, right2,
                                             "Up-and-down", "Keep all")), tbl)

    # Keep all from first table
    tbl <- structure(c(1L, 4L, 7L, 10L, NA, NA, NA, 2L, 5L, 8L, 11L, NA,
                       NA, NA, 3L, 6L, 9L, 12L, NA, NA, NA), .Dim = c(7L, 3L),
                     .Dimnames = list(c("left2 - A", "left2 - B", "C", "D", "A", "B", "E"),
                                      c("a", "b", "c")))
    expect_equal(suppressWarnings(Merge2Tables(left2, right2, "Up-and-down",
                                             "Keep all from first table")), tbl)

    # Keep all from second table
    tbl <- structure(c(NA, NA, NA, NA, 21L, 24L, 27L, NA, NA, NA, NA, 22L,
                       25L, 28L, NA, NA, NA, NA, 23L, 26L, 29L), .Dim = c(7L, 3L),
                     .Dimnames = list(c("left2 - A", "left2 - B", "C", "D", "A", "B", "E"),
                                      c("d", "e", "f")))
    expect_equal(suppressWarnings(Merge2Tables(left2, right2, "Up-and-down",
                                         "Keep all from second table")), tbl)
})

test_that("Duplicate rownames",
{
    tb1 <- structure(c(A = 0.01, B = 0.02, C = 0.03, D = 0.04, E = 0.05,
                   F = 0.06, G = 0.07, H = 0.08, I = 0.09, J = 0.1), statistic = "%")
    tb2 <- matrix(rnorm(26*5), 26, 5, dimnames = list(LETTERS, paste0("Q", 1:5)))
    rownames(tb2)[5] <- "D "
    expect_error(Merge2Tables(left = tb1, right = tb2, direction = "Side-by-side", nonmatching = "Keep all from first table"),
                "Duplicated rownames ('D' in rows 4, 5)", fixed = TRUE)

    rownames(tb2)[5] <- NA
    expect_error(Merge2Tables(left = tb2, right = tb1, direction = "Side-by-side", nonmatching = "Keep all from first table"),
                 "Row 5 in 'tb2' has missing name")

})

test_that("DS-3147: table rownames have newline char. instead of single whitespace",
{
    table.weird <- structure(c(62, 2, 0, 22, 2, 12), .Dim = c(6L, 1L), .Dimnames = list(
    c("Other", "Burger Shack", "Nuovo\n  Burger", "Arnold's",
      "Ma's\n  burgers", "Burger\n  Chef"), "Apr-Jun\n  15"))
    table2 <- structure(c(63, 2, 1, 22, 2, 11, 63, 3, 0, 21, 2, 11,
                          66, 2, 1, 20, 2, 9, 64, 2, 1, 22, 2, 9, 64,
                          2, 2, 20, 2, 10, 63, 3, 2, 19, 2, 11, 62, 2,
                          2, 19, 2, 12, 63, 3, 2, 18, 3, 11, 62, 4, 1,
                          20, 2, 11, 66, 3, 1, 19, 2, 9), statistic = "% Column Share",
                        .Dim = c(6L, 10L), .Dimnames = list(c("Other", "Burger Shack",
                                                              "Nuovo Burger", "Arnold's", "Ma's burgers",
                                                              "Burger Chef"),
                                                            c("Jul-Sep 15", "Oct-Dec 15", "Jan-Mar 16", "Apr-Jun 16", "Jul-Sep 16", "Oct-Dec 16", "Jan-Mar 17", "Apr-Jun 17", "Jul-Sep 17", "Oct-Dec 17")), basedescriptiontext = "sample size = 4853",
                        basedescription = list(Minimum = 4853L, Maximum = 4853L, Range = FALSE,
                                               Total = 4853L, Missing = 0L, EffectiveSampleSize = 4853L,
                                               EffectiveSampleSizeProportion = 100, FilteredProportion = 0),
                        questiontypes = c("NumberMulti", "Date"),
                        span = list(rows = structure(list(c("Other", "Burger Shack", "Nuovo Burger",
                                                            "Arnold's", "Ma's burgers", "Burger Chef")),
                                                     class = "data.frame",
                                                     .Names = "", row.names = c(NA, 6L)),
                                    columns = structure(list(c("Jul-Sep 15", "Oct-Dec 15",
                                                               "Jan-Mar 16", "Apr-Jun 16", "Jul-Sep 16",
                                                               "Oct-Dec 16", "Jan-Mar 17", "Apr-Jun 17",
                                                               "Jul-Sep 17", "Oct-Dec 17")),
                                                        class = "data.frame",
                                                        .Names = "", row.names = c(NA, 10L))),
                        name = "Q5 Number of times ordered in last month: Brand by Quarter",
                        questions = c("Q5 Number of times ordered in last month: Brand",
                                      "Quarter"))
    out <- Merge2Tables(table.weird, table2)
    expect_equal(nrow(out), nrow(table2))
    rownames.expect <- rownames(table.weird)
    rownames.expect <- gsub("\\s+", " ", rownames.expect)
    rownames.expect <- union(rownames.expect, rownames(table2))
    colnames.expect <- colnames(table.weird)
    colnames.expect <- gsub("\\s+", " ", colnames.expect)
    colnames.expect <- union(colnames.expect, colnames(table2))
    expect_equal(rownames(out), rownames.expect)
    expect_equal(colnames(out), colnames.expect)
})
