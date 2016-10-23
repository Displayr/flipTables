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
