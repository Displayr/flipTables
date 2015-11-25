context("Merging tables")

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
keep.all <- matrix(c(1:3, NA, 4:6, NA, 7:9, NA, 10:12, NA,
    21, 22, NA, 23, 24, 25, NA, 26, 27, 28, NA, 29),
    nrow = 4, ncol = 7, dimnames = list(letters[1:4], LETTERS[1:7]))

test_that("Merge by columns",
{
    expect_equal(MergeTables(left, right, "Join columns", "Matching only"), matching.only)
    expect_equal(MergeTables(left, right, "Join columns", "Keep all from first table"), keep.first)
    expect_equal(MergeTables(left, right, "Join columns", "Keep all from second table"), keep.second)
    expect_equal(MergeTables(left, right, "Join columns", "Keep all"), keep.all)
})

test_that("Merge by columns, multiple statistics",
{
    expect_warning(MergeTables(left.multistat, right, "Join columns", "Matching only"),
        "The first table contains multiple statistics.")
    expect_equal(suppressWarnings(MergeTables(left.multistat, right, "Join columns", "Matching only")),
        matching.only)
    expect_equal(suppressWarnings(MergeTables(left.multistat, right, "Join columns", "Keep all from first table")),
        keep.first)
    expect_equal(suppressWarnings(MergeTables(left.multistat, right, "Join columns", "Keep all from second table")),
        keep.second)
    expect_equal(suppressWarnings(MergeTables(left.multistat, right, "Join columns", "Keep all")),
        keep.all)

    expect_warning(MergeTables(left, right.multistat, "Join columns", "Matching only"),
        "The second table contains multiple statistics.")
    expect_equal(suppressWarnings(MergeTables(left, right.multistat, "Join columns", "Matching only")),
        matching.only)
    expect_equal(suppressWarnings(MergeTables(left, right.multistat, "Join columns", "Keep all from first table")),
        keep.first)
    expect_equal(suppressWarnings(MergeTables(left, right.multistat, "Join columns", "Keep all from second table")),
        keep.second)
    expect_equal(suppressWarnings(MergeTables(left, right.multistat, "Join columns", "Keep all")),
        keep.all)
})

test_that("Merge by columns, too many dimensions",
{
    expect_error(MergeTables(too.many.dims, right, "Join columns", "Matching only"),
        "One of the input tables has more than 3 dimensions.")
})

test_that("Using join rows inappropriately",
{
    expect_error(MergeTables(left, right, "Join rows", "Matching only"),
        "Can not find any matching columns.")
})


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
keep.all <- matrix(c(1:3, 21:23, 4:6, 24:26, 7:9, rep(NA, 3), 10:12, rep(NA, 6), 27:29),
    nrow = 6, ncol = 5, dimnames = list(letters[1:6], LETTERS[1:5]))

test_that("Merge by rows",
{
    expect_equal(MergeTables(left, right, "Join rows", "Matching only"), matching.only)
    expect_equal(MergeTables(left, right, "Join rows", "Keep all from first table"), keep.first)
    expect_equal(MergeTables(left, right, "Join rows", "Keep all from second table"), keep.second)
    expect_equal(MergeTables(left, right, "Join rows", "Keep all"), keep.all)
})

test_that("Using join columns inappropriately",
{
    expect_error(MergeTables(left, right, "Join columns", "Matching only"),
        "Can not find any matching rows.")
})
