context("HideEmptyRowsAndColumns")

tabWithN <- structure(c(12.2448979591837, 6.12244897959184, 4.08163265306122,
6.12244897959184, 4.08163265306122, 8.16326530612245, 22.4489795918367,
19.3877551020408, 17.3469387755102, 100, 32.2033898305085, 13.5593220338983,
5.08474576271187, 10.1694915254237, 5.08474576271187, 0, 5.08474576271187,
13.5593220338983, 15.2542372881356, 100, 11.8811881188119, 12.8712871287129,
12.8712871287129, 13.3663366336634, 8.41584158415842, 13.3663366336634,
13.3663366336634, 8.91089108910891, 4.95049504950495, 100, 10.8843537414966,
17.687074829932, 11.5646258503401, 7.48299319727891, 16.3265306122449,
3.40136054421769, 6.12244897959184, 22.4489795918367, 4.08163265306122,
100, 12.5, 6.25, 18.75, 6.25, 9.375, 12.5, 18.75, 15.625, 0,
100, 3.57142857142857, 19.6428571428571, 8.92857142857143, 16.0714285714286,
26.7857142857143, 5.35714285714286, 10.7142857142857, 8.92857142857143,
0, 100, 13.0769230769231, 2.30769230769231, 12.3076923076923,
20, 13.8461538461538, 6.92307692307692, 11.5384615384615, 12.3076923076923,
7.69230769230769, 100, 6.57894736842105, 15.7894736842105, 7.89473684210526,
5.26315789473684, 11.8421052631579, 9.21052631578947, 9.21052631578947,
28.9473684210526, 5.26315789473684, 100, 98, 98, 98, 98, 98,
98, 98, 98, 98, 98, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 202,
202, 202, 202, 202, 202, 202, 202, 202, 202, 147, 147, 147, 147,
147, 147, 147, 147, 147, 147, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 130, 130, 130,
130, 130, 130, 130, 130, 130, 130, 76, 76, 76, 76, 76, 76, 76,
76, 76, 76, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800,
800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800,
800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800,
800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800,
800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800,
800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800,
800, 800, 800, 800, 800), .Dim = c(10L, 8L, 3L), .Dimnames = list(
    c("18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44",
    "45 to 49", "50 to 54", "55 to 64", "65 or more", "NET"),
    c("Every or nearly every day", "4 to 5 days a week", "2 to 3 days a week",
    "Once a week", "Once every 2 weeks", "Once a month", "Less than once a month",
    "Never"), c("Column %", "Column n", "Base n")), name = "Age by Exercise frequency",         questions = c("Age", "Exercise frequency"))

test_that("HideEmptyRowsAndColumns is.percent parameter",
{
    x <- matrix(0:1, 2, 2)
    out <- HideEmptyRowsAndColumns(x, is.percent = NULL)
    expect_equal(out, x)

    out <- HideEmptyRowsAndColumns(x, is.percent = TRUE)
    expect_equal(out, x[2L, , drop = FALSE])

    attr(x, "statistic") <- "A %"
    out <- HideEmptyRowsAndColumns(x, is.percent = FALSE)
    expect_equal(out, x)

    out <- HideEmptyRowsAndColumns(tabWithN)
    expect_equal(dim(out), dim(tabWithN))
})


test_that("HideEmptyRowsAndColumns errors if remove creates empty table",
{
    x <- matrix(NA, 3, 3, dimnames = list(LETTERS[1:3],LETTERS[1:3]))
    expect_error(HideEmptyRowsAndColumns(x),
                 "^Hiding empty rows/columns gives empty input matrix")

    x <- matrix(0, 2, 2)
    attr(x, "statistic") <- "Column %"
    expect_error(HideEmptyRowsAndColumns(x),
                 "^Hiding empty rows/columns gives empty input matrix")

    x <- rep.int(NA, 2)
    expect_error(HideEmptyRowsAndColumns(x),
                 "^Hiding empty elements gives empty input vector")

    x <- numeric(10L)
    attr(x, "statistic") <- "Column %"
    expect_error(HideEmptyRowsAndColumns(x),
                 "^Hiding empty elements gives empty input vector")

})

test_that("GetNonEmptyElements",
{

    expect_equal(length(GetNonEmptyElements(NA)), 0)
    q1.os <- structure(c(7.08868501529052, 3.84709480122324, 17.4617737003058
    ), .Dim = 3L, statistic = "Average", .Dimnames = list(c("Colas (e.g., Coca-Cola, Pepsi Max)?",
    "Sparkling mineral water", "SUM")), name = "Number Multi", questions = c("Number Multi",
                                                                             "SUMMARY"))
    q1.os[1L] <- NA
    expect_equal(unname(GetNonEmptyElements(q1.os, FALSE)), 2:3, check.attributes = TRUE)
    expect_equal(unname(GetNonEmptyElements(q1.os, TRUE)), names(q1.os)[2:3],
                 check.attributes = TRUE)

    q1.os[2] <- 0
    attr(q1.os, "statistic") <- "Col %"
    expect_equal(unname(GetNonEmptyElements(q1.os, FALSE)), 3)
})

test_that("GetNonEmptyRowsAndColumns",
{
    q2.os <- structure(c(4.05405405405405, 10.8108108108108, 12.1621621621622,
    12.1621621621622, 9.45945945945946, 8.10810810810811, 12.1621621621622,
    12.1621621621622, 9.45945945945946, 9.45945945945946, 9.45945945945946,
    8.10810810810811, 8.10810810810811, 14.8648648648649, 10.8108108108108,
    5.40540540540541, 9.45945945945946, 14.8648648648649, 9.45945945945946,
    9.45945945945946, 3.44827586206897, 17.2413793103448, 8.62068965517241,
    15.5172413793103, 6.89655172413793, 5.17241379310345, 12.0689655172414,
    8.62068965517241, 10.3448275862069, 12.0689655172414, 5.47945205479452,
    2.73972602739726, 16.4383561643836, 12.3287671232877, 8.21917808219178,
    9.58904109589041, 9.58904109589041, 6.84931506849315, 16.4383561643836,
    12.3287671232877, 11.7647058823529, 10.2941176470588, 7.35294117647059,
    5.88235294117647, 8.82352941176471, 16.1764705882353, 8.82352941176471,
    10.2941176470588, 8.82352941176471, 11.7647058823529, 8.33333333333333,
    4.16666666666667, 12.5, 12.5, 18.75, 8.33333333333333, 8.33333333333333,
    12.5, 8.33333333333333, 6.25, 14.2857142857143, 7.14285714285714,
    17.1428571428571, 8.57142857142857, 4.28571428571429, 12.8571428571429,
    12.8571428571429, 11.4285714285714, 2.85714285714286, 8.57142857142857,
    6.38297872340426, 8.51063829787234, 10.6382978723404, 10.6382978723404,
    13.8297872340426, 9.57446808510638, 10.6382978723404, 12.7659574468085,
    6.38297872340426, 10.6382978723404, 7.31707317073171, 9.75609756097561,
    14.6341463414634, 9.75609756097561, 9.75609756097561, 2.4390243902439,
    19.5121951219512, 12.1951219512195, 0, 14.6341463414634, 7.83333333333333,
    8.66666666666667, 11.8333333333333, 11.3333333333333, 10, 9,
    11.1666666666667, 11.3333333333333, 8.33333333333333, 10.5), .Dim = c(10L,
    10L), statistic = "Column %", .Dimnames = list(c("12/26/2011-1/22/2012",
    "1/23/2012-2/19/2012", "2/20/2012-3/18/2012", "3/19/2012-4/15/2012",
    "4/16/2012-5/13/2012", "5/14/2012-6/10/2012", "6/11/2012-7/8/2012",
    "7/9/2012-8/5/2012", "8/6/2012-9/2/2012", "9/3/2012-9/30/2012"
    ), c("18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44",
         "45 to 49", "50 to 54", "55 to 64", "65 +", "NET")), name = "Interview Date by Age Categories",
    questions = c("Interview Date",
  "Age Categories"))

    expect_equal(length(unlist(GetNonEmptyRowsAndColumns(matrix(NA, 2, 2)))), 0)

    q2.os[, 6:7] <- NA
    out <- GetNonEmptyRowsAndColumns(q2.os, FALSE)
    expect_equal(unname(out[[1L]]), 1:nrow(q2.os))
    expect_equal(unname(out[[2L]]), (1:ncol(q2.os))[c(-6, -7)])

    out <- GetNonEmptyRowsAndColumns(q2.os, TRUE)
    expect_equal(unname(out[[1L]]), rownames(q2.os)[1:nrow(q2.os)])
    expect_equal(unname(out[[2L]]), colnames(q2.os)[(1:ncol(q2.os))[c(-6, -7)]])

    q2.os[2:3, ] <- 0
    out <- GetNonEmptyRowsAndColumns(q2.os, FALSE)
    expect_equal(unname(out[[1L]]), (1:nrow(q2.os))[-c(2, 3)])
    expect_equal(unname(out[[2L]]), (1:ncol(q2.os))[c(-6, -7)])
})

test_that("HideEmptyRowsAndColumns list input",
{
    x <- list(x = 1:3, y = NULL, z = matrix(0, 2, 2))
    expect_equal(HideEmptyRowsAndColumns(x), x[-2L])
})

test_that("GetNonEmptyElements character input",
{
    x <- c("a", "", "")
    expect_equal(HideEmptyRowsAndColumns(x), x[1L])
})

test_that("GetNonEmptyRowsAndColumns character matrix",
{
    x <- matrix("", 2, 2)
    x[1, 1] <- "1"
    expect_equal(HideEmptyRowsAndColumns(x), x[1L, 1L, drop = FALSE])
})
