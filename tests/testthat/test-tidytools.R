context("TidyTools")

dat <- structure(c(1.13154172560113, 0.99009900990099, 0.282885431400283,
    0.565770862800566, 1.6973125884017, 2.12164073550212, 1.13154172560113,
    2.54596888260255, 1.83875530410184, 12.3055162659123, 0, 0.99009900990099,
    0.707213578500707, 0.848656294200849, 0.565770862800566, 0.707213578500707,
    3.96039603960396, 1.27298444130127, 2.54596888260255, 11.5983026874116,
    0, 0.848656294200849, 0.707213578500707, 1.41442715700141, 0.848656294200849,
    0, 1.6973125884017, 2.97029702970297, 1.98019801980198, 10.4667609618105,
    0.282885431400283, 0.424328147100424, 1.83875530410184, 0.565770862800566,
    0.99009900990099, 0.848656294200849, 2.68741159830269, 1.83875530410184,
    1.83875530410184, 11.3154172560113, 0, 0.848656294200849, 0.424328147100424,
    0.848656294200849, 0.424328147100424, 1.83875530410184, 0.99009900990099,
    1.41442715700141, 4.52616690240453, 11.3154172560113, 0, 0, 0.565770862800566,
    1.13154172560113, 0.848656294200849, 0.424328147100424, 0.424328147100424,
    2.26308345120226, 1.98019801980198, 7.63790664780764, 0, 0.848656294200849,
    0.282885431400283, 0.848656294200849, 1.27298444130127, 0.99009900990099,
    2.26308345120226, 1.98019801980198, 3.67751060820368, 12.1640735502122,
    1.55586987270156, 0.848656294200849, 1.41442715700141, 2.12164073550212,
    2.68741159830269, 2.26308345120226, 1.41442715700141, 0.848656294200849,
    3.67751060820368, 16.8316831683168, 0.282885431400283, 0, 0,
    0, 0.99009900990099, 1.6973125884017, 0, 3.11173974540311, 0.282885431400283,
    6.36492220650637, 3.25318246110325, 5.7991513437058, 6.22347949080622,
    8.34512022630834, 10.3253182461103, 10.8910891089109, 14.5685997171146,
    18.2461103253182, 22.3479490806223, 100),
.Dim = c(10L, 10L), statistic = "Total %", .Dimnames = list(
c("Less than $15,000", "$200,001 or more", "$150,001 to $200,000",
"$120,001 to $150,000", "$30,001 to $45,000", "$15,001 to $30,000",
"$90,001 to $120,000", "$45,001 to $60,000", "$60,001 to $90,000",
"NET"), c("18 to 24", "25 to 29", "30 to 34", "35 to 39",
"40 to 44", "45 to 49", "50 to 54", "55 to 64", "65 or more",
"NET")), name = "Income by Age", questions = c("Income", "Age"))

# This is QTable with the Base n statistic
# Note that 'Statistics - Right' and 'Statistics - Below' are never passed
# to the R output
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

test_that("SelectRows",
{
    xx <- matrix(1:12, 3, 4, dimnames=list(LETTERS[1:3], letters[1:4]))
    x2 <- SelectRows(xx, "A,A,C")
    expect_equal(x2[1,], x2[2,])
    expect_warning(SelectRows(xx, "A,B,D,E,F"), "Table does not contain rows 'D','E','F'.")
    expect_warning(x3 <- SelectRows(xx, "D,E,F"), "Table does not contain rows 'D','E','F'.")
    expect_equal(nrow(x3), 0)
    x4 <- SelectRows(xx, "A,B,C,3,2,1")
    expect_warning(x5 <- SelectRows(xx, "A,B,C,3,2,1,0,7"), "Table does not contain rows '0','7'.")
    expect_equal(x5[,1], c(A=1,B=2,C=3,C=3,B=2,A=1))

    res <- SelectRows(tabWithN, "18 to 24, 25 to 29, 3")
    expect_equal(dim(res), c(3, 8, 3))
    expect_equal(rownames(res), c("18 to 24", "25 to 29", "30 to 34"))
})

test_that("SelectColumns",
{
    res <- SelectColumns(tabWithN, "1, 2, Never")
    expect_equal(dim(res), c(10, 3, 3))
    expect_equal(colnames(res), c("Every or nearly every day", "4 to 5 days a week", "Never"))
})

test_that("SortRows",
{
    res <- SortRows(dat, decreasing = TRUE)
    expect_equal(dim(res), c(10, 10))
    expect_equal(rownames(res), rownames(dat)[c(9:1,10)])

    res <- SortRows(tabWithN)
    rownames(res) <- c("65 or more", "40 to 44", "55 to 64", "18 to 24", "25 to 29",
                       "30 to 34", "35 to 39", "45 to 49", "50 to 54", "NET")
})

test_that("HideOutputWithSmallSampleSizes",
{
    expect_silent(HideOutputsWithSmallSampleSizes(tabWithN, 30))
    expect_error(HideOutputsWithSmallSampleSizes(tabWithN, 1000))
})

test_that("HideRowsAndColumnsWithSmallSampleSizes",
{
    expect_silent(HideRowsAndColumnsWithSmallSampleSizes(tabWithN, 30))
    expect_warning(HideRowsAndColumnsWithSmallSampleSizes(tabWithN, 100),
                   "Columns 1,2,5,6,8 have sample size less than 100 and have been removed")
})

test_that("TopKAndSpecifiedRows",
{
    res <- TopKAndSpecifiedRows(tabWithN, 4, "65 or more, NET")
    expect_equal(dim(res), c(6, 8, 3))
    expect_equal(rownames(res), c("18 to 24", "25 to 29", "30 to 34", "35 to 39", "65 or more", "NET"))
})

test_that("LastKRows",
{
    res <- LastKRows(tabWithN, 3)
    expect_equal(dim(res), c(3, 8, 3))
})

test_that("LastKColumns",
{
    res <- LastKColumns(dat, 4)
    expect_equal(colnames(res), c("50 to 54", "55 to 64", "65 or more", "NET"))
    expect_equal(attr(res, "statistic"), "Total %")
})

