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
    c("18 to 24 ", "25 to 29", "30 to 34", "35 to 39", "40 to 44",
    "45 to 49", "50 to 54", "55 to 64", "65 or more", "NET"),
    c("Every or nearly every day", "4 to 5 days a week", "2 to 3 days a week",
    "Once a week", "Once every 2 weeks", "Once a month", "Less than once a month",
    "Never"), c("Column %", "Column n", "Base n")), name = "Age by Exercise frequency",         questions = c("Age", "Exercise frequency"))

set.seed(1234)
array1d <- table(rpois(20, 4))
attr(array1d, "statistic") <- "Count"

test_that("Select Rows",
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

    res <- SelectRows(tabWithN, NULL, NA, NA)
    expect_equal(tabWithN, res)

    res <- SelectRows(tabWithN, "18 to 24, 25 to 29, 3")
    expect_equal(dim(res), c(3, 8, 3))
    expect_equal(rownames(res), c("18 to 24 ", "25 to 29", "30 to 34"))

    res <- SelectRows(tabWithN, first.k = 4, "65 or more, NET")
    expect_equal(dim(res), c(6, 8, 3))
    expect_equal(rownames(res), c("18 to 24 ", "25 to 29", "30 to 34", "35 to 39", "65 or more", "NET"))

    res <- SelectRows(tabWithN, last.k = 3)
    expect_equal(dim(res), c(3, 8, 3))

    tb <- structure(c(2.75482093663912, 6.06060606060606, 12.6721763085399,
18.4573002754821, 24.7933884297521, 15.9779614325069, 6.06060606060606,
8.26446280991736, 4.95867768595041, 100, 3.77906976744186, 15.9883720930233,
7.84883720930233, 18.0232558139535, 19.7674418604651, 13.0813953488372,
10.7558139534884, 4.06976744186047, 6.68604651162791, 100, 3.25318246110325,
10.8910891089109, 10.3253182461103, 18.2461103253182, 22.3479490806223,
14.5685997171146, 8.34512022630834, 6.22347949080622, 5.7991513437058,
100), statistic = "Column %", .Dim = c(10L, 3L), .Dimnames = list(
    c("Less than $15,000", "$15,001 to $30,000", "$30,001 to $45,000",
    "$45,001 to $60,000", "$60,001 to $90,000", "$90,001 to $120,000",
    "$120,001 to $150,000", "$150,001 to $200,000", "$200,001 or more",
    "NET"), c("Male", "Female", "NET")), name = "Income by Gender", questions = c("Income",
"Gender"))
    res <- SelectRows(tb, select = c("Less than $15,000", "$15,001 to $30,000"))
    expect_equal(res, tb[1:2,], check.attributes = FALSE)
})

test_that("Select Columns",
{
    expect_warning(SelectColumns(tabWithN, "Once a week, Twice, Never"), "Table does not contain columns")
    res <- SelectColumns(tabWithN, "1, 2, Never")
    expect_equal(dim(res), c(10, 3, 3))
    expect_equal(colnames(res), c("Every or nearly every day", "4 to 5 days a week", "Never"))
    res <- SelectColumns(dat, last.k = 4)
    expect_equal(colnames(res), c("50 to 54", "55 to 64", "65 or more", "NET"))
    expect_equal(attr(res, "statistic"), "Total %")
})

test_that("SelectEntry",
{
    expect_warning(res <- SelectEntry(dat, 0, "18 to 24", return.single.value = TRUE), "Table does not contain rows '0'")
    expect_equal(res, structure(0, statistic = "%"))
    expect_warning(res <- SelectEntry(dat, 0, "18 to 24", return.single.value = FALSE), "Table does not contain rows '0'")
    expect_equal(res, structure(numeric(0), .Dim = 0:1, .Dimnames = list(NULL, "18 to 24"), statistic = "%",
        name = "Income by Age", questions = c("Income", "Age")))
    expect_error(res <- SelectEntry(dat, "NET", "1,35 to 39, 50 to 54"), NA)
    expect_equal(res, structure(c(0.123055162659123, 0.113154172560113, 0.121640735502122),
        .Dim = c(1L, 3L), .Dimnames = list("NET", c("18 to 24", "35 to 39",
        "50 to 54")), statistic = "%", name = "Income by Age", questions = c("Income",
        "Age")))
    expect_warning(res <- SelectEntry(dat, "NET", ""), "First column was returned as no column was specified")
    expect_equal(res, structure(0.123055162659123, .Dim = c(1L, 1L), .Dimnames = list("NET", "18 to 24"), statistic = "%",
        name = "Income by Age", questions = c("Income", "Age")))
    res <- SelectEntry(dat, 1:3, 1:2, return.single.value = FALSE)
    expect_equal(res,
        structure(c(0.0113154172560113, 0.0099009900990099, 0.00282885431400283,
        0, 0.0099009900990099, 0.00707213578500707), .Dim = 3:2, .Dimnames = list(
        c("Less than $15,000", "$200,001 or more", "$150,001 to $200,000"),
        c("18 to 24", "25 to 29")), statistic = "%", name = "Income by Age",
        questions = c("Income", "Age")))
    res <- SelectEntry(dat, 1:3, 1:2, return.single.value = TRUE)
    expect_equal(round(res,3), structure(0.041, statistic = "%"))
    expect_warning(res <- SelectEntry(tabWithN, "25 to 29  ", "  Never"), "Only the first statistic 'Column %' used")
    expect_equal(res, structure(0.157894736842105, .Dim = c(1L, 1L, 1L),
        .Dimnames = list("25 to 29", "Never", "Column %"), statistic = "%", name = "Age by Exercise frequency",
        questions = c("Age", "Exercise frequency")))
    expect_warning(res <- SelectEntry(tabWithN, "25 to 29  ", "  Never", return.single.value = TRUE),
        "Only the first statistic 'Column %' used")
    expect_equal(attr(res, "statistic"), "%")
    expect_warning(SelectEntry(dat[,1], "Pepsi", "Pepsi"), "Table does not contain row")
})

test_that("Sort Rows",
{
    res <- SortRows(dat, decreasing = TRUE)
    expect_equal(dim(res), c(10, 10))
    expect_equal(rownames(res), rownames(dat)[c(9:1,10)])

    res <- SortRows(tabWithN)
    rownames(res) <- c("65 or more", "40 to 44", "55 to 64", "18 to 24", "25 to 29",
                       "30 to 34", "35 to 39", "45 to 49", "50 to 54", "NET")
})

test_that("Sort Columns",
{
    tb <- structure(c(0, 11.1111111111111, 7.40740740740741, 7.40740740740741,
            3.7037037037037, 14.8148148148148, 11.1111111111111, 18.5185185185185,
            18.5185185185185, 7.40740740740741, 100, 0, 13.0177514792899,
            10.6508875739645, 7.69230769230769, 10.6508875739645, 8.87573964497041,
            10.0591715976331, 11.8343195266272, 17.7514792899408, 9.46745562130178,
            100, 0, 14.5038167938931, 14.5038167938931, 13.7404580152672,
            12.9770992366412, 12.2137404580153, 5.34351145038168, 11.4503816793893,
            12.2137404580153, 3.05343511450382, 100, 0, 13.4556574923547,
            11.9266055045872, 10.0917431192661, 11.0091743119266, 10.7033639143731,
            8.25688073394496, 12.2324159021407, 15.5963302752294, 6.72782874617737,
            100), .Dim = c(11L, 4L), statistic = "Column %", .Dimnames = list(
            c("Less than 18", "18 to 24", "25 to 29", "30 to 34", "35 to 39",
            "40 to 44", "45 to 49", "50 to 54", "55 to 64", "65 or more",
            "NET"), c("I am on a diet, so I tend to watch what I eat and drink",
            "I tend watch what I eat and drink, but don’t consider myself",
            "I typically eat and drink whatever I feel like", "NET")))
    res <- SortColumns(tb[2:10, 1:3], row = "") # match RGui input
    expect_equal(colnames(res), c("I typically eat and drink whatever I feel like",
            "I am on a diet, so I tend to watch what I eat and drink",
            "I tend watch what I eat and drink, but don’t consider myself"))
})

test_that("Reverse rows and columns",
{
    res <- ReverseRows(array1d)
    expect_equal(rownames(res), rev(names(array1d)))
    expect_equal(attr(res, "statistic"), attr(array1d, "statistic"))

    res <- ReverseColumns(tabWithN)
    expect_equal(dim(res), dim(tabWithN))
    expect_equal(colnames(res), rev(colnames(tabWithN)))
    expect_equal(dimnames(res)[[3]], dimnames(tabWithN)[[3]])
})

displayr1d <- structure(c(12.375, 11.75, 10.375, 11.375, 11.625, 7.875, 11.875,
15.75, 7, 100, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800
), .Dim = c(10L, 2L), .Dimnames = list(c("18 to 24", "25 to 29",
"30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
"65 or more", "NET"), c("%", "Sample Size")), name = "table.Age", questions = c("Age",
"SUMMARY"))

displayr2d <- structure(c(11.6455696202532, 11.6455696202532, 10.8860759493671,
9.36708860759494, 12.9113924050633, 8.10126582278481, 10.379746835443,
16.2025316455696, 8.86075949367089, 100, 13.0864197530864, 11.8518518518519,
9.87654320987654, 13.3333333333333, 10.3703703703704, 7.65432098765432,
13.3333333333333, 15.3086419753086, 5.18518518518519, 100, 12.375,
11.75, 10.375, 11.375, 11.625, 7.875, 11.875, 15.75, 7, 100,
395, 395, 395, 395, 395, 395, 395, 395, 395, 395, 405, 405, 405,
405, 405, 405, 405, 405, 405, 405, 800, 800, 800, 800, 800, 800,
800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800,
800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800,
800, 800, 800, 800, 800, 800, 800, 800), .Dim = c(10L, 3L, 3L
), .Dimnames = list(c("18 to 24", "25 to 29", "30 to 34", "35 to 39",
"40 to 44", "45 to 49", "50 to 54", "55 to 64", "65 or more",
"NET"), c("Male", "Female", "NET"), c("Column %", "Column Sample Size",
"Sample Size")), name = "table.Age.by.Gender", questions = c("Age",
"Gender"))


test_that("Checking for small sample sizes",
{
    expect_silent(HideOutputsWithSmallSampleSizes(tabWithN, 30))
    expect_error(HideOutputsWithSmallSampleSizes(tabWithN, 1000))

    expect_silent(HideOutputsWithSmallSampleSizes(displayr1d, 30))
    expect_error(HideOutputsWithSmallSampleSizes(displayr1d, 1000))

    expect_silent(HideColumnsWithSmallSampleSizes(tabWithN, 30))
    expect_warning(HideColumnsWithSmallSampleSizes(tabWithN, 100),
                   "Columns 1,2,5,6,8 have sample size less than 100 and have been removed")

    expect_silent(HideColumnsWithSmallSampleSizes(displayr2d, 30))
    expect_warning(HideColumnsWithSmallSampleSizes(displayr2d, 400),
                   "Columns 1 have sample size less than 400 and have been removed")

    expect_error(HideOutputsWithSmallSampleSizes(1:100, 30))
})

test_that("Automatic order rows/column by CA",
{
    dat <- structure(c(17.6551724137931, 13.6551724137931, 20.2758620689655,
                34.2068965517241, 16.551724137931, 78.3448275862069, 13.9310344827586,
                20.6896551724138, 94.7586206896552, 23.3103448275862, 22.2068965517241,
                37.9310344827586, 37.5172413793103, 25.9310344827586, 45.2413793103448,
                19.7241379310345, 23.8620689655172, 95.448275862069, 16.9655172413793,
                10.2068965517241, 12.2758620689655, 41.7931034482759, 27.7241379310345,
                27.0344827586207, 32.2758620689655, 43.1724137931034, 95.1724137931034,
                17.7931034482759, 11.5862068965517, 20.8275862068966, 37.6551724137931,
                38.2068965517241, 13.9310344827586, 36, 34.7586206896552, 98.7586206896552,
                11.3103448275862, 9.79310344827586, 11.1724137931034, 32.9655172413793,
                36, 20.9655172413793, 36.8275862068965, 51.8620689655172, 96.4137931034483,
                34.2068965517241, 29.1034482758621, 39.448275862069, 26.3448275862069,
                25.5172413793103, 51.1724137931034, 20.8275862068966, 19.1724137931034,
                97.7931034482759, 9.51724137931034, 6.20689655172414, 5.93103448275862,
                47.448275862069, 11.8620689655172, 63.8620689655172, 9.24137931034483,
                45.1034482758621, 98.2068965517241, 15.7241379310345, 35.7241379310345,
                88.2758620689655, 3.58620689655172, 13.5172413793103, 1.93103448275862,
                14.2068965517241, 3.17241379310345, 99.0344827586207, 3.72413793103448,
                2.06896551724138, 2.48275862068966, 35.448275862069, 6.89655172413793,
                71.8620689655172, 4.13793103448276, 43.3103448275862, 98.3448275862069,
                32.9655172413793, 33.2413793103448, 46.4827586206897, 52.1379310344828,
                33.9310344827586, 34.2068965517241, 30.4827586206897, 33.9310344827586,
                97.6551724137931, 10.4827586206897, 9.24137931034483, 9.51724137931034,
                44, 21.5172413793103, 39.1724137931035, 15.1724137931034, 44.2758620689655,
                85.3793103448276, 4.55172413793103, 3.58620689655172, 3.72413793103448,
                62.8965517241379, 16, 60.8275862068966, 10.0689655172414, 53.6551724137931,
                95.7241379310345, 11.3103448275862, 7.72413793103448, 8.27586206896552,
                23.448275862069, 11.1724137931034, 69.5172413793103, 9.51724137931034,
                21.1034482758621, 91.0344827586207, 22.6206896551724, 32.1379310344828,
                65.3793103448276, 24.551724137931, 22.0689655172414, 19.3103448275862,
                18.2068965517241, 15.5862068965517, 95.8620689655172, 11.1724137931034,
                9.24137931034483, 10.2068965517241, 48.551724137931, 16.2758620689655,
                46.0689655172414, 12.551724137931, 42.6206896551724, 94.8965517241379,
                10.7586206896552, 8.27586206896552, 8.27586206896552, 44.6896551724138,
                20.4137931034483, 57.6551724137931, 12.1379310344828, 43.8620689655172,
                95.448275862069, 14.3448275862069, 12.2758620689655, 13.3793103448276,
                49.2413793103448, 25.6551724137931, 37.2413793103448, 23.1724137931034,
                50.0689655172414, 98.2068965517241, 37.6551724137931, 37.3793103448276,
                57.3793103448276, 40.2758620689655, 37.5172413793103, 49.7931034482759,
                33.3793103448276, 35.0344827586207, 94.7586206896552, 63.3103448275862,
                72, 24.2758620689655, 6.48275862068965, 28, 4.96551724137931,
                47.1724137931034, 11.3103448275862, 92.6896551724138, 93.6551724137931,
                93.3793103448276, 97.9310344827586, 98.2068965517241, 95.0344827586207,
                99.0344827586207, 94.4827586206897, 98.0689655172414, 99.0344827586207
                ), .Dim = c(9L, 20L), statistic = "%", .Dimnames = list(c("AAPT/Cellular One",
                "New Tel", "One-tel", "Optus", "Orange (Hutchison)", "Telstra (Mobile Net)",
                "Virgin Mobile", "Vodafone", "NET"), c("Bureaucratic", "Slow service",
                "Friendly", "Low prices", "Fashionable", "Unfashionable", "Reliable",
                "Here today, gone tomorrow", "Good coverage", "Network often down",
                "The best phones", "Conveniently located stores", "High prices",
                "Unreliable", "Meet all my communication needs", "Leaders in mobile phone technology",
                "I like them", "I hate them", "Don't know much about them", "NET"
                )), name = "q20", questions = c("q20", "SUMMARY"))
    expect_silent(resR <- AutoOrderRows(dat[-9,-20]))
    expect_silent(resC <- AutoOrderColumns(dat[-9,-20]))
    expect_equal(rownames(resR), c("One-tel", "New Tel", "AAPT/Cellular One", "Virgin Mobile",
        "Orange (Hutchison)", "Optus", "Vodafone", "Telstra (Mobile Net)"))
    expect_equal(colnames(resC), c("Here today, gone tomorrow", "Don't know much about them",
        "Unreliable", "I hate them", "Unfashionable", "Network often down",
        "Slow service", "Low prices", "Fashionable", "Friendly", "Bureaucratic",
        "I like them", "The best phones", "Meet all my communication needs",
        "High prices", "Leaders in mobile phone technology", "Reliable",
        "Conveniently located stores", "Good coverage"))
})

datNA <- structure(list(V1 = c(11711, 93, NaN, 2762, NaN, NaN, NaN, NaN,
    NaN, NaN, NaN, NaN, NaN, NaN), `brand:datacracker` = c(NaN, NaN,
    NaN, NaN, NaN, NaN, NaN, 5, NaN, NaN, NaN, NaN, NaN, NaN), `brand:datacracker, dispatch:chrisfacer` = c(NaN,
    NaN, NaN, NaN, NaN, NaN, NaN, NaN, 93, NaN, 259, NaN, NaN, 90
    ), `brand:datacracker, dispatch:chrisfacer, dispatch:timali` = c(NaN,
    NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 1, NaN, NaN, NaN, NaN
    ), `brand:datacracker, dispatch:chrisfacer, dispatch:timbock` = c(NaN,
    NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 627, NaN, 1002
    ), `brand:datacracker, dispatch:mattiasengdahl` = c(NaN, NaN,
    NaN, NaN, NaN, NaN, NaN, NaN, 594, 8, 16, 347, 22, NA), `brand:datacracker, dispatch:mattiasengdahl, dispatch:timali` = c(NaN,
    NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 60, NaN, NaN, 68, NaN
    ), `brand:datacracker, dispatch:mattiasengdahl, dispatch:timali, dispatch:timbock` = c(NaN,
    NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 489, NaN
    ), `brand:datacracker, dispatch:mattsteele, dispatch:timbock` = c(NaN,
    NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NA
    ), `brand:datacracker, dispatch:timali` = c(NaN, NaN, NaN, NaN,
    NaN, NaN, NaN, NaN, NaN, 180, 26, 134, 130, 220), `brand:datacracker, dispatch:timali, dispatch:timbock` = c(NaN,
    NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 460
    ), `brand:datacracker, dispatch:timbock` = c(NaN, NaN, NaN, NaN,
    NaN, NaN, NaN, 169, 171, 325, 193, 1158, 1583, 664), `brand:displayr, dispatch:chrisfacer` = c(NaN,
    NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 111, NaN, NaN
    ), `brand:displayr, dispatch:chrisfacer, dispatch:timbock` = c(NaN,
    NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 150, NaN, NaN
    ), `brand:displayr, dispatch:mattiasengdahl` = c(NaN, NaN, NaN,
    NaN, NaN, NaN, NaN, 4, NaN, NaN, NaN, NaN, NaN, 2925), `brand:displayr, dispatch:timali` = c(NaN,
    NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 137, NaN, 2, NaN, NaN
    ), `brand:displayr, dispatch:timbock` = c(NaN, NaN, NaN, NaN,
    NaN, NaN, NaN, NaN, NaN, 157, 600, NaN, NaN, 423), `dispatch:chrisfacer` = c(NaN,
    43, 1034, 9, 156, 47, 1004, NaN, NaN, NaN, NaN, NaN, NaN, NaN
    ), `dispatch:chrisfacer, dispatch:mattiasengdahl` = c(NaN, NaN,
    657, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN),
    `dispatch:chrisfacer, dispatch:timbock` = c(NaN, NaN, NaN,
    2532, NaN, 3, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN), `dispatch:mattiasengdahl` = c(NaN,
    163, 2730, 411, 2, 22, 50, 9, NaN, NaN, NaN, NaN, NaN, NaN
    ), `dispatch:mattiasengdahl, dispatch:timbock` = c(NaN, 2344,
    NaN, NaN, NaN, NaN, 249, NaN, NaN, NaN, NaN, NaN, NaN, NaN
    ), `dispatch:timali` = c(NaN, 1737, 602, 3007, 2218, 2106,
    57, NaN, NaN, NaN, NaN, NaN, NaN, NaN), `dispatch:timbock` = c(NaN,
    113, 1938, 1028, 1666, 306, 1067, NaN, NaN, NaN, NaN, NaN,
    NaN, NaN)), .Names = c("V1", "brand:datacracker", "brand:datacracker, dispatch:chrisfacer",
    "brand:datacracker, dispatch:chrisfacer, dispatch:timali", "brand:datacracker, dispatch:chrisfacer, dispatch:timbock",
    "brand:datacracker, dispatch:mattiasengdahl", "brand:datacracker, dispatch:mattiasengdahl, dispatch:timali",
    "brand:datacracker, dispatch:mattiasengdahl, dispatch:timali, dispatch:timbock",
    "brand:datacracker, dispatch:mattsteele, dispatch:timbock", "brand:datacracker, dispatch:timali",
    "brand:datacracker, dispatch:timali, dispatch:timbock", "brand:datacracker, dispatch:timbock",
    "brand:displayr, dispatch:chrisfacer", "brand:displayr, dispatch:chrisfacer, dispatch:timbock",
    "brand:displayr, dispatch:mattiasengdahl", "brand:displayr, dispatch:timali",
    "brand:displayr, dispatch:timbock", "dispatch:chrisfacer", "dispatch:chrisfacer, dispatch:mattiasengdahl",
    "dispatch:chrisfacer, dispatch:timbock", "dispatch:mattiasengdahl",
    "dispatch:mattiasengdahl, dispatch:timbock", "dispatch:timali",
    "dispatch:timbock"), class = "data.frame", row.names = c("2017-02-12",
    "2017-03-05", "2017-03-12", "2017-03-19", "2017-03-26", "2017-04-02",
    "2017-04-09", "2017-04-16", "2017-04-23", "2017-04-30", "2017-05-07",
    "2017-05-14", "2017-05-21", "2017-05-28"))

test_that("Sorting NAs",
{
    res <- SortColumns(HideEmptyColumns(datNA[1:5,]), row = 2, exclude = "1")
    expect_equal(colnames(res), c("dispatch:chrisfacer", "dispatch:timbock", "dispatch:mattiasengdahl",
"dispatch:timali", "dispatch:mattiasengdahl, dispatch:timbock",
"dispatch:chrisfacer, dispatch:mattiasengdahl", "dispatch:chrisfacer, dispatch:timbock",
"V1"))


    res <- SortRows(HideEmptyColumns(datNA[1:5,]), column = 2, exclude = "5")
    expect_equal(rownames(res), c("2017-03-19", "2017-03-05", "2017-03-12", "2017-02-12", "2017-03-26"))
})

df <- structure(list(Name = c("Apple", "Dried fruit", "Carrot", "Berry"
), Date = structure(c(17471, 17622, 17896, 17712), class = "Date"),
    Score = c(4, 8, 3, 5)), .Names = c("Name", "Date", "Score"
), row.names = c("A", "B", "C", "D"), class = "data.frame")

test_that("Sorting non-numeric data",
{
    res1 <- SortRows(df)
    expect_equal(rownames(res1), c("C", "A", "D", "B"))
    res2 <- SortRows(df, column = 1)
    expect_equal(rownames(res2), c("A", "D", "C", "B"))
    res3 <- SortRows(df, column = 2)
    expect_equal(rownames(res3), c("A", "B", "D", "C"))

    res4 <- SortColumns(df)
    expect_equal(colnames(res4), c("Date", "Score", "Name"))
    res5 <- SortColumns(df, row = 2)
    expect_equal(colnames(res5), c("Date", "Score", "Name"))
})
