context("TidyTools")

vals <- c(
    1.1315417256, 0.9900990099, 0.2828854314, 0.5657708628, 1.6973125884,
    2.1216407355, 1.1315417256, 2.5459688826, 1.8387553041, 12.3055162659,
    0, 0.9900990099, 0.7072135785, 0.8486562942, 0.5657708628, 0.7072135785,
    3.9603960396, 1.2729844413, 2.5459688826, 11.5983026874, 0, 0.8486562942,
    0.7072135785, 1.4144271570, 0.8486562942, 0, 1.6973125884, 2.9702970297,
    1.9801980198, 10.4667609618, 0.2828854314, 0.4243281471, 1.8387553041,
    0.5657708628, 0.9900990099, 0.8486562942, 2.6874115983, 1.8387553041,
    1.8387553041, 11.3154172560, 0, 0.8486562942, 0.4243281471, 0.8486562942,
    0.4243281471, 1.8387553041, 0.9900990099, 1.4144271570, 4.5261669024,
    11.3154172560, 0, 0, 0.5657708628, 1.1315417256, 0.8486562942, 0.4243281471,
    0.4243281471, 2.2630834512, 1.9801980198, 7.6379066478, 0, 0.8486562942,
    0.2828854314, 0.8486562942, 1.2729844413, 0.9900990099, 2.2630834512,
    1.9801980198, 3.6775106082, 12.1640735502, 1.5558698727, 0.8486562942,
    1.4144271570, 2.1216407355, 2.6874115983, 2.2630834512, 1.4144271570,
    0.8486562942, 3.6775106082, 16.8316831683, 0.2828854314, 0, 0, 0, 0.9900990099,
    1.6973125884, 0, 3.1117397454, 0.2828854314, 6.3649222065, 3.2531824611,
    5.7991513437, 6.2234794908, 8.3451202263, 10.3253182461, 10.8910891089,
    14.5685997171, 18.2461103253, 22.3479490806, 100
)
dat.names <- list(
    c("Less than $15,000", "$200,001 or more", "$150,001 to $200,000",
      "$120,001 to $150,000", "$30,001 to $45,000", "$15,001 to $30,000",
      "$90,001 to $120,000", "$45,001 to $60,000", "$60,001 to $90,000", "NET"),
    c("18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49",
      "50 to 54", "55 to 64", "65 or more", "NET")
)
dat <- structure(
    vals,
    dim = c(10L, 10L),
    statistic = "Total %",
    dimnames = dat.names,
    name = "Income by Age",
    questions = c("Income", "Age")
)
# This is QTable with the Base n statistic
# Note that 'Statistics - Right' and 'Statistics - Below' are never passed
# to the R output
tab.vals <- c(
    12.2448979591, 6.1224489795, 4.0816326530, 6.1224489795, 4.0816326530,
    8.1632653061, 22.4489795918, 19.3877551020, 17.3469387755, 100, 32.2033898305,
    13.5593220338, 5.0847457627, 10.1694915254, 5.0847457627, 0, 5.0847457627,
    13.5593220338, 15.2542372881, 100, 11.8811881188, 12.8712871287, 12.8712871287,
    13.3663366336, 8.4158415841, 13.3663366336, 13.3663366336, 8.9108910891,
    4.9504950495, 100, 10.8843537414, 17.6870748299, 11.5646258503, 7.4829931972,
    16.3265306122, 3.4013605442, 6.1224489795, 22.4489795918, 4.0816326530,
    100, 12.5, 6.25, 18.75, 6.25, 9.375, 12.5, 18.75, 15.625, 0, 100, 3.5714285714,
    19.6428571428, 8.9285714285, 16.0714285714, 26.7857142857, 5.3571428571,
    10.7142857142, 8.9285714285, 0, 100, 13.0769230769, 2.3076923076, 12.3076923076,
    20, 13.8461538461, 6.9230769230, 11.5384615384, 12.3076923076, 7.6923076923,
    100, 6.5789473684, 15.7894736842, 7.8947368421, 5.2631578947, 11.8421052631,
    9.2105263157, 9.2105263157, 28.9473684210, 5.2631578947, 100, 98, 98,
    98, 98, 98, 98, 98, 98, 98, 98, 59, 59, 59, 59, 59, 59, 59, 59, 59,
    59, 202, 202, 202, 202, 202, 202, 202, 202, 202, 202, 147, 147, 147,
    147, 147, 147, 147, 147, 147, 147, 32, 32, 32, 32, 32, 32, 32, 32, 32,
    32, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 130, 130, 130, 130, 130,
    130, 130, 130, 130, 130, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 800,
    800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800,
    800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800,
    800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800,
    800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800,
    800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800,
    800, 800, 800, 800, 800, 800, 800, 800, 800
)
tab.names <- list(
    c("18 to 24 ", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54",
      "55 to 64", "65 or more", "NET"),
    c("Every or nearly every day", "4 to 5 days a week", "2 to 3 days a week", "Once a week",
      "Once every 2 weeks", "Once a month", "Less than once a month", "Never"),
    c("Column %", "Column n", "Base n")
)
tabWithN <- structure(
    tab.vals,
    dim = c(10L, 8L, 3L),
    dimnames = tab.names,
    name = "Age by Exercise frequency",
    questions = c("Age", "Exercise frequency")
)

set.seed(1234)
array1d <- table(rpois(20, 4))
attr(array1d, "statistic") <- "Count"

test_that("Select Rows",
{
    xx <- matrix(1:12, nrow = 3, ncol = 4,
                 dimnames = list(LETTERS[1:3], letters[1:4]))
    x2 <- SelectRows(xx, "A,A,C")
    expect_equal(x2[1, ], x2[2, ])
    expect_warning(SelectRows(xx, "A,B,D,E,F"),
                   "Table does not contain rows 'D','E','F'.")
    expect_error(SelectRows(xx, "D,E,F"),
                 "Table does not contain rows 'D','E','F'.")
    x4 <- SelectRows(xx, "A,B,C,3,2,1")
    expect_warning(x5 <- SelectRows(xx, "A,B,C,3,2,1,0,7"),
                   "Table does not contain rows '0','7'.")
    expect_equal(x5[, 1], setNames(c(1:3, 3:1), LETTERS[c(1:3, 3:1)]))

    res <- SelectRows(tabWithN, NULL, NA, NA)
    expect_equal(tabWithN, res)

    res <- SelectRows(tabWithN, "18 to 24, 25 to 29, 3")
    expect_equal(dim(res), c(3, 8, 3))
    expect_equal(rownames(res), c("18 to 24 ", "25 to 29", "30 to 34"))

    res <- SelectRows(tabWithN, first.k = 4, "65 or more, NET")
    expect_equal(dim(res), c(2, 8, 3))
    expect_equal(rownames(res), c("65 or more", "NET"))

    res <- SelectRows(tabWithN, last.k = 3)
    expect_equal(dim(res), c(3, 8, 3))

    tb.vals <- c(
        2.7548209366, 6.0606060606, 12.6721763085, 18.4573002754, 24.7933884297,
        15.9779614325, 6.0606060606, 8.2644628099, 4.9586776859, 100, 3.7790697674,
        15.9883720930, 7.8488372093, 18.0232558139, 19.7674418604, 13.0813953488,
        10.7558139534, 4.0697674418, 6.6860465116, 100, 3.2531824611, 10.8910891089,
        10.3253182461, 18.2461103253, 22.3479490806, 14.5685997171, 8.3451202263,
        6.2234794908, 5.7991513437, 100
    )
    tb.names <- list(
        c("Less than $15,000", "$15,001 to $30,000", "$30,001 to $45,000",
          "$45,001 to $60,000", "$60,001 to $90,000", "$90,001 to $120,000",
          "$120,001 to $150,000", "$150,001 to $200,000", "$200,001 or more",
          "NET"),
        c("Male", "Female", "NET")
    )
    tb <- structure(
        tb.vals,
        statistic = "Column %",
        dim = c(10L, 3L),
        dimnames = tb.names,
        name = "Income by Gender",
        questions = c("Income", "Gender")
    )
    res <- SelectRows(tb, select = c("Less than $15,000", "$15,001 to $30,000"))
    expect_equal(res, tb[1:2, ], check.attributes = FALSE)

    res <- SelectRows(tb, first.k = 1, last.k = 2)
    expect_equal(rownames(res), c("Less than $15,000", "$200,001 or more", "NET"))

    res <- SelectRows(tb, c("$15,001 to $30,000", "$30,001 to $45,000",
                "45,001 to $60,000"), first.k = 2)
    expect_equal(rownames(res), c("$15,001 to $30,000", "$30,001 to $45,000"))
})
flavours.vals <- c(
    0.6587078651, 0.4171348314, 0.6011235955, 0.3988764044, 0.4325842696,
    0.3904494382, 0.375, 0.3146067415, 0.2668539325, 0.1657303370, 0.1629213483
)
flavours.names <- list(
    c("Flavors: Classic Bubble Gum", "Flavors: Super-Strong Bubble Gum",
      "Flavors: Grape", "Flavors: Orange", "Flavors: Sour", "Flavors: Strawberry",
      "Flavors: Cola", "Flavors: Apple", "Flavors: Chocolate", "Flavors: Peach",
      "Flavors: Watermelon"),
    NULL
)
flavours <- structure(
    flavours.vals,
    dim = c(11L, 1L),
    dimnames = flavours.names,
    statistic = "Average"
)

test_that("Partial matches",
{
    expect_warning(SelectRows(flavours, c("Apple", "Sour", "Flavors: Classic", "Flavors")),
        "'Flavors' matched multiple values")
    expect_error(SelectRows(flavours, "Bubble Gum"), "'Bubble Gum' matched multiple values")
    expect_error(SelectRows(flavours, "Pineapple"), "Table does not contain row 'Pineapple'")
    rownames(flavours)[3] <- "9"
    expect_warning(SelectRows(flavours, "9"), "'9' treated as an index")
})

test_that("Select Columns",
{
    expect_warning(SelectColumns(tabWithN, "Once a week, Twice, Never"),
                   "Table does not contain column 'Twice'")
    expect_warning(SelectColumns(tabWithN, "Once a week, Twice, Never,Blah"),
                   "Table does not contain columns 'Twice','Blah'")
    res <- SelectColumns(tabWithN, "1, 2, Never")
    expect_equal(dim(res), c(10, 3, 3))
    expect_equal(colnames(res), c("Every or nearly every day", "4 to 5 days a week", "Never"))
    res <- SelectColumns(dat, last.k = 4)
    expect_equal(colnames(res), c("50 to 54", "55 to 64", "65 or more", "NET"))
    expect_equal(attr(res, "statistic"), "Total %")
    # Edge cases
    expect_equal(SelectColumns(array1d), array1d)
    expect_equal(SelectColumns(tabWithN, select = NULL), tabWithN)
    expect_equal(SelectColumns(tabWithN, select = ""), tabWithN)
})

test_that("SelectEntry",
{
    expect_error(SelectEntry(dat, 1, "19 to 25"), "Table does not contain column")
    expect_error(res <- SelectEntry(dat, 1, "18 to 24", return.single.value = TRUE), NA)
    expect_equal(res, structure(0.0113154172560113, format = "%"))
    expect_error(res <- SelectEntry(dat, 1, "18 to 24", return.single.value = TRUE,
        use.statistic.attribute = TRUE), NA)
    expect_equal(res, structure(1.13154172560113, statistic = "%"))

    expect_error(res <- SelectEntry(dat, 1, "18 to 24", return.single.value = FALSE), NA)
    expect_equal(res, structure(0.0113154172560113, .Dim = c(1L, 1L),
        .Dimnames = list("Less than $15,000", "18 to 24"), format = "%",
        name = "Income by Age", questions = c("Income", "Age")))
    expect_error(res <- SelectEntry(dat, 1, "18 to 24", return.single.value = FALSE,
        use.statistic.attribute = TRUE), NA)
    expect_equal(res, structure(1.13154172560113, .Dim = c(1L, 1L),
        .Dimnames = list("Less than $15,000", "18 to 24"), statistic = "%",
        name = "Income by Age", questions = c("Income", "Age")))

    expect_error(res <- SelectEntry(dat, "NET", "1,35 to 39, 50 to 54"), NA)
    expect_equal(res, structure(c(0.123055162659123, 0.113154172560113, 0.121640735502122),
        .Dim = c(1L, 3L), .Dimnames = list("NET", c("18 to 24", "35 to 39",
        "50 to 54")), format = "%", name = "Income by Age", questions = c("Income",
        "Age")))
    expect_error(res <- SelectEntry(dat, "NET", "1,35 to 39, 50 to 54", use.statistic.attribute = TRUE), NA)
    expect_equal(res, structure(c(0.123055162659123, 0.113154172560113, 0.121640735502122) * 100,
        .Dim = c(1L, 3L), .Dimnames = list("NET", c("18 to 24", "35 to 39",
        "50 to 54")), statistic = "%", name = "Income by Age", questions = c("Income",
        "Age")))
    expect_warning(res <- SelectEntry(dat, "NET", ""), "First column was returned as no column was specified")

    expect_warning(res <- SelectEntry(dat, "NET", ""), "First column was returned as no column was specified")
    expect_equal(res, structure(0.123055162659123, .Dim = c(1L, 1L), .Dimnames = list("NET",
        "18 to 24"), format = "%", name = "Income by Age", questions = c("Income", "Age")))
    expect_warning(res <- SelectEntry(dat, "NET", "", use.statistic.attribute = TRUE),
        "First column was returned as no column was specified")
    expect_equal(res, structure(12.3055162659123, .Dim = c(1L, 1L), .Dimnames = list("NET",
        "18 to 24"), statistic = "%", name = "Income by Age", questions = c("Income", "Age")))
    res <- SelectEntry(dat, 1:3, 1:2, return.single.value = FALSE)
    expect_equal(res,
        structure(c(0.0113154172560113, 0.0099009900990099, 0.00282885431400283,
        0, 0.0099009900990099, 0.00707213578500707), name = "Income by Age",
        questions = c("Income", "Age"), .Dim = 3:2, .Dimnames = list(
        c("Less than $15,000", "$200,001 or more", "$150,001 to $200,000"),
        c("18 to 24", "25 to 29")), format = "%"))
    res <- SelectEntry(dat, 1:3, 1:2, return.single.value = TRUE)
    expect_equal(round(res,3), structure(0.041, format = "%"))
    res <- SelectEntry(dat, 1:3, 1:2, return.single.value = TRUE, use.statistic.attribute = TRUE)
    expect_equal(round(res,1), structure(4.1, statistic = "%"))
    expect_warning(res <- SelectEntry(tabWithN, "25 to 29  ", "  Never"),
        "Only the first statistic 'Column %' used")
    expect_equal(res, structure(0.157894736842105, .Dim = c(1L, 1L, 1L),
        .Dimnames = list("25 to 29", "Never", "Column %"), format = "%",
        name = "Age by Exercise frequency", questions = c("Age", "Exercise frequency")))
    expect_warning(res <- SelectEntry(tabWithN, "25 to 29  ", "  Never",
        use.statistic.attribute = TRUE), "Only the first statistic 'Column %' used")
    expect_equal(res, structure(15.7894736842105, .Dim = c(1L, 1L, 1L),
        .Dimnames = list("25 to 29", "Never", "Column %"), statistic = "%",
        name = "Age by Exercise frequency", questions = c("Age", "Exercise frequency")))
    expect_warning(res <- SelectEntry(tabWithN, "25 to 29  ", "  Never", return.single.value = TRUE),
        "Only the first statistic 'Column %' used")
    expect_equal(res, structure(0.157894736842105, format = "%"))
    expect_warning(res <- SelectEntry(tabWithN, "25 to 29  ", "  Never", return.single.value = TRUE,
        use.statistic.attribute = TRUE), "Only the first statistic 'Column %' used")
    expect_equal(attr(res, "statistic"), "%")
    expect_error(SelectEntry(dat[,1], "Pepsi", "Pepsi"), "Table does not contain row")
    arr.test <- setNames(1:10, letters[1:10])
    expect_warning(SelectEntry(arr.test, 1:5, column = "2"),
                   "Column 2 ignored for a 1-dimensional table")
    expect_warning(SelectEntry(arr.test, 1:5, column = "B"),
                   "Column B ignored for a 1-dimensional table")
    expect_setequal(capture_warnings(SelectEntry(arr.test, 1:5, column = c("B", NA))),
                    c("NAs introduced by coercion", "Columns B, NA ignored for a 1-dimensional table"))
})

test_that("Sort Rows",
{
    res <- SortRows(dat, decreasing = TRUE)
    expect_equal(dim(res), c(10, 10))
    expect_equal(rownames(res), rownames(dat)[c(9:1, 10)])

    expect_warning(res <- SortRows(tabWithN), "duplicates")
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
    expect_equal(ReverseColumns(array1d), array1d)
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

tb2 <- structure(c(51.9472021093372, NA, 35.5308609996093, 45.6738258412044,
    58.4905809953716, 49.6455398756096, 50.2632940710299, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, 49.5650983155624, 47.843121822468,
    37.4419352342298, NA, NA, 49.7070348252363, 52.5664287913937,
    46.9122328370734, 47.4781018184579, 58.5332573349286, 75.9701034675582,
    76.1366780842766, 73.6532474371167, 49.3122031689706, 60.8751983368752,
    51.0820316717213, 34.7633413573599, NA, 44.5224347195836, 53.5852974197742,
    30.6667081176553, NA, NA, 53.8358070952296, 52.0786814187607,
    34.2033866453389, 49.3459942633275, 60.1021357034282, 77.2176106035434,
    76.2831307305653, 73.5481747803698, 48.240508741376, 61.8350270978055,
    53.9426371666606, 38.0600345420049, NA, 47.6939625925842, 52.2611605052822,
    36.7888493925003, NA, NA, 53.2248117691342, 52.8740811358816,
    41.3616321711294, 51.8001631362509, 63.5878465004998, 77.0496074348522,
    75.9816278787044, 72.5165550574067, 48.7459638243879, 62.798402301744,
    54.1707117249785, 37.8565225376816, NA, 1055, 0, 1055, 1055,
    1055, 1055, 1055, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1004, 1004,
    1004, 0, 0, 1004, 1004, 1004, 1004, 1004, 1004, 1004, 1004, 1004,
    1004, 1004, 1004, 0, 1033, 1033, 1033, 0, 0, 1033, 1033, 1033,
    1033, 1033, 1033, 1033, 1033, 1033, 1033, 1033, 1033, 0, 1026,
    1026, 1026, 0, 0, 1026, 1026, 1026, 1026, 1026, 1026, 1026, 1026,
    1026, 1026, 1026, 1026, 0), .Dim = c(18L, 4L, 2L),
    .Dimnames = list(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K",
    "L", "M", "N", "O", "P", "Q", "R"), c("May", "June", "July",
    "October"), c("Column %", "Column n")),
    basedescriptiontext = "base n = from 0 to 4118; total n = 4118; 4118 missing; effective sample size = 4295 (104%); 93% filtered out",
    basedescription = list(Minimum = 0L, Maximum = 4118L, Range = TRUE, Total = 4118L,
    Missing = 4118L, EffectiveSampleSize = 4295L, EffectiveSampleSizeProportion = 104,
    FilteredProportion = 92.5136800770811), questiontypes = c("PickAny","PickOne"))

tb1d.with.N <- structure(c(19.1402714932127, 21.3457446808511, 13.1246105919003,
14.8669871794872, 69.3826247689464, 663, 564, 642, 624, 541), .Dim = c(5L,
2L), .Dimnames = list(c("Optus network fails", "Orange network fails",
"Telstra network fails", "Vodafone network fails", "SUM"), c("Average",
"Sample Size")), basedescriptiontext = "sample size = from 541 to 663; total sample size = 725; 184 missing", basedescription = list(
    Minimum = 541L, Maximum = 663L, Range = TRUE, Total = 725L,
    Missing = 184L, EffectiveSampleSize = 663L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), questiontypes = "NumberMulti", span = list(
    rows = structure(list(c("Optus network fails", "Orange network fails",
    "Telstra network fails", "Vodafone network fails", "SUM")), class = "data.frame", .Names = "", row.names = c(NA,
    5L))), name = "table.Percieved.proportion.of.time.the", questions = c("Percieved proportion of time the",
"SUMMARY"))


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

    expect_silent(res <- HideValuesWithSmallSampleSizes(displayr2d, 400))
    expect_true(all(is.na(res[,1,1])))
    expect_true(all(!is.na(res[,2,1])))

    expect_silent(res <- HideValuesWithSmallSampleSizes(displayr1d, 30))
    expect_true(all(!is.na(res[,,1])))
    expect_silent(res <- HideValuesWithSmallSampleSizes(displayr1d, 900))
    expect_true(all(is.na(res[,,1])))

    res <- HideValuesWithSmallSampleSizes(tb2, 30)
    expect_equal(res[,,1],
        structure(c(51.9472021093372, NA, 35.5308609996093, 45.6738258412044,
        58.4905809953716, 49.6455398756096, 50.2632940710299, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, 49.5650983155624, 47.843121822468,
        37.4419352342298, NA, NA, 49.7070348252363, 52.5664287913937,
        46.9122328370734, 47.4781018184579, 58.5332573349286, 75.9701034675582,
        76.1366780842766, 73.6532474371167, 49.3122031689706, 60.8751983368752,
        51.0820316717213, 34.7633413573599, NA, 44.5224347195836, 53.5852974197742,
        30.6667081176553, NA, NA, 53.8358070952296, 52.0786814187607,
        34.2033866453389, 49.3459942633275, 60.1021357034282, 77.2176106035434,
        76.2831307305653, 73.5481747803698, 48.240508741376, 61.8350270978055,
        53.9426371666606, 38.0600345420049, NA, 47.6939625925842, 52.2611605052822,
        36.7888493925003, NA, NA, 53.2248117691342, 52.8740811358816,
        41.3616321711294, 51.8001631362509, 63.5878465004998, 77.0496074348522,
        75.9816278787044, 72.5165550574067, 48.7459638243879, 62.798402301744,
        54.1707117249785, 37.8565225376816, NA), .Dim = c(18L, 4L), .Dimnames = list(
        c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K",
        "L", "M", "N", "O", "P", "Q", "R"), c("May", "June", "July",
        "October"))))
    expect_equal(res[,,2], tb2[,,2])

    res <- HideValuesWithSmallSampleSizes(tb1d.with.N, min.size = 600)
    expect_equal(res[,,1], c(`Optus network fails` = 19.1402714932127,
        `Orange network fails` = NA, `Telstra network fails` = 13.1246105919003,
        `Vodafone network fails` = 14.8669871794872, SUM = NA))
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
    expect_error(AutoOrderColumns(LifeCycleSavings), NA)
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
    expect_warning(res <- SortColumns(HideEmptyColumns(datNA[1:5,]), row = 2, exclude = "1"),
        "Table has been sorted on row 2 containing 7 values with 2 NAs")
    expect_equal(colnames(res), c("dispatch:chrisfacer", "dispatch:timbock", "dispatch:mattiasengdahl",
"dispatch:timali", "dispatch:mattiasengdahl, dispatch:timbock",
"dispatch:chrisfacer, dispatch:mattiasengdahl", "dispatch:chrisfacer, dispatch:timbock",
"V1"))


    expect_warning(res <- SortRows(HideEmptyColumns(datNA[1:5,]), column = 2, exclude = "5"),
        "Table has been sorted on column 2 containing 4 values with 1 NA")
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

test_that("Check for matches that contain commas",
{
    xx <- structure(4:1, .Names = c("E", "D", "C", "A,B"))
    expect_equal(SelectRows(xx, "A,B,D"),
           structure(c(1L, 3L), .Dim = 2:1, .Dimnames = list(c("A,B", "D"), NULL)))

})

test_that("Check that partial matches are only used for unmatched rownames",
{
    xx <- structure(4:1, .Names = c("Blue", "Dark Red", "Green Blue", "Yellow"))
    res <- expect_warning(SelectRows(xx, "Red,Dark Red,Blue"),
                "Table does not contain row 'Red'", fixed = TRUE)
    expect_equal(res, structure(3:4, .Dim = 2:1, .Dimnames = list(c("Dark Red", "Blue"), NULL)))
})

test_that("DS-4298: Don't split strings from Displayr controls", {
    test.names <- c("A, B", "A", "B", "C")
    control.string <- "A, B"
    attr(control.string, "is.control") <- TRUE
    expect_equal(getMatchIndex("A, B", test.names), c(2,3))
    expect_equal(getMatchIndex(control.string, test.names), 1)
})

test_that("DS-3886: Only CopyAttributes if not a Q Table", {
    vals <- c(`15-18` = 9.91, `19 to 24` = 17.39, `25 to 29` = 11.00, `30 to 34` = 14.63,
              `35 to 39` = 16.01, `40 to 44` = 17.06, `45 to 49` = 13.99, NET = 100)
    q.stat <- data.frame(significancearrowratio = c(1, 1, 1, 0, 0.58, 1, 0, 1),
                         significancedirection = c("Down", "Up", "Down", "None", "Up", "Up", "None", "Up"),
                         significancefontsizemultiplier = c(0.20, 4.89, 0.20, 1, 3.29, 4.89, 1, 4.89),
                         significanceissignificant = as.logical(c(1L, 1L, 1L, 0L, 1L, 1L, 0L, 1L)),
                         zstatistic = c(-8.70, 6.18, -6.53, 0.68, 3.43, 5.52, -0.58, 170.63),
                         pcorrected = c(0, 0, 0, 0.69, 0.0008, 0, 0.78, 0))
    single.dim.table <- structure(vals,
        statistic = "%",
        dim = 8L,
        dimnames = list(names(vals)),
        class = c("array", "qTable"),
        dimnets = list(8L),
        dimduplicates = list(8L),
        span = list(rows = data.frame(names(vals), fix.empty.names = FALSE)),
        basedescriptiontext = "base n = 4853",
        basedescription = list(Minimum = 4853L, Maximum = 4853L, Range = FALSE, Total = 4853L,
                               Missing = 0L, EffectiveSampleSize = 4853L,
                               EffectiveSampleSizeProportion = 100, FilteredProportion = 0),
        QStatisticsTestingInfo = q.stat,
        questiontypes = "PickOne",
        footerhtml = paste0("Total sample; Unweighted; base n = 4853; Multiple comparison correction: ",
                            "False Discovery Rate (FDR) (p = 0.05)"),
        name = "table.S1.Age",
        questions = c("S1 Age", "SUMMARY"))
    expected.tab.as.mat <- single.dim.table
    attr(expected.tab.as.mat, "dim") <- c(length(vals), 1L)
    attr(expected.tab.as.mat, "dimnames") <- list(names(vals), NULL)
    class(expected.tab.as.mat) <- c("qTable", "matrix", "array")
    expect_equal(convertToMatrix(single.dim.table), expected.tab.as.mat)
})

test_that("DS-3886: Conversion to 3d table", {
    vals <- c(`15-18` = 9.91, `19 to 24` = 17.39, `25 to 29` = 11.00, `30 to 34` = 14.63,
              `35 to 39` = 16.01, `40 to 44` = 17.06, `45 to 49` = 13.99, NET = 100)
    q.stat <- data.frame(significancearrowratio = c(1, 1, 1, 0, 0.58, 1, 0, 1),
                         significancedirection = c("Down", "Up", "Down", "None", "Up", "Up", "None", "Up"),
                         significancefontsizemultiplier = c(0.20, 4.89, 0.20, 1, 3.29, 4.89, 1, 4.89),
                         significanceissignificant = as.logical(c(1L, 1L, 1L, 0L, 1L, 1L, 0L, 1L)),
                         zstatistic = c(-8.70, 6.18, -6.53, 0.68, 3.43, 5.52, -0.58, 170.63),
                         pcorrected = c(0, 0, 0, 0.69, 0.0008, 0, 0.78, 0))
    single.dim.table <- structure(vals,
        statistic = "%",
        dim = 8L,
        dimnames = list(names(vals)),
        class = c("array", "qTable"),
        dimnets = list(8L),
        dimduplicates = list(),
        span = list(rows = data.frame(names(vals), fix.empty.names = FALSE)),
        basedescriptiontext = "base n = 4853",
        basedescription = list(Minimum = 4853L, Maximum = 4853L, Range = FALSE, Total = 4853L,
                               Missing = 0L, EffectiveSampleSize = 4853L,
                               EffectiveSampleSizeProportion = 100, FilteredProportion = 0),
        QStatisticsTestingInfo = q.stat,
        questiontypes = "PickOne",
        footerhtml = paste0("Total sample; Unweighted; base n = 4853; Multiple comparison correction: ",
                            "False Discovery Rate (FDR) (p = 0.05)"),
        name = "table.S1.Age",
        questions = c("S1 Age", "SUMMARY"))
    multi.stat.1d.table <- structure(c(vals, rep(500L, length(vals))),
        dim = c(length(vals), 2L),
        dimnames = list(names(vals), c("%", "Sample size")),
        class = c("array", "matrix", "qTable"),
        dimnets = list(8L),
        dimduplicates = list(),
        span = list(row = data.frame(names(vals), fix.empty.names = FALSE),
                    column = data.frame(c("%", "Sample size"), fix.empty.names = FALSE)),
        basedescriptiontext = "base n = 4853",
        basedescription = list(Minimum = 4853L, Maximum = 4853L, Range = FALSE, Total = 4853L,
                               Missing = 0L, EffectiveSampleSize = 4853L,
                               EffectiveSampleSizeProportion = 100, FilteredProportion = 0),
        QStatisticsTestingInfo = q.stat,
        questiontypes = "PickOne",
        footerhtml = paste0("Total sample; Unweighted; base n = 4853; Multiple comparison correction: ",
                            "False Discovery Rate (FDR) (p = 0.05)"),
        name = "table.S1.Age",
        questions = c("S1 Age", "SUMMARY"))
    output.1d.multi.stat <- convertTo3dQTable(multi.stat.1d.table)
    expect_equal(as.vector(output.1d.multi.stat), as.vector(multi.stat.1d.table))
    original.attributes <- attributes(multi.stat.1d.table)
    output.attributes <- attributes(output.1d.multi.stat)
    expect_equal(output.attributes[["dim"]], c(8L, 1L, 2L))
    expect_equal(output.attributes[["dimnames"]],
                 append(dimnames(output.1d.multi.stat), NULL, 1L))
    output.q.stat <- output.attributes[["QStatisticsTestingInfo"]]
    original.q.stat <- original.attributes[["QStatisticsTestingInfo"]]
    expect_true(all(c("Row", "Column") %in% names(output.q.stat)))
    expect_equal(output.q.stat[["Row"]], rownames(multi.stat.1d.table))
    expect_true(all(output.q.stat[["Column"]] == 1))
    output.q.stat <- output.q.stat[!names(output.q.stat) %in% c("Row", "Column")]
    expect_equal(output.q.stat, original.q.stat)
    attr.to.ignore <- c("dim", "dimnames", "class", "QStatisticsTestingInfo")
    output.attr.names <- names(output.attributes)
    original.attr.names <- names(original.attributes)
    expect_setequal(output.attr.names, original.attr.names)
    original.attr.names <- original.attr.names
    output.attributes <- output.attributes[!output.attr.names %in% attr.to.ignore]
    original.attributes <- original.attributes[!original.attr.names %in% attr.to.ignore]
    output.attributes <- output.attributes[names(original.attributes)]
    expect_equal(output.attributes, original.attributes)
})
