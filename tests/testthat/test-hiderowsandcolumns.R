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

test_that("HideEmptyRows and HideEmptyColumns",
{
    x <- c(NA, NA, 1, 2, 3)
    expect_equal(HideEmptyRowsAndColumns(x), c(1, 2, 3))
    expect_equal(HideEmptyRows(x), HideEmptyRowsAndColumns(x))
    expect_equal(HideEmptyColumns(x), x)

    x <- structure(c(NA, NA, NA, 4, 5, 6, 7, 8, 9, 10, 11, 12), .Dim = 3:4)
    expect_equal(dim(HideEmptyRowsAndColumns(x)), c(3, 3))
    expect_equal(HideEmptyColumns(x), HideEmptyRowsAndColumns(x))
    expect_equal(dim(HideEmptyRows(x)), c(3, 4))
})

tb <- structure(c(51.6908212560386, 42.512077294686, 44.4444444444444,
    58.4541062801932, 39.6135265700483, 57.487922705314, 42.512077294686,
    47.3429951690821, 37.6811594202899, 41.5458937198068, 33.3333333333333,
    17.8743961352657, NA, 50.7246376811594, 43.9613526570048, 43.4782608695652,
    46.8599033816425, 43.4782608695652, 42.512077294686, 30.9178743961353,
    30.4347826086957, 56.5217391304348, 32.3671497584541, NA, 41.0628019323672,
    49.2753623188406, NA, 33.8164251207729, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, 15.5339805825243, 15.5339805825243,
    17.4757281553398, 24.2718446601942, 12.621359223301, 25.2427184466019,
    17.4757281553398, 14.5631067961165, 9.70873786407767, 9.70873786407767,
    7.76699029126214, 6.79611650485437, NA, 3.88349514563107, 10.6796116504854,
    19.4174757281553, 16.504854368932, 13.5922330097087, 10.6796116504854,
    21.3592233009709, 8.7378640776699, 18.4466019417476, 19.4174757281553,
    NA, 15.5339805825243, 15.5339805825243, NA, 15.5339805825243,
    NA, 9.23076923076923, 6.15384615384615, 15.3846153846154, 12.3076923076923,
    13.8461538461538, 12.3076923076923, 16.9230769230769, 10.7692307692308,
    7.69230769230769, 4.61538461538462, 3.07692307692308, 1.53846153846154,
    NA, 6.15384615384615, 4.61538461538462, 20, 7.69230769230769,
    6.15384615384615, 9.23076923076923, 26.1538461538462, 9.23076923076923,
    6.15384615384615, 20, NA, 10.7692307692308, 10.7692307692308,
    NA, 15.3846153846154, NA, 19.5652173913043, 18.4782608695652,
    20.6521739130435, 28.2608695652174, 17.3913043478261, 28.2608695652174,
    18.4782608695652, 17.3913043478261, 22.8260869565217, 19.5652173913043,
    8.69565217391304, 8.69565217391304, NA, 14.1304347826087, 16.304347826087,
    22.8260869565217, 18.4782608695652, 20.6521739130435, 18.4782608695652,
    21.7391304347826, 13.0434782608696, 23.9130434782609, 19.5652173913043,
    NA, 16.304347826087, 22.8260869565217, NA, 14.1304347826087,
    NA, 17.3913043478261, 14.975845410628, 15.4589371980676, 16.9082125603865,
    14.4927536231884, 26.0869565217391, 18.3574879227053, 17.8743961352657,
    13.0434782608696, 16.4251207729469, 9.17874396135266, 6.28019323671498,
    NA, 15.9420289855072, 13.5265700483092, 23.1884057971014, 14.4927536231884,
    16.9082125603865, 14.4927536231884, 16.9082125603865, 11.5942028985507,
    20.7729468599034, 15.9420289855072, NA, 12.56038647343, 22.2222222222222,
    NA, 14.4927536231884, NA, 31.4009661835749, 22.2222222222222,
    28.0193236714976, 41.5458937198068, 28.9855072463768, 37.1980676328502,
    32.8502415458937, 24.6376811594203, 22.2222222222222, 26.5700483091787,
    11.1111111111111, 11.5942028985507, NA, 16.4251207729469, 27.536231884058,
    40.0966183574879, 32.8502415458937, 30.9178743961353, 24.1545893719807,
    23.1884057971014, 13.0434782608696, 30.9178743961353, 25.1207729468599,
    NA, 31.8840579710145, 31.4009661835749, NA, 22.2222222222222,
    NA, 9.66183574879227, 11.1111111111111, 15.9420289855072, 11.1111111111111,
    10.6280193236715, 15.9420289855072, 11.5942028985507, 11.5942028985507,
    9.17874396135266, 9.66183574879227, 5.79710144927536, 7.72946859903382,
    NA, 7.72946859903382, 7.72946859903382, 14.0096618357488, 7.2463768115942,
    8.69565217391304, 8.21256038647343, 13.5265700483092, 6.28019323671498,
    12.0772946859903, 12.56038647343, NA, 9.17874396135266, 9.66183574879227,
    NA, 12.0772946859903, NA, 15.5555555555556, 17.037037037037,
    19.2592592592593, 21.4814814814815, 33.3333333333333, 22.2222222222222,
    37.037037037037, 18.5185185185185, 11.1111111111111, 10.3703703703704,
    8.14814814814815, 5.18518518518519, NA, 7.40740740740741, 15.5555555555556,
    34.0740740740741, 14.8148148148148, 22.2222222222222, 8.14814814814815,
    26.6666666666667, 11.1111111111111, 8.14814814814815, 29.6296296296296,
    NA, 27.4074074074074, 19.2592592592593, NA, 19.2592592592593,
    NA, 207, 207, 207, 207, 207, 207, 207, 207, 207, 207, 207, 207,
    0, 207, 207, 207, 207, 207, 207, 207, 207, 207, 207, 0, 207,
    207, 0, 207, 0, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45,
    0, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 0, 45, 45, 0, 45,
    0, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103,
    0, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 0, 103,
    103, 0, 103, 0, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65,
    0, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 0, 65, 65, 0, 65,
    0, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 0, 92, 92,
    92, 92, 92, 92, 92, 92, 92, 92, 0, 92, 92, 0, 92, 0, 207, 207,
    207, 207, 207, 207, 207, 207, 207, 207, 207, 207, 0, 207, 207,
    207, 207, 207, 207, 207, 207, 207, 207, 0, 207, 207, 0, 207,
    0, 207, 207, 207, 207, 207, 207, 207, 207, 207, 207, 207, 207,
    0, 207, 207, 207, 207, 207, 207, 207, 207, 207, 207, 0, 207,
    207, 0, 207, 0, 207, 207, 207, 207, 207, 207, 207, 207, 207,
    207, 207, 207, 0, 207, 207, 207, 207, 207, 207, 207, 207, 207,
    207, 0, 207, 207, 0, 207, 0, 135, 135, 135, 135, 135, 135, 135,
    135, 135, 135, 135, 135, 0, 135, 135, 135, 135, 135, 135, 135,
    135, 135, 135, 0, 135, 135, 0, 135, 0), .Dim = c(29L, 9L, 2L), .Dimnames = list(
    c("Attracts the best students", "Fosters leadership qualities",
      "Has a good social/campus life", "Has a strong academic reputation",
      "Has excellent links with industry and employers", "Has globally recognised degrees",
      "Has modern infrastructure and facilities", "Has rigorous selection criteria for entry",
      "Influences government policy", "Influences public and social issues",
      "Is arrogant", "Is behind the times", "Is conservative",
      "Is highly traditional", "Is influential", "Is international and culturally diverse",
      "Is known for high quality research", "Is known for high quality teaching",
      "Is more expensive than other universities", "Is open and inviting",
      "Is overly bureaucratic", "Is prestigious", "Is progressive",
      "Is trustworthy", "Known for producing the most employable graduates",
      "Produces Australia's leaders", "Provides a range of admission pathways",
      "Wants to make lives better", "NET"), c("The University of Sydney",
      "Macquarie University", "Monash University", "Queensland University of Technology",
      "The Australian National University", "The University of Melbourne",
      "The University of New South Wales", "The University of Queensland",
      "University of Technology Sydney"), c("%", "Sample Size")),
      name = "table.D2.D3.Grid.Main.Universities.2", questions = c("D2/D3 Grid - Main Universities",
      "SUMMARY"), filter.names = "bLSHHJ", filter.labels = "2018")

tb2 <- structure(c(0, 13.75, 11.25, 9.375, 10, 11.875, 8.125, 11.25,
       16.875, 7.5, 100, 0, 13.1736526946108, 12.5748502994012, 10.7784431137725,
       11.9760479041916, 9.58083832335329, 8.38323353293413, 13.1736526946108,
       14.3712574850299, 5.98802395209581, 100, 0, 13.4556574923547,
       11.9266055045872, 10.0917431192661, 11.0091743119266, 10.7033639143731,
       8.25688073394496, 12.2324159021407, 15.5963302752294, 6.72782874617737,
       100), .Dim = c(11L, 3L), statistic = "Column %", .Dimnames = list(
         c("Less than 18", "18 to 24", "25 to 29", "30 to 34", "35 to 39",
           "40 to 44", "45 to 49", "50 to 54", "55 to 64", "65 or more",
           "NET"), c("Male", "Female", "NET")), name = "Q3. Age by Q2. Gender",
       questions = c("Q3. Age","Q2. Gender"))

tab1d <- structure(c(0.0, 11.75, 10.375, 11.375, 11.625, 7.875, 11.875,
       15.75, 7, 100, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800,
       1.1375, 0.575, -0.6625, 0.237500000000001, 0.462500000000001,
       -2.9125, 0.6875, 4.175, -3.7, 80, 0.255329324592568, 0.565291296994401,
       0.507650834828445, 0.812268919201514, 0.643722802327307, 0.00358548200450471,
       0.491767700760523, 2.97986053738875e-05, 0.000215599466954778,
       0), .Dim = c(10L, 4L), .Dimnames = list(c("18 to 24", "25 to 29",
       "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
       "65 or more", "NET"), c("%", "Base n", "z-Statistic", "p")),
       name = "Age", questions = c("Age", "SUMMARY"))

test_that("Remove empty columns in table with multiple statistics",
{
    expect_equal(colnames(HideEmptyColumns(tb)),
                 c("The University of Sydney", "Monash University", "Queensland University of Technology",
                   "The Australian National University", "The University of Melbourne",
                   "The University of New South Wales", "The University of Queensland",
                   "University of Technology Sydney"))
    expect_equal(dim(HideEmptyRows(tb)), c(25, 9, 2))
    expect_equal(dim(HideEmptyColumns(tb2)), c(11,3))
    expect_equal(dim(HideEmptyRows(tb2)), c(10,3))
    expect_equal(dim(HideEmptyColumns(tab1d)), c(10,4))
    expect_equal(dim(HideEmptyRows(tab1d)), c(9,4))
})

tb.with.stats <- structure(c(3.29508196721311, 2.97368421052632, 3.06153846153846,
     3.21538461538462, 3.23529411764706, 3.52112676056338, 3.47058823529412,
     3, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0, 0, 0, 0, 0,
     1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), dim = c(1L, 19L, 3L),
     dimnames = list("Moyenne – Information en cas perturbation ou de détour",
     c("June 2024", "July 2024", "August 2024", "September 2024",
     "October 2024", "November 2024", "December 2024", "January 2025",
     "February 2025", "March 2025", "April 2025", "May 2025",
     "June 2025", "July 2025", "August 2025", "September 2025", "October 2025", 
     "November 2025", "December 2025"), c("Average", "signifUp#0000FF", "signifDown#FF0000")), 
     dimnets = list(integer(0), integer(0)), dimduplicates = list(integer(0), integer(0)), 
     span = list(rows = structure(list("Moyenne – Information en cas perturbation ou de détour"),
     class = "data.frame", names = "", row.names = 1L), columns = structure(list(c("June 2024", 
     "July 2024", "August 2024", "September 2024", "October 2024", "November 2024", "December 2024",
     "January 2025", "February 2025", "March 2025", "April 2025",
     "May 2025", "June 2025", "July 2025", "August 2025", "September 2025",
     "October 2025", "November 2025", "December 2025")), class = "data.frame", names = "", 
     row.names = c(NA, 19L))), basedescriptiontext = "sample size = 486; total sample size = 814; 328 missing",
     basedescription = list(Minimum = 486L, Maximum = 486L, Range = FALSE, Total = 814L,
     Missing = 328L, EffectiveSampleSize = 486L, EffectiveSampleSizeProportion = 100,
     FilteredProportion = 0), QStatisticsTestingInfo = structure(list(
     significancearrowratio = structure(c(0, 0.0719794344473008, 0, 0, 0, 0.0719794344473008, 0, 0, 0, 
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0), dim = 19L), significancedirection = structure(c("None",
     "Down", "None", "None", "None", "Up", "None", "None", "None",
     "None", "None", "None", "None", "None", "None", "None", "None",
     "None", "None"), dim = 19L), significancefontsizemultiplier = structure(c(1,
     0.78125, 1, 1, 1, 1.28, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), dim = 19L), 
     significanceissignificant = structure(c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, 
     FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), dim = 19L), 
     significanceargbcolor = structure(c(-8355712L, -65536L, -8355712L, -8355712L, -8355712L, 
     -16776961L, -8355712L, -8355712L, -8355712L, -8355712L, -8355712L, -8355712L, -8355712L,
     -8355712L, -8355712L, -8355712L, -8355712L, -8355712L, -8355712L), dim = 19L),
     backgroundargbcolor = structure(c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
     0L, 0L), dim = 19L), zstatistic = structure(c(NaN, -1.53761207080504,
     0.429985472931958, 0.748364315949354, 0.101239308187553,
     1.61569450566846, -0.245542729229572, -0.956547846008111,
     NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN), dim = 19L),
     pcorrected = structure(c(NaN, 0.124143494294862, 0.667206208608361,
     0.454240440212268, 0.919360493578279, 0.106160379490347,
     0.806036227423584, 0.338795523670985, NaN, NaN, NaN, NaN,
     NaN, NaN, NaN, NaN, NaN, NaN, NaN), dim = 19L)), class = "data.frame", row.names = c(NA, 19L)), 
    questiontypes = c("Number", "Date"), footerhtml = "Information en cas perturbation ou de détour - Average by Date de soumission_Mois&lt;br /&gt;sample size = 486; total sample size = 814; 328 missing; 80% confidence level; Significance: Compare to previous period",
    name = "Information en cas perturbation ou de détour - Average by Date de soumission_Mois", 
    questions = c("Information en cas perturbation ou de détour - Average",
    "Date de soumission_Mois"), assigned.rownames = TRUE, signif.annotations = list(
    list(type = "Arrow - up", data = "signifUp#0000FF", threstype = "above threshold",
    threshold = 0, color = "#0000FF", size = 12), list(type = "Arrow - down",
    data = "signifDown#FF0000", threstype = "above threshold", threshold = 0, color = "#FF0000", size = 12)))

test_that("Empty rows and columns are corrently removed from tables with statistics",
{
    res_hide_rows <- HideEmptyRows(tb.with.stats)
    expect_equal(dim(res_hide_rows), c(1, 19, 3))

    res_hide_cols <- HideEmptyColumns(tb.with.stats)
    expect_equal(dim(res_hide_cols), c(1, 8, 3))
})
