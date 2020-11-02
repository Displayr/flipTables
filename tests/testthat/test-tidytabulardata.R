context("TidyTabularData")

## Note that unit tests for the date argument of TidyTabularData are contained in
##    ./tests/testthat/test-stackyears.R

test_that("TidyTabularData: removes cols/rows properly (and transposes FIRST)",
{
    x <- structure(c(6.125, 57.125, 22.375, 8.875, 61.5, 9.375, 9.25,
    100, 2, 57.75, 53.5, 2.5, 57.875, 30.625, 17.375, 100, 10.5,
    21.625, 11.375, 10, 44.625, 6.875, 29.875, 100, 64.625, 22.5,
    5.375, 39, 9.875, 6.75, 7.25, 100, 22.375, 8.875, 50.625, 16.75,
    16.625, 49.25, 12.875, 100, 25.5, 4.75, 64, 17.75, 3.75, 44.75,
    15.25, 100, 9.5, 23.25, 9.75, 13.5, 29.75, 5.5, 38.875, 100,
    91.25, 14.625, 3, 54.75, 3.75, 4.375, 2.5, 100, 0.5, 76.125,
    63.875, 0, 76.625, 40.375, 5.75, 100, 98, 91.5, 94.875, 79.625,
    94.75, 86.375, 57.5, 100), .Dim = c(8L, 10L), statistic = "%", .Dimnames = list(
        c("Coke", "Diet Coke", "Coke Zero", "Pepsi", "Diet Pepsi",
        "Pepsi Max", "None of these", "NET"), c("Feminine", "Health-conscious",
        "Innocent", "Older", "Open to new experiences", "Rebellious",
        "Sleepy", "Traditional", "Weight-conscious", "NET")), name = "q5", questions = c("q5",
    "SUMMARY"))

    rr <- c("NET", "Total", "SUM", "Weight-conscious", "Feminine")
    cr <- c("NET", "Total", "SUM")
    dn <- dimnames(x)

    out <- TidyTabularData(x, row.names.to.remove = rr,
                      col.names.to.remove = cr, transpose = FALSE)
    expect_equal(dim(out), c(sum(!dn[[1L]] %in% rr), sum(!dn[[2L]] %in% cr)))

    out <- TidyTabularData(x, row.names.to.remove = rr,
                      col.names.to.remove = cr, transpose = TRUE)
    expect_equal(dim(out), c(sum(!dn[[1L]] %in% cr), sum(!dn[[2L]] %in% rr)))
})



test_that("TidyTabularData: works when needs to call AsTidyTabularData first",
{
    ## check QTable
    q1.os <- structure(c(7.08868501529052, 3.84709480122324, 17.4617737003058
    ), .Dim = 3L, statistic = "Average", .Dimnames = list(c("Colas (e.g., Coca-Cola, Pepsi Max)?",
    "Sparkling mineral water", "SUM")), name = "Number Multi", questions = c("Number Multi",
                                                                             "SUMMARY"))
    out <- TidyTabularData(q1.os)
    expect_identical(out, AsTidyTabularData(q1.os))
    expect_equal(attr(out, "name"), attr(q1.os, "name"))
    expect_null(dim(out))
})

test_that("TidyTabularData: remove all rows or columns",
{
    df <- data.frame(x = 1, y= 2)
    expect_error(TidyTabularData(df, col.names.to.remove = c("x", "y")),
                 "Removing column 'x', 'y' gives empty input matrix")
})

test_that("TidyTabularData: remove all but one row or column",
{
    x <- matrix(1:6, 2, 3)
    rownames(x) <- letters[1:2]
    colnames(x) <- LETTERS[1:3]
    out <- TidyTabularData(x, row.names.to.remove = "a")
    expect_equal(length(out), ncol(x))

    out <- TidyTabularData(x, col.names.to.remove = c("A", "B"))
    expect_equal(length(out), nrow(x))
})

test_that("TidyTabularData: transpose arg works",
{
    x <- rbind(letters[1:2], 1:2, 3:4, 5:6)
    out <- TidyTabularData(x, transpose = TRUE)
    expect_equal(rownames(out), x[1, ])
    expect_equal(dim(out), rev(dim(x[-1, ])))
})

x <- matrix(c(0.3004, 0.6864, 0.4975, 0.2908, 0.2781, 0.2642, 0.1916, 0.284,  0.3514, 0.2534, 0.2089,
              c(  0.0198, 0.4604, 0.2151, 0.5235, 0.1151, 0.12,
                0.5457, 0.3041, 0.06312,    0.384,  0.06064),
              c(  0.01114,    0.4111, 0.1904, 0.4494, 0.06931,
                0.1112, 0.4716, 0.2859, 0.0495, 0.3296, 0.03837),
              c(  0.01114,    0.2373, 0.089,  0.2707, 0.05322,
                0.06436,    0.2756, 0.1656, 0.02967,    0.1916,
                0.02228),
              c(  0.0198, 0.177,  0.07054,    0.0297, 0.0396, 0.02719,
                0.0136, 0.02847,    0.0198, 0.02847,    0.02472),
              c(  0.4543, 0.1275, 0.07673,    0.02847,    0.07293,
                0.1077, 0.01609,    0.05198,    0.321,  0.01856,
                0.0297),
              c(  0.06807,    0.1089, 0.06064,    0.0198, 0.1174,
                0.04084,    0.01609,    0.01733,    0.03465,
                0.01361,    0.03589),
              c(  0.08168,    0.224,  0.1015, 0.04579,    0.04815,
                0.04084,    0.03094,    0.05562,    0.05322,
                0.04084,    0.02847)),nrow=8,byrow=TRUE)
x.with.labels <- x
dimnames(x.with.labels) <- list(Brand=c('Coke','V',"Red\nBull",
                                        "Lift\nPlus",'Diet.Coke',
                                        'Fanta','Lift','Pepsi'),
                                Attribute=c('Kids', 'Teens',
                                            "Enjoy life",
                                            'Picks you up',
                                            'Refreshes',
                                            'Cheers you up',
                                            'Energy',   'Up-to-date',
                                            'Fun',  'When tired',
                                            'Relax'))

test_that("TidyTabularData replaces GetTidyTwoDimensionalArray",
{
    ## out.gta <- flipData::GetTidyTwoDimensionalArray(x.with.labels, "NET", "NET")
    ## out.bt <- TidyTabularData(x.with.labels, row.names.to.remove = "NET",
    ##                      col.names.to.remove = "NET")
    expect_silent(TidyTabularData(x.with.labels, row.names.to.remove = "NET",
                         col.names.to.remove = "Kids"))
    expect_silent(TidyTabularData(x))

    ## 3D array with no names
    z <- array(NA, c(8,11,2))
    z[,,1] <- x
    ## current TidyTabularData behaviour is to flatten **any** 3D or 4D array
    ## and warn if some dimnames are missing
    ## GetTidyTwoDimensionalArray behaviour was to error if dimnames are missin
    ## expect_error(GetTidyTwoDimensionalArray(z))
    expect_warning(TidyTabularData(z))
    dimnames(z) <- list(dimnames(x.with.labels)[[1]], dimnames(x.with.labels)[[2]], 1:2)
    expect_silent(TidyTabularData(z))

    ## SUM and NET
    dimnames(x.with.labels) <- list(Brand=c('SUM','NET',"Red\nBull",
                                            "Lift\nPlus",'Diet.Coke',
                                            'Fanta','Lift','Pepsi'),
                                    Attribute=c('SUM', 'NET',
                                                "Enjoy life",
                                                'Picks you up',
                                                'Refreshes',
                                                'Cheers you up',
                                                'Energy',
                                                'Up-to-date',   'Fun',
                                                'When tired',
                                                'Relax'))
    expect_silent(TidyTabularData(x.with.labels))
})

test_that("TidyTabularData removes rows and columns properly",
{
    out <- TidyTabularData(x.with.labels, row.names.to.remove = c("Coke", "V"),
                         col.names.to.remove = "Fun")
    expect_equal(dim(out), dim(x.with.labels) - c(2, 1))
})

test_that("TidyTabularData: converts 1D array to numeric",
{
    expect_silent(out <- TidyTabularData(array(1, dim = 3)))
    expect_null(dim(out))
    expect_equal(names(out), as.character(1:3))
})

test_that("TidyTabularData: preserves dimname names",
{
    x <- matrix(1:6, 2, 3)
    dimnames(x) <- list(row_lab = letters[1:2],
                        col_lab = LETTERS[1:3])
    out <- TidyTabularData(x)
    expect_equal(names(dimnames(out)), names(dimnames(x)))
})

test_that("TidyTabularData data.frame input, names set okay",
{

    df <- structure(list(A = c(1, 2, 4, 5, 6, 7, 9, 10), B = c(2, 3, 5,
    6, 7, 8, 10, 11)), .Names = c("A", "B"), row.names = c("A", "B",
    "D", "E", "F", "G", "I", "J"), class = "data.frame")
    out <- TidyTabularData(df)
    expect_equal(rownames(out), rownames(df))
    expect_is(out, "matrix")
})

test_that("TidyTabularData removes rows and columns properly",
{
    out <- TidyTabularData(x.with.labels, row.names.to.remove = c("Coke", "V"),
                         col.names.to.remove = "Fun")
    expect_equal(dim(out), dim(x.with.labels) - c(2, 1))
})

test_that("TidyTabularData removes entries from vector properly",
{
    q1.os <- structure(c(7.08868501529052, 3.84709480122324, 17.4617737003058
    ), .Dim = 3L, statistic = "Average", .Dimnames = list(c("Colas (e.g., Coca-Cola, Pepsi Max)?",
    "Sparkling mineral water", "SUM")), name = "Number Multi", questions = c("Number Multi",
                                                                             "SUMMARY"))
    out <- TidyTabularData(q1.os, row.names.to.remove = "SUM")
    expect_is(out, "numeric")
    expect_equal(names(out), names(q1.os)[-3])
    expect_equal(attr(out, "name"), attr(q1.os, "name"))
    expect_equal(attr(out, "questions"), attr(q1.os, "questions"))
    expect_equal(attr(out, "statistic"), attr(q1.os, "statistic"))

   # expect_error(TidyTabularData(q1.os, col.names.to.remove = names(q1.os)))

    x <- c(a = 1, b = 2, c = 3)
    out <- TidyTabularData(x, row.names.to.remove = "c", col.names.to.remove = c("a", "c"))
    expect_equal(names(out), c("a", "b"))

    x <- 1:4
    out <- TidyTabularData(x, row.names.to.remove = "a")
    expect_equal(names(out), as.character(x))
})

test_that("TidyTabularData rm entries from vector comma sep. names",
{
    x <- c(a = 1, b = 2, c = 3)
    out <- TidyTabularData(x, row.names.to.remove = "c", col.names.to.remove = c("a,c"))
    expect_equal(names(out), c("a","b"))

    out <- TidyTabularData(x, row.names.to.remove = "c;  b  ", split = "[;,]")
    expect_equal(names(out), "a")

    x <- 1:4
    out <- TidyTabularData(x, row.names.to.remove = "a;b")
    expect_equal(names(out), as.character(x))
})

test_that("TidyTabularData hide empty rows and columns",
{
    x <- matrix("", 3, 3)
    x[1, -1] <- letters[1:2]
    x[-1, 1] <- LETTERS[1:2]
    x[2, 2] <- "1"
    x[2, 3] <- "2"
    out <- TidyTabularData(x, hide.empty.rows.and.columns = FALSE)
    expect_equal(dim(out), dim(x) - c(1, 1))
    expect_true(all(is.na(out[2, ])))

    out <- TidyTabularData(x, hide.empty.rows.and.columns = TRUE)
    expect_equal(length(out), 2)
    expect_false(any(is.na(out)))
})

test_that("Data with stats",
{
    dat <- structure(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "0",
        "4.375", "7.5", "11.875", "11.875", "11.25", "9.375", "10.625",
        "17.5", "15.625", "100", "-", "-", "-", "-", "-", "-", "-", "-",
        "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-",
        "-"), .Dim = c(11L, 2L, 2L), .Dimnames = list(c("Less than 18 years",
        "18 to 24 years", "25 to 29 years", "30 to 34 years", "35 to 39 years",
        "40 to 44 years", "45 to 49 years", "50 to 54 years", "55 to 64 years",
        "65 years or more", "NET"), c("2", "1"), c("Column %", "Column Comparisons"
        )), name = "table.Q2.Age.by.newvariable", questions = c("Q2. Age",
        "newvariable"))
    expect_warning(TidyTabularData(dat), "Multiple statistics detected")
})

tb.with.colcmp <- structure(c("40.8019343203137", "0", "0", "9.9882622907311",
    "0", "0", "0", "42.4098580805392", "6.79994530841603", "100",
    "16.1211312952781", "8.12377194130752", "0", "8.14296506751172",
    "20.2588714833541", "5.65751197171707", "5.47745069788922", "20.9885378006794",
    "15.2297597422629", "100", "17.6407828217187", "5.28873022190416",
    "7.19935026396706", "10.710609726734", "3.53440560651413", "8.33518750439024",
    "12.883925051695", "25.3517174138779", "9.0552913891989", "100",
    "14.8120040232897", "6.14839944202981", "12.1101028262771", "11.3288805923426",
    "8.51122193389764", "14.1131311164125", "11.3288395298069", "6.71721279432833",
    "14.9302077416155", "100", "8.73153812054241", "11.1403375047201",
    "6.86063707055166", "8.7953543176689", "19.8662426149607", "8.12728277316852",
    "17.8657194780912", "16.4406294701883", "2.1722586501082", "100",
    "7.72482657630125", "26.6766211961164", "12.6956955453385", "18.7072826117255",
    "5.76149120749631", "2.05649334018489", "17.2441045613512", "9.13348496148598",
    "0", "100", "5.292872453104", "12.9138749782186", "17.9479856428852",
    "5.53945987648476", "10.2981924825729", "12.1793943960394", "7.88708501637093",
    "27.9411351543242", "0", "100", "1.78146762054535", "8.74963165903852",
    "12.6073536504966", "33.4843192373454", "7.60982588095622", "6.43449373264074",
    "7.45841481689702", "21.8744934020802", "0", "100", "18.6810593930059",
    "21.9675158190695", "13.8096056031627", "4.74708777707673", "12.5133771303897",
    "0", "16.0440026048516", "12.2373516724438", "0", "100", "12.4569467198421",
    "11.690744866522", "9.37041817312926", "12.073816164748", "11.2997473376437",
    "7.53404498234392", "12.4398105318749", "17.0344036523314", "6.10006757156468",
    "100", "18", "18", "18", "18", "18", "18", "18", "18", "18",
    "18", "55", "55", "55", "55", "55", "55", "55", "55", "55", "55",
    "57", "57", "57", "57", "57", "57", "57", "57", "57", "57", "96",
    "96", "96", "96", "96", "96", "96", "96", "96", "96", "120",
    "120", "120", "120", "120", "120", "120", "120", "120", "120",
    "76", "76", "76", "76", "76", "76", "76", "76", "76", "76", "45",
    "45", "45", "45", "45", "45", "45", "45", "45", "45", "34", "34",
    "34", "34", "34", "34", "34", "34", "34", "34", "30", "30", "30",
    "30", "30", "30", "30", "30", "30", "30", "531", "531", "531",
    "531", "531", "531", "531", "531", "531", "531", "e f g H", NA,
    NA, NA, NA, NA, NA, "D f", NA, "-", NA, NA, NA, NA, NA, NA, NA,
    NA, "e f g", "-", "h", NA, NA, NA, NA, NA, NA, "d", "f", "-",
    NA, NA, NA, NA, NA, NA, NA, NA, "e f g h", "-", NA, NA, NA, NA,
    NA, NA, NA, NA, NA, "-", NA, "c d", NA, NA, NA, NA, NA, NA, NA,
    "-", NA, NA, "b", NA, NA, NA, NA, "d", NA, "-", NA, NA, NA, "b d e g i",
    NA, NA, NA, NA, NA, "-", "h", NA, NA, NA, NA, NA, NA, NA, NA,
    "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-"), .Dim = c(10L,
    10L, 3L), .Dimnames = list(c("18 to 24", "25 to 29", "30 to 34",
    "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64", "65+",
    "NET"), c("Less than $15,000", "$15,001 to $30,000", "$30,001 to $45,000",
    "$45,001 to $60,000", "$60,001 to $90,000", "$90,001 to $120,000",
    "$120,001 to $150,000", "$150,001 to $200,000", "$200,001 or more",
    "NET"), c("Column %", "Column n", "Column Comparisons")),
     name = "D1 - Age by D2 - Income", questions = c("D1 - Age",
    "D2 - Income"), weight.name = "weight_", weight.label = "weighting")

test_that("Tidy tables that have been converted to character",
{
    res <- SortRows(tb.with.colcmp)
    expect_equal(rownames(res), c("65+", "45 to 49", "30 to 34", "40 to 44", "25 to 29",
        "35 to 39", "50 to 54", "18 to 24", "55 to 64", "NET"))
    res <- SortColumns(tb.with.colcmp, row = 1, decreasing = TRUE)
    expect_equal(colnames(res), c("Less than $15,000", "$200,001 or more", "$30,001 to $45,000",
        "$15,001 to $30,000", "$45,001 to $60,000", "$60,001 to $90,000",
        "$90,001 to $120,000", "$120,001 to $150,000", "$150,001 to $200,000",
        "NET"))

    expect_error(AutoOrderColumns(tb.with.colcmp), NA)
    expect_error(AutoOrderRows(tb.with.colcmp), NA)

    expect_warning(HideColumnsWithSmallSampleSizes(tb.with.colcmp, 40),
        "Columns 1,8,9 have sample size less than 40 and have been removed")
    expect_warning(HideRowsWithSmallSampleSizes(tb.with.colcmp, 1000),
        "Rows 1,2,3,4,5,6,7,8,9,10 have sample size less than 1000 and have been removed")

    res <- HideValuesWithSmallSampleSizes(tb.with.colcmp, 40)
    expect_equal(all(is.na(res[,c(1,8,9),1])), TRUE)


})

test_that("processDates", {
    dt <- c("2020-01-01", "2020-09-08", "2020-12-31")
    expected.output <- c(`2020-01-01` = 1, `2020-09-08` = 2, `2020-12-31` = 3)

    # Data frame with one column
    df <- data.frame(a = 1:3)
    rownames(df) <- dt
    expect_equal(processDates(df), expected.output)

    # Data frame with two columns, with first column containing dates
    df <- data.frame(dt = dt, a = 1:3)
    expect_equal(processDates(df), expected.output)

    # Vector which contains attributes (DS-3090)
    x <- 1:3
    names(x) <- dt
    attr(x, "some attribute") <- "attribute value"
    expect_equal(processDates(x), expected.output)

    # 1D array
    x <- array(1:3)
    names(x) <- dt
    expect_equal(processDates(x), expected.output)

    # Matrix where nrow > ncol
    m <- matrix(1:3, nrow = 3)
    rownames(m) <- dt
    expect_equal(processDates(m), expected.output)

    # Matrix where nrow == 1
    m <- matrix(1:3, nrow = 1)
    colnames(m) <- dt
    expect_equal(processDates(m), expected.output)

    # Matrix where nrow == 2
    m <- matrix(c(dt, 1:3), nrow = 2, byrow = TRUE)
    expect_equal(processDates(m), expected.output)

    # date parameter supplied
    expect_equal(processDates(1:3, date = dt), expected.output)

    # Invalid format error
    expect_error(processDates(formula(y ~ x)),
                 paste0("Input data is not a list, vector, table, matrix or ",
                        "1D/2D array. It should be one of these data formats."))

    # Different lengths for dates and values
    expect_error(processDates(1:4, dt),
                 paste0("The input dates and values have different lengths. ",
                        "They should have the same length."))

    # Duplicate dates error
    expect_error(processDates(1:3, c(dt[1], dt[1], dt[2])),
                 "Duplicate dates. Dates should be unique.")
})
