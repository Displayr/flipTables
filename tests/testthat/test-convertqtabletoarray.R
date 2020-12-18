context("ConvertQTableToArray")

tb.1d.nostat <- structure(c(`18 to 24` = 12.375, `25 to 29` = 11.75,
        `30 to 34` = 10.375, `35 to 39` = 11.375, `40 to 44` = 11.625,
        `45 to 49` = 7.875, `50 to 54` = 11.875, `55 to 64` = 15.75,
        `65 or more` = 7, NET = 100), statistic = "%", .Dim = 10L,
        .Dimnames = list(c("18 to 24", "25 to 29", "30 to 34", "35 to 39",
        "40 to 44", "45 to 49", "50 to 54", "55 to 64", "65 or more", "NET")),
        basedescriptiontext = "sample size = 800", basedescription = list(
        Minimum = 800L, Maximum = 800L, Range = FALSE, Total = 800L,
        Missing = 0L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
        FilteredProportion = 0), questiontypes = "PickOne", span = list(
        rows = structure(list(c("18 to 24", "25 to 29", "30 to 34",
        "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
        "65 or more", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
        10L))), name = "table.Age.2", questions = c("Age", "SUMMARY"))

tb.1d.multstat <- structure(c(12.375, 11.75, 10.375, 11.375, 11.625, 7.875, 11.875,
        15.75, 7, 100, 1.1375, 0.575, -0.6625, 0.237500000000001, 0.462500000000001,
        -2.9125, 0.6875, 4.175, -3.7, 80), .Dim = c(10L, 2L), .Dimnames = list(
        c("18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44",
        "45 to 49", "50 to 54", "55 to 64", "65 or more", "NET"),
        c("%", "z-Statistic")), basedescriptiontext = "sample size = 800",
        basedescription = list(
        Minimum = 800L, Maximum = 800L, Range = FALSE, Total = 800L,
        Missing = 0L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
        FilteredProportion = 0), questiontypes = "PickOne", span = list(
        rows = structure(list(c("18 to 24", "25 to 29", "30 to 34",
        "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
        "65 or more", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
        10L))), name = "table.Age.2", questions = c("Age", "SUMMARY"))

tb.2d.nostat <- structure(c(11.6455696202532, 11.6455696202532, 10.8860759493671,
        9.36708860759494, 12.9113924050633, 8.10126582278481, 10.379746835443,
        16.2025316455696, 8.86075949367089, 100, 13.0864197530864, 11.8518518518519,
        9.87654320987654, 13.3333333333333, 10.3703703703704, 7.65432098765432,
        13.3333333333333, 15.3086419753086, 5.18518518518519, 100, 12.375,
        11.75, 10.375, 11.375, 11.625, 7.875, 11.875, 15.75, 7, 100),
        statistic = "Column %", .Dim = c(10L,
        3L), .Dimnames = list(c("18 to 24", "25 to 29", "30 to 34", "35 to 39",
        "40 to 44", "45 to 49", "50 to 54", "55 to 64", "65 or more",
        "NET"), c("Male", "Female", "NET")), basedescriptiontext = "sample size = 800",
        basedescription = list(
        Minimum = 800L, Maximum = 800L, Range = FALSE, Total = 800L,
        Missing = 0L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
        FilteredProportion = 0), questiontypes = c("PickOne", "PickOne"),
        span = list(rows = structure(list(c("18 to 24", "25 to 29",
        "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
        "65 or more", "NET")), class = "data.frame", .Names = "",
        row.names = c(NA, 10L)), columns = structure(list(c("Male", "Female", "NET")),
        class = "data.frame", .Names = "", row.names = c(NA, 3L))),
        name = "table.Age.by.Gender.3", questions = c("Age", "Gender"))

tb.2d.multstat <- structure(c(11.6455696202532, 11.6455696202532, 10.8860759493671,
        9.36708860759494, 12.9113924050633, 8.10126582278481, 10.379746835443,
        16.2025316455696, 8.86075949367089, 100, 13.0864197530864, 11.8518518518519,
        9.87654320987654, 13.3333333333333, 10.3703703703704, 7.65432098765432,
        13.3333333333333, 15.3086419753086, 5.18518518518519, 100, 12.375,
        11.75, 10.375, 11.375, 11.625, 7.875, 11.875, 15.75, 7, 100,
        0.00823571315758493, 0.00823571315758493, 0.00797845483486244,
        0.00743018717643412, 0.00864296233199824, 0.00693253742102178,
        0.00780098494310835, 0.00959766346200534, 0.00723604673426212,
        0.0176873464560352, 0.00879903122867535, 0.00840168050416806,
        0.00771033797181093, 0.00887570618036955, 0.00789034279272665,
        0.00682779762445407, 0.00887570618036955, 0.009459335933107,
        0.00565607291555126, 0.0176873464560352, 0.0116496594405739,
        0.0113920767099202, 0.0107878586495562, 0.011232603710077, 0.0113393407076693,
        0.009528853324619, 0.0114443988354951, 0.0128870004646628, 0.0090264525732744,
        0, 0.536082900256765, 0.927820642717582, 0.639671305972622, 0.0773167026691277,
        0.262262932466245, 0.81448023121581, 0.196664361018653, 0.728585150490917,
        0.0416384099417911, NA, 0.536082900256765, 0.927820642717582,
        0.639671305972622, 0.0773167026691277, 0.262262932466245, 0.81448023121581,
        0.196664361018653, 0.728585150490917, 0.0416384099417911, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), .Dim = c(10L, 3L, 3L
        ), .Dimnames = list(c("18 to 24", "25 to 29", "30 to 34", "35 to 39",
        "40 to 44", "45 to 49", "50 to 54", "55 to 64", "65 or more",
        "NET"), c("Male", "Female", "NET"), c("Column %", "Standard Error",
        "p")), basedescriptiontext = "sample size = 800", basedescription = list(
            Minimum = 800L, Maximum = 800L, Range = FALSE, Total = 800L,
            Missing = 0L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
            FilteredProportion = 0), questiontypes = c("PickOne", "PickOne"
        ), span = list(rows = structure(list(c("18 to 24", "25 to 29",
        "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
        "65 or more", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
        10L)), columns = structure(list(c("Male", "Female", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
        3L))), name = "table.Age.by.Gender.3", questions = c("Age", "Gender"))

test_that("Converting tables",
{
    res <- ConvertQTableToArray(tb.1d.nostat)
    expect_equal(dimnames(res), list(c("18 to 24", "25 to 29",
            "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54",
            "55 to 64", "65 or more", "NET"), "", "%"))
    expect_equal(as.vector(res), as.vector(tb.1d.nostat))


    res <- ConvertQTableToArray(tb.1d.multstat)
    expect_equal(dimnames(res), list(c("18 to 24", "25 to 29", "30 to 34",
            "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
            "65 or more", "NET"), "", c("%", "z-Statistic")))

    res <- ConvertQTableToArray(tb.2d.nostat)
    expect_equal(dimnames(res), list(c("18 to 24", "25 to 29", "30 to 34",
            "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
            "65 or more", "NET"), c("Male", "Female", "NET"), "Column %"))

    res <- ConvertQTableToArray(tb.2d.multstat)

    expect_equal(dimnames(res), list(c("18 to 24", "25 to 29", "30 to 34",
            "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
            "65 or more", "NET"), c("Male", "Female", "NET"), c("Column %",
            "Standard Error", "p")))
})

