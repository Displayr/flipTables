context("Basic Table")

test_that("BasicTable: works with date arg",
{

})

test_that("BasicTable: works with by arg",
{

})

test_that("BasicTable: removes cols or rows properly",
{

})



test_that("BasicTable: works when needs to call AsBasicTable first",
{
    ## check QTable
    ## check identical to just calling AsBasicTable
})

test_that("BasicTable: remove all rows or columns",
{

})

test_that("BasicTable: remove all but one row or column",
{

})

test_that("BasicTable: transpose arg works",
{

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

test_that("BasicTable replaces GetTidyTwoDimensionalArray",
{
    ## out.gta <- flipData::GetTidyTwoDimensionalArray(x.with.labels, "NET", "NET")
    ## out.bt <- BasicTable(x.with.labels, row.names.to.remove = "NET",
    ##                      col.names.to.remove = "NET")
    expect_silent(BasicTable(x.with.labels, row.names.to.remove = "NET",
                         col.names.to.remove = "Kids"))
    expect_silent(BasicTable(x))

    ## 3D array with no names
    z <- array(NA, c(8,11,2))
    z[,,1] <- x
    ## current BasicTable behaviour is to flatten **any** 3D or 4D array
    ## and warn if some dimnames are missing
    ## GetTidyTwoDimensionalArray behaviour was to error if dimnames are missin
    ## expect_error(GetTidyTwoDimensionalArray(z))
    expect_warning(BasicTable(z))
    dimnames(z) <- list(dimnames(x.with.labels)[[1]], dimnames(x.with.labels)[[2]], 1:2)
    expect_silent(BasicTable(z))

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
    expect_silent(BasicTable(x.with.labels))
})

test_that("BasicTable removes rows and columns properly",
{
    out <- BasicTable(x.with.labels, row.names.to.remove = c("Coke", "V"),
                         col.names.to.remove = "Fun")
    expect_equal(dim(out), dim(x.with.labels) - c(2, 1))
})

test_that("BasicTable: converts 1D array to numeric",
{
    expect_silent(out <- BasicTable(array(1, dim = 3)))
    expect_null(dim(out))
    expect_equal(names(out), as.character(1:3))
})

test_that("BasicTable: preserves dimname names",
{
    x <- matrix(1:6, 2, 3)
    dimnames(x) <- list(row_lab = letters[1:2],
                        col_lab = LETTERS[1:3])
    out <- BasicTable(x)
    expect_equal(names(dimnames(out)), names(dimnames(x)))
})
