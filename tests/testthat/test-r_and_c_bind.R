library(gtools)

context("Rbind and Cbind")

v1 <- seq(1:10)
names(v1) <- letters[1:10]
v2 <- seq(1:11)
names(v2) <- letters[1:11]
v2 <- as.array(v2)
v3 <- seq(100:112)
v4 <- seq(100:113)
v4 <- as.array(v4)

t1 <- matrix(seq(1:18), nrow = 6, ncol = 3)
t2 <- matrix(seq(1:21), nrow = 7, ncol = 3)
colnames(t2) <- c("x", "y", "z")
t3 <- matrix(seq(1:24), nrow = 8, ncol = 3)
rownames(t3) <- letters[1:8]
colnames(t3) <- c("x", "y", "z")
t4 <- matrix(seq(1:27), nrow = 9, ncol = 3)
rownames(t4) <- letters[1:9]

all.items <- list(v1, v2, v3, v4, t1, t2, t3, t4)
perms <- permutations(n = length(all.items), r = 2)

test_that("Rbind",
          {
              expect_equal(suppressWarnings(Rbind(1:5, 1:5)), rbind(1:5, 1:5))
              a <- c(a = 1, b = 3)
              # expect_equivalent(Rbind(a), a) is not true because Rbind (and rbind) output matrix
              b <- c(a = 1, b = 2)
              # The following lines may fail and need to be commented out because testthat wrongly
              # says that Rbind(a, b) has no rownames.
              #expect_equal(Rbind(a, b), rbind(a, b))
              #expect_equal(Rbind(a), rbind(a))
              b <- c(a = 1, c = 2)
              expect_equivalent(Rbind(a, b), matrix(c(1, 1, NA, 2, 3, NA), nrow = 2))
              a <- NULL
              b <- c(a = 1, c = 2)
              expect_equivalent(Rbind(a, b), rbind(a, b))
              a = c("A" = 1, B = 2, D = 3)
              b = matrix(1:9, nrow = 3, dimnames = list(ROW = c("A", "D", "C"), COLUMNS = 1:3))
              a.and.b = structure(c(1, 1, 4, 7, 2, NA, NA, NA, 3, 2, 5, 8, NA, 3, 6,  9), .Dim = c(4L, 4L), .Dimnames = list(c("a", "1", "2", "3"),  c("A", "B", "D", "C")))
              expect_equivalent(Rbind(a, t(b)), a.and.b)
              a.and.b = a.and.b[,c("A", "D")]
              expect_equivalent(Rbind(a, t(b), keep.all = FALSE), a.and.b)
          })

for (i in 1:nrow(perms)) {
    test_that(paste("Rbind", i),
          {
            if (i %in% c(5, 6, 12, 13, 36, 37, 43, 44))
                expect_error(Rbind(all.items[[perms[i, 1]]], all.items[[perms[i, 2]]], keep.all = FALSE),
                             "Can not find any matching.")
            else if (i %in% c(1, 8, 41, 48))
                expect_error(Rbind(all.items[[perms[i, 1]]], all.items[[perms[i, 2]]]), NA)
            else
                expect_warning(Rbind(all.items[[perms[i, 1]]], all.items[[perms[i, 2]]]), "There are no matching.")
          })
}


test_that("Cbind",
          {
              expect_equal(suppressWarnings(Cbind(1:5, 1:5)), cbind(1:5, 1:5))
              a <- c(a = 1, b = 3)
              # expect_equivalent(Cbind(a), a) is not true because Cbind (and cbind) output matrix
              b <- c(a = 1, b = 2)
              # The following lines may fail and need to be commented out because testthat wrongly
              # says that Cbind(a,b) has no colnames.
              #expect_equal(Cbind(a, b), cbind(a, b))
              #expect_equal(Cbind(a), cbind(a))
              b <- c(a = 1, c = 2)
              expect_equivalent(Cbind(a, b), matrix(c(1, 1, NA, 2, 3, NA), nrow = 3, byrow = TRUE))
              a <- NULL
              b <- c(a = 1, c = 2)
              expect_equivalent(Cbind(a, b), cbind(a, b))
              a = c("A" = 1, B = 2, D = 3)
              b = matrix(1:9, nrow = 3, dimnames = list(ROW = c("A", "D", "C"), COLUMNS = 1:3))
              a.and.b = structure(c(1, 1, 4, 7, 2, NA, NA, NA, 3, 2, 5, 8, NA, 3, 6,  9), .Dim = c(4L, 4L), .Dimnames = list(c("a", "1", "2", "3"),  c("A", "B", "D", "C")))
              expect_equivalent(Cbind(a, b), t(a.and.b))
              a.and.b = a.and.b[,c("A", "D")]
              expect_equivalent(Cbind(a, b, keep.all = FALSE), t(a.and.b))
          })

test_that("Cbind",
          {

              for (i in 1:nrow(perms)) {
                  if (i %in% c(1, 6, 7, 8, 13, 14, 43, 44, 49, 50, 51, 56))
                      expect_error(Cbind(all.items[[perms[i, 1]]], all.items[[perms[i, 2]]]), NA)
                  else
                      expect_warning(Cbind(all.items[[perms[i, 1]]], all.items[[perms[i, 2]]]), "There are no matching.")
              }

          })

test_that("DS-3041: Rbind works with vectors",
{
    input.list <- list(`2007` = c(`2008` = 0L, `2010` = 0L, `2012` = 0L, `2014` = 0L,
    `2016` = 0L, `2018` = 0L), `2008` = c(`2007` = 0L, `2008` = 10L,
    `2009` = 11L, `2010` = 11L, `2011` = 11L, `2012` = 11L, `2013` = 10L,
    `2014` = 10L, `2015` = 11L, `2016` = 10L, `2017` = 10L, `2018` = 9L,
    `2019` = 2L), `2009` = c(`2007` = 0L, `2008` = 1L, `2009` = 44L,
    `2010` = 36L, `2011` = 35L, `2012` = 32L, `2013` = 34L, `2014` = 32L,
    `2015` = 29L, `2016` = 31L, `2017` = 29L, `2018` = 29L, `2019` = 17L
    ), `2010` = c(`2007` = 0L, `2008` = 0L, `2009` = 0L, `2010` = 46L,
    `2011` = 36L, `2012` = 31L, `2013` = 29L, `2014` = 26L, `2015` = 24L,
    `2016` = 23L, `2017` = 24L, `2018` = 22L, `2019` = 14L), `2011` = c(`2007` = 0L,
    `2008` = 0L, `2009` = 0L, `2010` = 2L, `2011` = 62L, `2012` = 52L,
    `2013` = 45L, `2014` = 40L, `2015` = 34L, `2016` = 33L, `2017` = 29L,
    `2018` = 29L, `2019` = 17L), `2012` = c(`2007` = 0L, `2008` = 0L,
    `2009` = 0L, `2010` = 0L, `2011` = 0L, `2012` = 67L, `2013` = 49L,
    `2014` = 44L, `2015` = 43L, `2016` = 40L, `2017` = 38L, `2018` = 34L,
    `2019` = 20L), `2013` = c(`2007` = 0L, `2008` = 0L, `2009` = 0L,
    `2010` = 0L, `2011` = 0L, `2012` = 0L, `2013` = 91L, `2014` = 66L,
    `2015` = 60L, `2016` = 57L, `2017` = 50L, `2018` = 48L, `2019` = 24L
    ), `2014` = c(`2007` = 0L, `2008` = 0L, `2009` = 0L, `2010` = 0L,
    `2011` = 0L, `2012` = 0L, `2013` = 1L, `2014` = 101L, `2015` = 76L,
    `2016` = 62L, `2017` = 53L, `2018` = 53L, `2019` = 26L), `2015` = c(`2007` = 0L,
    `2008` = 0L, `2009` = 0L, `2010` = 0L, `2011` = 0L, `2012` = 0L,
    `2013` = 0L, `2014` = 0L, `2015` = 133L, `2016` = 109L, `2017` = 94L,
    `2018` = 86L, `2019` = 54L), `2016` = c(`2007` = 0L, `2008` = 0L,
    `2009` = 0L, `2010` = 0L, `2011` = 0L, `2012` = 0L, `2013` = 0L,
    `2014` = 0L, `2015` = 1L, `2016` = 164L, `2017` = 125L, `2018` = 113L,
    `2019` = 60L), `2017` = c(`2007` = 0L, `2008` = 0L, `2009` = 0L,
    `2010` = 0L, `2011` = 0L, `2012` = 0L, `2013` = 0L, `2014` = 0L,
    `2015` = 0L, `2016` = 1L, `2017` = 222L, `2018` = 173L, `2019` = 87L
    ), `2018` = c(`2007` = 0L, `2008` = 0L, `2009` = 0L, `2010` = 0L,
    `2011` = 0L, `2012` = 0L, `2013` = 0L, `2014` = 0L, `2015` = 0L,
    `2016` = 0L, `2017` = 1L, `2018` = 314L, `2019` = 144L), `2019` = c(`2007` = 0L,
    `2008` = 0L, `2009` = 0L, `2010` = 0L, `2011` = 0L, `2012` = 0L,
    `2013` = 0L, `2014` = 0L, `2015` = 0L, `2016` = 0L, `2017` = 0L,
    `2018` = 3L, `2019` = 246L))

    out <- do.call(Rbind, input.list)
    unique.names <- unique(unlist(lapply(input.list, names)))
    expect_equal(dim(out), c(length(input.list), length(unique.names)))
    expect_equal(rownames(out), names(input.list))
})
