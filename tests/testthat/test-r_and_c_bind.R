context("Rbind")

test_that("Rbind",
          {
              expect_equal(suppressWarnings(Rbind(1:5, 1:5)), rbind(1:5, 1:5))
              a <- c(a = 1, b = 3)
              expect_equivalent(Rbind(a), a)
              b <- c(a = 1, b = 2)
              expect_equal(Rbind(a, b), rbind(a, b))
              b <- c(a = 1, c = 2)
              expect_equivalent(Rbind(a, b), matrix(c(1, 1, NA, 2, 3, NA)))
              a <- NULL
              b <- c(a = 1, c = 2)
              expect_equivalent(Rbind(a, b), rbind(a, b))
          })

