context("Rbind and Cbind")

test_that("Rbind",
          {
              expect_equal(suppressWarnings(Rbind(1:5, 1:5)), rbind(1:5, 1:5))
              a <- c(a = 1, b = 3)
              expect_equivalent(Rbind(a), a)
              b <- c(a = 1, b = 2)
              expect_equal(Rbind(a, b), rbind(a, b))
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


test_that("Rbind",
          {
              expect_equal(suppressWarnings(Cbind(1:5, 1:5)), cbind(1:5, 1:5))
              a <- c(a = 1, b = 3)
              expect_equivalent(Cbind(a), a)
              b <- c(a = 1, b = 2)
              expect_equal(Cbind(a, b), cbind(a, b))
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

