library(gtools)

context("Rbind and Cbind")

v1 <- seq(1:10)
names(v1) <- letters[1:10]
v2 <- seq(1:11)
names(v2) <- letters[1:11]
v3 <- seq(100:112)
v4 <- seq(100:113)

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
              expect_equivalent(Rbind(a), a)
              b <- c(a = 1, b = 2)
              # The following line may fail and need to be commented out because testthat wrongly
              # says that Rbind(a,b) has no rownames.
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

              for (i in 1:nrow(perms)) {
                  if (i %in% c(5, 6, 12, 13, 36, 37, 43, 44))
                      expect_error(Rbind(all.items[[perms[i, 1]]], all.items[[perms[i, 2]]]), "Can not find any matching.")
                  else if (i %in% c(1, 8, 41, 48))
                      expect_error(Rbind(all.items[[perms[i, 1]]], all.items[[perms[i, 2]]]), NA)
                  else
                      expect_warning(Rbind(all.items[[perms[i, 1]]], all.items[[perms[i, 2]]]), "There are no matching.")
              }
          })


test_that("Cbind",
          {
              expect_equal(suppressWarnings(Cbind(1:5, 1:5)), cbind(1:5, 1:5))
              a <- c(a = 1, b = 3)
              expect_equivalent(Cbind(a), a)
              b <- c(a = 1, b = 2)
              # The following line may fail and need to be commented out because testthat wrongly
              # says that Cbind(a,b) has no colnames.
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

              for (i in 1:nrow(perms)) {
                  if (i %in% c(1, 6, 7, 8, 13, 14, 43, 44, 49, 50, 51, 56))
                      expect_error(Cbind(all.items[[perms[i, 1]]], all.items[[perms[i, 2]]]), NA)
                  else
                      expect_warning(Cbind(all.items[[perms[i, 1]]], all.items[[perms[i, 2]]]), "There are no matching.")
              }

          })

