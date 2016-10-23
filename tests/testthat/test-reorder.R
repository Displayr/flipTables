context("Reorder")

test_that("Reorder",
          {
              #1D
              z <- matrix(1:10, ncol = 1, dimnames = list(LETTERS[1:10], "Dog"))
              expect_equal(Reorder(z, rows = "Ascending"), z)
              expect_equal(Reorder(z), z[10:1, , drop = FALSE])
              expect_equal(Reorder(z, "None", "None"), z)
              #2D
              z <- matrix(c(1:10,2:11), ncol = 2, dimnames = list(LETTERS[1:10], c("Smaller", "Bigger")))
              expect_equal(Reorder(z, "Ascending", "Ascending"), z)
              expect_equal(Reorder(z), z[10:1, 2:1])
              expect_equal(Reorder(z, "None", "None"), z)
              # Missing values
              z[9, 1] <- NA
              expect_equal(Reorder(z, "Ascending", "Ascending"), z)
              expect_equal(Reorder(z, "None", "None"), z)
              expect_equal(Reorder(z), z[10:1, 2:1])
              # Ties
              z[4:6, ] <- 6
              expect_equal(Reorder(z), z[10:1, 2:1])
              expect_equal(Reorder(z, "None", "None"), z)
              # All the same
              z <- matrix(9, 9, 9, dimnames = list(LETTERS[1:9], letters[1:9]))
              expect_equal(Reorder(z, "Ascending", "Ascending"), z)
              expect_equal(Reorder(z), z[9:1, 9:1])
              expect_equal(Reorder(z, "None", "None"), z)
          })
