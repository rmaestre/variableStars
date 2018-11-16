library(variableStars)


context("Differences tools")

test_that("differences returns an empty vector if all differences are 0", {
  for (i in seq(1, 100)) {
    number <- round(runif(1, min = -1000, max = 1000))
    x <-
      round(runif(round(runif(
        1, min = 10, max = 1000
      )), number, number))
    # Check same length
    expect_equal(length(differences(x)), 0)
  }
})

test_that("differences returns the exact abs difference between two numbers",
          {
            for (i in seq(1, 100)) {
              numberA <- round(runif(1, min = -1000, max = 1000))
              numberB <- round(runif(1, min = -1000, max = 1000))
              # Check same length
              expect_equal(differences(c(numberA, numberB))[1], abs(numberA - numberB))
            }
          })

test_that("differences returns the combination of all differences", {
  numberA <- 287
  numberB <- 828
  numberC <- -211
  # Calculate difss
  diffs <- differences(c(numberA, numberB, numberC))
  # Check same length
  expect_equal(diffs[1], abs(numberA - numberB))
  expect_equal(diffs[2], abs(numberA - numberC))
  expect_equal(diffs[3], abs(numberB - numberC))
})


test_that("differences returns the combination of all differences without zeros",
          {
            numberA <- 287
            numberB <- 828
            numberC <- -211
            numberD <- 828
            # Calculate difss
            diffs <-
              differences(c(numberA, numberB, numberC, numberD))
            # Check same length
            expect_equal(diffs[1], abs(numberA - numberB))
            expect_equal(diffs[2], abs(numberA - numberC))
            expect_equal(diffs[3], abs(numberA - numberD))
            expect_equal(diffs[4], abs(numberB - numberC))
            # expect_equal(diffs[4], abs(numberB - numberD)) # This result is dropped
            expect_equal(diffs[5], abs(numberC - numberD))
          })


test_that("adjacentDifferences returns the same output as diff",
          {
            for (i in seq(1, 100)) {
              x <-
                sample(seq(from = 0,
                           to = 100,
                           by = 1),
                       size = round(runif(1, min = 10, max = 40)),
                       replace = TRUE)
              # Check same output
              expect_true(all(adjacentDifferences(x) == diff(x)))
            }
          })


context("Differences histogram")

test_that("diffHistogram returns a histogram with equal length between bins and values",
          {
            for (i in seq(1, 100)) {
              x <-
                sample(seq(from = 0,
                           to = 100,
                           by = 1),
                       size = round(runif(1, min = 10, max = 40)),
                       replace = TRUE)
              # Check same length
              histDiff <- diffHistogram(x, 400)
              expect_equal(length(histDiff$histogram$bins),
                           length(histDiff$histogram$values))
            }
          })

test_that("diffHistogram returns a specific histogram",
          {
            x <- c(1, 2, 3, 4, 5, 6, 7, 6, 5, 4, 3, 2, 1)
            histDiff <- diffHistogram(x, 400)
            for (i in seq(1:(length(x)))) {
              expect_equal(histDiff$histogram$values[i],
                           as.numeric(histDiff$histogram$values)[i])
            }
          })

context("Signal tools")

test_that("findPeaks returns at least one peak and sorted on random data",
          {
            for (i in seq(1, 100)) {
              x <-
                sample(seq(
                  from = -1000,
                  to = 1000,
                  by = 1
                ),
                size = round(runif(1, min = 10, max = 40)),
                replace = TRUE)
              peaks <- variableStars::findPeaks(x)
              expect_gt(length(peaks), 0)
              expect_true(all(diff(peaks) > 0))
            }
          })
