context("Fourier transform")
library(variableStars)

test_that("compute_ftt returns the same length that the input vector", {
  for (i in seq(1, 100)) {
    x <-
      sample(seq(
        from = -100,
        to = 100,
        by = 0.1
      ),
      size = round(runif(1, min = 10, max = 1000)),
      replace = TRUE)
    # Check same length
    expect_equal(length(compute_fft(x)), length(x))
  }
})

test_that("compute_ftt returns complex numbers", {
  for (i in seq(1, 100)) {
    x <-
      sample(seq(
        from = -100,
        to = 100,
        by = 0.1
      ),
      size = round(runif(1, min = 10, max = 1000)),
      replace = TRUE)
    # Check all complex numbers
    expect_true(is.complex(compute_fft(x)))
  }
})



test_that("calculate_amplitudes returns two columns with numeric values", {
  for (i in seq(1, 10)) {
    x <-
      sample(seq(
        from = -100,
        to = 100,
        by = 0.1
      ),
      size = round(runif(1, min = 10, max = 1000)),
      replace = TRUE)
    # Get spectrum
    time <- seq(from = 1, to = length(x))
    spectrum <- calculate_amplitudes(time, x)
    # Check all complex numbers
    expect_true(is.numeric(spectrum$amplitude))
    expect_true(is.numeric(spectrum$frequency))
  }
})


test_that("calculate_amplitudes returns a maximum on a sin wave in comparision with a random one",
          {
            for (i in seq(1, 100)) {
              x <-
                sin(seq(from = 0,
                        to = 10,
                        by = 0.01))
              # Add noise
              x <- sin(x) + rnorm(length(x), 0, runif(1, 0.1, 1))
              # Add time as secuence of seconds
              time <- seq(from = 1, to = length(x))
              # Calculate the espectrum of the sin signal
              spectrumSin <- calculate_amplitudes(time, x)$amplitude
              # Calculate the espectrum of the random signal
              spectrumRand <-
                calculate_amplitudes(time, sample(x))$amplitude
              # Check amplitudes differences
              expect_gt(spectrumSin[which.max(spectrumSin)],
                        spectrumRand[which.max(spectrumRand)])
            }
          })
