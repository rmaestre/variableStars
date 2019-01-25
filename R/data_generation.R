#' @export
generate_data <- function(numFreqs,
                          distance,
                          periodF,
                          periodS,
                          baseAMplitudeFirst,
                          baseAMplitudeSecond,
                          seed,
                          freqOneRandRange,
                          freqTwoRandRange,
                          ampRandRange) {
  # Create data.frame
  dt <-
    data.frame(
      "x" = seq(from = 0, to = numFreqs - 1, by = periodF),
      "y" = baseAMplitudeFirst,
      "pattern" = 1
    )
  dtDos <-
    data.frame(
      "x" = seq(from = distance, to = numFreqs - 1, by = periodS),
      "y" = baseAMplitudeSecond,
      "pattern" = 2
    )
  dt <- rbind(dt, dtDos)
  
  # Apply random
  set.seed(seed)
  # Add random noise on frequence
  if (freqOneRandRange > 0) {
    dt[dt$pattern == 1, ]$x <-
      rnorm(nrow(dt[dt$pattern == 1, ]), dt[dt$pattern == 1, ]$x , freqOneRandRange)
  }
  if (freqTwoRandRange > 0) {
    dt[dt$pattern == 2, ]$x <-
      rnorm(nrow(dt[dt$pattern == 2, ]), dt[dt$pattern == 2, ]$x , freqTwoRandRange)
  }
  dt$y <- abs(rnorm(nrow(dt), dt$y, ampRandRange))
  # Sort by frecuency
  dt <- dt[order(dt$x),]
  return(dt)
}