#' @export
generate_data <- function(numFreqs, distance, baseAMplitudeFirst, baseAMplitudeSecond, shift, seed,
                          freqOneRandRange, freqTwoRandRange, ampRandRange) {
  # Create number of frecuences
  n <- round(numFreqs / 2)
  dis <- distance
  # Create data.frame
  dt <- data.frame("x"=seq(from=0, to=2*n*dis, by=dis), "y"=baseAMplitudeFirst)
  dt$pos <- seq(1,nrow(dt)) # Sequencial integers
  # Even elements with other amplitude value
  dt[lapply(dt$pos, "%%", 2) == 0,]$y <- baseAMplitudeSecond
  
  # Add shift displacement ob the first pattern
  dt[lapply(dt$pos, "%%", 2) == 0,]$x <- dt[lapply(dt$pos, "%%", 2) == 0,]$x + shift
  
  # Apply random 
  set.seed(seed)
  # Add random noise on frequence
  if (freqOneRandRange > 0) {
    dt[lapply(dt$pos, "%%", 2) == 0,]$x <- rnorm(n, 
                                                 dt[lapply(dt$pos, "%%", 2) == 0,]$x, 
                                                 freqOneRandRange)
  }
  if (freqTwoRandRange > 0) {
    dt[lapply(dt$pos, "%%", 2) != 0,]$x <- rnorm(n+1, 
                                                 dt[lapply(dt$pos, "%%", 2) != 0,]$x, 
                                                 freqTwoRandRange)
  }
  dt$y <- abs(rnorm(nrow(dt), dt$y, ampRandRange))
  return(dt)
}