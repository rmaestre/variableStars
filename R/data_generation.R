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



#' @export
generate_data_modes <- function(deltaNu, nuRange, deltaR, numPoints) {
  # DS to return data
  df <- data.frame()
  # Range for data generation
  range <- c(nuRange[1] * deltaNu, nuRange[2] * deltaNu)
  
  # L0 distance from origin
  range_dl0 <- c(0, 10)
  
  # Avoid negative ranges
  dl0 <- round(runif(1, range_dl0[1], range_dl0[2]), 2)
  while (range[2] - (range[1] + dl0)<0) {
    dl0 <- round(runif(1, range_dl0[1], range_dl0[2]), 2)
  }
  # Vector generation
  l0 <- seq(
    from = range[1] + dl0,
    to = range[2],
    by = (range[2] - (range[1] + dl0)) / numPoints
  )[1:numPoints]
  # Append data
  df <-
    rbind(df, data.frame(
      "frequency" = l0,
      "mode" = "l0",
      "amplitude" = 1
    ))
  
  
  # L1 distance from origin
  dl1 <-
    round(runif(1, 0, l0[2] - l0[1]), 2) # Rand betwee first and second L2
  # Vector generation
  print("-----")
  print(deltaNu)
  print(dl0)
  print(range[1] + dl0)
  print(range[2])
  print((range[2] - (range[1] + dl0)) / numPoints)
  
  print(range)
  print(l0)
  print(dl1)
  print(l0[1] + dl1)
  print("-----")
  l1 <- seq(
    from = l0[1] + dl1,
    to = range[2],
    by = (range[2] - (l0[1] + dl1)) / numPoints
  )[1:numPoints]
  
  # Append data
  df <-
    rbind(df,
          data.frame(
            "frequency" = l1,
            "mode" = "l1",
            "amplitude" = 0.8
          ))
  
  
  # M-1, M1 distance from l1
  dr <- round(runif(1, 0, deltaNu), 2)
  
  mp1 <- l1 + dr
  ml1 <- l1 - dr
  
  # Append data
  df <-
    rbind(df,
          data.frame(
            "frequency" = mp1,
            "mode" = "mp1",
            "amplitude" = 0.75
          ))
  df <-
    rbind(df,
          data.frame(
            "frequency" = ml1,
            "mode" = "ml1",
            "amplitude" = 0.75
          ))
  
  stopifnot(length(l0) == numPoints) # Length assert for L0
  stopifnot(length(l1) == numPoints) # Length assert for L0
  stopifnot(length(mp1) == numPoints) # Length assert for L0
  stopifnot(length(ml1) == numPoints) # Length assert for L0
  
  return(list(
    "data" = df,
    "dl0" = dl0,
    "dl1" = dl1,
    "dr" = dr,
    "dnu" = deltaNu
  ))
}
