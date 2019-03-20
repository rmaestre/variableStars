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
generate_data_modes <-
  function(deltaNu, nuRange, deltaR, numPoints) {
    # DS to return data
    df <- data.frame()
    # Range for data generation
    range <- c(nuRange[1] * deltaNu, nuRange[2] * deltaNu)
    
    # L0 distance from origin
    range_dl0 <- c(0, 10)
    
    # Avoid negative ranges
    dl0 <- round(runif(1, range_dl0[1], range_dl0[2]), 2)
    while (range[2] - (range[1] + dl0) < 0) {
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
      "dr" = dr / 0.0864,
      "dnu" = (df[2, ]$frequency - df[1, ]$frequency) / 0.0864
    ))
  }


#' @export
#'
# Normalize vectors
normalized <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}


#' @export
#'
# Minibatches samples
generate_batches <- function(experiment_number, input_dim, num_classes, cuts_breaks) {
    for (i in seq(from = 1, to = 5)) {
      # ND dimensional array for X train
      rows <- experiment_number + 1
      cols <- input_dim
      dimensions <- 3 # Number of channels
      m_xtrain <- array(0, c(rows, cols, dimensions))
      # Y train is a 1D matrix with rows and targets
      m_ytrain <- matrix(nrow = rows, ncol = num_classes)
      
      # Loop generating data
      count <- 1
      for (experiment in seq(1:(experiment_number + 1))) {
        # Select experiment parameters
        dnu <- trunc(runif(1, 1, 10), prec = 4)
        dr <- trunc(runif(1, 0, dnu), prec = 4)
        # Debug info with experiment configuration
        if (count %% 250 == 0) {
          print(paste("Experiment:",
                      count,
                      " | dnu:",
                      dnu,
                      " | dr:",
                      dr,
                      sep = ""))
        }
        
        # Data generation
        dt <- generate_data_modes(
          deltaNu = dnu,
          deltaR = dr,
          nuRange = c(2.5, 10),
          numPoints = 7
        )
        # Add noise
        dt$data <-
          rbind(dt$data,
                data.frame(
                  "frequency" = 10,
                  "mode" = "random",
                  "amplitude" = 1.0
                ))
        dt$data$amplitude <- 1.0
        
        
        # Execute experiment
        result <- process(
          frequency = dt$data$frequency,
          amplitude = dt$data$amplitude,
          filter = "uniform",
          gRegimen = 0,
          maxDnu = 1,
          minDnu = 15,
          numFrequencies = ifelse(nrow(dt$data) == 30, 31, 30),
          dnuGuessError = -1,
          debug = F
        )
        
        # X data. THe maximum value is processed in each bucket
        # ----------------------
        # Save fourier transform
        ftS <-
          stats.bin(as.numeric(result$fresAmps[[names(result$fresAmps)[1]]]$fInv),
                    as.numeric(result$fresAmps[[names(result$fresAmps)[1]]]$b),
                    breaks = cuts_breaks)$stats
        ft_1D <- ftS[8, 1:(length(cuts_breaks) - 1)]
        ft_1D[is.na(ft_1D)] <- 0
        
        
        # Save histogram of diffs
        diffS <-
          stats.bin(
            as.numeric(result$diffHistogram$histogram$bins),
            as.numeric(result$diffHistogram$histogram$values),
            breaks = cuts_breaks
          )$stats
        diff_2D <- diffS[8, 1:(length(cuts_breaks) - 1)]
        diff_2D[is.na(diff_2D)] <- 0
        
        # Save crosscorrelation
        cross <- stats.bin(
          as.numeric(result$crossCorrelation$index),
          as.numeric(result$crossCorrelation$autocorre),
          breaks = cuts_breaks
        )$stats
        cross_3D <- cross[8, 1:(length(cuts_breaks) - 1)]
        cross_3D[is.na(cross_3D)] <- 0
        
        # Assert all dimensions are equal
        stopifnot((length(ft_1D) == length(diff_2D)) ==
                    ((
                      length(diff_2D) == length(cross_3D)
                    ) ==
                      (
                        length(cross_3D) == length(cuts_breaks) - 1
                      )))
        
        # X data
        m_xtrain[count, , 1] <- normalized(ft_1D)
        m_xtrain[count, , 2] <- normalized(diff_2D)
        m_xtrain[count, , 3] <- normalized(cross_3D)
        
        
        # Y data
        # First pattern [dnu] is coded with "1" in the one-hot encoding
        # Second patter [dr] is coded with "2" in the one-hot encodinf
        m_ytrain[count,] <-
          to_categorical(trunc(dt$dnu, 3), num_classes) +
          (to_categorical(trunc(dt$dr, 3), num_classes) * 2)
        count <- count + 1
      }
      
      # Split train/test
      smp_size <- floor(0.75 * nrow(m_xtrain))
      set.seed(123)
      ind <- sample(seq_len(nrow(m_xtrain)), size = smp_size)
      
      # Prepare partition
      x_train <- m_xtrain[ind, , ]
      x_test  <- m_xtrain[-ind, , ]
      y_train <- m_ytrain[ind, ]
      y_test  <- m_ytrain[-ind, ]
      
      # Save to disk
      v_name_x_train <- paste("x_train", i, sep = "")
      v_name_x_test  <- paste("x_test", i, sep = "")
      v_name_y_train <- paste("y_train", i, sep = "")
      v_name_y_test  <- paste("y_test", i, sep = "")
      # Assign data to new variables names
      assign(v_name_x_train, x_train)
      assign(v_name_x_test, x_test)
      assign(v_name_y_train, y_train)
      assign(v_name_y_test, y_test)
      # Save variables to disk
      save(
        list = v_name_x_train,
        file = paste("~/Downloads/", v_name_x_train, ".RData", sep = "")
      )
      save(
        list = v_name_x_test,
        file =  paste("~/Downloads/", v_name_x_test, ".RData", sep = "")
      )
      save(
        list = v_name_y_train,
        file = paste("~/Downloads/", v_name_y_train, ".RData", sep = "")
      )
      save(
        list = v_name_y_test,
        file =  paste("~/Downloads/", v_name_y_test, ".RData", sep = "")
      )
    }
  }
