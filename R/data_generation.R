for (i in seq(from = 1, to = 10)) {
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
              "frequency" = runif(
                sample(seq(from = 5, to = 150), 1),
                min(dt$data$frequency),
                max(dt$data$frequency)
              ),
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
      numFrequencies = nrow(dt$data) + 1,
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
                ((length(diff_2D) == length(cross_3D)) ==
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
    m_ytrain[count, ] <-
      to_categorical(trunc(dt$dnu, 3), num_classes) +
      (to_categorical(trunc(dt$dr, 3), num_classes) * 2)
    count <- count + 1
  }
  
  # Split train/test
  smp_size <- floor(0.75 * nrow(m_xtrain))
  set.seed(123)
  ind <- sample(seq_len(nrow(m_xtrain)), size = smp_size)
  
  # Prepare partition
  x_train <- m_xtrain[ind, ,]
  x_test  <- m_xtrain[-ind, ,]
  y_train <- m_ytrain[ind,]
  y_test  <- m_ytrain[-ind,]
  
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