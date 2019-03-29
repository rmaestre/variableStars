
### Theoretical Oscillation models

This notebook expose the method to apply a *Depthwise separable convolutional NN* on the oscilation star observation.


```R
#library(devtools)
#install_github("rmaestre/variableStars", ref="oscillationCodes")
library(variableStars)
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(plotly)
library(keras)
library(plotly)
library(abind)
library(fields)
```

### Global parameters


```R
# Number of rows per batch training
experiment_number <- 100
# Resolution for target frequency [0-100]
resolution <- 0.5

# Input dimension
cuts_breaks <- c(-Inf, seq(0, 101, resolution), Inf)
input_dim <- length(cuts_breaks) - 1

# Output dimension
num_classes <-
  length(seq(
    from = 0.1,
    to = 14 / 0.0864,
    by = 1
  )) # Buckets of possible classes

normalized <- function(x) {
        (x - min(x)) / (max(x) - min(x))
      }
```

### Auxiliar functions


```R
trunc <-
  function(x, ..., prec = 1)
    base::trunc(x * 10 ^ prec, ...) / 10 ^ prec


c_dnu <- function(x) {
    length(which(x == 1)) == 1
  }
  c_dr <- function(x) {
    length(which(x == 2)) == 1
  }
  c_over <- function(x) {
    length(which(x == 3)) == 1
  }

flat <- function(x) {
  return(paste0(trunc(c(x), prec=4), collapse = ","))
}
```

### File processing


```R
file_processing <- function(df, dr, dnu, file_output) {
  # Execute experiment
  result <- process(
    frequency = df$nu,
    amplitude = df$amp,
    filter = "uniform",
    gRegimen = 0,
    maxDnu = 1,
    minDnu = 15,
    numFrequencies = 30,
    dnuGuessError = -1,
    debug = F,
    processFirstRangeOnly = 30
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
  
  
  write(paste(
    flat(normalized(ft_1D)),
    flat(normalized(diff_2D)),
    flat(normalized(cross_3D)),
    flat(c(dr, dnu)),
    sep = ","
  )
  ,
  file = file_output,
  append = T)
  
}
```

### Data processing


```R
# Start log file
number_lines_to_split_file <- 10000
file_id <- 0

# Max n_mode
max_n <- 3
base_dir <- "/home/roberto/Downloads/evolutionTracks/FILOU/"

# Loop over files
setwd(base_dir)
dirs <- list.dirs(recursive = T)
count <- 0
for (dir in dirs[grepl("*VO*", list.dirs(recursive = T))]) {
  # Get full file
  full_dir <- paste0(base_dir, basename(dir))
  if (!is.na(basename(dir))) {
    #print(paste0("Processing directory: ", full_dir))
    # Change directory work
    setwd(full_dir)
    for (file in list.files(pattern = "*frq")) {
      # Read file
      data = read.csv(
        paste0(full_dir, "/", file),
        header = FALSE,
        sep = "",
        skip = 25,
        stringsAsFactors = FALSE,
        col.names = c("n", "l", "m", "nu", "f", "no",
                      "pc", "i0")
      )
    
      # Transform fequencies
      data$nu <-  data$nu * 0.086
        
      if (nrow(data) != 0) {
        # DR estimation
        dr <-
          as.numeric(strsplit(readLines(
            file(paste0(full_dir, "/", file), "r"), n = 15, skipNul = T
          )[14:14], "\\s+")[[1]][3])
        
        # DNU estimation
        dnu <- data[data$n >= 2 & data$n <= 8 & data$m == 0, ]
        dnu <- mean(aggregate(nu ~ l, data=dnu, function(x) mean(diff(x)))$nu)

                       
        # Generate random frequencies
        data$amp <- runif(length(data$nu), 0, 1)
        
        # Drop n modes under   max_n
        data <- data[data$n < max_n, ]
        # Keep only l modes 2,3
        data <- data[!data$l %in% c(2, 3), ]
        
        
        
        # Save data file and create a new one
        if (count %% number_lines_to_split_file == 0 ||
            count == 0) {
          # file name
          file_output <- paste0("~/Downloads/data", file_id, ".log")
          # Create file
          write(paste0(),
                file = file_output,
                append = F)
          # Increment file ide
          file_id <- file_id + 1
          
        }
        file_processing(data, dr, dnu, file_output)
        count <- count + 1
      } else {
          print(paste0("Empty file:",paste0(full_dir, "/", file)))
      }
    }
  }
}
```
