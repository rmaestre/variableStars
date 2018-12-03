Experiments
================
Roberto Maestre
10/24/2018

Experiment configuration
------------------------

#### Parameters for execution

``` r
paramters = list(
  "filter" = "uniform",
  "gRegimen" = 0,
  "minDnu" = 15,
  "maxDnu" = 95,
  "dnuValue" = -1,
  "dnuGuessError" = 10,
  "dnuEstimation" = TRUE,
  "numFrequencies" = 30,
  "debug" = TRUE)
```

#### Data source

Choose:

-   dataFlag = T, to use synthetic data generated or,

-   dataFlag = F, to use a real pulsar photometry

``` r
dataFlag = F
```

Data generation
---------------

``` r
# Generate synthetic data or read pulsar data
if (dataFlag) {
  x <- sin(seq(from = 0,
               to = 100,
               by = 1))
  dt <-
    data.frame("time" = seq(from = 1, to = length(x)), "mmag" = x)
} else {
  # Read pulsar data
  dt <- data.table(read.csv("../data/pulsar.tsv", sep = "\t"))
}

# Sampling for large file
dt.plot <- dt
if(dim(dt)[1]>1000) {
  dt.plot <- dt[sample(nrow(dt), 1000),]
}
# Plot photometry
ggplot(aes(time, mmag), data = dt.plot) +
    geom_point() +
    geom_line() +
    theme_bw()
```

![](Experiments_files/figure-markdown_github/dataGeneration-1.png)

``` r
rm(dt.plot) # Drop memory
```

#### Frequences and Amplitudes on photometry data

``` r
# Calculate amplitudes and frequences
dt.spectrum <- data.frame(calculateSpectrum(dt$time, dt$mmag))
# Get max amplitude
maxAmplitude <- dt.spectrum[which.max(dt.spectrum$amplitude), ]
# Plot amplitudes
plot_spectrum(maxAmplitude$frequency - 1.5,
              maxAmplitude$frequency + 1.5,
              dt.spectrum)
```

![](Experiments_files/figure-markdown_github/calculateEspectrum-1.png)

``` r
# Save Data to disk (to be replicated)
write.table(
  dt.spectrum[c("frequency", "amplitude")],
  file = "/tmp/data.csv",
  sep = "\t",
  quote = F,
  row.names = F,
  col.names = F
)
```

Experiment execution
--------------------

process is the main method on the variableStars package to compute and estimate all parameters

``` r
result <- process(
  dt.spectrum$frequency,
  dt.spectrum$amplitude,
  filter = paramters$filter,
  gRegimen = paramters$gRegimen,
  minDnu = paramters$minDnu,
  maxDnu = paramters$maxDnu,
  dnuValue = paramters$dnuValue,
  dnuGuessError = paramters$dnuGuessError,
  dnuEstimation = paramters$dnuEstimation,
  numFrequencies = paramters$numFrequencies,
  debug = paramters$debug
)
```

    ## ::: Debug information :::
    ## 
    ## Number of frequences to be processed: 43372
    ## Number of frequences after drop the g regimen: 21686
    ## Frequencies: 264.505, 264.113, 260.586, 264.897, 263.721, 265.288, 268.423, 263.329, 256.668, 251.965, 265.68, 252.749, 268.815, 261.762, 262.154, 268.031, 269.207, 271.95, 272.342, 261.37, 
    ## Range: 30, 60, 90, 
    ##  Iteration over range: 30
    ##    Frequencies selected: 264.505, 264.113, 260.586, 264.897, 263.721, 265.288, 268.423, 263.329, 256.668, 251.965, 
    ##    Amplitudes selected: 2.18044, 0.514798, 0.512875, 0.489923, 0.370046, 0.343731, 0.263693, 0.263439, 0.251894, 0.221904, 
    ##     Dnu: 1.0693
    ##     Dnu Peak: 3.9403
    ##     Dnu Guess: 82.9435
    ##     Cross correlation calculated:
    ##  Iteration over range: 60
    ##    Frequencies selected: 264.505, 264.113, 260.586, 264.897, 263.721, 265.288, 268.423, 263.329, 256.668, 251.965, 
    ##    Amplitudes selected: 2.18044, 0.514798, 0.512875, 0.489923, 0.370046, 0.343731, 0.263693, 0.263439, 0.251894, 0.221904, 
    ##  Iteration over range: 90
    ##    Frequencies selected: 264.505, 264.113, 260.586, 264.897, 263.721, 265.288, 268.423, 263.329, 256.668, 251.965, 
    ##    Amplitudes selected: 2.18044, 0.514798, 0.512875, 0.489923, 0.370046, 0.343731, 0.263693, 0.263439, 0.251894, 0.221904,

#### Main results

-   **Dnu** (`result$dnu`) = 1.0693.
-   **DnuGuess** `(result$dnuGuess`) = 82.9434636.
-   **DnuPeak** (`result$dnuPeak`) = 3.9403.
-   **Frequency** (`result$photometry$frequency`) = ...
-   **Amplitude** (`result$photometry$amplitude`) = ...
-   **Diffs** (`result$diffHistogram$diffs`) = 0.3918589, 3.9185888, 0.3918589, 0.7837178, 0.7837178, 3.9185888, 1.1755766, 7.8371777, 12.5394843, 1.1755766, 11.7557665, 4.3104477, 2.7430122, 2.3511533, 3.5267299, 4.7023066, 7.4453188, 7.8371777, 3.1348711, 8.2290365, 11.3639076, 15.6743553, 15.2824964, 2.3511533, 2.7430122...

#### Apodization

``` r
# Plot frecuency and amplitude
ggplot(
  aes(x = frequences, y = amplitude),
  data = data.frame(
    "frequences" = result$apodization$frequences,
    "amplitude" = result$apodization$amp
  )
) +
  geom_point() +
  geom_line() +
  ggtitle("Apodization- Frequences and amplitudes") +
  theme_bw()
```

![](Experiments_files/figure-markdown_github/apodization-1.png)

#### Periodicities

``` r
# DS to save all data
dt <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("fInv", "b", "label"))
# Prepare data
ranges <- names(result$fresAmps)
for(range in ranges){
  dt <- rbind(dt, 
  data.frame("fInv"=result$fresAmps[[as.character(range)]][["fInv"]],
             "b"=result$fresAmps[[as.character(range)]][["b"]],
            "label"=paste(result$fresAmps[[as.character(range)]][["label"]]," freqs")))
}
# Plot frecuency and amplitude
ggplot(aes(x = fInv, y = b, group=label, colour=label), data = dt) +
  #geom_point(alpha=0.2) +
  geom_line(alpha=0.8) +
  ggtitle(expression(paste("Periodicities (",d^-1,")"))) +
  xlab(expression(paste("Periodicities (",mu,"hz)"))) +
  ylab("Amplitude") +
  theme_bw() + 
  scale_color_lancet()
```

![](Experiments_files/figure-markdown_github/ftPower-1.png)

#### Histogram fo differences.

We only show bins with &gt;0 values

``` r
dt <- data.frame(result$diffHistogram$histogram)
ggplot(aes(x = bins, y = values), data = dt[dt$values > 0,]) +
  geom_bar(stat = "identity") +
  ggtitle("Histogram of differences") +
  theme_bw()
```

![](Experiments_files/figure-markdown_github/diffsHistogram-1.png)

### Autocorrelation

``` r
dt <- data.frame(result$crossCorrelation)

ggplot(aes(x = index, y = autocorre), data = dt) +
  geom_line(stat = "identity") +
  ggtitle("Autocorrelacion (Crosscorrelation)") +
  xlab(expression(paste("Periodicities (",mu,"hz)"))) +
  ylab("Autocorrelation") +
  theme_bw()
```

![](Experiments_files/figure-markdown_github/autocor-1.png)
