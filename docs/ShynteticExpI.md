Synthetic Experiment. I
================
Roberto Maestre
12/17/2018

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

#### Frequences and Amplitudes on photometry data

``` r
# Generate first pattern
dt.spectrum <- data.frame(
  "frequency" = seq(from=0, to=10, by=0.25) ,
  "amplitude" = 10
)
# Generate second pattern as the biased first
dt.spectrum.bias <- data.frame(dt.spectrum)
dt.spectrum.bias$frequency <- dt.spectrum.bias$frequency + 0.3
dt.spectrum.bias$amplitude <- 5 + rnorm(nrow(dt.spectrum.bias),0,1.0)
#  
# All together
dt.spectrum <- rbind(dt.spectrum, dt.spectrum.bias)

# Get max amplitude
maxAmplitude <- dt.spectrum[which.max(dt.spectrum$amplitude), ]
# Plot amplitudes
plot_spectrum(min(dt.spectrum$frequency),
              max(dt.spectrum$frequency),
              dt.spectrum)
```

![](ShynteticExpI_files/figure-markdown_github/calculateEspectrum-1.png)

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
  filter = "uniform",
  gRegimen = 0,
  minDnu = 15,
  maxDnu = 95,
  dnuValue = -1,
  dnuGuessError = 10,
  dnuEstimation = TRUE,
  numFrequencies = 30,
  debug = TRUE
)
```

    ## ::: Debug information :::
    ## 
    ## Number of frequences to be processed: 82
    ## Number of frequences after drop the g regimen: 81
    ## Frequencies: 2.89352, 5.78704, 8.68056, 11.5741, 14.4676, 17.3611, 20.2546, 23.1481, 26.0417, 28.9352, 31.8287, 34.7222, 37.6157, 40.5093, 43.4028, 46.2963, 49.1898, 52.0833, 54.9769, 57.8704, 
    ## Range: 30, 60, 81, 
    ##  Iteration over range: 30
    ##    Frequencies selected: 2.89352, 5.78704, 8.68056, 11.5741, 14.4676, 17.3611, 20.2546, 23.1481, 26.0417, 28.9352, 
    ##    Amplitudes selected: 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 
    ##     Dnu: 2.8909
    ##     Dnu Peak: 2.8909
    ##     Dnu Guess: 0.964506
    ##     Cross correlation calculated:
    ##  Iteration over range: 60
    ##    Frequencies selected: 2.89352, 5.78704, 8.68056, 11.5741, 14.4676, 17.3611, 20.2546, 23.1481, 26.0417, 28.9352, 
    ##    Amplitudes selected: 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 
    ##  Iteration over range: 81
    ##    Frequencies selected: 2.89352, 5.78704, 8.68056, 11.5741, 14.4676, 17.3611, 20.2546, 23.1481, 26.0417, 28.9352, 
    ##    Amplitudes selected: 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,

#### Main results

-   **Dnu** (`result$dnu`) = 2.8909.
-   **DnuGuess** `(result$dnuGuess`) = 0.9645062.
-   **DnuPeak** (`result$dnuPeak`) = 2.8909.
-   **Frequency** (`result$photometry$frequency`) = ...
-   **Amplitude** (`result$photometry$amplitude`) = ...
-   **Diffs** (`result$diffHistogram$diffs`) = 2.8935185, 5.787037, 8.6805556, 11.5740741, 14.4675926, 17.3611111, 20.2546296, 23.1481481, 26.0416667, 28.9351852, 31.8287037, 34.7222222, 37.6157407, 40.5092593, 43.4027778, 46.2962963, 49.1898148, 52.0833333, 54.9768519, 57.8703704, 60.7638889, 63.6574074, 66.5509259, 69.4444444, 72.337963...

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

![](ShynteticExpI_files/figure-markdown_github/apodization-1.png)

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
  scale_color_lancet() +
  xlim(0, 10)
```

![](ShynteticExpI_files/figure-markdown_github/ftPower-1.png)

#### Histogram fo differences.

We only show bins with &gt;0 values

``` r
dt <- data.frame(result$diffHistogram$histogram)
ggplot(aes(x = bins, y = values), data = dt[dt$values > 0,]) +
  geom_bar(stat = "identity") +
  ggtitle("Histogram of differences") +
  theme_bw()
```

![](ShynteticExpI_files/figure-markdown_github/diffsHistogram-1.png)

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

![](ShynteticExpI_files/figure-markdown_github/autocor-1.png)

#### For first all frecuencies

``` r
dt <- data.frame(
  "x" = result$echelle$modDnuStacked,
  "y" = result$echelle$freMas,
  "h" = result$echelle$amplitudes
)
plot_echelle(dt)
```

![](ShynteticExpI_files/figure-markdown_github/echelle30-1.png)
