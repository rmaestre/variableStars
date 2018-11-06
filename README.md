Variable Stars - An optimized package to find patterns in variable stars
================
Roberto Maestre
10/24/2018

Library Introduction
--------------------

Talk about variable stars ... (tbd)

data arquitecto, data developer, data analyst

Introduction
------------

The Fourier transform (FT) decomposes a function of time (a signal) into the frequencies that make it up. In Astrophysics, specially in Pulsars study, this technique suposes the main tool to study it patterns, and therefore, clasiffy this kind of stars. The main formula of Discrete Fourier Transform is:

$$ F\_n = \\sum\_{k=0}^{N-1} f\_k \\cdot e^{-2 \\pi \\cdot i \\cdot n \\cdot \\frac{k}{N}}$$

Auxiliar functions
------------------

``` r
plot_spectrum <- function(min, max, dt) {
  maxAmplitude <- dt[which.max(dt$amplitude),]
  # Plot
  ggplot(aes(frequency, amplitude), data = dt) +
    geom_point(aes(alpha = 0.6)) +
    geom_point(aes(frequency, amplitude, colour = '#009988'), data = maxAmplitude) +
    geom_line() +
    theme_bw() +
    ylab("Amplitude") +
    xlab("Frecuency") +
    ggtitle(paste(
      "First Frecuency (F:",
      round(maxAmplitude$frequency, 4),
      ", A:",
      round(maxAmplitude$amplitude, 4),
      ")",
      sep = ""
    )) +
    xlim(min, max) +
    theme(legend.position = "none")
}
```

Examples
--------

### 1. Simple signal. Sin.

The first signal is the sin one. This signal is very clear and the period can be calculated visually.

``` r
x <- sin(seq(from = 0,
             to = 20,
             by = 0.05))
# Add secuential times
dt.test <-
  data.frame("time" = seq(from = 1, to = length(x)), "x" = x)
ggplot(aes(time, x), data = dt.test) +
  geom_point() +
  geom_line() +
  theme_bw()
```

![](README_files/figure-markdown_github/unnamed-chunk-2-1.png)

We campute the FT, calculating the amplitudes.

``` r
# Compute DFT
dt.espectrum <- calculate_amplitudes(dt.test$time, dt.test$x)
# Get max amplitude
maxAmplitude <- dt.espectrum[which.max(dt.espectrum$amplitude),]
# Plot amplitudes
plot_spectrum(0, 0.05, dt.espectrum)
```

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)

Therefore, the period is $p=\\frac{1}{F}$ = 133.6666667

### 2. A noisy signal example

Another synthetic example, where a more noisy signal is provided is the next one:

``` r
## noisy signal with amplitude modulation
x <- seq(from = 0,
         to = 1,
         length.out = 200)
n <- length(x)
# original data
y_org <- (1 + sin(2 * 2 * pi * x)) * sin(20 * 2 * pi * x)
# overlay some noise
x <- y_org + rnorm(n, sd = 0.2)
# Add secuential times
dt.test <-
  data.frame("time" = seq(from = 1, to = length(x)), "x" = x)
ggplot(aes(time, x), data = dt.test) +
  geom_point() +
  geom_line() +
  theme_bw()
```

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)

We use DFT to calculate the amplitude in each frecuency.

``` r
# Compute DFT
dt.spectrum <- calculate_amplitudes(dt.test$time, dt.test$x)
# Get max amplitude
maxAmplitude <- dt.spectrum[which.max(dt.spectrum$amplitude),]
# Plot amplitudes
plot_spectrum(0, 0.5, dt.spectrum)
```

![](README_files/figure-markdown_github/unnamed-chunk-5-1.png)

Therefore, the period is $p=\\frac{1}{F}$ = 10

### 3. Real data from a pulsar star

In this case, we use the photometry of a pulsar star, in which timestamp the magnitude of the pulsar is given. In this case, analize the patter in visually complex.

``` r
# Read pulsar data
dt.pulsar <- data.table(read.csv("data/pulsar.tsv", sep = "\t"))
ggplot(aes(time, magnitude), data = dt.pulsar[sample(nrow(dt.pulsar), 1000),]) +
  geom_point() +
  geom_line() +
  theme_bw()
```

![](README_files/figure-markdown_github/unnamed-chunk-6-1.png)

We use DFT to calculate the amplitude in each frecuency.

``` r
# Calculate
dt.spectrum <-
  calculate_amplitudes(dt.pulsar$time, dt.pulsar$magnitude)
# Get max amplitude
maxAmplitude <- dt.spectrum[which.max(dt.spectrum$amplitude),]
# Plot amplitudes
plot_spectrum(20, 25, dt.spectrum)
```

![](README_files/figure-markdown_github/unnamed-chunk-7-1.png)

Therefore, the period of this pulsar is $p=\\frac{1}{F}$ = 0.0437575.

Benchmark
---------

A benchmark is proposed to show the performance achieved by made all calculations witn C++ and Armadillo (RcppArmadillo). We achieve an average value of 310 ms to compute a DFT on 43372 time values.

``` r
m <-
  microbenchmark(dt <-
                   calculate_amplitudes(dt.pulsar$time, dt.pulsar$magnitude),
                 times = 100)
autoplot(m, log = F) +
  scale_x_discrete(labels = c("DFT on a Pulsar star data")) +
  xlab("")
```

    ## Coordinate system already present. Adding new coordinate system, which will replace the existing one.

    ## Scale for 'x' is already present. Adding another scale for 'x', which
    ## will replace the existing scale.

![](README_files/figure-markdown_github/unnamed-chunk-8-1.png)
