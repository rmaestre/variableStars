Neural Network approach for detecting Dnu and dr
================
Roberto Maestre
10/24/2018

[![Build Status](https://travis-ci.org/rmaestre/variableStars.svg?branch=master)](https://travis-ci.org/rmaestre/variableStars)

Please, [read the package introduction](https://github.com/rmaestre/variableStars/blob/master/README.md) for initial details about the software, algorithms, workflow and data used.

Neural Network approach for detecting Dnu and dr
================================================

[Depthwise separable convolutions](https://arxiv.org/pdf/1610.02357.pdf) for machine learning consists in *spatial convolution performed independently over each channel of an input, followed by a pointwise convolution, projecting the channels output by the depthwise convolution onto a new channel space. *

The next figure represents an overview of the main Neural Network architecture used to identify Dnu and dr from a given power espectrum of a variable star:

<img src="https://raw.githubusercontent.com/rmaestre/variableStars/master/docs/figures/nn-approach.png" data-canonical-src="https://raw.githubusercontent.com/rmaestre/variableStars/master/docs/figures/nn-approach.png" width="500" />

Methodology
===========

Input data
----------

Each input channel channel is processed with the *variableStars* package. The data from the variable star is process by the main method

``` r
result <- process(
  dt.star$frequency,
  dt.star$amplitude,
  filter = "uniform",
  gRegimen = 0,
  minDnu = 15,
  maxDnu = 95,
  dnuValue = -1,
  dnuGuessError = 10,
  dnuEstimation = TRUE,
  numFrequencies = 30,
  debug = F
)
```

and each channel is extracted as:

-   Fourier transform

``` r
dt <- prepare_periodicities_dataset(result$fresAmps)
dt <- dt[dt$label=="30  freqs",]
plot_periodicities_ggplot(dt)
```

![](NN-dnudr_files/figure-markdown_github/unnamed-chunk-2-1.png)

-   Histogram of differences

``` r
dt <- data.frame(result$diffHistogram$histogram)
plot_histogram_ggplot(dt)
```

![](NN-dnudr_files/figure-markdown_github/unnamed-chunk-3-1.png)

-   Autocorrelation

``` r
dt <- data.frame(result$crossCorrelation)
plot_crosscorrelation_ggplot(dt)
```

![](NN-dnudr_files/figure-markdown_github/unnamed-chunk-4-1.png)

Note: The informaction of each channel is scaled between \[0,1\].

Neural Network targets
----------------------

The input for the Nural Network, is bucketized with a given resolution; transforming the frecuency range into classes:

``` r
resolution <- 0.5
# Input dimension
cuts_breaks <- c(-Inf, seq(0, 101, resolution), Inf)
cuts_breaks
```

    ##   [1]  -Inf   0.0   0.5   1.0   1.5   2.0   2.5   3.0   3.5   4.0   4.5
    ##  [12]   5.0   5.5   6.0   6.5   7.0   7.5   8.0   8.5   9.0   9.5  10.0
    ##  [23]  10.5  11.0  11.5  12.0  12.5  13.0  13.5  14.0  14.5  15.0  15.5
    ##  [34]  16.0  16.5  17.0  17.5  18.0  18.5  19.0  19.5  20.0  20.5  21.0
    ##  [45]  21.5  22.0  22.5  23.0  23.5  24.0  24.5  25.0  25.5  26.0  26.5
    ##  [56]  27.0  27.5  28.0  28.5  29.0  29.5  30.0  30.5  31.0  31.5  32.0
    ##  [67]  32.5  33.0  33.5  34.0  34.5  35.0  35.5  36.0  36.5  37.0  37.5
    ##  [78]  38.0  38.5  39.0  39.5  40.0  40.5  41.0  41.5  42.0  42.5  43.0
    ##  [89]  43.5  44.0  44.5  45.0  45.5  46.0  46.5  47.0  47.5  48.0  48.5
    ## [100]  49.0  49.5  50.0  50.5  51.0  51.5  52.0  52.5  53.0  53.5  54.0
    ## [111]  54.5  55.0  55.5  56.0  56.5  57.0  57.5  58.0  58.5  59.0  59.5
    ## [122]  60.0  60.5  61.0  61.5  62.0  62.5  63.0  63.5  64.0  64.5  65.0
    ## [133]  65.5  66.0  66.5  67.0  67.5  68.0  68.5  69.0  69.5  70.0  70.5
    ## [144]  71.0  71.5  72.0  72.5  73.0  73.5  74.0  74.5  75.0  75.5  76.0
    ## [155]  76.5  77.0  77.5  78.0  78.5  79.0  79.5  80.0  80.5  81.0  81.5
    ## [166]  82.0  82.5  83.0  83.5  84.0  84.5  85.0  85.5  86.0  86.5  87.0
    ## [177]  87.5  88.0  88.5  89.0  89.5  90.0  90.5  91.0  91.5  92.0  92.5
    ## [188]  93.0  93.5  94.0  94.5  95.0  95.5  96.0  96.5  97.0  97.5  98.0
    ## [199]  98.5  99.0  99.5 100.0 100.5 101.0   Inf

The target output, is bucketized, transforming the frecuency range into classes:

``` r
# Output dimension
output_classes <- seq(from = 0,
                      to = 14 / 0.0864,
                      by = 1)
output_classes
```

    ##   [1]   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16
    ##  [18]  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33
    ##  [35]  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50
    ##  [52]  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67
    ##  [69]  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84
    ##  [86]  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 101
    ## [103] 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118
    ## [120] 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135
    ## [137] 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152
    ## [154] 153 154 155 156 157 158 159 160 161 162

Note: Input and output dimension are fixed.

Experiment I
============

A simple synthetic data without noise added.

![](NN-dnudr_files/figure-markdown_github/unnamed-chunk-7-1.png)

Experiment II
=============

A simple synthetic data without noise added.

![](NN-dnudr_files/figure-markdown_github/unnamed-chunk-8-1.png)

Results
-------

|                    |   loss|  acc.at1|  acc.at2|  acc.at4|
|--------------------|------:|--------:|--------:|--------:|
| Experiment I - Dnu |  1.630|    0.408|    0.639|    0.853|
| Experiment I - Dr  |  1.801|    0.482|    0.672|    0.812|
