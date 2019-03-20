
[![Build Status](https://travis-ci.org/rmaestre/variableStars.svg?branch=master)](https://travis-ci.org/rmaestre/variableStars)

Introduction
------------

Variable Star package provides the main funtions to analized patterns on the [oscilation modes of variable stars](https://en.wikipedia.org/wiki/Asteroseismology).

<img src="https://raw.githubusercontent.com/rmaestre/variableStars/master/docs/figures/oscilationModes.png" data-canonical-src="https://raw.githubusercontent.com/rmaestre/variableStars/master/docs/figures/oscilationModes.png" width="200" />

All the code is based on these two papers:

-   [Asteroseismic analysis of the CoRoT *δ* Scuti star HD 174936](https://www.aanda.org/articles/aa/full_html/2009/40/aa11932-09/aa11932-09.html)

-   [An in-depth study of HD 174966 with CoRoT photometry and HARPS spectroscopy](https://www.aanda.org/articles/aa/full_html/2013/11/aa20256-12/aa20256-12.html)

Installation
------------

``` r
install.packages("devtools")
library(devtools)
install_github("rmaestre/variableStars")
```

A UI for experimentation with synthetic data is provided:

``` r
library(variableStars)
runUISynthetic()
```

[![UI](https://raw.githubusercontent.com/rmaestre/variableStars/master/docs/figures/ui.png)](https://raw.githubusercontent.com/rmaestre/variableStars/master/docs/figures/ui.png)

Example of use on a pulsar data
-------------------------------

Please, find [here](docs/Experiment_-_HD174936.md) or [here](docs/Experiment_-_HD174966.md) the main execution of the complete package procedure.

Main Workflow
-------------

<img src="https://raw.githubusercontent.com/rmaestre/variableStars/master/docs/figures/diagrams.png" data-canonical-src="https://raw.githubusercontent.com/rmaestre/variableStars/master/docs/figures/diagrams.png" width="500" />

(The pulsar in the Crab Nebula is composed by images taken by Hubble (red) and Chandra X-Ray(blue))

Implementation
--------------

All core funcionalities are programmed [in C++ using RcppArmadillo integrated through Rcpp](https://github.com/rmaestre/variableStars/blob/master/src/tools.cpp). An example of function to calculate all differences between pair of element using Armadillo C++ library, iterators and std operattions:

``` c
  // Calculate all frequences differences
  int n = frequences.n_elem;
  int diagSupElements = n * (n - 1) / 2;
  arma::vec diff(diagSupElements); // Number of elements in the sup. diag.
  NumericVector::iterator it_first, it_second, it_diff;
  it_diff = diff.begin(); // output iterator
  int countElements = 0;
  // Double loop (n^2 complexity)
  for (it_first = frequences.begin(); it_first < frequences.end(); it_first++) {
    for (it_second = it_first; it_second < frequences.end() & it_diff < diff.end(); it_second++) {
      if (it_first != it_second) { // Jump same elements
        * it_diff =
          std::abs( * it_second - * it_first); // Save absolute difference
        if ( * it_diff != 0) {
          it_diff++; // Increase pointer
          countElements++; // Increase elements
        }
      }
    }
  }
  // Remove unused memory
  diff.resize(diagSupElements - (diagSupElements - countElements));
  // Return results
  return diff;
}
```

However, all code can be call from R easily with the next function

``` r
result <- process(
  data$frequency,
  data$amplitude,
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
