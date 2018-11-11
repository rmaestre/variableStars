#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace std;

//! Apply a sort of filter on a centered frequences vector
/*!
\param arma:vec vector with frequences to be processed
\param String sort of filter to be applied
\return A dataframe with centered frequences and processed
*/
//[[Rcpp::export]]
DataFrame apodization(arma::vec frequences, String filter) {
  // Data structures needed
  arma::vec frequencesCentered(frequences.n_elem);
  arma::vec amplitudes(frequences.n_elem);
  arma::vec factor(frequences.n_elem);
  
  // Center frequencies
  double max = frequences.max();
  double min = frequences.min();
  double middle = (max - min) / 2;
  frequencesCentered = frequences - min - middle;
  
  // Apply filter on centered frequences
  if (filter == "bartlett") {
    factor = 1 - arma::abs(frequencesCentered) / middle;
  } else if (filter == "blackman") {
    factor = 21.0 / 50.0 + 0.5 * arma::cos(M_PI * frequencesCentered / middle) +
      2.0 / 25.0 * arma::cos(2.0 * M_PI * frequencesCentered / middle);
  } else if (filter == "connes") {
    factor = arma::pow(1.0 - arma::pow((frequencesCentered / middle), 2), 2);
  } else if (filter == "cosine") {
    factor = arma::cos((M_PI * frequencesCentered) / (2.0 * middle));
  } else if (filter == "gaussian") {
    factor = arma::exp(
      -0.5 *
        arma::pow(frequencesCentered / arma::stddev(frequencesCentered), 2));
  } else if (filter == "hamming") {
    factor = 27.0 / 50.0 +
      23.0 / 50.0 * arma::cos(M_PI * frequencesCentered / middle);
  } else if (filter == "hanning") {
    factor =
      arma::pow(arma::cos((M_PI * frequencesCentered) / (2.0 * middle)), 2);
  } else if (filter == "welch") {
    factor = 1 - arma::pow((frequencesCentered / middle), 2);
  } else {
    factor = 1;
  }
  
  // Return results
  List results;
  results["frequencesCentered"] = frequencesCentered;
  results["amplitude"] = factor;
  return DataFrame(results);
}

int fact(int n) { return (n == 1 || n == 0) ? 1 : fact(n - 1) * n; }

//! Apply a sort of filter on a centered frequences vector
/*!
\param arma:vec vector with frequences to be processed
\param double the spectral resolution (dnu)
\return A dataframe with centered frequences and processed
*/
//[[Rcpp::export]]
DataFrame histogram(arma::vec frequences, double dnu) {
  // Maximum value to look at the histogram
  double freqf = 100;
  double binSize = dnu * 0.015;
  // Calculate all frequences differences
  int n = frequences.n_elem;
  int diagSupElements = n * (n - 1) / 2;
  arma::vec diff(diagSupElements);  // Number of elements in the sup. diag.
  diff -= 1;
  NumericVector::iterator it_first, it_second, it_diff;
  it_diff = diff.begin();  // output iterator
  int countElements = 0;
  // Double loop (n^2 complexity)
  for (it_first = frequences.begin(); it_first < frequences.end(); it_first++) {
    for (it_second = it_first;
         it_second < frequences.end() & it_diff < diff.end(); it_second++) {
      if (it_first != it_second) {  // Jump same elements
        *it_diff =
          std::abs(*it_second - *it_first);  // Save absolute difference
        if (*it_diff != 0) {
          it_diff++;        // Increase pointer
          countElements++;  // Increase elements
        }
      }
    }
  }
  // Remove unused memory
  diff.resize(diagSupElements - (diagSupElements - countElements));
  
  // Return results
  List results;
  results["diff"] = diff;
  return DataFrame(results);
}
