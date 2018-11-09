#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace std;

arma::vec abs_vec(arma::vec x) {
  // Output as vector structure
  arma::vec aux(x.n_elem);
  arma::vec::iterator it_out;  // Output iterator
  arma::vec::iterator it_in;   // Input iterator
  for (it_in = x.begin(), it_out = aux.begin(); it_in < x.end(); ++it_in, ++it_out) {
    *it_out = std::abs(*it_in);
  }
  return aux;
}

//! Create a vector as sequence of integers
/*!
 \param int first integer
 \param int last integer
 \return A numeric vector with integer secuence
 */
//[[Rcpp::export]]
DataFrame apodization(arma::vec frequences, String filter) {
  
  // Center frequencies
  arma::vec frequencesCentered(frequences.n_elem);
  arma::vec amplitudes(frequences.n_elem);
  arma::vec factor(frequences.n_elem);
    
  // Get values to center frequences
  double max = frequences.max();
  double min = frequences.min();
  double middle = (max - min) / 2;

  frequencesCentered = frequences - min - middle;
    
  // Apply filter
  if( filter == "bartlett" ) {
    factor = 1 - abs_vec(frequencesCentered) / middle;
  } else if( filter == "blackman" ) {
    factor = 3;
  } else {
    factor = 1;
  }
  
  // Calculated amplitudes based on the choosen filter
  amplitudes = amplitudes + 1;
  amplitudes = amplitudes.t() * factor;

  // Return results
  List results;
  results["frequencesCentered"] = frequencesCentered;
  results["amplitude"] = amplitudes;
  results["factor"] = factor;
  return DataFrame(results);
}