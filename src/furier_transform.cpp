#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace std;

//! Create a vector as sequence of integers
/*!
\param int first integer
\param int last integer
\return A numeric vector with integer secuence
*/
//[[Rcpp::export]]
NumericVector seq_int(int first, int last) {
  NumericVector y(abs(last - first) + 1);
  iota(y.begin(), y.end(), first);
  return y;
}
//[[Rcpp::export]]
NumericVector seq_rev(NumericVector x) {
  NumericVector revX = clone<NumericVector>(x);
  reverse(revX.begin(), revX.end());
  return revX;
}

//! Computing Fast Discrete Fourier Transform
/*!
\param x a vector with time series
\return The Discrete Fourier Transform
*/
//[[Rcpp::export]]
arma::cx_vec compute_fft(arma::vec x) { return arma::fft(x); }

//! Computing Discrete Fourier Transform. For academic propose only
/*!
\param x a vector with time series
\return The Discrete Fourier Transform
*/
arma::cx_vec compute_fft_manual(arma::vec x) {
  // Output sctructure as complex vector
  arma::cx_vec output(x.n_elem);
  // Complex variables for fourier calculation
  complex<double> sum = 0;
  const complex<double> cc(0, 2);
  const complex<double> n(x.n_elem, 0);
  // Iterators over data structures
  arma::cx_vec::iterator it_out;  // Output iterator
  arma::vec::iterator it_in;      // Input iterator
  complex<double> i = 0, t = 0;
  // Double loop
  for (it_out = output.begin(); it_out < output.end(); ++it_out) {
    sum = 0;
    for (it_in = x.begin(); it_in < x.end(); ++it_in) {
      sum += *it_in * exp(cc * M_PI * t * i / n);  // Complex exp
      t.operator+=(1.0);
    }
    *it_out = sum;
    i.operator+=(1.0);
  }
  return output;
}

//! Computing Discrete Fourier Transform and return the calculated amplitudes
/*!
\param x a vector with time series
\param totTime an integer representing the total time to calculate frecuencies
\return The amplitudes
*/
//[[Rcpp::export]]
DataFrame calculate_amplitudes(arma::vec time, arma::vec x) {
  // Constants from data input
  int n = x.n_rows;
  
  // Calculate frequency
  NumericVector frequency = seq_int(1, n);
  NumericVector freq1 = seq_int(1, n / 2);
  NumericVector freq2 = seq_rev(freq1 * -1);
  // Delta and fNyquist
  double delta = time[2] - time[1];
  double fNyquist = (double)1 / 2 / delta;
  // Vector concatenation
  for (int i = 0; i < n && i < frequency.length(); i++) {
    if (i < n / 2) {
      frequency[i] = freq1[i];
    } else {
      frequency[i] = freq2[i - (n / 2)];
    }
  }
  frequency = fNyquist * frequency / (n / 2.0);
  
  // Fourier transformation
  arma::cx_vec furierTt = compute_fft(x);  // Use Fast Fourier Transform (fft)
  // Get values
  furierTt = furierTt.submat(1, 0, (n / 2), 0);
  // Calculate amplitude getting modules
  NumericVector amplitudes(n);
  NumericVector::iterator out_amp;
  NumericVector phases(n);
  NumericVector::iterator out_pha;
  arma::cx_vec::iterator it;  // Get iterator over the complex vector
  for (it = furierTt.begin(), out_amp = amplitudes.begin(), out_pha=phases.begin();
       it < furierTt.end();
  ++it, ++out_amp, ++out_pha) {
    complex<double> cx = *it;
    *out_amp = (sqrt(pow(real(cx), 2) + pow(imag(cx), 2)) / furierTt.n_elem);
    *out_pha = atan(imag(cx)/real(cx));
  }
  
  // Return results
  List results;
  results["amplitude"] = amplitudes;
  results["phase"] = phases;
  results["frequency"] = frequency;
  return DataFrame(results);
}
