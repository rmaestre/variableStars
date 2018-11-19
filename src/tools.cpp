#include <RcppArmadillo.h>
#include <Rcpp.h>

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
  for (it = furierTt.begin(), out_amp = amplitudes.begin(),
       out_pha = phases.begin();
       it < furierTt.end(); ++it, ++out_amp, ++out_pha) {
    complex<double> cx = *it;
    *out_amp = (sqrt(pow(real(cx), 2) + pow(imag(cx), 2)) / furierTt.n_elem);
    *out_pha = atan(imag(cx) / real(cx));
  }
  
  // Return results
  List results;
  results["amplitude"] = amplitudes;
  results["phase"] = phases;
  results["frequency"] = frequency;
  return DataFrame(results);
}

//! Apply a sort of filter on a centered frequences vector
/*!
\param arma:vec vector with frequences to be processed
\param String sort of filter to be applied
\return A dataframe with centered frequences and processed
*/
//[[Rcpp::export]]
arma::vec apodization(arma::vec frequences, String filter) {
  if (frequences.n_elem == 0) {
    throw std::range_error("apodization:: Frequences vector is empty");
  }
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
    factor = arma::ones(frequences.n_elem);
  }
  return factor;
}

//! Apply a sort of filter on a centered frequences vector
/*!
\param arma:vec vector with frequences to be processed
\param double the spectral resolution (dnu)
\return A dataframe with centered frequences and processed
*/
//[[Rcpp::export]]
arma::vec differences(arma::vec frequences) {
  if (frequences.n_elem == 0) {
    throw std::range_error("differences:: Frequences vector is empty");
  }

  // Calculate all frequences differences
  int n = frequences.n_elem;
  int diagSupElements = n * (n - 1) / 2;
  arma::vec diff(diagSupElements);  // Number of elements in the sup. diag.
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
  return diff;
}

//! Apply a sort of filter on a centered frequences vector
/*!
\param arma:vec vector with frequences to be processed
\param double the spectral resolution (dnu)
\return A dataframe with centered frequences and processed
*/
//[[Rcpp::export]]
List diffHistogram(arma::vec frequences, double dnu) {
  // Calculalate differences among frequences
  arma::vec diffs = differences(frequences);
  
  // Histogram bin paramterns
  double maxHistogramBin = 100;  // Max value in histogram
  double binSize = dnu * 0.015;  // Bin length
  // Generate space for bins for histogram
  arma::vec bins = arma::regspace(0, binSize, maxHistogramBin);
  // Return results
  List results;
  results["diffs"] = diffs;
  // Calculate histogram
  results["histogram"] =
    List::create(_["bins"] = bins, _["values"] = arma::hist(diffs, bins));
  return results;
}

//! Computing Discrete Fourier Transform and return the calculated amplitudes
/*!
\param x a vector with time series
\param totTime an integer representing the total time to calculate frecuencies
\return The amplitudes
*/
//[[Rcpp::export]]
List ft(arma::vec x, String filter) {
  // Paramters
  const double maxFreq = 100.0;  // max value for FT computing
  const double fNyquist = 1;    // fNyquist value
  const double unknow = 10000;  // ??? Numero de puntos
  const int n = x.n_rows; // Frequences number
  
  // Frequencies apodization
  arma::vec amp = apodization(x, filter);
  // Calculate frequence differences
  arma::vec diff = differences(x);
  // Minimum difference
  double minDiff = arma::min(diff);
  // Calculate delnu
  double delnu = (maxFreq - fNyquist) / unknow;
  arma::vec f = 1.0 / arma::regspace(minDiff, delnu, maxFreq);
  // Outer product
  arma::mat outerProduct = f * x.t();
  // Ccalculate real and imaginary part
  // Calculate real and imaginary parts of the outer product
  arma::mat _real = arma::cos(2.0 * M_PI * outerProduct) * amp;
  arma::mat _imag = arma::sin(2.0 * M_PI * outerProduct) * amp;
  // Calculate power spectrum divided by the number of frecuences
  arma::mat powerSpectrum = (arma::pow(_real, 2) + arma::pow(_imag, 2)) /
    std::pow(n, 2);
  // Return results
  return List::create(_["real"] = _real,
                      _["imag"] = _imag,
                      _["delnu"] = delnu, 
                      _["f"] = f, 
                      _["powerSpectrum"] = powerSpectrum,
                      _["powerSpectrumInverse"] = 1 / powerSpectrum);
}

//! Computing Adjacent Differences from a given vector
/*!
\param x a vector with values
\return The vector with the adjacent differences
*/
//[[Rcpp::export]]
arma::vec adjacentDifferences(arma::vec x) {
  // Create output vector with differences
  arma::vec adjDiffs(x.n_elem);
  // Calculate diffs
  std::adjacent_difference(x.begin(), x.end(), adjDiffs.begin());
  if (adjDiffs.n_elem > 0){
    adjDiffs.shed_row(0); // Remove unused header
  }
  return adjDiffs;
}


//! Find peaks from a numeric vector
/*!
\param x a vector with values
\return The vector with the peaks index
*/
//[[Rcpp::export]]
arma::uvec findPeaks(arma::vec x) {
  // Vector with peaks
  arma::vec d = adjacentDifferences(arma::sign(adjacentDifferences(x)));
  // Return peaks index
  return find(d < 0) + 3;
}

//[[Rcpp::export]]
List go(arma::vec frequency, arma::vec amplitude, String filter, 
        double gRegimen = 0.0, double numFrequencies = 30) {
  // Work in muHz
  frequency /= 0.0864;
  
  // Drop frequencies in g mode regimen
  arma::uvec ids = find(frequency > gRegimen);
  frequency = frequency.elem(ids);
  amplitude = amplitude.elem(ids);
  
  // Sort frecuencies by amplitude
  arma::uvec idsSort = sort_index(amplitude, "descend");
  frequency = frequency.elem(idsSort);
  amplitude = amplitude.elem(idsSort);
  
  // Check the frequencies vector of splitted elements
  arma::vec range(3);
  if (frequency.n_elem < numFrequencies){
    range[0] = 1;
    range.shed_rows(1, 2);
  } else if (frequency.n_elem > numFrequencies & 
             frequency.n_elem <= 2*numFrequencies) {
    range[0] = numFrequencies;
    range[1] = frequency.n_elem;
    range.shed_row(2);
  } else if (frequency.n_elem > 2*numFrequencies & 
             frequency.n_elem <= 3*numFrequencies) {
    range[0] = numFrequencies;
    range[1] = 2*numFrequencies;
    range[2] = frequency.n_elem;
  } else {
    range[0] = numFrequencies;
    range[2] = 2*numFrequencies;
    range[3] = frequency.n_elem;
  }
  
  // Get the peaks
  arma::uvec peaksInd = findPeaks(amplitude);
  
  // Fourier transformation
  List res = ft(frequency, filter);
  
  // Differences
  arma::vec diff = differences(frequency);
  
  // Histogram
  List _diffHistogram = diffHistogram(diff, 19);
  
  return List::create(_["photometry"] = 
                        List::create(_["frequency"]=frequency, 
                                     _["amplitude"]=amplitude),
                      _["ft"]=res,
                      _["diff"] = List::create(_["diffHistogram"]=_diffHistogram, 
                                           _["diff"]=diff)
                        );
}
