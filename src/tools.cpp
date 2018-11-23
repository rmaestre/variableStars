#include <RcppArmadillo.h> 
#include <Rcpp.h>

using namespace Rcpp;
using namespace std;

template<typename T>
void printVector(const T& t, int limit) {
  if (limit > t.size()) {
    limit = t.size();
  }
    std::copy(t.cbegin(), t.cbegin() + limit, 
              std::ostream_iterator<typename T::value_type>(Rcout, ", "));
}

//' Create a vector as sequence of integers
//'
//' This function helps to create a vector with a secuence
//' of integers equally separated
//'
//' @param first The first element of the sequence
//' @param last The last element of the sequence
//' @return A numeric vector with the whole secuence of integers
//' @author Roberto Maestre
//' @examples
//' \dontrun{
//' # simple call:
//' seqIntegers(1,10)
//' # simple call with negative numbers
//' seqIntegers(-10,10)
//' }
//' @export
//[[Rcpp::export]]
NumericVector seqIntegers(int first, int last) {
  if (first>=last) {
    throw std::range_error("seqIntegers:: First element should be greater that the last one");
  }
  NumericVector y(abs(last - first) + 1);
  std::iota(y.begin(), y.end(), first);
  return y;
}

//' Revert the order of an input vector
//'
//' Revert the order of the elements an input vector
//'
//' @param vector Numerix vector to be reversed
//' @return A numeric vector with elements in reverse mode
//' @author Roberto Maestre
//' @examples
//' \dontrun{
//' # simple call:
//' vectorRev(c(1,2,3,4,5,6))
//' # simple call with negative nummbers
//' vectorRev(c(1,-2,3,-4,5,-6))
//' }
//' @export
//[[Rcpp::export]]
NumericVector vectorRev(NumericVector vector) {
  NumericVector revV = clone < NumericVector > (vector);
  std::reverse(revV.begin(), revV.end());
  return revV;
}


//' Fast Fourier Transform
//'
//' Compute the discrete fourier transform using the 
//' the fast FT algorithm provided by the Armadillo library
//'
//' @param v Armadillo vector with numeric elements
//' @return A complex vector with the FT result
//' @author Roberto Maestre
//' @examples
//' \dontrun{
//' # simple call:
//' computeFft(sin(seq(1:100)))
//' }
//' @export
//[[Rcpp::export]]
arma::cx_vec computeFft(arma::vec v) {
  return arma::fft(v);
}

//' Calculate the spectrum from a given vector
//'
//' Calculate frequency, amplitude and phase from a given vector
//' using the Fast Fourier Tramsform algorithm.
//'
//' @param time Armadillo vector representing the meassurements time
//' @param x Armadillo vector with numeric elements
//' @return A complex vector with the calculated spectrum
//' @author Roberto Maestre
//' @examples
//' \dontrun{
//' # simple call:
//' sample <- sin(seq(1:100))
//' calculateSpectrum(seq(1:length(sample)),sample)
//' }
//' @export
//[[Rcpp::export]]
List calculateSpectrum(arma::vec time, arma::vec x) {
  // Constants from data input
  int n = x.n_rows;
  
  // Calculate frequency
  NumericVector frequency = seqIntegers(1, n);
  NumericVector freq1 = seqIntegers(1, n / 2);
  NumericVector freq2 = vectorRev(freq1 * -1);
  // Delta and fNyquist
  double delta = time[2] - time[1];
  double fNyquist = (double) 1 / 2 / delta;
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
  arma::cx_vec furierTt = computeFft(x); // Use Fast Fourier Transform (fft)
  // Get values
  furierTt = furierTt.submat(1, 0, (n / 2), 0);
  // Calculate amplitude getting modules
  NumericVector amplitudes(n);
  NumericVector::iterator out_amp;
  NumericVector phases(n);
  NumericVector::iterator out_pha;
  arma::cx_vec::iterator it; // Get iterator over the complex vector
  for (it = furierTt.begin(), out_amp = amplitudes.begin(),
       out_pha = phases.begin(); it < furierTt.end(); ++it, ++out_amp, ++out_pha) {
    complex < double > cx = * it;
    * out_amp = (sqrt(pow(real(cx), 2) + pow(imag(cx), 2)) / furierTt.n_elem);
    * out_pha = atan(imag(cx) / real(cx));
  }
  // Return results
  return List::create(_["amplitude"] = amplitudes,
                      _["phase"] = phases,
                      _["frequency"] = frequency);
}

//' Apodization
//'
//' Apodization, tapering or window function is a function 
//' used to smoothly bring a sampled signal down to zero at the edges 
//' of the sampled region
//'
//' @param frequences Armadillo vector with frequences
//' @param filter A string with a specific filter (bartlett, blackman,
//' connes, cosine, gaussian, hamming, hanning, welch or uniform by-default)
//' @return The apoditazed frequence vector
//' @author Roberto Maestre
//' @examples
//' \dontrun{
//' # simple call:
//' apodization(c(1,2,3,4), "gaussian")
//' apodization(c(1,2,3,4), "blackman")
//' 
//' }
//' @export
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
    factor = arma::exp(-0.5 *
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

//' Triangular Superior absolute differences of a vector
//'
//' Given a vector, this function calculates the triangular
//' superior differences combination. All differences are
//' absolute and zero-difference will be drop
//'
//' @param frequences Armadillo vector with frequences
//' @return The vector with all differences
//' @author Roberto Maestre
//' @examples
//' \dontrun{
//' # simple call:
//' differences(c(1,2,3,4,5))
//' # simple call returning an empty vector because all 
//' # differences are equal to 0:
//' differences(rep(1,10))
//' 
//' }
//' @export
//[[Rcpp::export]]
arma::vec differences(arma::vec frequences) {
  if (frequences.n_elem == 0) {
    throw std::range_error("differences:: Frequences vector is empty");
  }
  
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

//' Histogram of differences
//'
//' Given a vector, this function calculates the histogram
//' of the non-zero and absolute differences of a given vector
//' taking into account a Dnu
//'
//' @param frequences Armadillo vector with frequences
//' @param dnu Numeric value for the spectral resolution
//' @return A list with bins and values representing an histogram
//' @author Roberto Maestre
//' @examples
//' \dontrun{
//' # simple call:
//' differences(c(1,2,3,4,5))
//' # simple call returning an empty vector because all 
//' # differences are equal to 0:
//' differences(rep(1,10))
//' 
//' }
//' @export
//[[Rcpp::export]]
List diffHistogram(arma::vec frequences, double dnu) {
  // Calculalate differences among frequences
  arma::vec diffs = differences(frequences);
  
  // Histogram bin paramterns
  double maxHistogramBin = 100; // Max value in histogram
  double binSize = dnu * 0.015; // Bin length
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

//' Apodization and FT
//'
//' Apodization of a given vector with one filter and
//' Fourier Transform calculation
//'
//' @param frequences Armadillo vector with frequences
//' @param filter A string with a specific filter (bartlett, blackman,
//' connes, cosine, gaussian, hamming, hanning, welch or uniform by-default)
//' @return A list with amplitudes, frequences, inverse frecuences and the
//' power spectrum calculated.
//' @author Roberto Maestre
//' @examples
//' \dontrun{
//' # simple call:
//' r <- ft(sin(seq(1:100)), "uniform")
//' plot(r$f, r$powerSpectrum)
//' }
//' @export
//[[Rcpp::export]]
List apodizationFt(arma::vec frequences, String filter) {
  // Paramters
  const double maxFreq = 100.0; // max value for FT computing
  const double fNyquist = 1; // fNyquist value
  const double unknow = 10000; // ??? Numero de puntos
  const int n = frequences.n_rows; // Frequences number
  
  // Frequencies apodization
  arma::vec amp = apodization(frequences, filter);
  // Calculate frequence differences
  arma::vec diff = differences(frequences);
  // Minimum difference
  //double minDiff = arma::min(diff);   --->¿Why is not the min?
  double minDiff = 1.0;
  // Calculate delnu
  double delnu = (maxFreq - fNyquist) / unknow;
  arma::vec f = 1.0 / arma::regspace(minDiff, delnu, maxFreq);
  // Outer product
  arma::mat outerProduct = f * frequences.t();
  // Ccalculate real and imaginary part
  // Calculate real and imaginary parts of the outer product
  arma::mat _real = arma::cos(2.0 * M_PI * outerProduct) * amp;
  arma::mat _imag = arma::sin(2.0 * M_PI * outerProduct) * amp;
  // Calculate power spectrum divided by the number of frecuences
  arma::mat powerSpectrum = (arma::pow(_real, 2) + arma::pow(_imag, 2)) /
    std::pow(n, 2);
  // Return results
  return List::create(_["amp"] = amp,
                      _["frequences"] = frequences,
                      _["f"] = f,
                      _["powerSpectrum"] = powerSpectrum,
                      _["powerSpectrumInverse"] = 1 / powerSpectrum);
}

//! Computing Adjacent Differences from a given vector
/*!
\param x a vector with values
\return The vector with the adjacent differences
*/



//' Calculate adjacent differences in a vector
//'
//' All differences between adjacent elements in a vector
//'
//' @param x Armadillo vector with numeric element
//' @return Armadillo vector with the adjacent differences
//' @author Roberto Maestre
//' @examples
//' \dontrun{
//' # simple call: All adjacent elements has the same length 1
//' adjacentDifferences(c(1,2,3,4,5))
//' # simple call: There are positive and negative diffferences
//' adjacentDifferences(c(1,2,0,14,-2))
//' }
//' @export
//[[Rcpp::export]]
arma::vec adjacentDifferences(arma::vec x) {
  // Create output vector with differences
  arma::vec adjDiffs(x.n_elem);
  // Calculate diffs
  std::adjacent_difference(x.begin(), x.end(), adjDiffs.begin());
  if (adjDiffs.n_elem > 0) {
    adjDiffs.shed_row(0); // Remove unused header
  }
  return adjDiffs;
}

//' Find highest peaks in a time series
//'
//' Inspired in the quantmod::findPeaks() R algorithm, 
//' this function find the highest peaks in a time series.
//'
//' @param x Armadillo vector with numeric element
//' @return Armadillo unsigned vector with the index representing 
//' the position of the selected peaks
//' @author Roberto Maestre
//' @examples
//' \dontrun{
//' # simple call: Returns position 1,4 and 7 with the highest peaks
//' findPeaks(c(1,2,1,2,10,1,2,3,1))
//' }
//' @export
//[[Rcpp::export]]
arma::uvec findPeaks(arma::vec x) {
  // Vector with peaks
  arma::vec d = adjacentDifferences(arma::sign(adjacentDifferences(x)));
  // Return peaks index
  return find(d < 0) + 1;
}

//' Split elements in ranges
//'
//' Inspired in the quantmod::findPeaks() R algorithm, 
//' this function find the highest peaks in a time series.
//'
//' @param nElements Number of elements to be splitted
//' @param numFrequencies Number of total frequences
//' @return A vector of ranges in integer format
//' @author Roberto Maestre
//' @examples
//' \dontrun{
//' # simple call:
//' calculateRange(100,100)
//' calculateRange(100,100)
//' calculateRange(10,200)
//' calculateRange(1,20)
//' }
//' @export
//[[Rcpp::export]]
arma::ivec calculateRange(int nElements, int nFrequencies) {
  // Check the frequencies vector of splitted elements
  arma::ivec range(3);
  if (nElements < nFrequencies) {
    range(0) = nElements;
    range.shed_rows(1, 2); // Release memory
  } else if (nElements > nFrequencies &
    nElements <= 2 * nFrequencies) {
    range(0) = nFrequencies;
    range(1) = nElements;
    range.shed_row(2); // Release memory
  } else if (nElements > 2 * nFrequencies &
    nElements <= 3 * nFrequencies) {
    range(0) = nFrequencies;
    range(1) = 2 * nFrequencies;
    range(2) = nElements;
  } else {
    range(0) = nFrequencies;
    range(1) = 2 * nFrequencies;
    range(2) = 3 * nElements;
  }
  return range;
}

//' Main process function
//'
//' The complete workflow can be found in the readme section.
//'
//' @param frequency Vector with frequences
//' @param amplitude Vector with amplitudes
//' @param filter Apodization filter
//' @param gRegimen Value to drop frecuencies in the G regimen
//' @param numFrequencies Number of frecuences to be processes calculated
//' for a range
//' @param maxDnu Maximum Dnu allowed
//' @param minDnu Minimum Dnu allowed
//' @param dnuGuessError Dnu error guessing
//' @param dnuValue Epecific Dnu value
//' @param dnuEstimation Flag to estimate ot not the Dnu vale
//' @param debug Flag to activate verbose debuggin information
//' @return A vector with histogram of differences and other middle-results
//' @author Roberto Maestre
//' @examples
//' \dontrun{
//' # simple call:
//' 
//' paramters = list(
//'   "filter" = "gaussian",
//'   "gRegimen" = 0,
//'   "minDnu" = 15,
//'   "maxDnu" = 95,
//'   "dnuValue" = -1,
//'   "dnuGuessError" = 10,
//'   "dnuEstimation" = TRUE,
//'   "numFrequencies" = 30,
//'   "debug" = TRUE)
//' 
//' result <- process(
//'     dt.spectrum$frequency,
//'     dt.spectrum$amplitude,
//'     filter = paramters$filter,
//'     gRegimen = paramters$gRegimen,
//'     minDnu = paramters$minDnu,
//'     maxDnu = paramters$maxDnu,
//'     dnuValue = paramters$dnuValue,
//'     dnuGuessError = paramters$dnuGuessError,
//'     dnuEstimation = paramters$dnuEstimation,
//'     numFrequencies = paramters$numFrequencies,
// '    debug = paramters$debug)
//' }
//' @export
//[[Rcpp::export]]
List process(arma::vec frequency, arma::vec amplitude, String filter,
             double gRegimen, double numFrequencies,
             double maxDnu, double minDnu, double dnuGuessError,
             double dnuValue = -1, bool dnuEstimation = false,
             bool debug = false) {
  // Work in muHz
  frequency /= 0.0864;
  
  if (debug) {
    Rcout << "::: Debug information :::" << "\n\n";
    Rcout << "Number of frequences to be processed: " << frequency.n_elem << "\n";
  }
  
  // Drop frequencies in g mode regimen
  arma::uvec ids = find(frequency > gRegimen);
  frequency = frequency.elem(ids);
  amplitude = amplitude.elem(ids);
  
  if (debug) {
    Rcout << "Number of frequences after drop the g regimen: " << frequency.n_elem << "\n";
  }
  
  // Sort frecuencies by amplitude
  arma::uvec idsSort = sort_index(amplitude, "descend");
  frequency = frequency.elem(idsSort);
  amplitude = amplitude.elem(idsSort);
  if (debug) {
    Rcout << "Frequencies: ";
    printVector(frequency, 20);
  }
  // Calculate the range
  arma::ivec range = calculateRange(frequency.n_elem, numFrequencies);
  if (debug) {
    Rcout << "Range: ";
    printVector(range, 20);
  }

  // Data sctutures
  arma::vec frequencyGlobal, amplitudeGlobal, f, fInv, b;
  List res, _diffHistogram;
  double dnu, dnuPeak, dnuGuess;
  
  // Loop over frequencies vector
  arma::ivec::iterator numIt;
  bool first = true;
  for (numIt = range.begin(); numIt < range.end(); numIt++) {
    if (debug) {
      Rcout << " Iteration over range: " << * numIt;
      Rcout << "\n";
    }
    if (first) {
      first = false; // Calculations only with the N first sorted frequencies
      // Calculate the range for subselecting frecuences
      arma::uvec pos( * numIt);
      std::iota(pos.begin(), pos.end(), 0);
      
      // Loop subselection of frecuences and amplitudes
      frequencyGlobal = frequency.elem(pos);
      amplitudeGlobal = amplitude.elem(pos);
      
      if (debug) {
          Rcout << "   Frequencies selected: ";
          printVector(frequencyGlobal, 20);
          Rcout << "\n";
          Rcout << "   Amplitudes selected: ";
          printVector(amplitudeGlobal, 20);
          Rcout << "\n";
      }
      
      // Calculate FT
      res = apodizationFt(frequencyGlobal, filter);
      // Calculate the inverse frecuence
      f = as < arma::vec > (res["f"]);
      fInv = 1.0 / f;
      b = as < arma::vec > (res["powerSpectrum"]);
      
      // Get the peaks
      arma::uvec peaksInd = findPeaks(b);
      arma::vec localMax = fInv.elem(peaksInd);
      arma::vec localMaxB = b.elem(peaksInd);
      
      // Get DNU on the peak
      arma::vec maxSel = fInv.elem(find(b == * std::max_element(localMaxB.begin(), localMaxB.end())));
      dnu = 0.0;
      dnuPeak = maxSel(0); // Get the dnu on the peak
      dnuGuess = arma::min(frequencyGlobal) / 3.0;
      // Check for an input Dnu value
      if (dnuValue < 0) {
        // Use the F0/Dnu estimation
        if (dnuEstimation) {
          if (dnuGuess < minDnu | dnuGuess > maxDnu | (arma::min(fInv) > dnuGuess + dnuGuessError)) {
            dnu = dnuPeak;
          } else {
            dnu = arma::min(fInv.elem(peaksInd) - dnuGuess) + dnuGuess;
          }
        } else {
          dnu = dnuPeak;
          dnuGuess = 0.0;
        }
      }
      if (debug) {
        Rcout << "    Dnu: " << dnu << "\n";
        Rcout << "    Dnu Peak: " << dnuPeak << "\n";
        Rcout << "    Dnu Guess: " << dnuGuess << "\n";
      }
      
      // Histogram of differences
      _diffHistogram = diffHistogram(frequencyGlobal, dnu);
    } else {
      if (debug) {
        Rcout << "    Nothing to do" << "\n";
      }
    } // End first iteration
  } // End range loop
  
  // Calculated the connected ind
  
  // Return the output with all valuable elements
  return List::create(_["photometry"] =
                      List::create(_["frequency"] = frequency,
                                   _["amplitude"] = amplitude),
                                   _["ft"] = res,
                                   _["diffHistogram"] = _diffHistogram,
                                   _["f"] = res["f"],
                                               _["fInv"] = fInv,
                                               _["dnuPeak"] = dnuPeak,
                                               _["dnu"] = dnu,
                                               _["dnuGuess"] = dnuGuess
  );
}