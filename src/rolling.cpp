#include <Rcpp.h>
using namespace Rcpp;

// This function calculates rolling means taking acount of the window size and a
// data capture threshold. Means are calculated only when the data capture is >
// than 'cap' else NA is returned.

RcppExport SEXP rollingMean(SEXP x, SEXP len, SEXP cap) {
  NumericVector A(x); // the data
  double capr = as<double>(cap); // data capture %
  int lenr = as<int>(len); // window size %
  NumericVector res(A.size()); // for results
  LogicalVector NA(x); // for missings
  NumericVector missing(1);
  int n = A.size(); // length of data
  double sum = 0.0;
  int sumNA = 0; // number of missings
  NA = is_na(A) ; // logical vector of missings
  missing[0] = NA_REAL;

  // main loop
  for (int i = 0; i <= (n - lenr); i++) {
    sum = 0; // initialise
    sumNA = 0;

    // now go through each window
    for (int j = i; j < i + lenr; j++) {

      if (NA(j)) {
        sumNA += 1; // count missings
      }
      else
        {
          sum += A(j); // sum values that are not missing
        }
    }

    // calculate mean if within data capture threshold, if not set to missing

    if (1 - sumNA / lenr < capr / 100) {
      res(i + lenr - 1) = missing[0];
    }
    else
      {
        res(i + lenr - 1) = sum / (lenr - sumNA);
      }
  }

  // pad out missing data at start
  for (int i = 0; i < lenr - 1; i++) {
    res(i) = missing[0];
  }
  return res;
}

