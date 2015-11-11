
#include <Rcpp.h>
#include <algorithm>
// #include "which_na.cpp"
// #include "sort_indices.cpp"
#include <iostream>
#include <vector>

using namespace Rcpp;
using namespace std;

//-----------------------------------------------------------------------------
// @title Weighted Mean (C++)
// @description Compute a weighted mean
// @param x A \code{NumericVector} of values whose mean is to be computed.
// @param w A \code{NumericVector} of weights of the same length as \code{x}
// giving the weights to use for the elements of x.
// @return a scalar.
// @seealso \code{\link[stats]{weighted.mean}}
// [[Rcpp::export]]
double weighted_mean(NumericVector& x, NumericVector w) {

// [AW - 11/6/2015] removing this error message. It appears to be confused by NA values of x
// even when the x.size() == w.size()
//  if (x.size() != w.size()) {
//    cout << "ERROR: Length of x and w differ." << endl;
//    return -1;
//  }

  // normalize weights and compute weighted mean
  double s= 0.0, wm= 0.0;
  for (int i = 0; i < w.size(); i++) {
    s += w[i];
  }
  for (int i = 0; i < w.size(); i++) {
    w[i] = w[i] / s;
    wm += x[i] * w[i];
  }

  return wm;
}

//-----------------------------------------------------------------------------
IntegerVector callRfunc (NumericVector& x, Function f) {
  IntegerVector res = f(x);
  return res;
}

//-----------------------------------------------------------------------------
//' @title Sort vector and return indexes
//' @description Sort the vector by the values
//' Return the indexes of the sorted vector according to original
//' @param values The vector that should be sorted

// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
IntegerVector sort_indices(NumericVector& values) {

  // initialize original index locations
  IntegerVector idx(values.size());
  for (unsigned int i = 0; i != idx.size(); ++i) {
    idx[i] = i;
  }

  // sort indexes based on comparing values in v
  std::sort(idx.begin(), idx.end(),
       [&values](size_t i1, size_t i2) {return values[i1] < values[i2];});

  return idx;
}

//-----------------------------------------------------------------------------
// this function is equivalent to calculating via
// kernelMatrix(kern, c(0, smallest_distances))[1, -1, drop= TRUE]
// in R library(kernlab)
// [[Rcpp::export]]
NumericVector kern_wt (double& sigma, NumericVector& x) {
  NumericVector ret(x.size());

  for (int i = 0; i < x.size(); i++) {
    ret[i] = std::exp(2 * sigma * -1 * (double) (x[i] * x[i] / 2));
  }

  return ret;
}

//-----------------------------------------------------------------------------
//' @title Imputation function for kNN
//' @description Function for KNN imputation for a single element.
//' Distances are weighted by a kernal function to produce a weighted
//' imputation.
//' @param values The values from which imputation will take place
//' @param distances The distances associated with each value
//' @param k The number of neighbors used to impute
//' @param sigma The standard deviation (ie sigma parameter) of the Gaussian kernal used for weighting
// [[Rcpp::export]]
double impute_fn_knn (NumericVector& values, NumericVector& distances, int& k, double& sigma) {
  NumericVector small_dist(k), knn_values(k);
  // IntegerVector rnks = callRfunc(x, order) - 1;
  IntegerVector rnks = sort_indices(distances);

  for (int i = 0; i < k; i++) {
    small_dist[i] = distances[ rnks[i] ];
    knn_values[i] = values[ rnks[i] ];
  }

  NumericVector d = kern_wt(sigma, small_dist);
  return weighted_mean(knn_values, d);
}
