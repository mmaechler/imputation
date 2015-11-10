
#include <Rcpp.h>
#include <math.h>

using namespace Rcpp;

//-----------------------------------------------------------------------------
NumericMatrix row_erase (NumericMatrix& x, IntegerVector& rowID) {
  rowID = rowID.sort();

  NumericMatrix x2(Dimension(x.nrow()- rowID.size(), x.ncol()));
  int iter = 0;
  int del = 1; // to count deleted elements
  for (int i = 0; i < x.nrow(); i++) {
    if (i != rowID[del - 1]) {
      x2.row(iter) = x.row(i);
      iter++;
    } else {
      del++;
    }
  }
  return x2;
}

NumericMatrix col_erase (NumericMatrix& x, IntegerVector& colID) {
  colID = colID.sort();

  NumericMatrix x2(Dimension(x.nrow(), x.ncol()- colID.size()));
  int iter = 0;
  int del = 1;
  for (int i = 0; i < x.ncol(); i++) {
    if (i != colID[del - 1]) {
      x2.column(iter) = x.column(i);
      iter++;
    } else {
      del++;
    }
  }
  return x2;
}

