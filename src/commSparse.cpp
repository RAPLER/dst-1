// [[Rcpp::depends(BH)]]
// [[Rcpp::depends(RcppProgress)]]
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <stdio.h>
#include <boost/functional/hash.hpp>
#include <boost/dynamic_bitset.hpp>
#include <progress.hpp>
#include <progress_bar.hpp>
#include "eta_progress_bar.hpp"
using namespace Rcpp;

//' Comptue commonality values of a group of simple support functions on its closure elements
//' @name commSparse
//' @param x The non-trivial support of the mass function
//' @param x_c A sparse binary matrix of closure elements
//' @param a Mass assigned to the non-trivial support
//' @param display_progress = true: to show progress bar. Default = FALSE
//' @return A sparse binary matrix including the closure elements
//' @export

// [[Rcpp::export]]
NumericVector commSparse(arma::sp_mat x, arma::sp_mat x_c, double a, bool display_progress = false) {
  int n = x.n_rows;
  int m = x.n_cols;
  int nc = x_c.n_rows;
  
  std::vector<boost::dynamic_bitset<>> x_bits(n);
  std::vector<boost::dynamic_bitset<>> xc_bits(nc);
  NumericVector qq(nc);
  
  // Convert x
  for (int i = 0; i < n; ++i) {
    boost::dynamic_bitset<> row_bits(m);
    for (arma::sp_mat::const_row_iterator it = x.begin_row(i); it != x.end_row(i); ++it) {
      row_bits[it.col()] = 1;
    }
    x_bits[i] = row_bits;
  }
  
  // Convert x_c
  for (int i = 0; i < nc; ++i) {
    boost::dynamic_bitset<> row_bits(m);
    for (arma::sp_mat::const_row_iterator it = x_c.begin_row(i); it != x_c.end_row(i); ++it) {
      row_bits[it.col()] = 1;
    }
    xc_bits[i] = row_bits;
  }
  
  ETAProgressBar pb;
  Progress p(nc, display_progress, pb);
  
  for (int i = 0; i < nc; ++i) {
    if (Progress::check_abort()) break;
    p.increment();
    
    int count_not_subsets = 0;
    for (int j = 0; j < n; ++j) {
      if ((xc_bits[i] & x_bits[j]) != xc_bits[i]) {
        count_not_subsets++;
      }
    }
    qq[i] = std::pow(1.0 - a, count_not_subsets);
  }
  
  return qq;
}
