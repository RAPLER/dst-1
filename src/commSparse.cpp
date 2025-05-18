// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppProgress)]]
#include <RcppArmadillo.h>
#include <progress.hpp>
#include <progress_bar.hpp>
#include "eta_progress_bar.hpp"
#include <bitset>
#include <vector>
#include <cmath>

//' Comptue commonality values of a group of simple support functions on its closure elements
//' @name commSparse
//' @param x The non-trivial support of the mass function
//' @param x_c A sparse binary matrix of closure elements
//' @param a Mass assigned to the non-trivial support
//' @param display_progress = true: to show progress bar. Default = FALSE
//' @return A sparse binary matrix including the closure elements
//' @export

using namespace Rcpp;

const size_t MAX_COLS = 5000;  // <-- Adjust as needed for your use case

// [[Rcpp::export]]
NumericVector commSparse(arma::sp_mat x, arma::sp_mat x_c, double a, bool display_progress = false) {
  int n = x.n_rows;
  int m = x.n_cols;
  int nc = x_c.n_rows;
  
  if (m > MAX_COLS) {
    stop("Number of columns exceeds MAX_COLS in std::bitset.");
  }
  
  using Bitset = std::bitset<MAX_COLS>;
  
  std::vector<Bitset> x_bits;
  NumericVector qq(nc);
  
  // Convert sparse matrix x to vector of bitsets
  for (int i = 0; i < n; ++i) {
    Bitset row_bits;
    for (arma::sp_mat::const_row_iterator it = x.begin_row(i); it != x.end_row(i); ++it) {
      row_bits.set(it.col());
    }
    x_bits.push_back(row_bits);
  }
  
  ETAProgressBar pb;
  Progress p(nc, display_progress, pb);
  
  for (int i = 0; i < nc; ++i) {
    if (Progress::check_abort()) break;
    p.increment();
    
    Bitset c_bits;
    for (arma::sp_mat::const_row_iterator it = x_c.begin_row(i); it != x_c.end_row(i); ++it) {
      c_bits.set(it.col());
    }
    
    int count_not_subsets = 0;
    
    for (int j = 0; j < n; ++j) {
      if ((x_bits[j] & c_bits) != c_bits) {
        count_not_subsets++;
      }
    }
    
    qq[i] = std::pow(1.0 - a, count_not_subsets);
  }
  
  return qq;
}



