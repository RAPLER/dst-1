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

//' Comptue iota elements of a sparse binary matrix of closure elements
//' @name iotaSparse
//' @param tt A sparse matrix of closure elements
//' @param display_progress = true: to show progress bar. Default = FALSE
//' @return A sparse binary matrix of the iota elements
//' @export

// [[Rcpp::export]]
arma::sp_mat iotaSparse(arma::sp_mat tt, bool display_progress = false) {
  int n = tt.n_cols;
  
  // Map to store unique iota bitsets and their assigned row index
  std::unordered_map<boost::dynamic_bitset<>, size_t> iota_map;
  
  ETAProgressBar pb;
  Progress p(n, display_progress, pb);
  
  for (int omega = 0; omega < n; ++omega) {
    if (Progress::check_abort()) break;
    p.increment();
    
    boost::dynamic_bitset<> i(n);
    i.set(); // full set
    bool included = false;
    
    for (int row = 0; row < tt.n_rows; ++row) {
      boost::dynamic_bitset<> FFbs(n);
      for (arma::sp_mat::const_row_iterator it = tt.begin_row(row); it != tt.end_row(row); ++it) {
        FFbs.set(it.col());
      }
      
      if (FFbs.test(omega)) {
        included = true;
        i &= FFbs;
        if (i.count() == 1 && i.test(omega)) {
          break;
        }
      }
    }
    
    if (included) {
      if (iota_map.find(i) == iota_map.end()) {
        iota_map[i] = iota_map.size(); // assign row index
      }
    }
  }
  
  // Create triplets for sparse matrix
  std::vector<arma::uword> row_indices, col_indices;
  for (const auto& [bs, row] : iota_map) {
    for (size_t col = 0; col < bs.size(); ++col) {
      if (bs[col]) {
        row_indices.push_back(row);
        col_indices.push_back(col);
      }
    }
  }
  
  arma::sp_mat W24(iota_map.size(), n);
  for (size_t k = 0; k < row_indices.size(); ++k) {
    W24(row_indices[k], col_indices[k]) = 1.0;
  }
  
  return W24;
}
