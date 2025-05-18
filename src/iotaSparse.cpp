// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppProgress)]]
#include <RcppArmadillo.h>
#include <progress.hpp>
#include <progress_bar.hpp>
#include "eta_progress_bar.hpp"
#include <bitset>
#include <unordered_map>
#include <vector>

using namespace Rcpp;

//' Comptue iota elements of a sparse binary matrix of closure elements
//' @name iotaSparse
//' @param tt A sparse matrix of closure elements
//' @param display_progress = true: to show progress bar. Default = FALSE
//' @return A sparse binary matrix of the iota elements
//' @export

const size_t MAX_COLS = 5000; // Adjust this if necessary

// [[Rcpp::export]]
arma::sp_mat iotaSparse(arma::sp_mat tt, bool display_progress = false) {
  int n = tt.n_cols;
  int m = tt.n_rows;
  
  if (n > MAX_COLS) {
    stop("Number of columns exceeds MAX_COLS in std::bitset.");
  }
  
  // Precompute row bitsets
  Rcout << "Converting to bitsets\n";
  ETAProgressBar pb1;
  Progress p1(m, display_progress, pb1);
  
  std::vector<std::bitset<MAX_COLS>> row_bitsets(m);
  for (int row = 0; row < m; ++row) {
    if (Progress::check_abort()) break;
    p1.increment();
    
    for (arma::sp_mat::const_row_iterator it = tt.begin_row(row); it != tt.end_row(row); ++it) {
      row_bitsets[row].set(it.col());
    }
  }
  
  std::unordered_map<std::bitset<MAX_COLS>, size_t> iota_map;
  
  Rcout << "Computing iota elements\n";
  ETAProgressBar pb2;
  Progress p2(n, display_progress, pb2);
  
  for (int omega = 0; omega < n; ++omega) {
    if (Progress::check_abort()) break;
    p2.increment();
    
    std::bitset<MAX_COLS> i;
    i.set(); // full set
    bool included = false;
    
    for (int row = 0; row < m; ++row) {
      const std::bitset<MAX_COLS>& FFbs = row_bitsets[row];
      
      if (FFbs.test(omega)) {
        included = true;
        i &= FFbs;
        if (i.count() == 1 && i.test(omega)) {
          break;
        }
      }
    }
    
    if (included && iota_map.find(i) == iota_map.end()) {
      iota_map[i] = iota_map.size();
    }
  }
  
  // Create triplets for sparse matrix
  std::vector<arma::uword> row_indices, col_indices;
  for (const auto& [bs, row] : iota_map) {
    for (size_t col = 0; col < n; ++col) {
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
