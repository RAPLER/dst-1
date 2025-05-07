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
  
  std::unordered_map<boost::dynamic_bitset<>, size_t> iota_map;
  std::vector<boost::dynamic_bitset<>> iota_list;
  
  ETAProgressBar pb;
  Progress p(n, display_progress, pb);
  
  for (int omega = 0; omega < n; ++omega) {
   if (Progress::check_abort()) break;
   p.increment();
   
   boost::dynamic_bitset<> i(n);
   i.set();
   bool included = false;
   
   for (int row_index = 0; row_index < tt.n_rows; ++row_index) {
     boost::dynamic_bitset<> FFbs(n);
     for (arma::sp_mat::const_row_iterator it = tt.begin_row(row_index);
          it != tt.end_row(row_index); ++it) {
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
       iota_map[i] = iota_map.size();  // assign index
       iota_list.push_back(i);         // store for later sorting
     }
   }
  }
  
  // Sort iota_list by cardinality
  std::sort(iota_list.begin(), iota_list.end(), [](const auto& a, const auto& b) {
   return a.count() < b.count();
  });
  
  // Convert sorted list into triplet format
  std::vector<arma::uword> row_indices, col_indices;
  for (size_t r = 0; r < iota_list.size(); ++r) {
   const auto& bs = iota_list[r];
   for (size_t c = 0; c < bs.size(); ++c) {
     if (bs[c]) {
       row_indices.push_back(r);
       col_indices.push_back(c);
     }
   }
  }
  
  arma::vec ones(row_indices.size(), arma::fill::ones);
  arma::sp_mat W24(iota_list.size(), n);
  for (size_t k = 0; k < row_indices.size(); ++k) {
   W24(row_indices[k], col_indices[k]) = 1.0;
  }
  
  return W24;
}
