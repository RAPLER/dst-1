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

//' Augment a sparse binary matrix with closure elements
//' @name closureSparse
//' @param ttx A sparse binary matrix
//' @param computeJoin = true: to compute join closure. Default = TRUE
//' @param display_progress = true: to show progress bar. Default = FALSE
//' @return A sparse binary matrix including the closure elements
//' @export

// [[Rcpp::export]]
arma::sp_mat closureSparse(arma::sp_mat ttx, bool computeJoin = true, bool display_progress = false) {
  // Declare lists of bitsets
  std::vector<boost::dynamic_bitset<>> ttxlv;
  std::vector<boost::dynamic_bitset<>> ttylv;
  std::unordered_map<boost::dynamic_bitset<>, size_t> m0;
  
  // Convert input arma::sp_mat to bitset vectors
  for (size_t i = 0; i < ttx.n_rows; ++i) {
    boost::dynamic_bitset<> bitset_row(ttx.n_cols);
    
    // Use sparse row iterator to efficiently access non-zero elements
    for (arma::sp_mat::const_row_iterator it = ttx.begin_row(i); it != ttx.end_row(i); ++it) {
      bitset_row[it.col()] = 1;
    }
    
    ttxlv.push_back(bitset_row);
    ttylv.push_back(bitset_row);
    m0.emplace(bitset_row, 0);
  }
  
  // Compute closures
  ETAProgressBar pb;
  Progress p(ttxlv.size(), display_progress, pb);
  
  for (size_t i = 0; i < ttxlv.size(); ++i) {
    if (Progress::check_abort()) break;
    p.increment();
    for (size_t j = 0; j < ttylv.size(); ++j) {

      // Always compute meet (AND)
      boost::dynamic_bitset<> meet = ttylv[i] & ttylv[j];
      if (m0.find(meet) == m0.end()) {
        ttylv.push_back(meet);
        m0.emplace(meet, 0);
      }
      
      // Conditionally compute join (OR)
      if (computeJoin) {
        boost::dynamic_bitset<> join = ttylv[i] | ttylv[j];
        if (m0.find(join) == m0.end()) {
          ttylv.push_back(join);
          m0.emplace(join, 0);
        }
      }
    }
  }
  
  // Convert result to arma::sp_mat
  size_t n_rows = ttylv.size();
  size_t n_cols = ttx.n_cols;  // assume all bitsets have the same length
  
  arma::sp_mat tty(n_rows, n_cols);
  for (size_t i = 0; i < n_rows; ++i) {
    const auto& bitset = ttylv[i];
    for (size_t j = 0; j < n_cols; ++j) {
      tty(i, j) = static_cast<int>(bitset[j]);  // R logical is just 0/1 here
    }
  }
  return tty;
}



