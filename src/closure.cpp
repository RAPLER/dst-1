// [[Rcpp::depends(BH)]]
// [[Rcpp::depends(RcppProgress)]]
#include <stdio.h>
#include <boost/functional/hash.hpp>
#include <boost/dynamic_bitset.hpp>
#include <progress.hpp>
#include <progress_bar.hpp>
#include "eta_progress_bar.hpp"
using namespace Rcpp;

//' Augment a binary matrix with closure elements.
//' @name closure
//' @param ttx A binary matrix.
//' @param computeJoin = true: to compute join closure. Default = TRUE.
//' @param display_progress = true: to show progress bar. Default = FALSE.
//' @return tty A binary matrix including the closure elements.
//' @examples
//' ttx <- matrix(c(0,1,1,1,1,0,1,1,1), nrow=3, byrow = TRUE)
//' tty <- closure(ttx, computeJoin = FALSE)
//' @export

// [[Rcpp::export]]
LogicalMatrix closure(IntegerMatrix ttx, bool computeJoin = true, bool display_progress = false) {
  // Declare lists of bitsets
  std::vector<boost::dynamic_bitset<>> ttxlv;
  std::vector<boost::dynamic_bitset<>> ttylv;
  std::unordered_map<boost::dynamic_bitset<>, size_t> m0;
  
  // Convert input matrix to bitset vectors
  for (int i = 0; i < ttx.nrow(); ++i) {
    IntegerVector vec = IntegerVector(ttx.row(i));  // convert row to IntegerVector
    boost::dynamic_bitset<> bitset1(vec.size());
    
    for (int j = 0; j < vec.size(); ++j) {
      bitset1[j] = (vec[j] != 0);  // shorter and safe
    }
    
    ttxlv.push_back(bitset1);
    ttylv.push_back(bitset1);
    m0.emplace(bitset1, 0);
  }
  
  // Compute closures
  ETAProgressBar pb;
  Progress p(ttxlv.size(), display_progress, pb);
  for (size_t i = 0; i < ttxlv.size(); ++i) {
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
  
  // Convert result to LogicalMatrix
  size_t n_rows = ttylv.size();
  size_t n_cols = ttx.ncol();  // assume all bitsets have the same length
  
  Rcpp::LogicalMatrix tty(n_rows, n_cols);
  for (size_t i = 0; i < n_rows; ++i) {
    const auto& bitset = ttylv[i];
    for (size_t j = 0; j < n_cols; ++j) {
      tty(i, j) = static_cast<int>(bitset[j]);  // R logical is just 0/1 here
    }
  }
  return tty;
}



