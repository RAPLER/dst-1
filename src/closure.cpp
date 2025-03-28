// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <stdio.h>
#include <boost/functional/hash.hpp>
#include <boost/dynamic_bitset.hpp>
using namespace Rcpp;

//' Augment list of binary vectors with closure elements
//' 
//' @param ttxl list of binary vectors
//' @export


// [[Rcpp::export]]
List closure(List ttxl, bool computeJoin = true) {
  std::vector<boost::dynamic_bitset<>> ttxlv;
  std::vector<boost::dynamic_bitset<>> ttylv;
  std::unordered_map<boost::dynamic_bitset<>, size_t> m0;
  
  // Convert input list to bitset vector
  for (int i = 0; i < ttxl.size(); ++i) {
    IntegerVector vec = as<IntegerVector>(ttxl[i]);
    boost::dynamic_bitset<> bitset1(vec.size());
    
    for (size_t j = 0; j < vec.size(); ++j) {
      if (vec[j] != 0) {
        bitset1[j] = 1;
      }
    }
    
    ttxlv.push_back(bitset1);
    ttylv.push_back(bitset1);
    m0.emplace(bitset1, 0);
  }
  
  // Compute closures
  for (size_t i = 0; i < ttylv.size(); ++i) {
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
  
  // Convert result to R list
  Rcpp::List result;
  for (const auto& bitset : ttylv) {
    Rcpp::IntegerVector rvec(bitset.size());
    for (size_t i = 0; i < bitset.size(); ++i) {
      rvec[i] = bitset[i];
    }
    result.push_back(rvec);
  }
  
  return result;
}



