// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <stdio.h>
#include <boost/functional/hash.hpp>
#include <boost/dynamic_bitset.hpp>
using namespace Rcpp;

// [[Rcpp::export]]
List closure(List ttxl, List ttyl){
  std::vector<boost::dynamic_bitset<>> ttxlv;
  
  std::vector<boost::dynamic_bitset<>> ttylv;
  
  std::unordered_map<boost::dynamic_bitset<>, size_t> m0;
  
  for(int i=0; i<ttxl.size(); ++i) {
    
    IntegerVector vec = as<IntegerVector>(ttxl[i]);
    
    boost::dynamic_bitset<> bitset1(vec.size());

    for (size_t j = 0; j < vec.size(); ++j) {
      if (vec[j] != 0) {
        bitset1[j] = 1;
      }
    }
    
    ttxlv.push_back(bitset1);
    ttylv.push_back(bitset1);
    
    m0.emplace(bitset1,0);
    
  }
  
  for(int i=0; i<ttxlv.size(); ++i) {
    
    for(int j=i+1; j<ttylv.size(); ++j) {
      
      boost::dynamic_bitset<> intersection = ttxlv[i] & ttylv[j];
      
      if (m0.find(intersection) == m0.end()) {
        
        ttylv.push_back(intersection);
        
        m0.emplace(intersection,0);
        
      }
      
    }
    
  }

  Rcpp::List result;
  
  for (const auto& bitset : ttylv) {
    Rcpp::IntegerVector rvec(bitset.size());
    
    for (size_t i = 0; i < bitset.size(); ++i) {
      rvec[i] = bitset[i]; 
    }
    
    result.push_back(rvec);
  }
  
  return(result);
}

