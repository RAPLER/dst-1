// [[Rcpp::depends(BH)]]
// [[Rcpp::depends(RcppProgress)]]
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <stdio.h>
#include <boost/functional/hash.hpp>
#include <boost/dynamic_bitset.hpp>
#include <unordered_map>
#include <progress.hpp>
#include <progress_bar.hpp>
#include "eta_progress_bar.hpp"

using namespace Rcpp;

int findLastXX(const boost::dynamic_bitset<>& x) {
  for (int i = x.size() - 1; i >= 0; --i) {
    if (x[i]) return i;
  }
  return -1; // for the empty set
}

int findFirstXX(const boost::dynamic_bitset<>& x) {
  for (int i = 0; i < x.size(); ++i) {
    if (x[i]) return i;
  }
  return -1; // for the empty set
}

struct TreeNode {
  boost::dynamic_bitset<> x;
  double q;
  int index;
  int depth;
  std::shared_ptr<TreeNode> left = nullptr;
  std::shared_ptr<TreeNode> right = nullptr;
  std::shared_ptr<TreeNode> empty_set = nullptr;
  
  TreeNode(boost::dynamic_bitset<> x_, double q_, int idx_)
    : x(x_), q(q_), index(idx_), depth(findLastXX(x_)) {}
};

std::shared_ptr<TreeNode> insertNodeXX(std::shared_ptr<TreeNode> node1, std::shared_ptr<TreeNode> node2) {
  if (!node2) return node1;
  
  const boost::dynamic_bitset<>& x1 = node1->x;
  const boost::dynamic_bitset<>& x2 = node2->x;
  
  boost::dynamic_bitset<> mask(x1.size());
  mask.set();
  mask >>= (x1.size() - (node2->depth));
  
  bool match = ((x1 & mask) == (x2 & mask));
  
  if (!match) {
    boost::dynamic_bitset<> diff = x1 ^ x2;
    size_t depth_disj = findFirstXX(diff);
    
    boost::dynamic_bitset<> x_disj = x1;
    x_disj.set(depth_disj);
    
    boost::dynamic_bitset<> mask(x_disj.size());
    mask.set();
    mask >>= (x_disj.size() - (depth_disj + 1));
    
    x_disj &= mask;
    
    bool is_same = (x1 == x_disj);
    auto node_disj = std::make_shared<TreeNode>(
      x_disj,
      is_same ? node1->q : -1,
      is_same ? node1->index : -1
    );
    
    if (node_disj->depth < node2->depth && node_disj->depth < x_disj.size()) {
      if (x1[node_disj->depth]) {
        node_disj->right = node1;
        node_disj->left = node2;
      } else {
        node_disj->left = node1;
        node_disj->right = node2;
      }
      
      return node_disj;
    }
  }
  
  if (x1 == x2) {
    node2->q = node1->q;
    node2->index = node1->index;
    node2->depth = node1->depth;
    return node2;
  }
  
  if (x1[node2->depth] == x2[node2->depth]) {
    node2->right = insertNodeXX(node1, node2->right);
  } else {
    node2->left = insertNodeXX(node1, node2->left);
  }
  
  return node2;
}


std::shared_ptr<TreeNode> supersetXX(std::shared_ptr<TreeNode> node, const boost::dynamic_bitset<>& w) {
  if (!node) return nullptr;
  
  const boost::dynamic_bitset<>& x = node->x;
  
  // Check if node is a superset of w and has a valid q value
  if ((x & w) == w && node->q != -1) {
    return node;
  }
  
  // Check prefix up to depth
  if (node->depth >= 0) {
    boost::dynamic_bitset<> mask(w.size());
    mask.set();
    mask >>= (w.size() - (node->depth + 1));
    
    if (((x & mask) & w & mask) != (w & mask)) {
      return nullptr;
    }
  }
  
  // Traverse based on w[depth]
  if (w[node->depth]) {
    return supersetXX(node->right, w);
  } else {
    auto left = supersetXX(node->left, w);
    if (left) return left;
    return supersetXX(node->right, w);
  }
}

std::shared_ptr<TreeNode> buildTreeFastXX(const std::vector<boost::dynamic_bitset<>>& bitsets,
                                         const Rcpp::NumericVector& q,
                                         std::optional<std::vector<int>> indices = std::nullopt) {
  
  int n_rows = bitsets.size();
  
  std::vector<int> actual_indices;
  
  if (indices.has_value()) {
    actual_indices = indices.value();
  } else {
    actual_indices.resize(n_rows);
    std::iota(actual_indices.begin(), actual_indices.end(), 0);
  }
  
  std::vector<int> depths(n_rows);
  
  for (int i = 0; i < n_rows; ++i) {
    depths[i] = findLastXX(bitsets[i]);  // Boost has find_last()
  }
  
  std::vector<int> sort_order(n_rows);
  std::iota(sort_order.begin(), sort_order.end(), 0);
  std::sort(sort_order.begin(), sort_order.end(), [&](int a, int b) {
    return depths[a] < depths[b];
  });
  
  std::shared_ptr<TreeNode> tree = nullptr;
  std::shared_ptr<TreeNode> empty_node = nullptr;
  
  for (int k : sort_order) {
    const auto& bitset = bitsets[k];
    int index = actual_indices[k];
    
    if (bitset.none()) {
      empty_node = std::make_shared<TreeNode>(bitset, q[index], index);
      continue;
    }
    
    auto node = std::make_shared<TreeNode>(bitset, q[index], index);
    tree = insertNodeXX(node, tree);
  }
  
  if (empty_node && !tree) {
    tree = empty_node;
  } else if (empty_node && tree) {
    tree->empty_set = empty_node;
  }
  
  return tree;
}

// "multiple"

std::vector<std::shared_ptr<TreeNode>> buildTreesFastXX(const std::vector<boost::dynamic_bitset<>>& sets,
                                                       const Rcpp::NumericVector& q,
                                                       std::vector<int>& card_nodup) {
  int n = sets.size();
  std::vector<int> card(n);
  
  for (int i = 0; i < n; ++i) {
    card[i] = sets[i].count();
  }
  
  std::vector<int> sort_order(n);
  std::iota(sort_order.begin(), sort_order.end(), 0);
  std::sort(sort_order.begin(), sort_order.end(), [&](int a, int b) {
    return card[a] < card[b];
  });
  
  std::vector<int> card_sorted(n);
  for (int i = 0; i < n; ++i) {
    card_sorted[i] = card[sort_order[i]];
  }
  
  std::unique_copy(card_sorted.begin(), card_sorted.end(), std::back_inserter(card_nodup));
  
  std::vector<std::shared_ptr<TreeNode>> trees(card_nodup.size(), nullptr);
  
  for (size_t i = 0; i < card_nodup.size(); ++i) {
    int c = card_nodup[i];
    std::vector<int> idx_vec;
    
    for (int j = 0; j < n; ++j) {
      if (card[j] == c) idx_vec.push_back(j);
    }
    
    std::vector<boost::dynamic_bitset<>> sub_sets;
    Rcpp::IntegerVector indices(idx_vec.size());
    
    for (size_t k = 0; k < idx_vec.size(); ++k) {
      sub_sets.push_back(sets[idx_vec[k]]);
      indices[k] = idx_vec[k];
    }
    
    std::vector<int> idx_std(indices.begin(), indices.end());
    trees[i] = buildTreeFastXX(sub_sets, q, idx_std);
  }
  
  return trees;
}



//' superBcaFastBelplauSingleton is a C++ algorithm aimed to optimize the computation of multiple support functions defined on very large frames of discernment
//' @name superBcaFastBelplauSingleton
//' @export

// [[Rcpp::export]]
Rcpp::List superBcaFastBelplauSingleton(const arma::mat& x_input,
                       const arma::vec& y,
                       double a,
                       int y0 = 0,
                       bool flip = true,
                       std::string tree_type = "single",
                       bool dsa = false) {
 arma::mat ttx = x_input;
 
 // Optional inversion of rows
 for (size_t i = 0; i < y.n_elem; ++i) {
   if (static_cast<int>(y(i)) == y0) {
     for (size_t j = 0; j < ttx.n_cols; ++j) {
       ttx(i, j) = 1.0 - ttx(i, j);
     }
   }
 }
 
 // Declare lists of bitsets
 std::vector<boost::dynamic_bitset<>> ttxlv; // input simple supports
 std::vector<boost::dynamic_bitset<>> ttylv; // closure elements
 std::unordered_map<boost::dynamic_bitset<>, size_t> m0;
 
 for (size_t i = 0; i < ttx.n_rows; ++i) {
   boost::dynamic_bitset<> bitset_row(ttx.n_cols);
   for (size_t j = 0; j < ttx.n_cols; ++j) {
     if (ttx(i, j) != 0) {
       bitset_row[j] = 1;
     }
   }
   ttxlv.push_back(bitset_row);
   ttylv.push_back(bitset_row);
   m0.emplace(bitset_row, 0);
 }
 
 // Compute closure
 Rcpp::Rcout << "Computing meet-closure (intersection closure)..." << std::endl;
 ETAProgressBar pb1;
 Progress p1(ttxlv.size(), true, pb1);
 
 for (size_t i = 0; i < ttxlv.size(); ++i) {
   if (Progress::check_abort()) break;
   p1.increment();
   for (size_t j = 0; j < ttylv.size(); ++j) {
     boost::dynamic_bitset<> meet = ttylv[i] & ttylv[j];
     if (m0.find(meet) == m0.end()) {
       ttylv.push_back(meet);
       m0.emplace(meet, 0);
     }
   }
 }
 
 // Add full frame if missing
 boost::dynamic_bitset<> full_frame(ttx.n_cols);
 full_frame.set();
 if (m0.find(full_frame) == m0.end()) {
   ttylv.push_back(full_frame);
   m0.emplace(full_frame, 0);
 }
 
 // Compute commonality vector qq
 Rcpp::Rcout << "Computing commonality vector qq..." << std::endl;
 ETAProgressBar pb2;
 Progress p2(ttylv.size(), true, pb2);
 
 NumericVector qq(ttylv.size());
 for (size_t i = 0; i < ttylv.size(); ++i) {
   if (Progress::check_abort()) break;
   p2.increment();
   
   const auto& c_bits = ttylv[i];
   int count_not_subsets = 0;
   
   for (size_t j = 0; j < ttxlv.size(); ++j) {
     const auto& s_bits = ttxlv[j];
     if ((s_bits & c_bits) != c_bits) {
       ++count_not_subsets;
     }
   }
   
   qq[i] = std::pow(1.0 - a, count_not_subsets);
 }
 
 size_t n_rows = ttylv.size();
 size_t n_cols = ttx.n_cols;
 
 // Convert ttylv to arma::sp_mat
 arma::sp_mat tty(n_rows, n_cols);
 for (size_t i = 0; i < n_rows; ++i) {
   const auto& bitset = ttylv[i];
   for (size_t j = 0; j < n_cols; ++j) {
     if (bitset[j]) {
       tty(i, j) = 1.0;
     }
   }
 }
 
 Rcpp::Rcout << "Computing plausibility from commonality function of singletons..." << std::endl;
 
 int M = n_cols;
 NumericVector plau(M, 0.0);
 
 // Build tree from ttylv and qq
 std::shared_ptr<TreeNode> tree = buildTreeFastXX(ttylv, qq, std::nullopt);
 
 ETAProgressBar pb3;
 Progress p3(M, true, pb3);
 
 for (int j = 0; j < M; ++j) {
   if (Progress::check_abort()) break;
   p3.increment();
   
   boost::dynamic_bitset<> sj(M);       // {j}
   sj.set(j);
   
   auto node_plau = supersetXX(tree, sj);

   plau[j] = node_plau->q;
 }
 
 return Rcpp::List::create(
   Rcpp::Named("tt") = tty,
   Rcpp::Named("qq") = qq,
   Rcpp::Named("plau") = plau
 );
}






