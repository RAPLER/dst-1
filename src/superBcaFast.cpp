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

int findLastX(const boost::dynamic_bitset<>& x) {
  for (int i = x.size() - 1; i >= 0; --i) {
    if (x[i]) return i;
  }
  return -1; // for the empty set
}

int findFirstX(const boost::dynamic_bitset<>& x) {
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
    : x(x_), q(q_), index(idx_), depth(findLastX(x_)) {}
};

std::shared_ptr<TreeNode> insertNodeX(std::shared_ptr<TreeNode> node1, std::shared_ptr<TreeNode> node2) {
  if (!node2) return node1;
  
  const boost::dynamic_bitset<>& x1 = node1->x;
  const boost::dynamic_bitset<>& x2 = node2->x;
  
  boost::dynamic_bitset<> mask(x1.size());
  mask.set();
  mask >>= (x1.size() - (node2->depth));
  
  bool match = ((x1 & mask) == (x2 & mask));
  
  if (!match) {
    boost::dynamic_bitset<> diff = x1 ^ x2;
    size_t depth_disj = findFirstX(diff);
    
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
    node2->right = insertNodeX(node1, node2->right);
  } else {
    node2->left = insertNodeX(node1, node2->left);
  }
  
  return node2;
}


std::shared_ptr<TreeNode> supersetX(std::shared_ptr<TreeNode> node, const boost::dynamic_bitset<>& w) {
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
    return supersetX(node->right, w);
  } else {
    auto left = supersetX(node->left, w);
    if (left) return left;
    return supersetX(node->right, w);
  }
}


std::shared_ptr<TreeNode> updateTreeFastRecX(std::shared_ptr<TreeNode> node, const boost::dynamic_bitset<>& xx,
                                             const boost::dynamic_bitset<>& s, std::shared_ptr<TreeNode> root) {
  if (node) {
    if (node->q >= 0) {
      const boost::dynamic_bitset<>& y = node->x;
      std::shared_ptr<TreeNode> e = supersetX(root, y | xx);
      if (e) {
        const boost::dynamic_bitset<>& z = e->x;
        if (z != y) {
          boost::dynamic_bitset<> t = (y | s) & z;
          if (t == z) {
            node->q -= e->q;
          }
        }
      }
    }
    
    node->left = updateTreeFastRecX(node->left, xx, s, root);
    node->right = updateTreeFastRecX(node->right, xx, s, root);
    if (node->empty_set) {
      node->empty_set = updateTreeFastRecX(node->empty_set, xx, s, root);
    }
  }
  
  return node;
}


std::shared_ptr<TreeNode> buildTreeFastX(const std::vector<boost::dynamic_bitset<>>& bitsets,
                                         const Rcpp::NumericVector& q,
                                         bool display_progress = false,
                                         Rcpp::Nullable<Rcpp::IntegerVector> indices = R_NilValue) {
  
  int n_rows = bitsets.size();
  
  Rcpp::IntegerVector actual_indices;
  if (indices.isNotNull()) {
    actual_indices = indices.get();
  } else {
    actual_indices = Rcpp::seq(0, n_rows - 1);
  }
  
  std::vector<int> depths(n_rows);
  ETAProgressBar pb;
  Progress p(n_rows, display_progress, pb);
  
  for (int i = 0; i < n_rows; ++i) {
    if (Progress::check_abort()) break;
    p.increment();
    depths[i] = findLastX(bitsets[i]);  // Boost has find_last()
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
    tree = insertNodeX(node, tree);
  }
  
  if (empty_node && !tree) {
    tree = empty_node;
  } else if (empty_node && tree) {
    tree->empty_set = empty_node;
  }
  
  return tree;
}


void updateTreeFastX(std::shared_ptr<TreeNode> tree_ptr,
                     const boost::dynamic_bitset<>& xx,
                     const boost::dynamic_bitset<>& s) {
  updateTreeFastRecX(tree_ptr, xx, s, tree_ptr);
}


Rcpp::NumericVector unravelTreeFastX(std::shared_ptr<TreeNode> tree_ptr) {
  
  std::vector<std::pair<int, double>> values;
  
  std::function<void(std::shared_ptr<TreeNode>)> traverse = [&](std::shared_ptr<TreeNode> node) {
    if (!node) return;
    
    traverse(node->left);
    if (node->index >= 0 && !std::isnan(node->q)) {
      values.emplace_back(node->index, node->q);
    }
    traverse(node->right);
    
    if (node->empty_set) {
      traverse(node->empty_set);
    }
  };
  
  traverse(tree_ptr);
  
  std::sort(values.begin(), values.end(), [](const auto& a, const auto& b) {
    return a.first < b.first;
  });
  
  Rcpp::NumericVector result(values.size());
  for (size_t i = 0; i < values.size(); ++i) {
    result[i] = values[i].second;
  }
  
  return result;
}

//' superBcaFast is a C++ algorithm aimed to optimize the computation of multiple support functions defined on very large frames of discernment
//' @name superBcaFast
//' @export

// [[Rcpp::export]]
Rcpp::List superBcaFast(const arma::mat& x_input,
                        const arma::vec& y,
                        double a,
                        int y0 = 0,
                        bool flip = true,
                        std::string tree_type = "single") {
  
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
  
  // Compute iota elements
  std::unordered_map<boost::dynamic_bitset<>, size_t> iota_map;
  Rcpp::Rcout << "Computing iota elements (extreme atoms for each frame element)..." << std::endl;
  
  ETAProgressBar pb3;
  Progress p3(n_cols, true, pb3);
  
  for (size_t omega = 0; omega < n_cols; ++omega) {
    if (Progress::check_abort()) break;
    p3.increment();
    
    boost::dynamic_bitset<> i(n_cols);
    i.set();
    bool included = false;
    
    for (const auto& FFbs : ttylv) {
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
  
  // Collect iota elements
  std::vector<boost::dynamic_bitset<>> iota_bitsets;
  for (const auto& [bs, _] : iota_map) {
    iota_bitsets.push_back(bs);
  }
  
  std::sort(iota_bitsets.begin(), iota_bitsets.end(),
            [](const boost::dynamic_bitset<>& a, const boost::dynamic_bitset<>& b) {
              return a.count() < b.count();
            });
  
  // Convert iota_bitsets to arma::mat
  arma::mat W24(iota_bitsets.size(), n_cols, arma::fill::zeros);
  for (size_t i = 0; i < iota_bitsets.size(); ++i) {
    for (size_t j = 0; j < n_cols; ++j) {
      if (iota_bitsets[i][j]) {
        W24(i, j) = 1.0;
      }
    }
  }
  
  // Convert ttylv to arma::mat
  arma::mat tty(n_rows, n_cols, arma::fill::zeros);
  for (size_t i = 0; i < n_rows; ++i) {
    const auto& bitset = ttylv[i];
    for (size_t j = 0; j < n_cols; ++j) {
      if (bitset[j]) {
        tty(i, j) = 1.0;
      }
    }
  }
  
  // Tree update logic
  NumericVector m;
  if (tree_type == "single") {
    std::shared_ptr<TreeNode> tree = buildTreeFastX(ttylv, qq, false, R_NilValue);
    
    Rcpp::Rcout << "Updating commonality tree using iota elements..." << std::endl;
    ETAProgressBar pb4;
    Progress p4(n_cols, true, pb4);
    
    for (int i = static_cast<int>(iota_bitsets.size()) - 1; i >= 0; --i) {
      if (Progress::check_abort()) break;
      p4.increment();
      
      const auto& xx = iota_bitsets[i];
      boost::dynamic_bitset<> sup(n_cols);
      for (int j = 0; j <= i; ++j) {
        sup |= iota_bitsets[j];
      }
      
      updateTreeFastX(tree, xx, sup);
    }
    
    m = unravelTreeFastX(tree);
  } else if (tree_type == "multiple") {
    // (To be implemented)
  }
  
  return Rcpp::List::create(
    Rcpp::Named("tt") = tty,
    Rcpp::Named("qq") = qq,
    Rcpp::Named("W24") = W24,
    Rcpp::Named("m") = m
  );
}







