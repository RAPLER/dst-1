// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(BH)]]

#include <RcppArmadillo.h>
#include <boost/dynamic_bitset.hpp>
#include <memory>
#include <algorithm>

using namespace Rcpp;
using boost::dynamic_bitset;
using std::shared_ptr;

struct TreeNode {
  dynamic_bitset<> x;
  double q;
  int index;
  shared_ptr<TreeNode> left = nullptr;
  shared_ptr<TreeNode> right = nullptr;
  shared_ptr<TreeNode> empty_set = nullptr;
  // TODO: add depth
  
  TreeNode(dynamic_bitset<> x_, double q_, int idx_) : x(x_), q(q_), index(idx_) {}
};

shared_ptr<TreeNode> insertNode(shared_ptr<TreeNode> node, shared_ptr<TreeNode> root) {
  // TODO: use the correct insert
  if (!root) return node;
  
  if (node->x < root->x) {
    root->left = insertNode(node, root->left);
  } else {
    root->right = insertNode(node, root->right);
  }
  return root;
}

shared_ptr<TreeNode> superset(shared_ptr<TreeNode> node, const dynamic_bitset<>& z) {
  if (!node) return nullptr;
  
  if ((node->x & z) == node->x) {
    return node;
  }
  
  auto left = superset(node->left, z);
  if (left) return left;
  
  return superset(node->right, z);
}

// [[Rcpp::export]]
SEXP buildTreeFast(const arma::sp_mat& tt, const NumericVector& q) {
  std::vector<shared_ptr<TreeNode>> nodes;
  std::vector<int> depths(tt.n_rows);
  
  for (unsigned int i = 0; i < tt.n_rows; ++i) {
    dynamic_bitset<> bitset(tt.n_cols);
    int last = -1;
    for (arma::sp_mat::const_row_iterator it = tt.begin_row(i); it != tt.end_row(i); ++it) {
      bitset.set(it.col());
      last = std::max(last, static_cast<int>(it.col()));
    }
    depths[i] = last;
  }
  
  std::vector<int> sort_order(tt.n_rows);
  std::iota(sort_order.begin(), sort_order.end(), 0);
  std::sort(sort_order.begin(), sort_order.end(), [&](int a, int b) {
    return depths[a] < depths[b];
  });
  
  shared_ptr<TreeNode> tree = nullptr;
  shared_ptr<TreeNode> empty_node = nullptr;
  for (int i : sort_order) {
    dynamic_bitset<> bitset(tt.n_cols);
    for (arma::sp_mat::const_row_iterator it = tt.begin_row(i); it != tt.end_row(i); ++it) {
      bitset.set(it.col());
    }
    if (bitset.none()) {
      empty_node = std::make_shared<TreeNode>(bitset, q[i], i);
      continue;
    }
    auto node = std::make_shared<TreeNode>(bitset, q[i], i);
    tree = insertNode(node, tree);
  }
  
  if (empty_node && tree) {
    tree->empty_set = empty_node;
  }
  
  XPtr<shared_ptr<TreeNode>> ptr(new shared_ptr<TreeNode>(tree), true);
  return ptr;
}

shared_ptr<TreeNode> updateTreeFastRec(shared_ptr<TreeNode> node, const dynamic_bitset<>& xx,
                                       const dynamic_bitset<>& s, shared_ptr<TreeNode> root) {
  if (!node) return nullptr;
  
  if (node->q != 0) {
    dynamic_bitset<> y = node->x;
    shared_ptr<TreeNode> e = superset(root, y | xx);
    if (e && e->x != y && ((y | s) & e->x) == e->x) {
      node->q -= e->q;
    }
  }
  
  node->left = updateTreeFastRec(node->left, xx, s, root);
  node->right = updateTreeFastRec(node->right, xx, s, root);
  if (node->empty_set) {
    node->empty_set = updateTreeFastRec(node->empty_set, xx, s, root);
  }
  
  return node;
}

// [[Rcpp::export]]
SEXP updateTreeFast(SEXP tree_ptr, LogicalVector xx_vec, LogicalVector s_vec) {
  XPtr<shared_ptr<TreeNode>> ptr(tree_ptr);
  auto tree = *ptr;
  
  dynamic_bitset<> xx(xx_vec.size());
  for (int i = 0; i < xx_vec.size(); ++i) {
    if (xx_vec[i]) xx.set(i);
  }
  
  dynamic_bitset<> s(s_vec.size());
  for (int i = 0; i < s_vec.size(); ++i) {
    if (s_vec[i]) s.set(i);
  }
  
  auto updated = updateTreeFastRec(tree, xx, s, tree);
  return tree_ptr;
}

void collectValues(shared_ptr<TreeNode> node, std::vector<std::pair<int, double>>& out) {
  if (!node) return;
  
  collectValues(node->left, out);
  if (node->index >= 0) {
    out.emplace_back(node->index, node->q);
  }
  collectValues(node->right, out);
  if (node->empty_set) {
    collectValues(node->empty_set, out);
  }
}

// [[Rcpp::export]]
NumericVector unravelTreeFast(SEXP tree_ptr, int n) {
  XPtr<shared_ptr<TreeNode>> ptr(tree_ptr);
  auto tree = *ptr;
  
  std::vector<std::pair<int, double>> values;
  collectValues(tree, values);
  std::sort(values.begin(), values.end());
  
  NumericVector result(n);
  for (const auto& p : values) {
    result[p.first] = p.second;
  }
  return result;
}
