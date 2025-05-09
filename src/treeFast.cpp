// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(BH)]]

#include <RcppArmadillo.h>
#include <boost/dynamic_bitset.hpp>
#include <memory>
#include <algorithm>
#include <progress.hpp>
#include <progress_bar.hpp>
#include "eta_progress_bar.hpp"

using namespace Rcpp;
using boost::dynamic_bitset;
using std::shared_ptr;

int findLast(const dynamic_bitset<>& x) {
  for (int i = x.size() - 1; i >= 0; --i) {
    if (x[i]) return i;
  }
  return -1; // for the empty set
}

int findFirst(const dynamic_bitset<>& x) {
  for (int i = 0; i < x.size(); ++i) {
    if (x[i]) return i;
  }
  return -1; // for the empty set
}

struct TreeNode {
  dynamic_bitset<> x;
  double q;
  int index;
  int depth;
  shared_ptr<TreeNode> left = nullptr;
  shared_ptr<TreeNode> right = nullptr;
  shared_ptr<TreeNode> empty_set = nullptr;
  
  TreeNode(dynamic_bitset<> x_, double q_, int idx_)
    : x(x_), q(q_), index(idx_), depth(findLast(x_)) {}
};

shared_ptr<TreeNode> insertNode(shared_ptr<TreeNode> node1, shared_ptr<TreeNode> node2) {
  if (!node2) return node1;
  
  boost::dynamic_bitset<> mask(node1->x.size());
  mask.set();  // all bits to 1
  mask >>= (node1->x.size() - (node2->depth));  // keep only prefix bits
  
  bool match = ((node1->x & mask) == (node2->x & mask));
  
  if (!match) {
    // insert disjunction node
    
    boost::dynamic_bitset<> diff = node1->x ^ node2->x;
    size_t depth_disj = findFirst(diff);
    
    dynamic_bitset<> x_disj = node1->x;
    x_disj.set(depth_disj);
    
    dynamic_bitset<> mask(x_disj.size());
    mask.set(); // all 1s
    mask >>= (x_disj.size() - (depth_disj + 1)); 
    
    x_disj &= mask;
    
    bool is_same = (node1->x == x_disj);
    auto node_disj = std::make_shared<TreeNode>(
      x_disj,
      is_same ? node1->q : -1,
      is_same ? node1->index : -1
    );
    
    if (node_disj->depth < node2->depth && node_disj->depth < x_disj.size()) {
      if (node1->x[node_disj->depth]) {
        node_disj->right = node1;
        node_disj->left = node2;
      } else {
        node_disj->left = node1;
        node_disj->right = node2;
      }
      
      //if (!is_same) {
      //  node_disj = insertNode(node1, node_disj->right);
      //}

      return node_disj;
    }
  }
  
  if (node1->x == node2->x) { 
    node2->q = node1->q;
    node2->index = node1->index;
    node2->depth = node1->depth;
    return node2;
  }
  
  if (node1->x[node2->depth] == node2->x[node2->depth]) {
    node2->right = insertNode(node1, node2->right);
  } else {
    node2->left = insertNode(node1, node2->left);
  }
  
  return node2;
}

shared_ptr<TreeNode> superset(shared_ptr<TreeNode> node, const dynamic_bitset<>& w) {
  if (!node) return nullptr;
  
  // Check if node is a superset of w and has a valid q value
  if ((node->x & w) == w && node->q != -1) {
    return node;
  }
  
  // Check prefix up to depth (0-indexed): positions 0 to depth
  if (node->depth >= 0) {
    dynamic_bitset<> mask(w.size());
    mask.set();  // All 1s
    mask >>= (w.size() - (node->depth + 1));  // Keep only first (depth + 1) bits
    
    // If prefix bits of x and w differ, prune this branch
    if (((node->x & mask) & w & mask) != (w & mask)) {
      return nullptr;
    }
  }
  
  // Traverse based on w[depth]
  if (w[node->depth]) {
    return superset(node->right, w);
  } else {
    auto left = superset(node->left, w);
    if (left) return left;
    return superset(node->right, w);
  }
}


// [[Rcpp::export]]
SEXP buildTreeFast(const arma::sp_mat& tt, const NumericVector& q, bool display_progress = false) {
  std::vector<shared_ptr<TreeNode>> nodes;
  std::vector<int> depths(tt.n_rows);
  
  ETAProgressBar pb;
  Progress p(tt.n_rows, display_progress, pb);
  
  for (unsigned int i = 0; i < tt.n_rows; ++i) {
    
    if (Progress::check_abort()) break;
    p.increment();
    
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
  if (node) {
  
    if (node->q >= 0) {
      dynamic_bitset<> y = node->x;
      shared_ptr<TreeNode> e = superset(root, y | xx);
      if (e) {
        dynamic_bitset<> z = e->x;
        if (z != y) {
          dynamic_bitset<> t = (y | s) & z;
          if (t == z) {
            node->q -= e->q;
          }
        }
      }
    }
  
    node->left = updateTreeFastRec(node->left, xx, s, root);
    node->right = updateTreeFastRec(node->right, xx, s, root);
    if (node->empty_set) {
      node->empty_set = updateTreeFastRec(node->empty_set, xx, s, root);
    }
    
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
  if (node->index >= 0 && node->q != -1) {
    out.emplace_back(node->index, node->q);
  }
  collectValues(node->right, out);
  if (node->empty_set) {
    collectValues(node->empty_set, out);
  }
}

// [[Rcpp::export]]
SEXP supersetFast(SEXP node_ptr, LogicalVector z_vec) {
  XPtr<shared_ptr<TreeNode>> ptr(node_ptr);
  auto tree = *ptr;
  
  dynamic_bitset<> z(z_vec.size());
  for (int i = 0; i < z_vec.size(); ++i) {
    if (z_vec[i]) z.set(i);
  }
  
  auto result = superset(tree, z);
  XPtr<shared_ptr<TreeNode>> out(new shared_ptr<TreeNode>(result), true);
  return out;
}

// [[Rcpp::export]]
NumericVector unravelTreeFast(SEXP tree_ptr) {
  using namespace Rcpp;
  XPtr<std::shared_ptr<TreeNode>> ptr(tree_ptr);
  auto tree = *ptr;
  
  // Store index-q pairs
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
  
  traverse(tree);
  
  // Sort by index
  std::sort(values.begin(), values.end(), [](const auto& a, const auto& b) {
    return a.first < b.first;
  });
  
  // Extract q values
  NumericVector result(values.size());
  for (size_t i = 0; i < values.size(); ++i) {
    result[i] = values[i].second;
  }
  
  return result;
}



// [[Rcpp::export]]
List inspectNode(SEXP tree_ptr) {
  XPtr<shared_ptr<TreeNode>> ptr(tree_ptr);
  auto root = *ptr;
  
  if (!root) return R_NilValue;
  
  // Recursive function to convert the full tree into nested R lists
  std::function<List(shared_ptr<TreeNode>)> buildTree = [&](shared_ptr<TreeNode> n) -> List {
    if (!n) return R_NilValue;
    
    IntegerVector bits(n->x.size());
    for (size_t i = 0; i < n->x.size(); ++i) bits[i] = n->x[i];
    
    List result = List::create(
      _["x"] = bits,
      _["q"] = n->q,
      _["index"] = n->index,
      _["depth"] = n->depth,
      _["left"] = buildTree(n->left),
      _["right"] = buildTree(n->right)
    );
    
    if (n->empty_set) {
      result.push_back(buildTree(n->empty_set), "empty_set");
    }
    
    return result;
  };
  
  return buildTree(root);
}


