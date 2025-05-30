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

//' TreeFast is an algorithm aimed to optimize the computation of multiple support functions defined on very large frames of discernment
//' @name treeFast
//' @export

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
SEXP buildTreeFast(const arma::sp_mat& tt,
                   const Rcpp::NumericVector& q,
                   bool display_progress = false,
                   Rcpp::Nullable<Rcpp::IntegerVector> indices = R_NilValue) {

  int n_rows = tt.n_rows;
  int n_cols = tt.n_cols;
  
  IntegerVector actual_indices;
  if (indices.isNotNull()) {
    actual_indices = indices.get();
  } else {
    actual_indices = seq(0, n_rows - 1);  // default index vector
  }
  
  std::vector<int> depths(n_rows);
  ETAProgressBar pb;
  Progress p(n_rows, display_progress, pb);
  
  for (int i = 0; i < n_rows; ++i) {
    if (Progress::check_abort()) break;
    p.increment();
    
    dynamic_bitset<> bitset(n_cols);
    int last = -1;
    for (arma::sp_mat::const_row_iterator it = tt.begin_row(i); it != tt.end_row(i); ++it) {
      bitset.set(it.col());
      last = std::max(last, static_cast<int>(it.col()));
    }
    depths[i] = last;
  }
  
  std::vector<int> sort_order(n_rows);
  std::iota(sort_order.begin(), sort_order.end(), 0);
  std::sort(sort_order.begin(), sort_order.end(), [&](int a, int b) {
    return depths[a] < depths[b];
  });
  
  std::shared_ptr<TreeNode> tree = nullptr;
  std::shared_ptr<TreeNode> empty_node = nullptr;
  
  for (int k : sort_order) {
    dynamic_bitset<> bitset(n_cols);
    for (arma::sp_mat::const_row_iterator it = tt.begin_row(k); it != tt.end_row(k); ++it) {
      bitset.set(it.col());
    }
    
    int index = actual_indices[k];
    
    if (bitset.none()) {
      empty_node = std::make_shared<TreeNode>(bitset, q[index], index);
      continue;
    }
    
    auto node = std::make_shared<TreeNode>(bitset, q[index], index);
    tree = insertNode(node, tree);
  }
  
  if (empty_node && !tree) {
    tree = empty_node;
  } else if (empty_node && tree) {
    tree->empty_set = empty_node;
  }
  
  return XPtr<std::shared_ptr<TreeNode>>(new std::shared_ptr<TreeNode>(tree), true);
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

// Multiple trees
// TODO

// [[Rcpp::export]]
Rcpp::List inspectNodes(Rcpp::List trees) {

  IntegerVector card_nodup = trees.attr("card_nodup");
  int num_trees = card_nodup.size();
  List result(num_trees + 1);  // One extra for card_nodup
  
  std::function<List(std::shared_ptr<TreeNode>)> recurse = [&](std::shared_ptr<TreeNode> n) -> List {
    if (!n) return R_NilValue;
    
    IntegerVector bits(n->x.size());
    for (size_t i = 0; i < n->x.size(); ++i) bits[i] = n->x[i];
    
    List out = List::create(
      _["x"] = bits,
      _["q"] = n->q,
      _["index"] = n->index,
      _["depth"] = n->depth,
      _["left"] = recurse(n->left),
      _["right"] = recurse(n->right)
    );
    
    if (n->empty_set) {
      out.push_back(recurse(n->empty_set), "empty_set");
    }
    
    return out;
  };
  
  for (int i = 0; i < num_trees; ++i) {
    if (Rf_isNull(trees[i])) {
      result[i] = R_NilValue;
    } else {
      XPtr<std::shared_ptr<TreeNode>> ptr(trees[i]);
      std::shared_ptr<TreeNode> root = *ptr;
      
      if (root) {
        result[i] = recurse(root);
      } else if (ptr && (*ptr) && (*ptr)->empty_set) {
        result[i] = List::create(_["empty_set"] = recurse((*ptr)->empty_set));
      } else {
        result[i] = R_NilValue;
      }
    }
  }
  
  result[num_trees] = card_nodup;
  return result;
}

// [[Rcpp::export]]
Rcpp::List buildTreesFast(const arma::sp_mat& tt, const Rcpp::NumericVector& q) {

  int n = tt.n_rows;
  int p = tt.n_cols;
  
  // Compute cardinality (number of 1s per row)
  std::vector<int> card(n, 0);
  for (int i = 0; i < n; ++i) {
    for (arma::sp_mat::const_row_iterator it = tt.begin_row(i); it != tt.end_row(i); ++it) {
      card[i]++;
    }
  }
  
  // Sort indices by cardinality
  std::vector<int> sort_order(n);
  std::iota(sort_order.begin(), sort_order.end(), 0);
  std::sort(sort_order.begin(), sort_order.end(), [&](int a, int b) {
    return card[a] < card[b];
  });
  
  // Compute unique cardinalities
  std::vector<int> card_sorted(n);
  for (int i = 0; i < n; ++i) {
    card_sorted[i] = card[sort_order[i]];
  }
  std::vector<int> card_nodup;
  std::unique_copy(card_sorted.begin(), card_sorted.end(), std::back_inserter(card_nodup));
  
  List trees(card_nodup.size());
  
  for (int i = 0; i < static_cast<int>(card_nodup.size()); ++i) {
    int c = card_nodup[i];
    std::vector<int> idx_vec;
    for (int j = 0; j < n; ++j) {
      if (card[j] == c) idx_vec.push_back(j);
    }
    
    // Create submatrix manually for tt_sub
    arma::sp_mat tt_sub(idx_vec.size(), p);
    for (size_t row = 0; row < idx_vec.size(); ++row) {
      for (arma::sp_mat::const_row_iterator it = tt.begin_row(idx_vec[row]); it != tt.end_row(idx_vec[row]); ++it) {
        tt_sub(row, it.col()) = 1.0;
      }
    }
    
    IntegerVector indices(idx_vec.begin(), idx_vec.end());
    trees[i] = buildTreeFast(tt_sub, q, false, indices);  // modified buildTreeFast with index support
  }
  
  trees.attr("card_nodup") = IntegerVector(card_nodup.begin(), card_nodup.end());
  return trees;
}

// [[Rcpp::export]]
Rcpp::NumericVector unravelTreesFast(Rcpp::List trees) {

  IntegerVector card_nodup = trees.attr("card_nodup");
  int num_trees = card_nodup.size();
  
  std::vector<std::pair<int, double>> values;
  
  for (int i = 0; i < num_trees; ++i) {
    if (Rf_isNull(trees[i])) continue;
    
    XPtr<std::shared_ptr<TreeNode>> ptr(trees[i]);
    std::shared_ptr<TreeNode> root = *ptr;
    
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
    
    traverse(root);
  }
  
  // Sort by index
  std::sort(values.begin(), values.end(), [](const auto& a, const auto& b) {
    return a.first < b.first;
  });
  
  NumericVector result(values.size());
  for (size_t i = 0; i < values.size(); ++i) {
    result[i] = values[i].second;
  }
  
  return result;
}

// [[Rcpp::export]]
Rcpp::List updateTreesFast(Rcpp::List trees, Rcpp::NumericVector xx_vec, Rcpp::NumericVector s_vec) {

  IntegerVector card_nodup = trees.attr("card_nodup");
  
  dynamic_bitset<> xx(xx_vec.size());
  for (int i = 0; i < xx_vec.size(); ++i) {
    if (xx_vec[i] != 0.0) xx.set(i);
  }
  
  dynamic_bitset<> s(s_vec.size());
  for (int i = 0; i < s_vec.size(); ++i) {
    if (s_vec[i] != 0.0) s.set(i);
  }
  
  for (int t = 0; t < trees.size(); ++t) {
    if (Rf_isNull(trees[t])) continue;
    
    XPtr<std::shared_ptr<TreeNode>> root_ptr(trees[t]);
    std::shared_ptr<TreeNode> root = *root_ptr;
    
    std::function<shared_ptr<TreeNode>(shared_ptr<TreeNode>)> updateRec = [&](shared_ptr<TreeNode> node) -> shared_ptr<TreeNode> {
      if (!node) return nullptr;
      
      if (node->q >= 0) {
        dynamic_bitset<> y = node->x;
        int y_union_xx_card = (y | xx).count();
        
        for (int i = 0; i < card_nodup.size(); ++i) {
          if (card_nodup[i] >= y_union_xx_card) {
            for (int j = i; j < card_nodup.size(); ++j) {
              if (Rf_isNull(trees[j])) continue;
              
              XPtr<std::shared_ptr<TreeNode>> target_ptr(trees[j]);
              std::shared_ptr<TreeNode> target_root = *target_ptr;
              
              auto e = superset(target_root, y | xx);
              if (e) {
                dynamic_bitset<> z = e->x;
                if (z != y && ((y | s) & z) == z) {
                  node->q -= e->q;
                }
                goto NEXT_NODE;
              }
            }
          }
        }
      }
      
      NEXT_NODE:
        node->left = updateRec(node->left);
      node->right = updateRec(node->right);
      if (node->empty_set) {
        node->empty_set = updateRec(node->empty_set);
      }
      
      return node;
    };
    
    updateRec(root);
  }
  
  return trees;
}

