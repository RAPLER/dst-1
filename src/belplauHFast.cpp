#include <RcppArmadillo.h>
#include <boost/dynamic_bitset.hpp>
#include <progress.hpp>
#include <progress_bar.hpp>
#include "eta_progress_bar.hpp"

using namespace Rcpp;
using boost::dynamic_bitset;

//' Calculate belief, disbelief, unknown, plausibility, plausibility ratio
//' @name belplauHFast
//' @param MACC Vector of masses e.g. x$m
//' @param W2 Description matrix e.g. x$tt
//' @param h Hypotheses to be tested, same format as x$tt
//' @param display_progress Whether to show progress bar
//' @return A matrix of \code{M} rows by 5 columns is returned, where \code{M} is the number of hypothesis tested: \itemize{
//'  \item Column 1: the degree of Belief \code{bel};
//'  \item Column 2: the degree of Disbellief (belief in favor of the contrary hypothesis) \code{disbel};
//'  \item Column 3: the degree of Epistemic uncertainty \code{unc};
//'  \item Column 4: the degree of Plausibility \code{plau};
//'  \item Column 5: the Plausibility ratio \code{rplau}.
//'    }
//' @examples 1
//' @export

 
 // [[Rcpp::export]]
 NumericMatrix belplauHFast(NumericVector MACC, const arma::sp_mat& W2, NumericMatrix h, bool display_progress = false) {
   int M = h.nrow();          // Number of hypotheses
   int N = W2.n_rows;         // Number of focal elements
   int D = h.ncol();          // Dimension of the frame
   
   NumericVector bel(M, 0.0), disbel(M, 0.0);
   
   // Precompute W2 as bitsets
   std::vector<dynamic_bitset<>> W2_bitsets(N, dynamic_bitset<>(D));
   for (int i = 0; i < N; ++i) {
     for (arma::sp_mat::const_row_iterator it = W2.begin_row(i); it != W2.end_row(i); ++it) {
       W2_bitsets[i].set(it.col());
     }
   }
   
   // Convert hypotheses to bitsets (only one vector)
   std::vector<dynamic_bitset<>> h_bitsets(M, dynamic_bitset<>(D));
   for (int j = 0; j < M; ++j) {
     for (int d = 0; d < D; ++d) {
       if (h(j, d) != 0) h_bitsets[j].set(d);
     }
   }
   
   // Compute belief and disbelief
   ETAProgressBar pb;
   Progress p(N, display_progress, pb);
   for (int i = 0; i < N; ++i) {
     if (Progress::check_abort()) break;
     p.increment();
     const auto& Wi = W2_bitsets[i];
     for (int j = 0; j < M; ++j) {
       if ((Wi & h_bitsets[j]) == Wi) {
         bel[j] += MACC[i];
       }
       dynamic_bitset<> h_complement = ~h_bitsets[j];
       if ((Wi & h_complement) == Wi) {
         disbel[j] += MACC[i];
       }
     }
   }
   
   // Compute plausibility, uncertainty, ratio
   NumericVector plau = 1 - disbel;
   NumericVector rplau(M), unc(M);
   for (int j = 0; j < M; ++j) {
     unc[j] = plau[j] - bel[j];
     rplau[j] = plau[j] / (1 - bel[j]);
   }
   
   // Final result matrix
   NumericMatrix result(M, 5);
   result(_, 0) = bel;
   result(_, 1) = disbel;
   result(_, 2) = unc;
   result(_, 3) = plau;
   result(_, 4) = rplau;
   
   colnames(result) = CharacterVector::create("bel", "disbel", "unc", "plau", "rplau");
   
   return result;
 }
