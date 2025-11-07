#' Plausibility Transform for Combined Simple Dempster-Shafer Models (simDSM)
#'
#' @description
#' Performs the plausibility transform on the combination of multiple simple Dempster-Shafer models (simDSMs) defined over a large state-space model (SSM).
#'
#' @details
#' This function handles the case where several simple Dempster-Shafer models are combined on a very large SSM.  
#' Each row of the binary matrix \code{x} represents a focal element from one simDSM that is *not* the entire SSM.  
#' These focal elements can be specified directly or as complements, depending on which is more efficient to encode.  
#' A logical column matrix \code{y} identifies which rows of \code{x} need to be inverted (using the value \code{y0} as the check).  
#'
#' @param x A binary matrix where each row represents a focal element that is not the full SSM.
#' @param y A logical column matrix indicating which rows of \code{x} may need inversion.
#' @param a A numeric value (0 ≤ \code{a} ≤ 1) representing the mass assigned to each nontrivial focal element.
#' @param y0 A reference value (default = 0 or \code{FALSE}) used to determine which rows of \code{x} are inverted.
#' @param flip Logical; whether to perform row inversion where applicable (default = \code{TRUE}).
#'
#' @return A numeric plausibility vector \code{plau}.
#'
#' @author Peiyuan Zhu
#' @export
#'
#' @examples
#' # Example usage:
#' x <- matrix(c(1, 0, 1, 1, 0, 0), nrow = 2, byrow = TRUE)
#' y <- matrix(c(TRUE, FALSE))
#' PTsimDSM(x, y, a = 0.3)
#' @references \itemize{
#'   \item Shafer, G., (1976). A Mathematical Theory of Evidence. Princeton University Press, Princeton, New Jersey, p. 38: Basic probability assignment.
#'   \item Barnett, J. A. (1991). Calculating dempster-shafer plausibility. IEEE transactions on pattern analysis and machine intelligence, 13(6), 599-602.
#'   \item Cobb, B. R. and Shenoy, P.P. (2006). On the plausibility transformation method for translating belief function models to probability models. Journal of Approximate Reasoning, 41(3), April 2006, 314--330.
#' }
PTsimDSM <- function(x, y, a, y0 = 0, flip = TRUE) {
  stopifnot(is.matrix(x), is.numeric(a), a >= 0, a <= 1)
  
  if (flip) x[y == y0, ] <- 1 - x[y == y0, ]
  
  # Count how many simple DS models exclude each singleton
  exclude_counts <- colSums(x == 0)
  
  # Compute plausibility as (1 - a) ^ exclude_count
  unnormalized_plau <- (1 - a) ^ exclude_counts
  
  # Normalize plausibility vector
  plau <- unnormalized_plau / sum(unnormalized_plau)
  
  return(plau)
}
