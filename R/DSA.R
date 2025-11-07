#' Calculate the degrees of unambiguous support (p), unambiguous contradiction (q), and ambiguity (r) of a list of hypothesis based on a Dempster-Shafer Model (DSM).
#'
#' Degrees of unambiguous support \code{p} and unambiguous contradiction \code{q} of the focal sets of a DSM are computed. The ratio of the plausibility of a focal set against the plausibility of its contrary is also computed. Subsets with zero mass can be excluded from the calculations.\cr
#' @details The degree of unambiguous support (belief) \code{p} is defined by: \cr
#' \deqn{p(A) = Sum((m(B); B \subseteq A))}{p(A) = Sum((m(B); B <= A))} for every subset B of A.\cr
#' The degree of non-contradiction (plausibility) \code{plau} is defined by: \cr
#' \deqn{plau(A) = Sum[(m(B); B \cap A \neq \emptyset]}{pl(A) = Sum[(m(B); (B & A) not empty]} for every subset \code{B} of the state-space model. \cr
#' The plausibility ratio of a focal set \code{A} versus its contrary \code{not A} is defined by:  \eqn{(1-q(A))/(1-p(A))}.
#' @aliases belplau
#' @param x A DSM (see \code{\link{DSM}}).
#' @param remove = TRUE: Exclude subsets with zero mass.
#' @param h = NULL: Hypothesis to be tested. Description matrix in the same format than \code{x$tt}
#' @return A matrix of \code{M} rows by 3 columns is returned, where \code{M} is the number of focal sets: \itemize{
#'  \item Column 1: the degree of unambiguous support (Belief) \code{p};
#'  \item Column 2: the degree of unambiguous contradiction (Disbelief) \code{q};
#'  \item Column 3: the degree of ambiguity (Epistemic uncertainty) \code{r};
#'  \item Column 4: the degree of non-contradiction (Plausibility) \code{plau};
#'  \item Column 5: the Plausibility ratio \code{rplau}.
#'    }
#' @author Claude Boivin, Peiyuan Zhu
#' @export
#' @references \itemize{
#'   \item Shafer, G., (1976). A Mathematical Theory of Evidence. Princeton University Press, Princeton, New Jersey, p. 39-43.
#'   \item Williams, P., (1990). An interpretation of Shenoy and Shafer's axioms for local computation. International Journal of Approximate Reasoning 4, pp. 225-232.
#'   \item Dempster, A., (2008). The Dempster–Shafer calculus for statisticians. International Journal of approximate reasoning, 48(2), 365-377.
#' }
#' @examples 
#' x <- DSM(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
#' byrow = TRUE), m = c(0.2,0.5, 0.3), 
#' cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
#' belplau(x)
#' y <- DSM(tt = matrix(c(1,0,0,1,1,1),nrow = 2, 
#' byrow = TRUE), m = c(0.6, 0.4),  
#' cnames = c("a", "b", "c"),  varnames = "y", idvar = 1)
#' xy <- nzdsr(dsrwon(x,y))
#' belplau(xy)
#' print("compare all elementary events")
#' xy1 <- addToDSM(x = xy, tt = matrix(c(0,1,0,0,0,1), nrow = 2, byrow = TRUE))
#' belplau(xy1) 
#' belplau(xy1, remove = TRUE) 
#' belplau(xy1, h = matrix(c(1,0,0,0,1,1), nrow = 2, byrow = TRUE))
#' 
DSA <-function (x, remove = FALSE, h = NULL) {
  #
  # Local variables:  xtest, row_m_empty, MACC, W2, INUL, MACC, W2)
  # Functions calls: DSAh, nameRows, ttmatrix
  #
  .Deprecated("DSA", msg = "DSA is the new function name for the belplau function.", old = "belplau")
  # 1. Checking input data 
  #
  if ( inherits(x, "DSMspec") == FALSE) {
    stop("Input argument not of class DSMspec.")
  }
  # computation using description matrix tt
  #
  # use ssnames to reconstruct tt if null
  #
  if (is.null(x$spec)) {
    stop("Missing spec")
  } 
  if (is.null(x$tt) ) {
    if (is.null(x$ssnames) == FALSE) {
      z <- x$ssnames
      x$tt <- ttmatrix(z)
    }
    else {
      stop("No description matrix and no subsets names found. ")
    }
  }
  #
  # check if only one row, then convert to matrix
  xtest <- x$tt
  if (shape(shape(xtest)) < 2 ) {
    xtest <- matrix(xtest, nrow = 1)
  }
  #
  # check if m_empty present and if not 0
  if (sum((apply(xtest, 1, sum)) == 0) > 0) {
    row_m_empty <- match(1:nrow(xtest), apply(xtest, 1, sum) == 0)
    row_m_empty <- row_m_empty[1]
    if (!is.na(row_m_empty)) {
      if (x$spec[row_m_empty,2] > 0.000001) {
    stop("Invalid data: Empty set among the focal sets. Normalization necessary. Apply function normalize to your DSM.")
      }
    }
  }
  #
  # End checks and preparation
  #
  # 2. Prepare data for calculations of bel and pl functions
  #
  # vector of masses and description matrix tt needed for these calculations
  #
  MACC<-x$spec[,2]  # vector of masses
  W2 <- rbind(x$tt) # description matrix
  #
  # Case where user do not want subsets with zero mass (remove = TRUE)
  # Remove subsets with zero mass, but the SSM
  #
  INUL<-c(MACC[-length(MACC)]>0,TRUE)
  if (remove != FALSE) {
    MACC<-MACC[INUL]
    W2 <- rbind(W2[INUL,])
  } 
  #
  # 3.1 Check if there's hypothesis to be tested
  if (!is.null(h)) {
    # check that h is like x$tt
    if ((is.matrix(h) == FALSE) ) {
      stop("h parameter must be a (0,1) or logical matrix.")
    }
    if (ncol(h) != ncol(x$tt)) {
      stop("Error in input arguments: number of columns of h not equal to ncol(x$tt)") 
    }
    if (is.null(colnames(h)) ) {
      colnames(h) <- colnames(x$tt)
      rownames(h) <- nameRows(h)
    }  
    resul <- DSAh(MACC,W2,h) 
  } else {
    resul <- DSAh(MACC, W2, h = W2) 
  }
  return(resul)
}
#' @rdname DSA
#' @export
belplau <- DSA
