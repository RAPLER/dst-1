#' Calculation of the degrees of Belief and Plausibility of a basic chance assignment (bca).
#'
#'Degrees of Belief \code{Bel} and Plausibility \code{Pl} of the focal elements of a bca are computed. The ratio of the plausibility of a focal element against the plausibility of its contrary is also computed. Subsets with zero mass can be excluded from the calculations.\cr
#' @details The degree of belief \code{Bel} is defined by: \cr
#' \deqn{bel(A) = Sum((m(B); B \subseteq A))}{bel(A) = Sum((m(B); B <= A))} for every subset B of A.\cr
#' The degree of plausibility \code{pl} is defined by: \cr
#' \deqn{pl(A) = Sum[(m(B); B \cap A \neg \o]}{pl(A) = Sum[(m(B); B and A not empty]} for every subset \code{B} of the frame of discernment. \cr
#' The plausibility ratio of a focal element \code{A} versus its contrary \code{not A} is defined by:  \eqn{Pl(A)/(1-Bel(A))}.
#' @param x A basic chance assignment mass function (see \code{\link{bca}}).
#' @param remove = TRUE: Exclude subsets with zero mass.
#' @return A matrix of \code{M} rows by 3 columns is returned, where \code{M} is the number of focal elements: \itemize{
#'  \item Column 1: the degree of belief \code{Bel};
#'  \item Column 2: the degree of Plausibility \code{Pl};
#'  \item Column 3: the Plausibility ratio
#'    }
#' @author Claude Boivin, Stat.ASSQ
#' @export
#' @references \itemize{
#' \item Shafer, G., (1976). A Mathematical Theory of Evidence. Princeton University Press, Princeton, New Jersey, p. 39-43.
#' \item Williams, P., (1990). An interpretation of Shenoy and Shafer's axioms for local computation. International Journal of Approximate Reasoning 4, pp. 225-232.
#' }
#' @examples 
#' x <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow=3, 
#' byrow = TRUE), m=c(0.2,0.5, 0.3), 
#' cnames =c("a", "b", "c"), varnames = "x", varnb = 1)
#' belplau(x)
#' y <- bca(tt = matrix(c(1,0,0,1,1,1),nrow=2, 
#' byrow = TRUE), m=c(0.6, 0.4),  
#' cnames = c("a", "b", "c"),  varnames = "y", varnb = 1)
#' xy <- nzdsr(dsrwon(x,y))
#' belplau(xy)
#' print("compare all elementary events")
#' xy1 <- addTobca(xy, matrix(c(0,1,0,0,0,1), nrow=2, byrow = TRUE))
#' belplau(xy1) 
#' 
belplau<-function (x, remove=FALSE) {
  #
  # Local variables:  xtest, row_m_empty, MACC, W2, INUL, MACC1, W2a, IBEL, BEL, IPLAU, PLAU, rplau
  # Functions calls: dotprod
  #
  # 1. Checking input data 
  #
  if ( inherits(x, "bcaspec") == FALSE) {
    stop("Input argument not of class bcaspec.")
  }
  # check if matrix of only one row
  xtest <- x$tt
  if (is.matrix(xtest) == FALSE) { 
    xtest <- t(as.matrix(xtest)) 
  }
  # check if m_empty present and if not 0
  if (sum((apply(xtest, 1, sum)) == 0) > 0) {
    row_m_empty <- match(1:nrow(xtest), rownames(xtest) == "\u00f8")
    row_m_empty <- row_m_empty[1]
    if (!is.na(row_m_empty)) {
      if (x$spec[row_m_empty,2] > 0) {
    stop("Invalid data: Empty set among the focal elements. Normalization necessary. See nzdsr function.")
      }
    }
  }
  #
  # 2. Prepare data for calculations of bel and pl functions
  #
  MACC<-x$spec[,2] # vector of masses
  W2 <- rbind(x$tt)
  # Remove subsets with zero mass, but the frame
  INUL<-c(MACC[-length(MACC)]>0,TRUE)
  if (remove == TRUE) {
    MACC1<-MACC[INUL]
#    W2a<-matrix(W2[INUL,],ncol=ncol(W2))
    W2a <- rbind(W2[INUL,])
  } else {
    MACC1<-MACC
    W2a<-W2
  }
  #
  # 3. Indices for the calculation of the measure of belief
  #
  IBEL<-dotprod(W2a,t(W2a),g="&",f="<=") 
  ## Calculation of Bel
  BEL<-apply(IBEL*MACC1,2,sum)
  #
  # 4. Indices to calculate the measure of plausibility
  #
  IPLAU<-dotprod(W2a,t(W2a),g="|",f="&")
  ## Calculation of Plau
  PLAU<-apply(IPLAU*MACC1,2,sum)
  #
  # 4. Calculation of the plausibility ratio
  rplau<-PLAU/(1-BEL)
  #
  # 5. Final results  
  #
  resul<-cbind(BEL,PLAU, rplau)
  rownames(resul) <- nameRows(W2a)
  colnames(resul)<-c("Belief","Plausibility", "Plty Ratio")
  return(resul)
}

