#' Calculation of the degrees of Belief and Plausibility
#'
#'Degrees of Belief (Bel) and Plausibility (Pl) of the focal elements of a belief function are computed Then the ratio of the plausibility of a focal element against the plausibility of its contrary is computed. Focal elements with zero mass can be excluded from the calculations.\cr
#' @details The degree Belief Bel is defined by: \cr
#' \eqn{bel(A) = Sum{(m(B); B <= A)}}, for every subset A of the frame of discernment.
#' The plausibility function pl is defined by: \cr
#' \eqn{pl(A) = Sum{(m(B); B & A not empty}}, for every subset A of the frame of discernment.
#' The plausibility ratio of a focal element A versus its contrary ~A is defined by:  \eqn{Pl(A)/(1-Bel(A.))}.
#' @param x A belief function in its bca form (see \code{\link{bca}}).
#' @param remove = TRUE: Focal elements with 0 mass are excluded.
#' @return A matrix of M rows by 3 columns is returned, where M is the number of focal elements: \itemize{
#'  \item Column 1: the degree of belief Bel;
#'  \item Column 2: the degree of Plausibility Pl;
#'  \item Column 3: the Plausibility ratio
#'    }
#' @author Claude Boivin, Stat.ASSQ
#' @export
#' @references \itemize{
#' \item Shafer, G., (1976). A Mathematical Theory of Evidence. Princeton University Press, Princeton, New Jersey, p. 39-43.
#' \item Williams, P., (1990). An interpretation of Shenoy and Shafer's axioms for local computation. International Journal of Approximate Reasoning 4, pp. 225-232.
#' }
#' @examples 
#' x <- bca(f=matrix(c(0,1,1,1,1,0,1,1,1),nrow=3, byrow = TRUE), m=c(0.2,0.5, 0.3), cnames =c("a", "b", "c"), infovarnames = "x", n=1)
#' belplau(x)
#' y <- bca(f=matrix(c(1,0,0,1,1,1),nrow=2, byrow = TRUE), m=c(0.6, 0.4),  cnames = c("a", "b", "c"),  infovarnames = "y", n=1)
#' belplau(nzdsr(dsrwon(x,y)))
#' print("compare all elementary events")
#' xy1 <- addTobca(nzdsr(dsrwon(x,y)), matrix(c(0,1,0,0,0,1), nrow=2, byrow = TRUE))
#' belplau(xy1) 
belplau<-function (x, remove=FALSE) 
{
  # checking input data 
  if ( inherits(x, "bcaspec") == FALSE) {
    stop("Input argument not of class bcaspec.")
  }
  if (sum((apply(x$combination[,-1], 1, sum)) == 0) > 0) {
    stop("Invalid data: Empty set among the focal elements. Normalization necessary. See nzdsr function.")
  }
  MACC<-x$combination[,1] # vector of masses
  W2 <- rbind(x$combination[,-1])
  # to remove elements with mass=0, but the frame
  INUL<-c(MACC[-length(MACC)]>0,TRUE)
  if (remove == TRUE) {
    MACC1<-MACC[INUL]
#    W2a<-matrix(W2[INUL,],ncol=ncol(W2))
    W2a <- rbind(W2[INUL,])
  } else {
    MACC1<-MACC
    W2a<-W2
  }
 ## Indices for the calculation of the measure of belief
 IBEL<-dotprod(W2a,t(W2a),g="&",f="<=") 
  ## Calculation of Bel
  BEL<-apply(IBEL*MACC1,2,sum)
  ## Indices to calculate the measure of plausibility
  IPLAU<-dotprod(W2a,t(W2a),g="|",f="&")
  ## Calculation of Plau
  PLAU<-apply(IPLAU*MACC1,2,sum)
# Calculation of the plausibility ratio
  rplau<-PLAU/(1-BEL)
# Final results  
  resul<-cbind(BEL,PLAU, rplau)
  rownames(resul) <- nameRows(W2a)
  colnames(resul)<-c("Belief","Plausibility", "Plty Ratio")
  return(resul)
}

