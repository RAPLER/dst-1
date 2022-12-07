#' Prepare a table of results
#'
#' This utility function is a more detailed version of the \code{belplau} function. Different tables of measures of belief, plausibility and of the plausibility ratio can be obtained, namely by removing subsets with zero mass if present, or by asking for singletons only. Unlike function \code{belplau}, function \code{tabresul} does not reconstruct the row names. So you can assign simple rownames of your choice to the tt matrix of your resulting bca before calling function \code{tabresul}.
#' @aliases tabresul
#' @param x A basic chance assignment (bca)
#' @param removeZeroes = TRUE removes subsets with 0 mass.
#' @param singletonsOnly = TRUE reduces the table of results to elementary events (singletons).
#' @return A list of two elements: \itemize{
#'   \item mbp The table of focal elements with the addition of their associated mass, degree of belief, plausibility and the plausibility ratio.
#'   \item con The measure of conflict between subsets.
#'   }
#' @author Claude Boivin, Stat.ASSQ
#' @examples  
#' x <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3,
#' byrow = TRUE), m = c(0.2,0.5, 0.3), 
#' cnames = c("a", "b", "c"), 
#' varnames = "x", idvar = 1)
#' y <- bca(tt = matrix(c(1,0,0,1,1,1),nrow = 2, 
#' byrow = TRUE), m = c(0.6, 0.4),  
#' cnames = c("a", "b", "c"), varnames = "y", idvar = 1)
#' xy <- dsrwon(x,y)
#' xyNorm <- nzdsr(xy)
#' tabresul(xyNorm) 
#' ## print("Show all elementary events")
#' xy1 <- addTobca(nzdsr(dsrwon(x,y)), 
#' matrix(c(0,1,0,0,0,1), 
#' nrow = 2, byrow = TRUE))
#' tabresul(xy1)
#' ## print("Remove focal elements with 0 mass")
#' tabresul(xy1, removeZeroes = TRUE)
#' print("Retain singletons only")
#' tabresul(xy1, singletonsOnly = TRUE)
#' @export
#' 
tabresul <- function(x, singletonsOnly = FALSE, removeZeroes = FALSE) {  
  #
  # Local variables: row_m_empty, macc, W2, INUL, macc1, W2a, BP, mbp, 
  # Functions calls: belplau
  #
  # 1. check input data 
  #
  if ( inherits(x, "bcaspec") == FALSE) {
    stop("Input argument not of class bcaspec.")
  }
  #
  # check if m_empty present and if not 0
  if (sum((apply(x$tt, 1, sum)) == 0) > 0) {
    row_m_empty <- match(1:nrow(x$tt), rownames(x$tt) == "\u00f8")
    row_m_empty <- row_m_empty[1]
    if (!is.na(row_m_empty)) {
      if (x$spec[row_m_empty,2] > 0) {
        stop("Invalid data: Empty set among the focal elements. Normalization necessary. See nzdsr function.")
      }
    }
  }
  #
  # Processing
  # 3.1. Compute Bel and Pl functions and prepare final result
  #
  BP<-belplau(x)
  macc<-t(rbind(x$spec[,2]))
  W2<-rbind(x$tt)
  # 
  # 3.2 Order the subsets so the frame is in the last position of tt matrix
  # we don't want it removed if 0
  #
  sort_order<-order(apply(W2,1,sum))
  W2 <- W2[sort_order,]
  if (is.matrix(W2) == FALSE) {
     W2 <- matrix(W2,ncol = length(W2), dimnames = list(NULL, names(W2)))
  }
  #
  # 3.3 Put mass vector and belief measures in the same order as the W2 matrix
  # Mass vector
  macc <- macc[sort_order]
  macc <- matrix(macc,ncol = 1, dimnames = list(NULL, "mass"))
  #
  # Belief measures
  BP <- BP[sort_order,]
  if (is.matrix(BP) == FALSE) {
    BP <- matrix(BP,ncol = length(BP), dimnames = list(NULL, names(BP)))
  }
  #
  # 3.4. remove elements with mass = 0, but the frame
  INUL<-c(macc[-length(macc)]>0,TRUE)
  if (removeZeroes == TRUE) {
    macc1<-t(rbind(macc[INUL]))
    W2a <- rbind(W2[INUL,])
    BP <- rbind(BP[INUL,])
  } else {
    macc1<-macc
    W2a<-W2
  }
  colnames(macc1)<-"mass"
  mbp<-cbind(W2a,macc1,BP)
  #
  # 3.5. Prepare a table of results reduced to the singletons
  if (singletonsOnly == TRUE) {
    r <- mbp
    z2 <- r[,c(1:(ncol(r)-4))]
    if (!is.null(dim(z2))) {
      r1 <- r[apply(z2,1,sum) == 1, , drop = FALSE]
    } else {
      z2 <- rbind(r[,c(1:(ncol(x$tt)))])
      r1 <- r[apply(z2,1,sum) == 1,]
      if (is.null(dim(r1))) {
        r1 <- t(as.matrix(r1))
      }
    }
    mbp <- r1
  }
  #
  # 4. Final result
  resul<-list(mbp = mbp, Conflict = x$con)
  return(resul)
}
 