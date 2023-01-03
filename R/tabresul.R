#' Prepare a table of results
#'
#' This utility function is a more detailed version of the \code{belplau} function. Different tables of measures of belief, plausibility and of the plausibility ratio can be obtained, namely by removing subsets with zero mass if present, or by asking for singletons only. Unlike function \code{belplau}, function \code{tabresul} does not reconstruct the row names from the column names. You can assign short rownames of your choice to the tt matrix of your resulting bca before calling function \code{tabresul}.
#' @aliases tabresul
#' @param x A basic chance assignment (bca)
#' @param removeZeroes = TRUE removes subsets with 0 mass.
#' @param singletonsOnly = TRUE reduces the table of results to elementary events (singletons).
#' @return A list of two elements: \itemize{
#'   \item mbp The table of focal elements with the addition of their associated mass, degree of belief, plausibility and the plausibility ratio.
#'   \item con The measure of conflict between subsets.
#'   }
#' @author Claude Boivin
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
  # Local variables: row_m_empty, macc, W2, INUL, macc1, W2a, BP, ztab, r, r1, mbp 
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
  ztab <- cbind(x$tt, x$spec[,2], BP)
  colnames(ztab)[1+ncol(x$tt)] <- "mass"
  # Order the subsets so the frame is in the last position of tt matrix
  # we don't want it removed if 0
  W2<-rbind(x$tt)
  sort_order<-order(apply(W2,1,sum))
  ztab=ztab[sort_order,]
  if (is.matrix(ztab) == FALSE) {
    ztab <- matrix(ztab,ncol = length(ztab), dimnames = list("frame", names(ztab)))
  }
  mbp <- ztab
  #
  # 3.2 Remove elements with mass = 0, but the frame 
  # (which is the last row of the table)
  # 
  if (removeZeroes == TRUE) {
    mbp = rbind(ztab[ztab[-nrow(ztab),(1+ncol(x$tt))] >0,], ztab[nrow(ztab),])
    rownames(mbp)[nrow(mbp)] <- "frame"
    }
  #
  # 3.3. Prepare a table of results reduced to the singletons
  if (singletonsOnly == TRUE) {
    r <- ztab
    if (nrow(r) > 1) {
      r1 <- rbind(r[apply(r[,c(1:(ncol(r)-4))],1,sum) == 1, , drop = FALSE], r[nrow(r),])
      rownames(r1)[nrow(r1)] <- "frame"
    } else {
      r1 <- ztab
      print("No singleton to print")
      }
    mbp <- r1
  }
  #
  # 4. Final result
  resul<-list(mbp = mbp, Conflict = x$con)
  return(resul)
}
 