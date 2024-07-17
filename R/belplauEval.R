#' Evaluate type I, II errors
#' 
#' Calculate error of the first kind, error of the second kind, and total error by comparing two vectors. One vector represents the truth and the other represents a numerical quantity of importance. The definition of the three types of errors is as follows:
#' \itemize{
#'  \item Error of the first kind: out of all the comparisons between relevant and irrelevant elements, what proportion of errors are due to indicating an irrelevant element as more important than a relevant element
#'  \item Error of the second kind: out of all the comparisons between relevant and irrelevant elements, what proportion of errors are due to indicating an relevant element as less important than an irrelevant element
#'  \item Total error: the sum of error of the first kind and the error of the second kind
#' }
#' 
#' @param belplau_mat belplau matrix e.g. belplau(bca) or a numerical vector quantifying order of importance of the elements of the frame.
#' @param true_order a binary vector representing the truth. 1 means relevant and 0 means not relevant.
#' @param var = "rplau" column name of the belplau matrix to be used as ordering.
#' @param err = "first kind" kind of error to be evaluated. Can also take value "second kind" or "total".
#' @param is_belplau = TRUE whether bel_plau is indeed a belplau matrix or just a numerical vector quantifying order of importance of elements.
#' @return A number of first, second kind, or total error.
#' @author Peiyuan Zhu
#' @export
#' @examples 
#' x <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
#' byrow = TRUE), m = c(0.2,0.5, 0.3), 
#' cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
#' belplau(x)
#' y <- bca(tt = matrix(c(1,0,0,1,1,1),nrow = 2, 
#' byrow = TRUE), m = c(0.6, 0.4),  
#' cnames = c("a", "b", "c"),  varnames = "y", idvar = 1)
#' xy <- nzdsr(dsrwon(x,y))
#' z<-belplau(xy,h=ttmatrixPartition(xy$infovar[2],xy$infovar[2]))
#' belplauEval(z,c(0,1,0))
belplauEval<-function(belplau_mat,true_order,var="rplau",err="first kind",is_belplau=TRUE) {
  if (is_belplau) observed_order<-belplau_mat[,var] else observed_order<-belplau_mat
  order_observed <- outer(observed_order,observed_order,">")
  order_true <- outer(true_order,true_order,">")
  validated <- order_observed-order_true
  validated <- validated[true_order>0,true_order==0]
  # In this matrix:
  # 1 means rplau > but actually <=
  # 0 means both > or both <
  # -1 means rplau < but actually >=
  err_i <- sum(validated==-1) / length(validated)
  err_ii <- sum(validated==1) / length(validated)
  total_err <- err_i + err_ii
  if (err=="first kind") {
    return(err_i)
  } else if (err=="second kind") {
    return(err_ii)
  } else if (err=="total") {
    return(total_err)
  } else stop("err can only be either first kind, second kind, or total")
}