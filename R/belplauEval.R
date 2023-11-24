#' Evaluate type I, II errors
#' 
#' @param bel_plau belplau object or a vector whose ordering is compared
#' @param true_order a vector representing the true ordering
#' @param var="rplau" variable name of the belplau to be used as ordering
#' @param err="type I" type of error to be evaluated
#' @param is_belplau=TRUE whether bel_plau is a belplau object
#' @return Type I, II error by comparing two orderings
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
#' z<-belplau(xy)
#' belplauEval(z,c(0,1,0))
belplauEval<-function(bel_plau,true_order,var="rplau",err="type I",is_belplau=TRUE) {
  # 1 means rplau > but actually <=
  # 0 means both > or both <
  # -1 means rplau < but actually >=
  if (is_belplau) observed_order<-bel_plau[,var] else observed_order<-bel_plau
  order_observed<-outer(bel_plau[,var],bel_plau[,var],">")
  order_true<-outer(true_order,true_order,">")
  validated<-order_observed-order_true
  if (err=="type I") {
    type_i<-validated[true_order>0,]
    type_i_v<-as.vector(type_i)
    type_i_err<-1-sum(type_i_v==0)/length(type_i_v)
    return(type_i_err)
  } else if (err=="type II") {
    type_ii<-validated[true_order==0,]
    type_ii_v<-as.vector(type_ii)
    type_ii_err<-1-sum(type_ii_v==0)/length(type_ii_v)
    return(type_ii_err)
  } else errorCondition("err can only be either type I or type II")
}