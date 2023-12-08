#' Computer norm between two basic chance assignment objects
#' 
#' @param x A bca to evaluate norm.
#' @param y A bca to evaluate norm.
#' @param p exponent parameter of the norm
#' @return a number of norm evaluation
#' @author Peiyuan Zhu
#' @export
#' @examples 
#'y1 <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
#'byrow = TRUE), m = c(0.2,0.5, 0.3),
#'cnames = c("a", "b", "c"),
#'varnames = "x", idvar = 1)
#'y2 <- bca(tt = matrix(c(1,0,0,1,1,1),nrow = 2, 
#'byrow = TRUE), m = c(0.6, 0.4),  
#'cnames = c("a", "b", "c"),  
#'varnames = "x", idvar = 1)
#'y1y2<-dsrwon(y1,y2)
#'bcaNorm(y1y2,y1)
bcaNorm <-function(x, y ,p=1) {
  xm<-x$spec[,2]
  ym<-y$spec[,2]
  xs<-unlist(lapply(x$ssnames, function(xx) Reduce("paste",xx)))
  ys<-unlist(lapply(y$ssnames, function(yy) Reduce("paste",yy)))
  ws<-union(xs,ys)
  zs<-intersect(xs,ys)
  xxs<-setdiff(xs,zs)
  yys<-setdiff(ys,zs)
  Lp=(sum(unlist(lapply(1:length(ws), 
                        function(W) if(ws[W] %in% zs) abs(xm[xs==ws[W]]-ym[ys==ws[W]])**p else 
                          if (ws[W] %in% xxs) abs(xm[xs==ws[W]])**p else 
                            if (ws[W] %in% yys) abs(ym[ys==ws[W]])**p else 0))))**(1/p)
  return(Lp)
}