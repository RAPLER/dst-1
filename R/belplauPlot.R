#' Plot belplau matrix
#' 
#' @param belplau_mat: belplau matrix e.g. belplau(bpa)
#' @param xlab: x-axis labels e.g. c("1:34","35:68","69:101")
#' @param color: color of xlab e.g. c(0,1,0)
#' @param y="rplau": column name of belplau matrix
#' @param is_log_scale=TRUE: whether to use log-scale
#' @return a plot of belplau matrix
#' @author Peiyuan Zhu
#' @export
#' @examples
#' bpa <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
#' byrow = TRUE), m = c(0.2,0.5, 0.3), 
#' cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
#' bel_plau <- belplau(bpa)
#' belplauPlot(bel_plau)
belplauPlot<-function(belplau_mat, xlab, color, y="rplau", x="index", is_log_scale=TRUE) {
  ggplot(as.data.frame(belplau_mat) %>% mutate(!!sym(y):=(if(is_log_scale) log(!!sym(y)) else !!sym(y))) %>% mutate(!!x:=factor(xlab, xlab))) +
    geom_point(aes(x=!!sym(x),y=!!sym(y),colour=factor(color, unique(color)))) + 
    labs(color="") + theme_bw() + ylab(if (is_log_scale) paste0("log(",y,")") else y)
}
