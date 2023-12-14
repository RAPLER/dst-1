#' Plot belplau matrix
#' 
#' @param belplau_mat: belplau matrix e.g. belplau(bpa)
#' @param xlab: x-axis labels e.g. c("1:34","35:68","69:101")
#' @param color: color of xlab e.g. c(0,1,0)
#' @param y="rplau": column name of belplau matrix
#' @param levels=NULL: levels of color in order
#' @param main_title="": main title
#' @param legend_title="": title of legend
#' @param is_log_scale=TRUE: whether to use log-scale
#' @param is_negative=TRUE: whether to multiple by -1
#' @return a plot of belplau matrix
#' @author Peiyuan Zhu
#' @export
#' @examples
#' bpa <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
#' byrow = TRUE), m = c(0.2,0.5, 0.3), 
#' cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
#' bel_plau <- belplau(bpa)
#' belplauPlot(bel_plau)
belplauPlot<-function(belplau_mat, xlab, color, y="rplau", x="index", levels=NULL, legend_title="", main_title="",is_log_scale=TRUE,is_negative=FALSE) {
  ggplot(as.data.frame(cbind(belplau_mat,color)) %>% mutate(!!sym(y):=(if(is_log_scale) log(!!sym(y)) * (if(is_negative) -1 else 1) else !!sym(y))) %>% mutate(!!sym(x):=xlab) %>% mutate(color=factor(color, if (is.null(levels)) unique(color) else levels) )%>% arrange(desc(color))) +
    geom_point(aes(x=!!sym(x),y=!!sym(y),colour=color)) + 
    labs(title=main_title,color=legend_title) + theme_bw() + ylab(if (is_log_scale) paste0(if(is_negative) "-","log(",y,")") else y) 
}
