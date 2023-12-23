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
#' @param is_factor=FALSE: whether to plot all x labels
#' @return a plot of belplau matrix
#' @author Peiyuan Zhu
#' @export
#' @examples
#' bpa <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
#' byrow = TRUE), m = c(0.2,0.5, 0.3), 
#' cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
#' bel_plau <- belplau(bpa)
#' belplauPlot(bel_plau)
belplauPlot <- function(belplau_mat,
                        xlab,
                        color,
                        y = "rplau", x = "index",
                        levels = NULL,
                        legend_title = "",
                        main_title = "",
                        is_log_scale = TRUE,
                        is_negative = FALSE,
                        is_factor=FALSE) {
  if (is.null(levels)) levels <- unique(color)

  dat <- as.data.frame(belplau_mat) %>%
    mutate(
      !!sym(x) := if(is_factor) factor(xlab, levels = xlab) else xlab,
      color = factor(color, levels = levels)
    )

  if (is_log_scale) {
    ylab <- paste0("log(", y, ")")
    .mult <- 1
    if (is_negative) {
      .mult <- -1
      ylab <- paste0("-", ylab)
    }
    dat <- dat %>%
      mutate(
        !!sym(y) := log(!!sym(y)) * .mult
        )
  } else {
    ylab <- y
  }

  dat$order <- reorder(dat$color, rep(1, nrow(dat)), FUN = length)
  
  dat <- dat[order(dat$order, decreasing = TRUE), ]
  
  ggplot(dat) +
    geom_point(
      aes(
        x = !!sym(x),
        y = !!sym(y),
        colour = color
      )
    ) +
    labs(
      title = main_title, color = legend_title,
    ) +
    ylab(ylab) +
    theme_bw()
}

