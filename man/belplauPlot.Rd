% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/belplauPlot.R
\name{belplauPlot}
\alias{belplauPlot}
\title{Plot belplau matrix}
\usage{
belplauPlot(
  belplau_mat,
  xlab,
  color,
  y = "rplau",
  x = "index",
  levels = NULL,
  legend_title = "",
  main_title = "",
  is_log_scale = TRUE,
  is_negative = FALSE,
  is_factor = FALSE
)
}
\arguments{
\item{belplau_mat}{Belplau matrix e.g. belplau(bpa) or a numerical vector quantifying order of importance of the elements of the frame}

\item{xlab}{X-axis labels e.g. c("1:34","35:68","69:101")}

\item{color}{Color of xlab e.g. c(0,1,0)}

\item{y}{= "rplau": column name of belplau matrix. Ignore if it's not belplau matrix.}

\item{x}{= "index": x-axis name}

\item{levels}{= NULL: levels of color in order}

\item{legend_title}{= "": title of legend}

\item{main_title}{= "": main title}

\item{is_log_scale}{= TRUE Whether to use log-scale}

\item{is_negative}{= TRUE Whether to multiple by -1}

\item{is_factor}{= FALSE Whether to plot all x labels}
}
\value{
a plot of a column of the belplau matrix or a numerical vector quantifying order of importance of the elements of the frame
}
\description{
Plot belplau matrix
}
\examples{
bpa <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
byrow = TRUE), m = c(0.2,0.5, 0.3), 
cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
bel_plau <- belplau(bpa)
belplauPlot(bel_plau, c("a","b","c"), c(1,3,2))
}
\author{
Peiyuan Zhu
}
