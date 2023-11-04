#' Print summary statistics of large mass functions
#' 
#' @param x A basic chance assignment (see \code{\link{bca}}).
#' @param info_list="all" statistics to be printed in a vector of characters
#' \itemize{
#'   \item "all": everything
#'   \item "basic_subset_stat": basic statistics of subsets
#'   \item "comp_subset_stat": more comprehensive statistics of subsets
#'   \item "basic_mass_stat": basic statistics of masses
#'   \item "comp_mass_stat": more comprehensive statistics of masses
#'   \item "basic_joint_stat": basic statistics of masses vs subsets
#'   \item "comp_joint_stat": more comprehensive statistics of masses vs subsets
#' }
#' @param num_top_mass = 10 number of top masses to be printed
#' @param cut_width_size width of a cut among subset sizes
#' @param cut_width_m width of a cut among masses 
#' @return 
#' \itemize{
#'   \item table of basic and more comprehensive statistics of subsets
#'   \item table of basic and more comprehensive statistics of masses
#'   \item table of basic and more comprehensive statistics of masses vs subsets
#' }
#' @author Peiyuan Zhu
#' @export
#' @examples
#' library(tidyverse)
#' x <- bca(tt = matrix(c(1,1,0,1,1,1), nrow = 2, byrow = TRUE), m = c(0.8, 0.2), cnames = c(1,2,3))
#' bcaPrintLarge(x)

bcaPrintLarge <- function(x, info_list="all", num_top_mass=10, cut_width_size=10, cut_width_m=1e-5) {
  # Local variables: conf, labs, n, a, size, m, df, df_by_size, df_by_m
  conf <- x$con
  labs <- x$ssnames
  n <- min(length(x$ssnames), num_top_mass)
  size <- unlist(lapply(labs, length))
  m <- x$spec[, 2]
  df <- data.frame(m=m, size=size)
  df_by_size <- df %>% mutate(size_bins = cut_width(size, cut_width_size)) %>% group_by(size_bins)
  df_by_m <- df %>% mutate(m_bins = cut_width(m, cut_width_m)) %>% group_by(m_bins)
  
  if (info_list=="all" || "basic_subset_stat" %in% info_list) {
    # basic statistics of subsets
    print(paste("num subsets :", length(labs)))
  }
  
  if (info_list=="all" || "comp_subset_stat" %in% info_list) {
    # more comprehensive statistics of subsets
    print("subset size dist :")
    print(ftable(size))
  }
  
  if (info_list=="all" || "basic_mass_stat" %in% info_list) {
    # basic statistics of masses
    print(paste("conflict :", conf))
    print(paste("top", n, "largest masses :"))
    print(sort(m, decreasing = TRUE)[1:n])
    print(paste("top", n, "smallest masses :"))
    print(sort(m, decreasing = FALSE)[1:n])
  }
  
  if (info_list=="all" || "comp_mass_stat" %in% info_list) {
    # more comprehensive statistics of masses
    print("mass dist :")
    print(quantile(m))
  }
  
  if (info_list=="all" || "basic_joint_stat" %in% info_list) {
    # basic joint statistics of masses vs subset
    print("total mass over subsets of size :")
    print(df_by_size %>% summarise(p=sum(m)))
    
    print("subset num by mass size :")
    print(df_by_m %>% summarise(n=n()))
  }
  
  if (info_list=="all" || "comp_joint_stat" %in% info_list) {
    # more comprehensive joint statistics of masses vs subsets
    print("mass dist by subset size :")
    print(df_by_size %>% summarise(q=list(quantile(m))) %>% unnest_wider(q))
    
    print("subset dist by mass size :")
    print(df_by_m %>% summarise(q=list(quantile(size))) %>% unnest_wider(q))
  }
  
  # statistics of computation
  print(paste("memory size :", object.size(x),"bytes"))
}