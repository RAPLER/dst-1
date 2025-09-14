#' Fast combination of multiple simple support functions
#' 
#' @details This function handles the case of combining several simple support functions defined on a very large frame of discernment (Fod). The simple support functions are arranged row by row in a binary array \code{x}. The number of rows of the array is equal to the number of support functions to be combined.The support functions can be defined as is or by their complement when it's faster to code. A column matrix \code{y} of TRUE/FALSE values is used to identify the rows of the array that need to be inverted.
#'  
#' @param x A binary matrix of simple support functions define on a same frame of discernment (Fod).
#' @param y A column matrix \code{y} of TRUE/FALSE values.
#' @param a The mass value allotted to each simple support function. All support functions have the same mass. 
#' @param y0 A value used to check which rows of the matrix \code{x} are to be inverted. Set at 0 (FALSE).
#' @param flip Parameter used when some rows of the table \code{x} need to be reversed. The default value is TRUE (check rows).
#' @param normalize Parameter used if plausibility values \code{pl} are wanted instead of probability distribution. The default value is TRUE (probability distribution of the singletons).
#' @return pl Plausibility vector (normalize = FALSE) or probability distribution (default)
#' @author Peiyuan Zhu
#' @export
#' @examples
#' 1
superBcaPlauSingleton <- function(x, y, a, y0=0, flip=TRUE, normalize = TRUE) {
  if(flip) x[y==y0,] <- 1 - x[y==y0,]
  stopifnot(is.matrix(x), is.numeric(a), a >= 0, a <= 1)
  
  # Count how many rows (SSF focal sets) *exclude* each singleton m
  exclude_counts <- colSums(x == 0)
  
  # Compute relative plausibility: (1 - a)^exclude_count
  pl <- (1 - a)^exclude_counts
  
  # Normalize if needed (optional)
  if(normalize) pl <- pl / sum(pl)
  
  return(pl)
}