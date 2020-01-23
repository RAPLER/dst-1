#' Simple printing of a bca (0,1)-matrix and mass function
#'
#' This utility function does a simple printing of a bca
#' 
#' @param x A list of class bcaspec.
#' @return A table of subsets with their mass. Subsets are identified by rowname.
#' @author Claude Boivin, Stat.ASSQ
#' @export
#' @examples
#' z <- bca(f=matrix(c(0,1,1,1,1,0,1,1,1),nrow=3, 
#' byrow = TRUE), m=c(0.2,0.5, 0.3), 
#' cnames =c("a", "b", "c"), varnb = 1)
#' bcaPrint(z)
bcaPrint <- function(x) {
# Test of input. x must be a bca specification
if ( inherits(x, "bcaspec") == FALSE) {
    stop("Input argument not of class bcaspec.")
}
y <- as.data.frame(cbind(rownames(x$tt), x$spec))
colnames(y)[1] <- deparse(substitute(x))
rownames(y) <- NULL
print(y)
}