#'  Reduction of a relation
#' 
#' This function eliminates a chosen variable from a relation and calculate the bcaspec of the remaining variables.
#' @param rel the relation to modify
#' @param x the number of the variable to eliminate
#' @return The reducted relation
#' @author Claude Boivin, Stat.ASSQ
#' @export
#' 
elim <- function(rel, x) {
  varnb <- rel$infovar[,1]         # numbers of the variables 
  varRank <- match(x, varnb)       # rank of the variable
  dim_to_keep <- (1:length(varnb))*!varnb %in% x # axes to keep
  n <- nrow(rel$tt)
  m <- rel$spec[,2]                # extract mass vector
  size_vars <- rel$infovar[,2]    # vector of the sizes of axes
  itab <-array(t(rel$tt), c(size_vars, n)) # restructure data
  itab <- aperm(itab, c(2,1,(1+length(varnb))))
  fun3 <- function(x, oper) {reduction(x, f=oper)}
  proj <- t(apply(itab, c(dim_to_keep,(1+length(varnb))), FUN= fun3, oper= "|"))
z2=proj  # complete code see APL function ELIM
w1 <- doubles(z2*1) # mult by 1 transforms from logical to numeric
I12 <- dotprod(w1, t(z2), g = "&", f = "==")
m1<- t(array(m,c(ncol(I12), nrow(I12))))
m1 <- apply(I12*m1, 1, sum)
tt=w1
idnames <- names(rel$infovaluenames)[varRank]
colnames(tt) <- rel$infovaluenames[[idnames]]
# varnb <- rel$infovar[,1 ]
infovar <- subset(rel$infovar, (as.logical(dim_to_keep)))
infovaluenames <- subset(rel$infovaluenames, (as.logical(dim_to_keep)))
colnames(infovar) <- c("varnb", "size")
inforel <- matrix(c(rel$inforel[,1], rel$inforel[,2]-1), ncol = 2)
colnames(inforel) <- c("relnb", "depth")
 r <- bca(f = tt, m = m1, cnames = rownames(w1), con = rel$con, infovar = infovar, infovaluenames = infovaluenames, inforel = inforel)
return(r)
}
