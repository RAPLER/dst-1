#' Combination of two mass functions
#' 
#'The unnormalized Dempster's rule is used to combine two mass functions \code{mx} and \code{my} defined  on the same frame of discernment and described by their respective basic chance assignments \code{x}  and \code{y}. Dempster's rule of combination is applied. The normalization is not done, leaving the choice  to the user to normalize the results or not (for the normalization operation, see function \code{\link{nzdsr}}).
#' @details The two bca's \code{x} and \code{y} must be defined on the same frame of discernment for the combination to take place. The relation number of the x input is given to the output result.  
#' @param x A basic chance assignment (see \code{\link{bca}}).
#' @param y A  A basic chance assignment (see \code{\link{bca}}).
#' @param varnames A character string to name the resulting variable. named "z" if omitted.
#' @param infovarnames Deprecated. Old name for \code{varnames}.
#' @param relnb Identification number of the relation. Can be omitted.
#' @return A basic chance assignment with these two components added: \itemize{
#'   \item I12 Intersection table of subsets.
#'   \item Sort_order Sort order of subsets.
#'   }
#' @author Claude Boivin, Stat.ASSQ
#' @export
#' @examples 
#' x1 <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
#' byrow = TRUE), m = c(0.2,0.5, 0.3), 
#' cnames = c("a", "b", "c"),  
#' varnames = "x", varnb=1)
#' x2 <- bca(tt = matrix(c(1,0,0,1,1,1),nrow = 2, 
#' byrow = TRUE), m = c(0.6, 0.4),  
#' cnames = c("a", "b", "c"),  
#' varnames = "x", varnb = 1)
#' dsrwon(x1,x2)
#' frame <- bca(matrix(c(1,1,1), nrow = 1), m = 1, cnames = c("a","b","c"))
#' dsrwon(frame, frame)
#' @references Shafer, G., (1976). A Mathematical Theory of Evidence. Princeton University Press, Princeton, New Jersey, pp. 57-61: Dempster's rule of combination.
dsrwon<-function(x,y, varnames = NULL, relnb = NULL, infovarnames) {
  #
  # Local variables: x1, y1, z, values1, values2, V12, N12, W1, I12, MAC, nMAC
  # Functions calls: doubles, nameRows, dotprod
  # test
  # 2022-12-15 call to doubles removed
  #
  # 0. Catch old parameters names, if any and replace by the new ones
  #
  calls <- names(sapply(match.call(), deparse))[-1]
  # catch old parameter infovarnames and replace by varnames if used instead of varnames
  if(any("infovarnames" %in% calls) & missing(varnames)) {
    warning("Parameter name 'infovarnames' is deprecated. Use 'varnames' instead.")
    varnames <- infovarnames
  }
  # end catches
  #
  ## 1. Checks
  # class bcaspec
  #
  if ( (inherits(x, "bcaspec") == FALSE) | (inherits(y, "bcaspec") == FALSE)) {
    stop("One or more inputs not of class bcaspec.")
  }
  #
  # x1 and x2 must have same frame of discernment
  # and same number of elements
  #
  x1<-rbind(x$tt)  # (M x K) matrix
  y1<-rbind(y$tt)  # (N x K) matrix
  if (ncol(x1) != ncol(y1)) {
    stop("Nb of elements of frame x and frame y not equal.") 
  }
  #
  ## x1 and x2 must have the same value names put in the same order
  #
  values1 <- unlist(x$valuenames)
  values2 <- unlist(y$valuenames)
  nbval <- sum(values1 == values2)
  if ((length(values1) != length(values2)) | (nbval != length(values1))) {
    stop("Value names of the two frames differ. Check value names of variables as well as their position.")
  }
  #
  ## 2. Calculations
  #
  ## 2.1 Combine masses
  V12<-outer(x$spec[,2],y$spec[,2], "*")     # compute masses
  #
  ## 2.2 combine subsets
  # transform table of intersections: (M x N) rows by K 
  #
  N12<-inters(x1,y1)         # intersection of the subsets
  N12<-aperm(N12,c(2,1,3))   # transformation
  N12<-array(c(N12),c(dim(N12)[1],prod(dim(N12)[-1])), dimnames= list(colnames(x1), as.vector(outer(rownames(x1), rownames(y1) , FUN="paste"))))
  N12<-aperm(N12,c(2,1)) 
  #
  # Remove duplicates from the table
  #
  # test 2022-12-15
  # W1<- doubles(N12)      ## remove duplicates 
  W1<- N12[!duplicated(N12),]  ## remplace fonction "doubles"
  rownames(W1) <- nameRows(W1)
  #
  ## 2.3 Identify contributions to each subset and compute mass
  #
  I12<-dotprod(W1,aperm(N12,c(2,1)),g="&",f="==")    
  MAC<-apply(I12*t(array(V12,dim(t(I12)))),1,sum)     
  #
  ## 2.4  Order the subsets to find if the empty subset is there. Put empty set in first position of tt matrix
  # 
  sort_order<-order(apply(W1,1,sum))
  tt <- W1[sort_order,]
  if (is.matrix(tt) == FALSE) {
    tt <- matrix(tt,ncol = length(tt), dimnames = list(NULL, names(tt)))
  }
  #
  ## 2.5 Identify if the empty set is present and define m_empty accordingly with it mass
  # Put masses in the same order as the tt matrix
  #
  z<- sum(W1[sort_order[1],])
   if (z==0) {
     empty<-sort_order[1]  
     m_empty<-MAC[empty] 
   } else {
     empty<-0
     m_empty<-0
   }
  # 
   MAC <- MAC[sort_order]
  # m_empty <- MAC[1]
   mMAC <-matrix(MAC,ncol=1, dimnames =list(NULL, "mass"))
   #
   ## 2.6 define spec matrix
   #
  spec <- cbind((1:nrow(tt)), mMAC)
  colnames(spec) <- c("specnb", "mass")
  #
  #
  ## 2.7  Revised 2021-04-23
  con12<-1-(1-x$con)*(1-y$con) # conflicting evidence inputted
  if  ((con12 == 1) | (m_empty == 1)) { 
    warning('Totally conflicting evidence (con = 1). Data is inconsistent.')}
   # "con" stays unchanged by a vacuous combination
 if (nrow(x$tt)==1 |nrow(y$tt) == 1) {
   con <- con12 
   } else {
   con<-1-(1-con12)*(1-m_empty) # OK checked
   }
  # End Revision 2021-04-23
  #
  ## 3. The result
  #
  ## 3.1 Name the resulting variables and fix parameters
  #
  # varnames and valuenames
 valuenames <- x$valuenames
  if (!is.null(varnames)) {
    names(valuenames) <- varnames
  }
  if (missing(varnames) | is.null(varnames)) {
    varnames <- names(valuenames)
  } 
  #
  infovar <- x$infovar
  #
  # inforel parameter
  if (missing(relnb) | is.null(relnb)) { 
    inforel <- x$inforel
    } else {
    depth <- x$inforel[,2]
    inforel <- matrix(c(relnb, depth), ncol = 2)
    colnames(inforel) <- c("relnb", "depth")
    }
  #
  # construction of the result
  #
  z <- list(con = con, tt=tt, spec = spec, infovar = infovar, varnames = varnames, valuenames = valuenames, inforel = inforel, I12=I12, sort_order=sort_order)
  class(z) <- append(class(z), "bcaspec")
  return(z)
  }
 