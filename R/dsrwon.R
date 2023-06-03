#' Combination of two mass functions
#' 
#'The unnormalized Dempster's rule is used to combine two mass functions \code{mx} and \code{my} defined  on the same frame of discernment and described by their respective basic chance assignments \code{x}  and \code{y}. Dempster's rule of combination is applied. The normalization is not done, leaving the choice  to the user to normalize the results or not (for the normalization operation, see function \code{\link{nzdsr}}).
#'
#'The calculations make use of multiple cores available.
#' @details The two bca's \code{x} and \code{y} must be defined on the same frame of discernment for the combination to take place. The relation number of the x input is given to the output result.  
#' @param x A basic chance assignment (see \code{\link{bca}}).
#' @param y A basic chance assignment (see \code{\link{bca}}).
#' @param mcores Make use of multiple cores ("yes") or not ("no"). Default = "no".
#' @param varnames A character string to name the resulting variable. named "z" if omitted.
#' @param infovarnames Deprecated. Old name for \code{varnames}.
#' @param relnb Identification number of the relation. Can be omitted.
#' @return A basic chance assignment with these two components added: \itemize{
#'   \item I12 Intersection table of subsets.
#'   \item Sort_order Sort order of subsets.
#'   }
#' @author Claude Boivin
#' @export
#' @examples 
#' y1 <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
#' byrow = TRUE), m = c(0.2,0.5, 0.3), 
#' cnames = c("a", "b", "c"),  
#' varnames = "x", idvar = 1) 
#' y2 <- bca(tt = matrix(c(1,0,0,1,1,1),nrow = 2, 
#' byrow = TRUE), m = c(0.6, 0.4),  
#' cnames = c("a", "b", "c"),  
#' varnames = "x", idvar = 1)
#' dsrwon(y1,y2)
#' x1 <- bca(m = c(0.2,0.5, 0.3), 
#' varnames = "x", idvar = 1, 
#' ssnames = list(c("b", "c"), c("a", "b"), c("a", "b", "c")), sfod = 3)
#' x2 <- bca(m = c(0.6, 0.4),  
#' varnames = "x", idvar = 1, 
#' ssnames = list(c("a"), c("a", "b", "c")), sfod = 3)
#' dsrwon(x1,x2)
#' vacuous <- bca(matrix(c(1,1,1), nrow = 1), m = 1, cnames = c("a","b","c"))
#' dsrwon(vacuous, vacuous)
#' vacuous <- bca(m = 1,  
#' varnames = "x", idvar = 1, 
#' ssnames = list(c("a", "b", "c")), sfod = 3)
#' @references Shafer, G., (1976). A Mathematical Theory of Evidence. Princeton University Press, Princeton, New Jersey, pp. 57-61: Dempster's rule of combination.
dsrwon<-function(x, y, mcores = "no", varnames = NULL, relnb = NULL, infovarnames) {
  #
  # Local variables: m1,, m2, zx, zy, x1, y1, z, values1, values2, V12, N12, W1, I12, MAC, nMAC
  # Functions calls: nameRows, dotprod
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
  if   ( nrow(x$infovar) != nrow(y$infovar) ) {
    stop("Specification of parameter infovar of the two bca's must identical.")
  }
  #
  # Put the bca with the largegst tt mattrix in second
  if (!is.null(x$tt)) {
  if (nrow(x$tt) <= nrow(y$tt) ) {
    zx <- x
    zy <-y
  } else {
    zx <- y
    zy <- x
  }
  # 
  # x1 and y1 must have same frame of discernment
  # and same number of elements
  #
  x1<-rbind(zx$tt)  # (M x K) matrix
  y1<-rbind(zy$tt)  # (N x K) matrix
  if (ncol(x1) != ncol(y1)) {
    stop("Nb of elements of frame x and frame y not equal.") 
  }
  #
  ## x1 and x2 must have the same value names put in the same order
  #
  values1 <- unlist(zx$valuenames)
  values2 <- unlist(zy$valuenames)
  nbval <- sum(values1 == values2)
  if ((length(values1) != length(values2)) | (nbval != length(values1))) {
    stop("Value names of the two frames differ. Check value names of variables as well as their position.")
  }
  ## 2a. Calculations
  #
  # Check mass vector
  m1 <- zx$spec[,2]
  m2 <- zy$spec[,2]
  if ( (abs(sum(m1)-1)>0.000001) | (abs(sum(m2)-1)>0.000001)) {
    print(m1)
    print(m2)
    stop("Invalid data, sum of masses of one vector, or both, greater than one.")
  }
  ## 2a.1 Combine mass vectors
  V12<-outer(zx$spec[,2],zy$spec[,2], "*")      # compute masses OK, not long.
  } else {
  if (length(x$ssnames) <= length(y$ssnames) ) {
      zx <- x
      zy <-y
    } else {
      zx <- y
      zy <- x
    }
  # NB refaire ici les tests de L74-L90 avec ssnames
  #
  ## 2b. Calculations
  #
  # Check mass vector
    m1 <- zx$spec[,2]
    m2 <- zy$spec[,2]
    if ( (abs(sum(m1)-1)>0.000001) | (abs(sum(m2)-1)>0.000001)) {
      print(m1)
      print(m2)
      stop("Invalid data, sum of masses of one vector, or both, greater than one.")
    }
  ## 2b.1 Combine mass vectors

  V12<-t(outer(zx$spec[,2],zy$spec[,2], "*") )     # compute masses OK, not long.
  } 
  #
  ## 2.2 combine subsets
  # transform table of intersections: (M x N) rows by K 
  # 
  if (is.null(zx$ssnames) | is.null(zy$ssnames) ) {
  # Use multiple cores = "yes"
  if (mcores == "yes") {
  y1_df <- as.data.frame(t(y1))
  if  (requireNamespace("parallel", quietly = TRUE) ) {
    library(parallel) 
    ncores <- detectCores(logical = FALSE)
    grappe <- makeCluster(ncores-1)
    clusterEvalQ(cl = grappe, expr = library(dst))
    clusterExport(cl = grappe, varlist = list("x1", "y1_df"), envir = environment() )
    mx1y1_par <- parSapply( cl = grappe, X=1:ncol(y1_df), FUN = function(X) { inters(x1, t(y1_df[X])) }, simplify = FALSE, USE.NAMES = TRUE ) # intersection of the subsets
    stopCluster(grappe)
  }
  N12 <- array(unlist(mx1y1_par), dim = c(shape(mx1y1_par[[1]])[1:2], shape(mx1y1_par)), dimnames = list(unlist(dimnames(mx1y1_par[[1]])[1]), colnames(x1), rownames(y1)) )
  } else {
    N12<-inters(x1, y1)         # intersection of the subsets
    }
  #
  N12<-aperm(N12,c(2,1,3))   # transformation
  N12<-array(c(N12),c(dim(N12)[1],prod(dim(N12)[-1])), dimnames= list(colnames(x1), NULL) )
  N12<-aperm(N12,c(2,1)) 
  rownames(N12) <- nameRows(N12)
  #
  # Remove duplicates from the table
  W1<- N12[!duplicated(N12),]  ## remove duplicates 
  if (is.matrix(W1) == FALSE) {
    W1 <- t(as.matrix(W1))
  }
  #
  ## 2.3 Identify contributions to each subset and compute mass
  #
  if (mcores == "yes") {
  if  (requireNamespace("parallel", quietly = TRUE) ) {
    z2_df <- as.data.frame(aperm(N12,c(2,1)))
    ncores <- detectCores(logical = FALSE)
    grappe <- makeCluster(ncores-1)
    clusterEvalQ(cl = grappe, expr = library(dst))
    clusterExport(cl = grappe, varlist = list("W1", "z2_df"), envir = environment() )
  #
  ## 2.3 Identify contributions to each subset and compute mass
  #
   I12_par <- parSapply( cl = grappe, X=1:ncol(z2_df), FUN = function(X) {  dotprod  (W1, as.matrix(z2_df[,X]), g = "&",f = "==")  }, simplify = FALSE, USE.NAMES = TRUE ) 
   stopCluster(grappe)
  }
  # List to array conversion
  I12 <- array(unlist(I12_par), dim = c(shape(I12_par[[1]])[1], shape(I12_par)))
  } else {
   I12<-dotprod(W1,aperm(N12,c(2,1)),g="&",f="==")  
   # 2023-05-04 to test
   # replace dotprod by outer(rownames(W1), rownames(N12), FUN = "==")
   # end test
  }
   MAC<-apply(I12*t(array(V12,dim(t(I12)))),1,sum) 
 
  ## 2.4.1  Order the subsets to find if the empty subset is there. Put empty set in first position of tt matrix
  sort_order<-order(apply(W1,1,sum))
  tt <- W1[sort_order,]
  if (is.matrix(tt) == FALSE) {
    tt <- matrix(tt,ncol = length(tt), dimnames = list(NULL, names(tt)))
  }
  ## 2.5.1 Identify if the empty set is present and define m_empty accordingly with it mass
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
  MAC <- MAC[sort_order]
  mMAC <-matrix(MAC,ncol=1, dimnames =list(NULL, "mass"))
  #
  ## 2.6.1 Define spec matrix
  #
  spec <- cbind(1:nrow(tt), mMAC)
  colnames(spec) <- c("specnb", "mass")
  # 
  ## 2.7.1  Revised 2021-04-23
  con12<-1-(1-zx$con)*(1-zy$con) # conflicting evidence inputted
  if  ((con12 == 1) | (m_empty == 1)) { 
    warning('Totally conflicting evidence (con = 1). Data is inconsistent.')}
  # "con" stays unchanged by a vacuous combination
  if (nrow(zx$tt)==1 |nrow(zy$tt) == 1) {
    con <- con12 
  } else {
    con<-1-(1-con12)*(1-m_empty) # OK checked
  }
  # test avec subset names
  } 
  #
  # NEW. Test avec utilisation de subsets names
  #
  else {
    # compute N12 and transform
    #
    N12 <-  mapply(FUN= function(X,Y) {lapply(X = 1:length(zx$ssnames), FUN = function(X) {intersect(zx$ssnames[[X]], zy$ssnames[[Y]])} )}, Y=1:length(zy$ssnames) ) 
    # Transform N12
    cN12 <- c(N12)
    cN12c <- lapply(X=1:length(cN12), FUN =  function(X) { reduction(cN12[[X]], f = "paste")})
    cN12cf <- unlist(lapply(X=1:length(cN12c), FUN = function(X) {if (length(cN12c[[X]]) == 0){ cN12c[[X]] = "Empty"} else cN12c[[X]] }) )
    #
    # Compute W1 as character vector and list
    W1 <- cN12cf[!duplicated(cN12cf)]
    W1_list <- cN12[!duplicated(cN12)]
     #
    # Transformations to obtain I12 matrix
    # Transform W1
    #
    # W1c <- lapply(X=1:length(W1), FUN =  function(X) { reduction(W1[[X]], f = "paste")})
    # W1cf <- unlist(lapply(X=1:length(W1c), FUN = function(X) {if (length(W1c[[X]]) == 0){ W1c[[X]] = "Empty"} else W1c[[X]] }) )
    #
    # Compute I12 matrix
    I12 <- outer(W1, cN12cf, FUN = "==" )
   #
   # 2.4.2  Order the subsets to find if the empty subset is there. Put empty set in first position of ssnames list
   #
   MAC<-apply(I12*t(array(t(V12),dim(t(I12)))),1,sum)   
   sort_order<-order(unlist(lapply(X=1:length(W1_list), FUN = function(X) {shape(W1_list[[X]])} )))
   tt <- NULL
    #
  ## 2.5.2 Identify if the empty set is present and define m_empty accordingly with it mass
  # Put masses in the same order as the ssnames list
  #
  z <- unlist(W1_list[sort_order[[1]]])
   if (rlang::is_empty(z) == TRUE) {
     empty<-sort_order[1]  
     m_empty<-MAC[empty] 
   } else {
     empty<-0
     m_empty<-0
   }
  # 
   MAC <- MAC[sort_order]
   mMAC <-matrix(MAC,ncol=1, dimnames =list(NULL, "mass"))
   #
   ## 2.6.2 Define spec matrix
   #
  spec <- cbind(1:shape(W1), mMAC)
  colnames(spec) <- c("specnb", "mass")
  #
  #
  ## 2.7.2  
  con12<-1-(1-zx$con)*(1-zy$con) # conflicting evidence inputted
  if  ((con12 == 1) | (m_empty == 1)) { 
    warning('Totally conflicting evidence (con = 1). Data is inconsistent.')}
   #
   # NOTE: "con" stays unchanged by a vacuous combination
   #
 if (shape(zx$ssnames) == 1 | shape(zy$ssnames) == 1) {
   con <- con12 
   } else {
   con<-1-(1-con12)*(1-m_empty) 
   }
  }
  # end test avec subsets names
  #
  ## 3. The result
  #
  ## 3.1 Name the resulting variables and fix parameters
  #
  # varnames and valuenames
  valuenames <- zx$valuenames
  if (!is.null(varnames)) {
    names(valuenames) <- varnames
  }
  if (missing(varnames) | is.null(varnames)) {
    varnames <- names(valuenames)
  } 
  #
  infovar <- zx$infovar
  #
  # inforel parameter
  if (missing(relnb) | is.null(relnb)) { 
    inforel <- zx$inforel
    } else {
    depth <- zx$inforel[,2]
    inforel <- matrix(c(relnb, depth), ncol = 2)
    colnames(inforel) <- c("relnb", "depth")
    }
  #
  # construction of the result
  #
  if (is.null(zx$ssnames) | is.null(zy$ssnames) ) {
    z <- list(con = con, tt=tt, spec = spec, infovar = infovar, varnames = varnames, valuenames = valuenames, inforel = inforel, I12=I12, sort_order=sort_order, ssnames = NULL, sfod = NULL)
  class(z) <- append(class(z), "bcaspec") 
  } else {
    z <- list(con = con, tt = NULL, spec = spec, infovar = infovar, varnames = varnames, valuenames = valuenames, inforel = inforel, I12=I12, sort_order=sort_order, ssnames = W1_list[sort_order], sfod = zx$sfod)
    class(z) <- append(class(z), "bcaspec") 
  }
  return(z)
  }
 