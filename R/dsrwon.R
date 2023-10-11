#' Combination of two mass functions
#' 
#'The unnormalized Dempster's rule is used to combine two mass functions \code{mx} and \code{my} defined  on the same frame of discernment and described by their respective basic chance assignments \code{x}  and \code{y}. Dempster's rule of combination is applied. The normalization is not done, leaving the choice  to the user to normalize the results or not (for the normalization operation, see function \code{\link{nzdsr}}).
#'
#'The calculations make use of multiple cores available.
#' @details The two bca's \code{x} and \code{y} must be defined on the same frame of discernment for the combination to take place. The relation number of the x input is given to the output result.  
#' @param x A basic chance assignment (see \code{\link{bca}}).
#' @param y A basic chance assignment (see \code{\link{bca}}).
#' @param sparseM Put TRUE to indicate the use of sparse matrices as input. Default = FALSE.
#' @param mcores Make use of multiple cores ("yes") or not ("no"). Default = "no".
#' @param varnames A character string to name the resulting variable. named "z" if omitted.
#' @param skpt_tt Skip reconstruction of tt matrix. Default = FALSE.
#' @param infovarnames Deprecated. Old name for \code{varnames}.
#' @param relnb Identification number of the relation. Can be omitted.
#' @return A basic chance assignment with these two components added: \itemize{
#'   \item I12 Intersection table of subsets.
#'   \item Sort_order Sort order of subsets.
#'   }
#' @author Claude Boivin, Peiyuan Zhu
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
#' # Sparse matrices
#' y1s <- y1
#' y2s <- y2
#' y1s$tt <- methods::as(y1$tt, "RsparseMatrix")
#' y2s$tt <- methods::as(y2$tt, "RsparseMatrix")
#' y1y2s <- dsrwon(y1s, y2s, sparseM = TRUE)
#' vacuous <- bca(matrix(c(1,1,1), nrow = 1), m = 1, cnames = c("a","b","c"))
#' dsrwon(vacuous, vacuous)
#' @references Shafer, G., (1976). A Mathematical Theory of Evidence. Princeton University Press, Princeton, New Jersey, pp. 57-61: Dempster's rule of combination.
dsrwon<-function(x, y, sparseM = FALSE, mcores = "no", varnames = NULL, relnb = NULL, skip_tt = FALSE, infovarnames) {
  #
  # Local variables: m1, m2, zx, zy, x1, y1, z, zz1 ,W1_list, W1s, values1, values2, V12, N12, W1, I12, MAC, nMAC
  # Functions calls: nameRows, dotprod
  # 0. Catch old parameters names, if any and replace by the new ones
  #
  calls <- names(sapply(match.call(), deparse))[-1]
  # catch old parameter infovarnames and replace by varnames if used instead of varnames
  if(any("infovarnames" %in% calls) & missing(varnames)) {
    warning("Parameter name 'infovarnames' is deprecated. Use 'varnames' instead.")
    varnames <- infovarnames
  }
  #
  # end catches
  #
  ## 1. Checks
  # 1.1. class bcaspec
  #
  if ( (inherits(x, "bcaspec") == FALSE) | (inherits(y, "bcaspec") == FALSE)) {
    stop("One or more inputs not of class bcaspec.")
  }
  #
  # 1.2. x and y must have the same number of variables
  if   ( nrow(x$infovar) != nrow(y$infovar) ) {
    stop("Specification of parameter infovar of the two bca's must identical.")
  }
  #
  # 1.3.  Check mass vector
  #
  m1 <- x$spec[,2]
  m2 <- y$spec[,2]
  if ( (abs(sum(m1)-1)>0.000001) | (abs(sum(m2)-1)>0.000001)) {
    print(m1)
    print(m2)
    stop("Invalid data, sum of masses of one vector, or both, greater than one.")
  }
  #
  # 1.4 prepare data for parallel processing 
  # Put the bca with the largegst number of subsets in second
  #
  if  ( nrow(x$spec) <= nrow(y$spec) ) {
    zx <- x
    zy <-y
  } 
  else {
    zx <- y
    zy <- x
  }
  # 2. Computation using tt matrices
  # 2.1. checks specific to the tt matrix 
  if  ((!is.null(x$tt)) & (!is.null(y$tt) ) ) {
    # 
    # 2.1.1. x1 and y1 must have same frame of discernment
    # and same number of elements
    #
    x1<-rbind(zx$tt)  # (M x K) matrix or sparse matrix
    y1<-rbind(zy$tt)  # (N x K) matrix or sparse matrix
    if (ncol(x1) != ncol(y1)) {
      stop("Nb of elements of frame x and frame y not equal.") 
    }
    #
    ## 2.1.2. x1 and x2 must have the same value names put in the same order
    #
    values1 <- unlist(zx$valuenames)
    values2 <- unlist(zy$valuenames)
    nbval <- sum(values1 == values2)
    if ((length(values1) != length(values2)) | (nbval != length(values1))) {
      stop("Value names of the two frames differ. Check value names of variables as well as their position.")
    }
    #
    ## 2a. Calculations
    #
  }
  ## 2a.1 Combine mass vectors
  V12<-outer(zx$spec[,2],zy$spec[,2], "*")
  #
  # # NB refaire ici les tests de L90-L114 avec ssnames
  #
  ## 2.2 combine subsets
  # transform table of intersections: (M x N) rows by K 
  # 
  # Obtain ssnames via tt colnames iif mising
  if (is.null(zx$ssnames) | is.null(zy$ssnames)) {
    if ((!is.null(zx$tt)) & (!is.null(zy$tt) ) ) {
      zframe <-colnames(zx$tt)
      zx$ssnames <- lapply(X=1:nrow(zx$tt), FUN = function(X) {zframe[zx$tt[X,]*1:ncol(zx$tt)]})
      zy$ssnames <- lapply(X=1:nrow(zy$tt), FUN = function(X) {zframe[zy$tt[X,]*1:ncol(zy$tt)]})
    }
  }  
  #
  #  Calculations with tt matrices, non sparse)
  if (sparseM == FALSE ) {
    # if ((!is.null(zx$tt)) & (!is.null(zy$tt) ) ) {
    #
    # 3.0  test 20230727
    # Use multiple cores = "yes"
    if  (mcores == "yes") {
      ncores <- parallel::detectCores(logical = FALSE)
      grappe <- parallel::makeCluster(ncores-1)
      # use devtools for testing
      #
      # test
      # parallel::clusterEvalQ(cl = grappe, expr = devtools::load_all("."))
      parallel::clusterEvalQ(cl = grappe, expr = library(dst))
      # end test
      #
      parallel::clusterExport(cl = grappe, varlist = list("x1", "y1"), envir = environment() )
      mx1y1_par <-  parallel::parSapply( cl = grappe, X=1:nrow(y1), FUN = function(X) { inters(x1, matrix(y1[X,], nrow = 1) ) }, simplify = FALSE, USE.NAMES = TRUE ) # intersection of the subsets
      parallel::stopCluster(grappe)
      # Transformation
      N12 <- mx1y1_par[[1]]
      for (i in 2:shape(mx1y1_par) ) {
        N12 <- rbind(N12, mx1y1_par[[i]] )
      }
    }
    else {
      N12<-inters(x1, y1)         # intersection of the subsets
    }
    #
    # Matrix transformation
    #
    if (isS4(N12) == TRUE ) {
      N12 <- as.matrix(N12) # convert sparse to matrix. to be able to apply duplicated() operator
    }
    #
    # Remove duplicates from the table
    W1<- N12[!duplicated(N12),]  ## remove duplicates
    if (is.matrix(W1) == FALSE) {
      W1 <- t(as.matrix(W1))
    }
    #
    ## 2.3 Identify contributions to each subset and compute mass
    #
    if  (mcores == "yes") {
      ncores <-  parallel::detectCores(logical = FALSE)
      grappe <-  parallel::makeCluster(ncores-1)
      # use devtools for testing
      #
      # test
      # parallel::clusterEvalQ(cl = grappe, expr = devtools::load_all("."))
      parallel::clusterEvalQ(cl = grappe, expr = library(dst))
      parallel::clusterExport(cl = grappe, varlist = list("W1", "N12"), envir = environment() )
      # end test
      #
      I12_par <-  parallel::parSapply( cl = grappe, X=1:nrow(N12), FUN = function(X) {  outer(rownames(W1), rownames(N12)[X], FUN = "==")  }, simplify = FALSE, USE.NAMES = TRUE )
      parallel::stopCluster(grappe)
      #
      # List to array conversion
      I12 <- array(unlist(I12_par), dim = c(shape(I12_par[[1]])[1], shape(I12_par)))
    }
    else {
      I12 <- outer(rownames(W1), rownames(N12), FUN = "==")
      if (isS4(N12) == TRUE) {
        I12 <- methods::as(I12, "RsparseMatrix")
      }
    }
    #
    # mmass vector
    if (mcores == "yes") {
      MAC<-apply(I12*t(array(V12,dim(I12)[2:1]) ),1,sum)
    }
    else {
      MAC<-apply(I12*t(array(t(V12),dim(I12)[2:1]) ),1,sum)
    }
    #
    ## 2.4.1  Order the subsets to find if the empty subset is there. Put empty set in first position of tt matrix
    sort_order <-order(apply(W1,1,sum))
    tt <- W1[sort_order,]
    if ((is.matrix(tt) == FALSE) & (is.matrix(W1) == TRUE) ) {
      tt <- matrix(tt,ncol = length(tt), dimnames = list(NULL, names(tt)))
    }
    #
    ## 2.5.1 Identify if the empty set is present and define m_empty accordingly with it mass
    # Put masses in the same order as the tt matrix
    #
    z<- sum(W1[sort_order[1],])
    if (z==0) {
      empty<-sort_order[1]
      m_empty<-MAC[empty]
    }
    else {
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
    ## 2.7.1  Revised 2023-06-14
    ## measure of contradiction (con). It is simply m_empty, always
    #
    con <- m_empty
    if  (con == 1) {
      warning('Totally conflicting evidence (con = 1). Data is inconsistent.')
    }
  } 
  #
  # 3. NEW. Test avec utilisation de subsets names
  #
  if (sparseM == TRUE ) {
    # if ((is.null(zx$tt)) | (is.null(zy$tt) ) ) {
    #  
    # 3.1.compute N12 and transform
    #
    # Use multiple cores = "yes"
    if  (mcores == "yes") {
      ncores <- parallel::detectCores(logical = FALSE)
      grappe <- parallel::makeCluster(ncores-1)
      # use devtools for testing
      #
      # test
      # parallel::clusterEvalQ(cl = grappe, expr = devtools::load_all("."))
      parallel::clusterEvalQ(cl = grappe, expr = library(dst))
      # end test
      #
      zzx=zx$ssnames
      zzy=zy$ssnames
      parallel::clusterExport(cl = grappe, varlist = list("zzx", "zzy"), envir = environment() )
      N12 <-  parallel::mcmapply(FUN= function(X,Y) {lapply(X = 1:length(zzx), FUN = function(X) {intersect(zzx[[X]], zzy[[Y]])} )}, Y=1:length(zzy) )  # intersection of the subsets
      parallel::stopCluster(grappe)
    }
    else {
      N12 <-  mapply(FUN= function(X,Y) {lapply(X = 1:length(zx$ssnames), FUN = function(X) {intersect(zx$ssnames[[X]], zy$ssnames[[Y]])} )}, Y=1:length(zy$ssnames) ) 
    }
    # Transform N12
    # for every element of the list, sequence of elements of the subsets
    cN12 <- c(t(N12) )
    # Obtain ssnames as a list
    # # Test  pour enlever "reduction"
    # cN12c <- lapply(X=1:length(cN12), FUN =  function(X) { paste(cN12[[X]], collapse = ' ')}) 
    # # Transform list in character vector
    # cN12v <- unlist(lapply(X=1:length(zzz), FUN = function(X) {if (zzz[[X]] == ""){ zzz[[X]] = "Empty"} else zzz[[X]] }) )
    # # Fin test pour enlever "reduuction"
    #
    # cN12c <- lapply(X=1:length(cN12), FUN =  function(X) { reduction(cN12[[X]], f = "paste")})
    cN12c <- lapply(X=1:length(cN12), FUN =  function(X) { Reduce("paste", cN12[[X]])}) # Plus rapide
    # Transform list in character vector
    cN12v <- unlist(lapply(X=1:length(cN12c), FUN = function(X) {if (length(cN12c[[X]]) == 0){ cN12c[[X]] <- "Empty"} else cN12c[[X]] }) )
    #
    # 3.2 Remove duplicattes in N12 to obtain the result of the intersections (W1 as character vector and list)
    #
    W1 <- cN12v[!duplicated(cN12v)]
    W1_list <- cN12[!duplicated(cN12)]
    #
    # identify contribution of cN12v to each subset of W1
    if  (mcores == "yes") {
      ncores <- parallel::detectCores(logical = FALSE)
      grappe <- parallel::makeCluster(ncores-1)
      # use devtools for testing
      #
      # test
      # parallel::clusterEvalQ(cl = grappe, expr = devtools::load_all("."))
      parallel::clusterEvalQ(cl = grappe, expr = library(dst))
      # end test
      #
      parallel::clusterExport(cl = grappe, varlist = list("W1", "cN12v"), envir = environment() )
      I12 <- parallel::parSapply( cl = grappe, X=1:length(W1),  FUN= function(X)  {which(W1[X] == cN12v)}, simplify = FALSE, USE.NAMES = TRUE ) 
      parallel::stopCluster(grappe)
    }
    else {
      I12 <- lapply(X=1:length(W1), FUN= function(X)  {which(W1[X] == cN12v)})
    }
    #  Compute sum of masses of each subset
    V12_vec <- c(t(V12))
    MAC <- unlist(lapply(X=1:length(I12), FUN= function(X)  {sum(V12_vec[I12[[X]]]) } ) )
    # 
    # Order W1 to put empty first and put masses in the same order
    #  
    # Sort_order
    sort_order<-order(unlist(lapply(X=1:length(W1_list), FUN = function(X) {shape(W1_list[[X]])} )))
    # W1 sorted
    W1s <- W1_list[sort_order] # put W1_list in order
    #
    # Reconstruct tt from ssnames
    #
    if (!skip_tt) {
      zframe <- W1s[[length(W1s)]]
      zz1 <- lapply(X=1:length(W1s), FUN= function(X) {lapply(X=1:length(W1s[[X]]), FUN = function(Y) {which(zframe == (W1s[[X]])[Y])}) } )
      #
      rowIdx <- vector()
      colIdx <- vector()
      for (i in 1:length(W1s) )  {
        tempc <-  (unlist(zz1[[i]]) )
        colIdx <- c(colIdx, tempc )
        tempr <- rep(i,sum(tempc >0) )
        rowIdx <- c(rowIdx, tempr)
      }
      # Obtain sparse tt matrix of the result
      tt <- Matrix::sparseMatrix(
        i = rowIdx,
        j = colIdx, 
        x = 1, 
        dims = c(length(W1s), length(zframe) )
      )
      tt <- methods::as(tt, "RsparseMatrix")
      colnames(tt) <- zframe
      rownames(tt) <- nameRows(tt)
    } else {
      tt <- NULL
    }
    #
    ## 2.5.2 Identify if the empty set is present and define m_empty accordingly with it mass
    #
    # Put masses in the same order as the ssnames list
    #
    z <- unlist(W1s[[1]])
    if (rlang::is_empty(z) == TRUE) {
      empty<-sort_order[1]  
      m_empty<-MAC[empty] 
    } else 
    {
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
    ## 2.7.2  Revised 2023-06-14
    # Measure of conflict (con). It is simply m_empty, always
    #
    con <- m_empty
    if  (con == 1) { 
      warning('Totally conflicting evidence (con = 1). Data is inconsistent.')
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
  } 
  else {
    depth <- zx$inforel[,2]
    inforel <- matrix(c(relnb, depth), ncol = 2)
    colnames(inforel) <- c("relnb", "depth")
  }
  #
  # construction of the result
  #
  if (sparseM == FALSE) {
    #if ((!is.null(zx$tt)) & (!is.null(zy$tt) ) ) {
    #  if (is.null(zx$ssnames) | is.null(zy$ssnames) ) {
    z <- list(con = con, tt=tt, spec = spec, infovar = infovar, varnames = varnames, valuenames = valuenames, inforel = inforel, I12=I12, sort_order=sort_order, ssnames = NULL, sfod = NULL)
    class(z) <- append(class(z), "bcaspec") 
  } 
  else {
    z <- list(con = con, tt = tt, spec = spec, infovar = infovar, varnames = varnames, valuenames = valuenames, inforel = inforel, I12=I12, sort_order=sort_order, ssnames = W1_list[sort_order], sfod = zx$sfod)
    class(z) <- append(class(z), "bcaspec") 
  }
  return(z)
}
