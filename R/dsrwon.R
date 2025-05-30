#' Combination of two mass functions
#' 
#'The unnormalized Dempster's rule is used to combine two mass functions \code{mx} and \code{my} defined  on the same frame of discernment and described by their respective basic chance assignments \code{x}  and \code{y}. Dempster's rule of combination is applied. The normalization is not done, leaving the choice  to the user to normalize the results or not (for the normalization operation, see function \code{\link{nzdsr}}).
#'
#'The calculations make use of multiple cores available.
#' @details The two bca's \code{x} and \code{y} must be defined on the same frame of discernment for the combination to take place. The relation number of the x input is given to the output result.  
#' @param x A basic chance assignment (see \code{\link{bca}}).
#' @param y A basic chance assignment (see \code{\link{bca}}).
#' @param mcores Make use of multiple cores ("yes") or not ("no"). Default = "no".
#' @param use_ssnames = TRUE to use ssnames instead of tt matrix to do the intersections. Default = FALSE
#' @param use_qq = TRUE to use qq instead of tt matrix to do the intersections. Default = FALSE
#' @param use_sparse Make use of sparse matrices ("yes") or not ("no"). Default = "no".
#' @param tree_type tree_type to use M trees ("multiple") or 1 tree ("single"). Default = "single"L
#' @param method Can be "emt" or "emt-m", Default is "emt-m".
#' @param varnames A character string to name the resulting variable. named "z" if omitted.
#' @param skpt_tt Skip reconstruction of tt matrix. Default = FALSE.
#' @param infovarnames Deprecated. Old name for \code{varnames}.
#' @param relnb Identification number of the relation. Can be omitted.
#' @return A basic chance assignment with these two components added: \itemize{
#'   \item I12 Intersection table of subsets.
#'   \item Sort_order Sort order of subsets.
#'   }
#' @author Claude Boivin, Peiyuan Zhu
#' @import methods bit
#' @importFrom utils hashtab
#' @importClassesFrom Matrix RsparseMatrix
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
#' y1y2s <- dsrwon(y1s, y2s, use_ssnames = TRUE)
#' 
#' # using commonalities
#' bma <- bca(tt=matrix(c(1,1,0,1,rep(1,4)), ncol = 4, byrow = TRUE), 
#' m = c(0.1, 0.9), cnames = c("a", "b", "c", "d"), method = "ezt-m")
#' bma2 <- dsrwon(bma, bma, use_qq = TRUE, use_sparse = "yes")
#' 
#' vacuous <- bca(matrix(c(1,1,1), nrow = 1), m = 1, cnames = c("a","b","c"))
#' dsrwon(vacuous, vacuous)
#' @references Shafer, G., (1976). A Mathematical Theory of Evidence. Princeton University Press, Princeton, New Jersey, pp. 57-61: Dempster's rule of combination.
dsrwon<-function(x, y, mcores = "no", use_ssnames = FALSE, use_qq = FALSE, use_sparse = "no", method = "emt-m", tree_type = "single", varnames = NULL, relnb = NULL, skpt_tt = FALSE, infovarnames) {
  # Local variables: m1, m2, q1, q2, zx, zy, colx, coly, zorder_check, x1, y1, z, zz1 ,W1_list, W1s, W1cs, V12, N12, W1, I12, MAC, nMAC
  # Functions calls: nameRows, dotprod
  #
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
  # 1.1. x and y of class bcaspec
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
  m1 <- x$spec[,2]
  m2 <- y$spec[,2]
  if ( ((abs(sum(m1)-1)>0.000001) | (abs(sum(m2)-1)>0.000001)) && use_qq == FALSE) {
    stop("Invalid data, sum of masses of one vector, or both, greater than one.")
  }
  # End input checks
  #
  # 1.4 prepare data for parallel processing 
  # Put the bca with the largest number of subsets in second
  if  ( nrow(x$spec) <= nrow(y$spec) && use_qq == FALSE) {
    zx <- x
    zy <-y
  } 
  else {
    zx <- y
    zy <- x
  }
  #
  # Checks specific to the tt matrix, if there is one
  if  ((!is.null(x$tt)) & (!is.null(y$tt) ) && use_qq == FALSE) {
    # 
    # x and y must have same frame of discernment
    #  and same number of elements
    #
    x1<-rbind(zx$tt)  # (M x K) matrix or sparse matrix
    y1<-rbind(zy$tt)  # (N x K) matrix or sparse matrix
    if (ncol(x1) != ncol(y1)) {
      stop("Nb of elements of frame x and frame y not equal.") 
    }
    #
    # x1 and x2 must have the same columns names put in the same order
    #
    colx <- colnames(x1)
    coly <- colnames(y1)
    zorder_check <- sum(diag(outer(colx, coly, "==")))
    if(zorder_check < ncol(x1) ) {
      stop("Value names of the two frames differ. Check value names of variables as well as their position.")
    }
    #
  }
  #
  # Combine mass vectors
  if (use_qq == FALSE) {
    V12<-outer(zx$spec[,2],zy$spec[,2], "*")
  }
  #
  # End Section 1
  #
  # Section 2 Calculations with tt matrices, default setup)
  #
  #
  if (use_ssnames == FALSE && use_qq == FALSE) {
    if ((is.null(zx$tt)) | (is.null(zy$tt) ) ) {
      stop("One or more description matrix missing.")
    }
    #
    # 2.1 Compute intersections
    #  Case with multiple cores = "yes"
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
    # 2.2 Obttain unique subsets resulting from the intersections 
    #
    if (isS4(N12) == TRUE ) {
      N12 <- as.matrix(N12) # convert sparse to matrix. to be able to apply duplicated() operator
    }
    #
    # Select unique subsets by removing duplicates from the N12 table of intersecions
    W1<- N12[!duplicated(N12),]  ## remove duplicates
    if (is.matrix(W1) == FALSE) {
      W1 <- t(as.matrix(W1))
    }
    #
    # 2.3 Identify contributions to each subset and compute the sum of masses
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
    # 2.4 Compute mass vector
    if (mcores == "yes") {
      MAC<-apply(I12*t(array(V12,dim(I12)[2:1]) ),1,sum)
    }
    else {
      MAC<-apply(I12*t(array(t(V12),dim(I12)[2:1]) ),1,sum)
    }
    #
    # 2.5 Order the subsets to find if the empty subset among them.
    sort_order <-order(apply(W1,1,sum))
    tt <- W1[sort_order,]
    if ((is.matrix(tt) == FALSE) & (is.matrix(W1) == TRUE) ) {
      tt <- matrix(tt,ncol = length(tt), dimnames = list(NULL, names(tt)))
    }
    #
    # Identify if the empty set is present and define m_empty accordingly with it mass
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
    ## 2.6  Recalculate spec matrix and contradiction Indice
    #
    spec <- cbind(1:nrow(tt), mMAC)
    colnames(spec) <- c("specnb", "mass")
    #
    ## 2.7 Measure of contradiction (con). Revised 2024-01-25
    #
    if (x$con == 0 ) {
      con <- 0
    } else {
      con <- x$con
    }
    #
  } 
  #
  # 3. Intersections made with subsets names
  #
  if (use_ssnames == TRUE && use_qq == FALSE) {
    if ((is.null(zx$ssnames)) | (is.null(zy$ssnames) ) ) {
      stop("One or more ssnames list is missing.")
    }
    #
    if (length(zx$ssnames[[length(zx$ssnames)]]) !=zx$infovar[1,2] ) {
      stop("Number of elements of frame differs from infovar parameter.")
    }
    if (length(zy$ssnames[[length(zy$ssnames)]]) !=zy$infovar[1,2] ) {
      stop("Number of elements of frame differs from infovar parameter.")
    }
    #
    # 3.1.compute intersections (N12 table) and transform to appropriate format
    # Case with multiple cores = "yes"
    if  (mcores == "yes") {
      ncores <- parallel::detectCores(logical = FALSE)
      grappe <- parallel::makeCluster(ncores-1)
      # use devtools for testing
      # parallel::clusterEvalQ(cl = grappe, expr = devtools::load_all("."))
      parallel::clusterEvalQ(cl = grappe, expr = library(dst))
      #
      zzx=zx$ssnames
      zzy=zy$ssnames
      parallel::clusterExport(cl = grappe, varlist = list("zzx", "zzy"), envir = environment() )
      N12 <-  parallel::mcmapply(FUN= function(X,Y) {lapply(X = 1:length(zzx), FUN = function(X) {intersBySSName(zzx[[X]], zzy[[Y]])} )}, Y=1:length(zzy) )  # intersection of the subsets
      parallel::stopCluster(grappe)
    }
    else {
      N12 <-  mapply(FUN= function(X,Y) {lapply(X = 1:length(zx$ssnames), FUN = function(X) {intersBySSName(zx$ssnames[[X]], zy$ssnames[[Y]])} )}, Y=1:length(zy$ssnames) ) 
    }
    # Transform N12 to an appropriate format
    # for every element of the list, obtain the elements forming the subsets
    cN12 <- c(t(N12) )
    cN12v <- unlist(lapply(X=1:length(cN12), FUN = function(X) { Reduce("paste", cN12[[X]])}))
    #
    # 3.2 Obtain unique subsets resulting from the intersections  (W1 as character vector and list)
    #
    W1 <- cN12v[!duplicated(cN12v)]
    W1_list <- cN12[!duplicated(cN12)]
    #
    # 3.3 identify contribution of cN12v to each subset of W1  
    # and compute the sum of masses
    #
    if  (mcores == "yes") {
      ncores <- parallel::detectCores(logical = FALSE)
      grappe <- parallel::makeCluster(ncores-1)
      # use devtools for testing
      # parallel::clusterEvalQ(cl = grappe, expr = devtools::load_all("."))
      parallel::clusterEvalQ(cl = grappe, expr = library(dst))
      parallel::clusterExport(cl = grappe, varlist = list("W1", "cN12v"), envir = environment() )
      I12 <- parallel::parSapply( cl = grappe, X=1:length(W1),  FUN= function(X)  {which(W1[X] == cN12v)}, simplify = FALSE, USE.NAMES = TRUE ) 
      parallel::stopCluster(grappe)
    }
    else {
      I12 <- lapply(X=1:length(W1), FUN= function(X)  {which(W1[X] == cN12v)})
    }
    #
    #  3.4 Compute sum of masses of each subset
    V12_vec <- c(t(V12))
    MAC <- unlist(lapply(X=1:length(I12), FUN= function(X)  {sum(V12_vec[I12[[X]]]) } ) )
    # 
    # 3.5 Order W1 to put empty first and put masses in the same order
    #  
    sort_order<-order(unlist(lapply(X=1:length(W1_list), FUN = function(X) {shape(W1_list[[X]])} )))
    W1s <- W1_list[sort_order] # list of ordered resulting subset names 
    #
    # 3.6. Construct the sparse tt matrix corresponding to ssnames
    #
    if (skpt_tt == FALSE) {
      tt <- ttmatrix(W1s, sparse = "yes") # replacing lines 300-321 to be deleted further
    }
    else {
      tt <- NULL
    }
    #
    # 3.7 Identify if the empty set is present and define m_empty  with its mass accordingly.
    # Put masses in the same order as the ssnames list
    #
    # test 20240526
    W1cs <- W1[sort_order] # vector of ordered resulting subset names 
    z <- W1cs[1]
    # end test
    if(z == "Empty") { 
      empty<-sort_order[1]  
      m_empty<-MAC[empty] 
    } 
    else  {
      empty<-0
      m_empty<-0
    }
    # 
    # Calculate vector of masses
    MAC <- MAC[sort_order]
    mMAC <-matrix(MAC,ncol=1, dimnames =list(NULL, "mass"))
    #
    # 3.8 Define spec matrix and contradiction indice
    #
    spec <- cbind(1:shape(W1), mMAC)
    colnames(spec) <- c("specnb", "mass")
    #
    ## Measure of contradiction (con). Revised 2024-01-25
    #
    if (x$con == 0 ) {
      con <- 0
    } else {
      con <- x$con
    }
    #
  }
  # 
  # End case with use of subsets names
  #
  if (use_qq == TRUE) {
    use_sparse = "yes"
    q1 <- x$qq
    q2 <- y$qq
    
    # Augment q1 with q2, q2 with q1
    n <- unname(x$infovar[,2])
    cnames <- x$valuenames[[1]]
    tt1 <- ttmatrixFromQQ(q1,n,cnames,sparse = "yes")
    
    n <- unname(x$infovar[,2])
    cnames <- x$valuenames[[1]]
    tt2 <- ttmatrixFromQQ(q2,n,cnames,sparse = "yes")
    
    # Take union
    ttx <- rbind(tt1,tt2)
    
    # Remove duplicates of the joint
    ttx <- ttx[!duplicated(rownames(ttx)),]
    
    # Add closure elements
    if (method=="emt" || method=="emt-m") { 
      
      if (method=="emt") {
        if(use_sparse=="no") tty <- closure(ttx) else if(use_sparse=="yes") tty <- closureSparse(ttx) else stop("use_sparse can be either yes or no")
      } else if (method=="emt-m") { 
        if(use_sparse=="no") tty <- closure(ttx,FALSE) else if(use_sparse=="yes") tty <- closureSparse(ttx,FALSE) else stop("use_sparse can be either yes or no")
      }
      
      colnames(tty) <- colnames(ttx)
      rownames(tty) <- nameRows(tty)
      
      x <- imputeQQ(tty,tt1,tt2,q1,q2,tree_type = tree_type)
      q1 <- x$q1
      q2 <- x$q2
      
    } else {
      stop("method needs to be one of emt, emt-m if use_qq is TRUE")
    }
    
    # Combine
    qq <- unlist(q1) * unlist(q2)
    con <- 0
    tt <- NULL
    spec <- NULL
  } else {
    qq <- NULL
  }
  # 4. The result
  #
  ## 4.1 Naming the resulting variables and fix some parameters
  #
  valuenames <- zx$valuenames
  if (!is.null(varnames)) {
    names(valuenames) <- varnames
  }
  if (missing(varnames) | is.null(varnames)) {
    varnames <- names(valuenames)
  } 
  infovar <- zx$infovar
  if (missing(relnb) | is.null(relnb)) { 
    inforel <- zx$inforel
  } 
  else {
    depth <- zx$inforel[,2]
    inforel <- matrix(c(relnb, depth), ncol = 2)
    colnames(inforel) <- c("relnb", "depth")
  }
  #
  # 4.2. construction of the result
  #
  if (use_ssnames == FALSE && use_qq == FALSE) {
    z <- list(con = con, tt=tt, qq=qq, spec = spec, infovar = infovar, varnames = varnames, valuenames = valuenames, inforel = inforel, sort_order=1:nrow(tt))
    class(z) <- append(class(z), "bcaspec") 
  } 
  #
  if (use_ssnames == TRUE && use_qq == FALSE) {
    znames <- W1_list[sort_order]
    znames <- lapply(X=1:length(znames), FUN = function(X) {if (length(znames[[X]]) == 0){ znames[[X]] <- "Empty"} else znames[[X]] })
    z <- list(con = con, tt = tt, qq=qq, spec = spec, infovar = infovar, varnames = varnames, valuenames = valuenames, inforel = inforel, sort_order = sort_order, ssnames = znames, sfod = zx$sfod)
    class(z) <- append(class(z), "bcaspec") 
  }
  if (use_qq == TRUE) {
    z <- list(con = con, tt = tt, qq=qq, spec = spec, infovar = infovar, varnames = varnames, valuenames = valuenames, inforel = inforel)
    class(z) <- append(class(z), "bcaspec") 
  }
  return(z)
}