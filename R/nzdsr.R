#' Normalization of a basic chance assignment
#'
#' It may occur that the result of the combination of two basic chance assignments with Dempster's Rule of combination contains a non-zero mass allocated to the empty set. The function \code{nzdsr} normalizes the result of function \code{dsrwon} by dividing the mass value of the non-empty subsets by 1 minus the mass of the empty set. 
#' @param x A basic chance assignment, i.e. a object of class bcaspec.
#' @return z The normalized basic chance assignment.
#' @author Claude Boivin
#' @references Shafer, G., (1976). A Mathematical Theory of Evidence. Princeton University Press, Princeton, New Jersey, pp. 57-61: Dempster's rule of combination.
#' @examples 
#' x1 <- bca(tt= matrix(c(1,0,1,1),nrow = 2, byrow = TRUE), 
#' m = c(0.9,0.1), cnames = c("yes", "no"),
#' varnames = "x", idvar = 1)
#' x2 <- bca(tt = matrix(c(0,1,1,1),nrow = 2, byrow = TRUE), 
#' m = c(0.5,0.5), cnames = c("yes", "no"), 
#' varnames = "x", idvar = 1)
#' print("combination of x1 and x2")
#' x1x2 <- dsrwon(x1,x2, varname = "x")
#' nzdsr(x1x2) 
#' 
#' print("normalization of a bca definition.")
#' y2 <- bca(tt = matrix(c(0,0,0,1,0,0,1,1,1),nrow = 3, 
#' byrow = TRUE), m = c(0.2,0.5,0.3), 
#' cnames = c("a", "b", "c"), idvar = 1)
#' cat("y2")
#' cat("\  ")
#' y2
#' nzdsr(y2)  
#' @export
#' 
nzdsr<-function(x) {
  #
  # Local variables: nc, vacuous, w1, w12, mac, MACC, empty, m_empty, tri, ind
  # Functions calls: nameRows
  #
  ## 1. Checks
  if ( inherits(x, "bcaspec") == FALSE) {
    stop("Input argument not of class bcaspec.")
  }
  #
  # 2023-06-12.
  #
  # Remove this test The conflict indice does not play a role in the combination by Dempster's rule. This is only a decision aid in the analysis of conflicting evidence
  # if (x$con == 1) { 
  #   stop('Completely conflicting evidence (con = 1). Data is inconsistent.')}
  #
  # End 2023-06-12
  #
  ## 2. Reconstruct I12 matrix (need to be updated if missing or if function addTobca has been used to add subsets)
  #
  # 2023-06-22 
  #
  # Put in comment all code pertaining to reconstruction of I12 and sort_order, using dsrwon. I12 and sort_order are is not necessary to this function when the bca inputted has not changed (by addTobca),
  #
  ## Start comment for I12
  #
  # if (is.null(x$ssnames) ) {
  # nc <- ncol(x$tt)
  # vacuous <- bca(matrix(rep(1, nc), nrow=1), m=1, cnames = colnames(x$tt))
  # } else { 
  #   vacuous <- bca(m = 1,  
  #     varnames = "x", 
  #     ssnames = list(x$ssnames[[length(x$ssnames)]]), sfod = x$sfod)
  #   }
  # vacuous$valuenames <- x$valuenames
  # vacuous$infovar <- x$infovar
  # x <- dsrwon(x,vacuous)
  # 
  # End comment for I12 
  # case of tt matrix
  if (is.null(x$ssnames)  ) {
  #
  # 3. Assign variables
  #
  w12<-cbind(x$spec[,2], x$tt)
  w1<- x$tt
  mac<-x$spec[,2]
  nc=ncol(w1) 
  #
  ## 2023-06-26 test reconstruct sort_order if missing
  #
  if (is.null(x$sort_order)) {
  x$sort_order<-order(apply(x$tt,1,sum))
  }
  # End 2023-06-28 Test
  #
  tri<-x$sort_order
  #
  # 4. remove empty set and normalize masses
  #
  ind <- w12[tri[1],]
  if ((ind[1] != 0) & (sum(ind[-1]) == 0)) {
    empty <- tri[1]  
    W2 <- matrix(w1[tri[-1],], ncol=nc)
    ## calculate normalized masses
    MACC <- mac[tri[-1]]/(1-mac[empty]) 
    m_empty <- mac[empty] 
  } 
  else {
    empty<-0 
    W2 <- matrix(w1,ncol=nc)
    MACC <- mac
    m_empty <- 0
  }
  #
  # 5. Update bca parameters 
  #
  # tt matrix
  tt <- W2
  colnames(tt) <- colnames(x$tt)
  rownames(tt) <- nameRows(tt)
  #
  # spec parameter
  #
  spec <- cbind((1:nrow(tt)), MACC)
  colnames(spec) <- c("specnb", "mass")
  #
  # infovar, varnames, valuenames, inforel parameters
  #
  infovar <- x$infovar
  varnames <- x$varnames
  valuenames <- x$valuenames
  relnb <- (x$inforel)[1,1]
  inforel <- matrix(c(relnb, nrow(infovar)), ncol = 2)
  colnames(inforel) <- c("relnb", "depth") 
  #
  # construction of the result
  #
  z <- list(con = m_empty, tt = tt, spec = spec, infovar = infovar, varnames = varnames, valuenames = valuenames, inforel = inforel, ssnames = NULL, sfod = NULL)
  class(z) <- append(class(z), "bcaspec")
  } else {
  #  
  # 4b. remove empty set and normalize masses
  #  
  MAC <- x$spec[,2]
  if (x$spec[1,2] != 0) {
    MACC <- MAC[-1]/(1-MAC[1])  
    } else {
    MACC <- MAC
    }
  #
  # 5b. Update bca parameters 
  #
  # ssnames
  ssnames <- x$ssnames[-1]
  #
  # spec parameter
  #
  spec <- cbind((1:(-1+length(x$ssnames)) ), MACC)
  colnames(spec) <- c("specnb", "mass") 
  #
  # infovar, varnames, valuenames, inforel parameters
  #
  infovar <- x$infovar
  varnames <- x$varnames
  relnb <- (x$inforel)[1,1]
  inforel <- matrix(c(relnb, nrow(infovar)), ncol = 2)
  colnames(inforel) <- c("relnb", "depth") 
  #
  # construction of the result
  #
  z <- list(con = m_empty, tt = NULL, spec = spec, infovar = infovar, varnames = varnames, valuenames = NULL, inforel = inforel, ssnames = ssnames, sfod = length(ssnames[[length(ssnames)]]))
  class(z) <- append(class(z), "bcaspec")
  }
  return(z)
    }