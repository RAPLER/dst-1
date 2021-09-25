#' The peeling algorithm
#'
#'An implementation of the peeling algorithm based on the description of the algorithm in terms of hypergraphs by R. Almond [1989].\cr
#' @details The peeling algorithm works on an undirected graph. Nodes (variables) of the graph are eliminated (removed of the graph) one by one until only the variable of interest remains. An order of elimination (peeling) of the variables must be chosen, since no algorithm has yet been provided for that. At each step, a procedure of extension is applied to the bca's to merge, and marginalization is applied. The marginalization has the effect to integrate in the remaining nodes the information of the eliminated variable.
#' @param vars_def A list of the variables and their possible values. Simply concatenate the valuenames parameter of all the variables of the hypergraph to obtain this list.
#' @param hgm The incidence matrix of the hypergraph (bipartite graph), which is the  description of the relations between the variables. The variables are the nodes of the hypergraph, and the relations are the edges. Each column describes a relation between the variables by a (0,1) vector. A "1" indicates that a variable belongs to the relation and a "0" not. This matrix must have row and column names. These names are used to show the graph. They need not be the same as variables and relations names of the set of bca's to be analyzed. Use shorter names to obtain a clearer graph.
#' @param hg_rel_names The names of the relations, which are objects of class "bcaspec".
#' @param elim_order The order of elimination of the variables. A vector of length nrow(hgm). variables are identified by numbers. The first number gives the first variable to eliminate. The variable of interest comes last.
#' @param verbose = TRUE: print steps on the console. Default = FALSE.
#' @return A bca class object.
#' @author Claude Boivin, Stat.ASSQ
#' @export
#' @references \itemize{
#' \item Almond, R. G. (1989) Fusion and Propagation of Graphical Belief Models: An Implementation and an Example. Ph. D. Thesis, the Department of Statistics, Harvard University. 288 pages (for the description of the algorithm, see pages 52-53).
#' }
#' @examples 
#' # Zadeh's Example
#' 
#' # 1. Defining variables and relations 
#' # (for details, see vignette: Zadeh_Example)
#' e1 <- bca(f= matrix(c(1,0,0,1,1,1), ncol=2, byrow=TRUE),
#'  m= c(0.99, 0.01, 0), cnames =c("M", "T"), 
#'  varnames = "D1", varnb = 1)
#' e2 <- bca(f= matrix(c(1,0,0,1,1,1), ncol=2, byrow=TRUE), 
#' m= c(0.99, 0.01, 0), cnames =c("C", "T"), 
#' varnames = "D2", varnb = 2)
#' p_diag <- bca(f= matrix(c(1,1,1), ncol=3, byrow=TRUE), 
#' m= c(1), cnames =c("M", "T", "C"), 
#' varnames = "D", varnb = 3)
#' # Defining the relation between the variables
#' # tt matrix
#' tt_r1 <- matrix(c(1,0,1,0,1,0,0,1,0,1,0,0,0,1,
#' 1,0,0,1,1,0,0,1,0,0,1,0,1,0,0,1,1,0,0,1,0,
#' 0,1,1,0,0,0,1,0,1,0,1,0,1,0,1,1,1,1,1,1,1), 
#' ncol = 7,byrow = TRUE)
#' colnames(tt_r1) = c("M", "T", "C", "T", "M", "T", "C")
#' # The mass function
#' spec_r1 = matrix(c(rep(1,7),2, rep(1,7), 0), ncol = 2, dimnames = list(NULL, c("specnb", "mass"))) 
#' # Variables numbers and dimension of their Fod
#' info_r1 =matrix(c(1:3, 2,2,3), ncol = 2, dimnames = list(NULL, c("varnb", "size")) )
#' #  The relation between e1, e2 and a patient p
#' r1 <-bcaRel(tt = tt_r1, spec = spec_r1, infovar = info_r1, varnames = c("D1", "D2", "D"), relnb = 1)
#' 
#' # 2. Setting the incidence matrix of the grapph
#' rel1 <- 1*1:3 %in% r1$infovar[,1]
#' ev1 <- 1*1:3 %in% e1$infovar[,1]
#' ev2 <- 1*1:3 %in% e2$infovar[,1]
#' meddiag_hgm <- matrix(c(ev1,ev2, rel1), ncol=3, 
#' dimnames = list(c("D1", "D2", "D"), c("e1","e2", "r1")))
#' 
#' # 3. Setting the names of the variables and their frame of discernment
#' meddiag_vars1 <- c(e1$valuenames, e2$valuenames, p_diag$valuenames)
#'
#' # 4. Names of bca specifications (evidence and relations)
#' meddiag_rel_names <- c("e1", "e2", "r1")
#' 
#' # 5. Order of elimination of variables
#' elim_order <- c(1,2,3)
#' 
#' tabresul(peeling(vars_def = meddiag_vars1, hgm = meddiag_hgm,
#' hg_rel_names = meddiag_rel_names, elim_order = c(1, 2, 3)) )
#' 
peeling <- function ( vars_def, hgm, hg_rel_names, elim_order, verbose = FALSE) {
  #
  # Local variables: varmarge, ordelim, var_to_elim, i, j, irel_to_elim, rels_nb, rels_names, nb_rel, yv, yv2, yinfov, infovar, infovalues, init_tt, init_spec, init_info, relRef, xtnd_rel, name_relXtnd, name_newcol, newrelnb, name_rel_comb, name_rel_marge, rel_marginalized
  #
  # Functions calls: nameCols, extmin, dsrwon, nzdsr, elim 
  #
  # 1. Inputs checks
  #1.1. hgm not a matrix
  if (is.matrix(hgm) == FALSE) {
    stop("Incidence matrix missing.")
  }
  #
  # 1.2. hgm not a binary matrix
  if (sum(hgm <= 1) < length(hgm) ) {
    stop("Incidence matrix not in binary form.")
  }
  #
  # 1.3. Incidence matrix must have rownames and column names
  if (  is.null(rownames(hgm)) | is.null(colnames(hgm)) ) {
    stop("Row names or column names missing.")
  }
  #
  # 1.4. Number of declared variables not equal to number of rows of hgm
  if (length(vars_def) != nrow(hgm)) {
    stop("Number of variables in var_def parm and number of rows of hgm not equal.")
  }
  #
  # 1.5. Number of declared relations not equal to number of columns of hgm
  if (length(hg_rel_names) != ncol(hgm)) {
    stop("Number of relations declared and number of columns of hgm not equal.")
  }
  #
  # 1.6. Number of declared variables not equal to length of elim_order parameter.
  if (length(vars_def) != length(elim_order)) {
    stop("Number of variables and length of elim_order parameter not equal.")
  }
  #
  # 2. Inits
  varmarge <- rownames(hgm)[elim_order[length(elim_order)]]
  ordelim <- elim_order[1:(length(elim_order)-1)]
  if ( verbose == TRUE ) {
    cat("Elimination order : ", ordelim ,"\n")
    }
  var_to_elim <- rownames(hgm)[ordelim] 
  #
  #
  # 4. LOOP 1: Variable Elimination
  # 
  if ( verbose == TRUE ) {
    cat( "Hg matrix", print(hgm), "\n")
    }
  for(i in 1:length(ordelim)) {
    cat("i = :", i, ". Variable no ",  ordelim[i], ":", var_to_elim[i] , "\n")
    #
    print("")
   irel_to_elim <- hgm[var_to_elim[i],]*1:ncol(hgm)
   rels_nb <- irel_to_elim[irel_to_elim>0]
   cat("rels numbers to elim", rels_nb, "\n" )
   #
   # 4.1. find variables numbers of each relation to obtain the space to construct
   rels_names = hg_rel_names[rels_nb]
   nb_rel <- length(rels_names)
   yv = get(rels_names[1])$infovar
   j=2
   while (j <=nb_rel) {
     if ( verbose == TRUE )  {
      cat("Relations to combine: ", rels_names)
      print("")
      }
     yv2 = get(rels_names[j])$infovar
     yv=rbind(yv,yv2)
     j = j+1
   }
   yinfov = doubles(yv)
   infovar <- yinfov[order(yinfov[,1]),]
   # extract valuenames
   infovalues <- vars_def[infovar[,1]] 
   #
   #  4.2. extend the relations before combining them
   #  making an empty reference relation with mass(frame) = 1 and
   # extending a bca to it.
   # A: construct the tt matrix
   init_tt= matrix(rep(1,sum(infovar[,2])),nrow=1)
  colnames(init_tt) <- nameCols(valuenames = infovalues, size = infovar[,2])
   # B: mass values
   init_spec <- matrix(c(1,1), ncol = 2, 
                       dimnames = list(NULL, c("specnb", "mass")))
   # C: info on variables
   init_info <- matrix(as.vector(infovar), ncol = 2,
                       dimnames = list(NULL, c("varnb", "size")) )
   # D: the relation
   relRef <- bcaRel(tt = init_tt, spec = init_spec,
            infovar = init_info, varnames = names(infovalues), relnb = 1+ncol(hgm))
   #
   # 4.3. LOOP 2: Extend all the relations containing the variable to eliminate and combine with Dempster's Rule
   # Some  inits
   name_relXtnd <- character()
   rel_comb <- relRef
   newrelnb <- 1+ncol(hgm)
   #
   # The loop 
      for(j in 1:nb_rel) {
        # A: Extending relations
        xtnd_rel <- extmin(get(hg_rel_names[rels_nb[j]]), relRef)
        name_relXtnd[j] <- paste(hg_rel_names[rels_nb[j]],"_ext", sep="")
        assign(name_relXtnd[j], xtnd_rel)
        # B: combine two extended relations
        if ( verbose == TRUE ) {
          cat("combining extended relation number :" ,j, "\n")
          }
        rel_comb <- nzdsr(dsrwon(rel_comb, get(name_relXtnd[j]), relnb = newrelnb) )
        # C: remove old relation from hypergraph
        hgm[,rels_nb[j]] <- 0
        }    # End Loop 2
   #
   # 4.4. Name the new relation and modify hypergraph
   name_rel_comb <- paste("rel", as.character(newrelnb),sep="")
   assign(name_rel_comb, rel_comb)
   #
   # 4.5. Eliminate the variable ordelim[i] from the new relation obtained
   if ( verbose == TRUE )  {
    cat( "eliminating variable", ordelim[i], "\n" )
    }
   #
   newrelnb <- 1+ncol(hgm)
   name_rel_marge <- paste("rel", as.character(newrelnb),sep="")
   rel_marginalized <- elim(get(name_rel_comb), xnb = ordelim[i]) 
   #
   # 4.6. Update hypergraph and relations
   # add new relation to hypergraph
   assign(name_rel_marge, rel_marginalized)
   name_newcol <- paste("R",as.character(newrelnb),sep="")
   newcol <- 1*1:(nrow(hgm)) %in% (get(name_rel_marge))$infovar[,1]
   assign(name_newcol, newcol)
   #
   # Update hypergraph
   hgm <- cbind(hgm,get(name_newcol))
   colnames(hgm)[ncol(hgm)] <- name_newcol 
   if ( verbose == TRUE )  {
    cat("graph updated", "\n")
    print(hgm)
    }
   # 4.7. Update relations
   hg_rel_names <- c(hg_rel_names, name_rel_marge)
   if ( verbose == TRUE ) {
    cat("relations updated", hg_rel_names, "\n")
    }
   # 
  } # End loop 1
  #
  # 5. Combine relations pertaining to varmarge if needed
  #
  irel_to_elim <- hgm[varmarge,]*1:ncol(hgm)
  rels_nb <- irel_to_elim[irel_to_elim>0]
  if ( verbose == TRUE )  {
    cat("rels numbers to combine", rels_nb, "\n" )
    }
    rels_names = hg_rel_names[rels_nb]
  nb_rel <- length(rels_names)
  rel_comb <- get(hg_rel_names[rels_nb[1]])
  # loop if more than one relation
  if (nb_rel > 1) {
  for(j in 2:nb_rel) {
    # A: combine two relations
    if ( verbose == TRUE )  {
      cat("combining extended relation number :" ,j, "\n")
      }
    rel_comb <- nzdsr(dsrwon(rel_comb, get(hg_rel_names[rels_nb[j]]), relnb = newrelnb) )
    # B: remove old relation from hypergraph
    hgm[,rels_nb[j]] <- 0
  }
  }  
  cat( "Peeling ended", "\n")
  return(rel_comb)
}