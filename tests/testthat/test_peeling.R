# Tests "peeling" function
context("Eliminate variables of a graph to compute the belief function of a variable of interest")
library(dst)
test_that("peeling", {
  # 
  # T1 hgm must be a matrix. 
  # 
  xhg <- c(1, 0, 0, 0, 1, 0, 1, 1, 1)
  xvars<- list(A = c("a", "b"), B=c("b", "c"), C=c("a", "b", "c"))
  xrel <- c("r1", "r2", "e1")
  expect_error(peeling(vars_def = xvars, hgm = xhg,   hg_rel_names = xrel, elim_order = c(1, 2, 3), verbose = TRUE, showgraph = TRUE), "Incidence matrix missing.")
  #
  # T2. hgm must be a binary matrix
  # 
  xhg <- matrix(c(1, 3, 0, 0, 1, 0, 1, 1, 1), ncol=3)
  xvars<- list(A = c("a", "b"), B=c("b", "c"), C=c("a", "b", "c"))
  xrel <- c("r1", "r2", "e1")
  expect_error(peeling(vars_def = xvars, hgm = xhg,   hg_rel_names = xrel, elim_order = c(1, 2, 3), verbose = TRUE, showgraph = TRUE), "Incidence matrix not in binary form.")
  #
  # T3. hgm must have row and column names
  # 
  xhg <- matrix(c(1, 0, 0, 0, 1, 0, 1, 1, 1), ncol=3)
  xvars<- list(A = c("a", "b"), B=c("b", "c"), C=c("a", "b", "c"))
  xrel <- c("r1", "r2", "e1")
  expect_error(peeling(vars_def = xvars, hgm = xhg,   hg_rel_names = xrel, elim_order = c(1, 2, 3), verbose = TRUE, showgraph = TRUE), "Row names or column names missing.")
  #
  # T4. Number of declared variables must match number of rows of hgm
  # 
  xhg <- matrix(c(1, 0, 0, 0, 1, 0, 1, 1, 1), ncol=3)
  rownames(xhg) <- c("A", "B", "C")
  colnames(xhg) <- c("r1", "r2", "e1")
  xvars<- list(A = c("a", "b"), B=c("b", "c"))
  xrel <- c("r1", "r2", "e1")
  expect_error(peeling(vars_def = xvars, hgm = xhg,   hg_rel_names = xrel, elim_order = c(1, 2, 3), verbose = TRUE, showgraph = TRUE), "Number of variables in var_def parm and number of rows of hgm not equal.")
  #
  # T5. Number of declared relations must match number of columns of hgm
  # 
  xhg <- matrix(c(1, 0, 0, 0, 1, 0, 1, 1, 1), ncol=3)
  rownames(xhg) <- c("A", "B", "C")
  colnames(xhg) <- c("r1", "r2", "e1")
  xvars<- list(A = c("a", "b"), B=c("b", "c"), C=c("a", "b", "c"))
  xrel <- c("r1", "r2")
  expect_error(peeling(vars_def = xvars, hgm = xhg,   hg_rel_names = xrel, elim_order = c(1, 2, 3), verbose = TRUE, showgraph = TRUE), "Number of relations declared and number of columns of hgm not equal.")
  #
  # T6. Number of declared variables must match length of elim_order parameter.
  # 
  xhg <- matrix(c(1, 0, 0, 0, 1, 0, 1, 1, 1), ncol=3)
  rownames(xhg) <- c("A", "B", "C")
  colnames(xhg) <- c("r1", "r2", "e1")
  xvars<- list(A = c("a", "b"), B=c("b", "c"), C=c("a", "b", "c"))
  xrel <- c("r1", "r2", "e1")
  expect_error(peeling(vars_def = xvars, hgm = xhg,   hg_rel_names = xrel, elim_order = c(1, 2), verbose = TRUE, showgraph = TRUE), "Number of variables and length of elim_order parameter not equal.")
  #
})