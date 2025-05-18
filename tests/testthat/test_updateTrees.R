# Tests "updateTrees" function
context("update a list of trees")
library(dst)
test_that("updateTrees", {
  # Compare updateTrees vs updateTree
  x <- matrix(c(1,0,0,
                0,0,1,
                0,1,1,
                1,1,1), nrow = 4, byrow = TRUE, dimnames = list(NULL,c("a","b","c")))
  q <- c(0.1,0.2,0.3,0.4)
  
  tree <- buildTree(x,q)
  trees <- buildTrees(x,q)
  
  xx <- as.bit(c(1,1,0))
  s <- as.bit(c(1,1,1))
  
  tree <- updateTree(tree,xx,s)
  
  for(i in 1:length(trees$card_nodup)) {
    
    trees[[i]] <- updateTrees(trees[[i]], xx, s, tree=trees, card_nodup=trees$card_nodup)
    
  }
  
  q1<-unravelTree(tree)
  q2<-unravelTrees(trees)
  expect_equal(q1,q2)
  
  
})