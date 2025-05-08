# Tests "buildTreeFast" function
context("build a tree")
library(dst)
test_that("buildTreeFast", {
  # Test Fig 12
  x <- matrix(c(1,0,0,
                0,0,1,
                0,1,1,
                1,1,1), nrow = 4, byrow = TRUE, dimnames = list(NULL, c("a","b","c")))
  q <- c(0.1,0.2,0.3,0.4)
  
  tree <- buildTreeFast(methods::as(x, "RsparseMatrix"),q)
  qx <- unravelTreeFast(tree, length(q))
  expect_equal(q,qx)
  
  tree <- inspectNode(tree)
  
  # 1 0 0
  expect_equal(tree$x,as.bit(x[1,]))
  expect_equal(tree$q,q[1])
  expect_equal(tree$depth,0)
  expect_equal(tree$index,1)
  
  # disjunction node
  expect_equal(tree$left$x,as.bit(c(0,1,0)))
  expect_equal(tree$left$q,NULL)
  expect_equal(tree$left$depth,1)
  expect_equal(tree$left$index,NULL) 
  
  # 1 1 1
  expect_equal(tree$right$x,as.bit(x[4,]))
  expect_equal(tree$right$q,q[4])
  expect_equal(tree$right$depth,2)
  expect_equal(tree$right$index,4) 
  
  # children of disjunction node
  expect_equal(tree$left$left$x,as.bit(x[2,]))
  expect_equal(tree$left$left$q,q[2])
  expect_equal(tree$left$left$depth,2)
  expect_equal(tree$left$left$index,2)
  
  expect_equal(tree$left$right$x,as.bit(x[3,]))
  expect_equal(tree$left$right$q,q[3])
  expect_equal(tree$left$right$depth,2)
  expect_equal(tree$left$right$index,3)
  
  # Test Fig 12 reordered
  x <- matrix(c(1,1,1,
                0,1,1,
                0,0,1,
                1,0,0), nrow = 4, byrow = TRUE, dimnames = list(NULL,c("a","b","c")))
  q <- c(0.1,0.2,0.3,0.4)
  
  tree <- buildTree(x,q)
  
  # 1 0 0
  expect_equal(tree$x,as.bit(x[4,]))
  expect_equal(tree$q,q[4])
  expect_equal(tree$depth,0)
  expect_equal(tree$index,4)
  
  # disjunction node
  expect_equal(tree$left$x,as.bit(c(0,1,0)))
  expect_equal(tree$left$q,NULL)
  expect_equal(tree$left$depth,1)
  expect_equal(tree$left$index,NULL) 
  
  # 1 1 1
  expect_equal(tree$right$x,as.bit(x[1,]))
  expect_equal(tree$right$q,q[1])
  expect_equal(tree$right$depth,2)
  expect_equal(tree$right$index,1) 
  
  # children of disjunction node
  expect_equal(tree$left$left$x,as.bit(x[3,]))
  expect_equal(tree$left$left$q,q[3])
  expect_equal(tree$left$left$depth,2)
  expect_equal(tree$left$left$index,3)
  
  expect_equal(tree$left$right$x,as.bit(x[2,]))
  expect_equal(tree$left$right$q,q[2])
  expect_equal(tree$left$right$depth,2)
  expect_equal(tree$left$right$index,2)
  
  # Test overwriting disjunction node
  # Test Fig 12 + b in the end
  x <- matrix(c(1,0,0,
                0,0,1,
                0,1,1,
                1,1,1,
                0,1,0), nrow = 5, byrow = TRUE, dimnames = list(NULL,c("a","b","c")))
  q <- c(0.1,0.2,0.3,0.3,0.1)
  
  tree <- buildTree(x,q)
  
  expect_equal(tree$x,as.bit(x[1,]))
  expect_equal(tree$q,q[1])
  expect_equal(tree$depth,0)
  expect_equal(tree$index,1)
  
  expect_equal(tree$left$x,as.bit(x[5,]))
  expect_equal(tree$left$q,q[5])
  expect_equal(tree$left$depth,1)
  expect_equal(tree$left$index,5)
  
  expect_equal(tree$right$x,as.bit(x[4,]))
  expect_equal(tree$right$q,q[4])
  expect_equal(tree$right$depth,2)
  expect_equal(tree$right$index,4)
  
  expect_equal(tree$left$left$x,as.bit(x[2,]))
  expect_equal(tree$left$left$q,q[2])
  expect_equal(tree$left$left$depth,2)
  expect_equal(tree$left$left$index,2)
  
  expect_equal(tree$left$right$x,as.bit(x[3,]))
  expect_equal(tree$left$right$q,q[3])
  expect_equal(tree$left$right$depth,2)
  expect_equal(tree$left$right$index,3)
  
  # Test Fig 12 + b in the middle
  x <- matrix(c(1,0,0,
                0,0,1,
                0,1,0,
                0,1,1,
                1,1,1), nrow = 5, byrow = TRUE, dimnames = list(NULL,c("a","b","c")))
  q <- c(0.1,0.2,0.3,0.3,0.1)
  
  tree <- buildTree(x,q)
  
  expect_equal(tree$x,as.bit(x[1,]))
  expect_equal(tree$q,q[1])
  expect_equal(tree$depth,0)
  expect_equal(tree$index,1)
  
  expect_equal(tree$left$x,as.bit(x[3,]))
  expect_equal(tree$left$q,q[3])
  expect_equal(tree$left$depth,1)
  expect_equal(tree$left$index,3)
  
  expect_equal(tree$right$x,as.bit(x[5,]))
  expect_equal(tree$right$q,q[5])
  expect_equal(tree$right$depth,2)
  expect_equal(tree$right$index,5)
  
  expect_equal(tree$left$left$x,as.bit(x[2,]))
  expect_equal(tree$left$left$q,q[2])
  expect_equal(tree$left$left$depth,2)
  expect_equal(tree$left$left$index,2)
  
  expect_equal(tree$left$right$x,as.bit(x[4,]))
  expect_equal(tree$left$right$q,q[4])
  expect_equal(tree$left$right$depth,2)
  expect_equal(tree$left$right$index,4)
  
  ## Test emptyset
  # Test Fig 12 + emptyset in the last row
  x <- matrix(c(1,0,0,
                0,0,1,
                0,1,1,
                1,1,1,
                0,0,0), nrow = 5, byrow = TRUE, dimnames = list(NULL,c("a","b","c")))
  q <- c(0.1,0.2,0.3,0.3,0.1)
  
  tree <- buildTree(x,q)
  
  expect_equal(tree$empty_set$x,as.bit(x[5,]))
  expect_equal(tree$empty_set$q,q[5])
  expect_equal(tree$empty_set$depth,-1)
  
  # Test Fig 12 + emptyset in the middle
  x <- matrix(c(0,0,1,
                0,0,0,
                1,0,0,
                0,1,1,
                1,1,1), nrow = 5, byrow = TRUE, dimnames = list(NULL,c("a","b","c")))
  q <- c(0.1,0.2,0.3,0.3,0.1)
  
  tree <- buildTree(x,q)
  
  expect_equal(tree$empty_set$x,as.bit(x[2,]))
  expect_equal(tree$empty_set$q,q[2])
  expect_equal(tree$empty_set$depth,-1)
  
  # Test Fig 12 + emptyset in the first row
  x <- matrix(c(0,0,0,
                0,0,1,
                1,0,0,
                0,1,1,
                1,1,1), nrow = 5, byrow = TRUE, dimnames = list(NULL,c("a","b","c")))
  q <- c(0.1,0.2,0.3,0.3,0.1)
  
  tree <- buildTree(x,q)
  
  expect_equal(tree$empty_set$x,as.bit(x[1,]))
  expect_equal(tree$empty_set$q,q[1])
  expect_equal(tree$empty_set$depth,-1)
  
  # Test simple support
  x <- matrix(c(0,1,1,
                1,1,0,
                0,1,0,
                1,1,1), nrow = 4, byrow = TRUE, dimnames = list(NULL,c("a","b","c")))
  q <- c(0.1,0.2,0.3,0.4)
  
  tree <- buildTree(x,q)
  
  # disjunction node
  expect_equal(tree$x,as.bit(c(1,0,0)))
  expect_equal(tree$q,NULL)
  expect_equal(tree$depth,0)
  expect_equal(tree$index,NULL)
  
  expect_equal(tree$left$x,as.bit(x[3,]))
  expect_equal(tree$left$q,q[3])
  expect_equal(tree$left$depth,1)
  expect_equal(tree$left$index,3) 
  
  expect_equal(tree$left$right$x,as.bit(x[1,]))
  expect_equal(tree$left$right$q,q[1])
  expect_equal(tree$left$right$depth,2)
  expect_equal(tree$left$right$index,1)
  
  expect_equal(tree$right$x,as.bit(x[2,]))
  expect_equal(tree$right$q,q[2])
  expect_equal(tree$right$depth,1)
  expect_equal(tree$right$index,2) 
  
  expect_equal(tree$right$right$x,as.bit(x[4,]))
  expect_equal(tree$right$right$q,q[4])
  expect_equal(tree$right$right$depth,2)
  expect_equal(tree$right$right$index,4)
  
  # TODO: add more tests
})
