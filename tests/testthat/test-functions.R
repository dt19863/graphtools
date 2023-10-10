test_that("distance_matrix works", {
  xs <- c(0,3,3,6)
  ys <- c(0,0,4,0)
  D <- rbind(c(0,3,5,6),c(3,0,4,3),c(5,4,0,5),c(6,3,5,0))
  expect_equal(distance_matrix(xs,ys), D)
})

test_that("prim works", {
  xs <- c(0,3,3,6)
  ys <- c(0,0,4,0)
  A <- rbind(c(0,1,0,0),c(1,0,1,1),c(0,1,0,0),c(0,1,0,0))
  expect_equal(prim(xs,ys), A)
})

test_that("edge_set finds correct number of edges", {
  A <- rbind(c(0,1,0,0),c(1,0,1,1),c(0,1,0,0),c(0,1,0,0))
  edgemat <- edge_set(A)
  expect_true(nrow(edgemat)==3)
})

test_that("graph_plot returns an error if dimensions don't match", {
  xs <- c(0,1,2,3)
  ys <- c(0,1,2,3)
  A <- D <- matrix(rep(0),nrow=2,ncol=2)
  expect_error(graphplot(xs,ys,A))
})


