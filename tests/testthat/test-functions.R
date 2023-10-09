test_that("distance_matrix works", {
  xs <- c(0,3,3,6)
  ys <- c(0,0,4,0)
  D <- matrix(rep(0),nrow=4,ncol=4)
  D[1,2] <- 3
  D[2,1] <- 3
  D[1,3] <- 5
  D[3,1] <- 5
  D[1,4] <- 6
  D[4,1] <- 6
  D[2,3] <- 4
  D[3,2] <- 4
  D[2,4] <- 3
  D[4,2] <- 3
  D[3,4] <- 5
  D[4,3] <- 5
  expect_equal(distance_matrix(xs,ys), D)
})

test_that("graph_plot returns an error if dimensions don't match", {
  xs <- c(0,1,2,3)
  ys <- c(0,1,2,3)
  A <- D <- matrix(rep(0),nrow=2,ncol=2)
  expect_error(graphplot(xs,ys,A))
})


