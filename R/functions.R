#' Distance Matrix
#'
#' distance_matrix creates a distance matrix for the nodes of a graph, based on their euclidean distance.
#'
#' @param x_coords the x-coordinates of the graph nodes, as a vector.
#' @param y_coords the y-coordinates of the graph nodes, as a vector.
#'
#' @return a matrix D, where D[i,j] is the euclidean distance between node i and node j.
#'
#' @examples
#' xs <- c(1,0,3)
#' ys <- c(1,2,4)
#' distance_matrix(xs,ys)
#'
#' @export
distance_matrix <- function(x_coords,y_coords) {
  num_nodes <- length(x_coords)
  distances <- matrix(rep(0),nrow=num_nodes,ncol=num_nodes)
  for (i in 1:num_nodes) {
    for (j in 1:num_nodes) {
      if (i != j) {
        euclid_dis <- sqrt((x_coords[i]-x_coords[j])^2+(y_coords[i]-y_coords[j])^2)
        distances[i,j] <- euclid_dis
        distances[j,i] <- euclid_dis
      }
    }
  }
  distances
}
#' Minimum Spanning Tree using Prim's Algorithm
#'
#' prim finds a minimum spanning tree for the nodes of a graph, based on their euclidean distance.
#' Note that if there is more than one minimum spanning tree, only one will be returned.
#'
#' @param x_coords the x-coordinates of the graph nodes, as a vector.
#' @param y_coords the y-coordinates of the graph nodes, as a vector.
#'
#' @return the adjacency matrix, A, for a minimimum spanning tree of the graph.
#' If A[i,j] == 1, then nodes i and j are connected. If A[i,j] == 0, then they are not.
#'
#' @examples
#' xs <- c(1,0,3)
#' ys <- c(1,2,4)
#' prim(xs,ys)
#'
#' @export
prim <- function(x_coords,y_coords) {
  num_nodes <- length(x_coords)
  D <- distance_matrix(x_coords,y_coords)
  adjacency_matrix <- matrix(rep(0),nrow=num_nodes,ncol=num_nodes)
  included_nodes <- matrix(rep(0),nrow=num_nodes,ncol=num_nodes)
  included_nodes[1,1] <- 1
  not_nodes <- diag(1,nrow=num_nodes,ncol=num_nodes)
  not_nodes[1,1] <- 0
  num_inc_nodes <- 1
  while (num_inc_nodes < num_nodes) {
    poss_next <- included_nodes %*% D %*% not_nodes # helpful trick from https://stackoverflow.com/a/53908399
    minimum_edges <- which(poss_next == min(poss_next[poss_next > 0]), arr.ind = TRUE)
    num_minimums <- nrow(minimum_edges)
    if (num_minimums > 1) {
      chosen_index <- sample(num_minimums,size=1)
      chosen_edge <- minimum_edges[chosen_index,]
      join_node <- chosen_edge[1]
      added_node <- chosen_edge[2]
    } else {
      chosen_edge <- minimum_edges
      join_node <- chosen_edge[1]
      added_node <- chosen_edge[2]
    }
    adjacency_matrix[join_node,added_node] <- 1
    adjacency_matrix[added_node,join_node] <- 1
    included_nodes[added_node,added_node] <- 1
    not_nodes[added_node,added_node] <- 0
    num_inc_nodes <- num_inc_nodes + 1
  }
  adjacency_matrix
}
#' Edges of a Graph from Adjacency Matrix
#'
#' edge_set converts an adjacency matrix for a graph into a matrix representing the edges of the graph.
#' Note that if a node is connected to itself, this edge will be ignored.
#'
#' @param adj_mat the adjacency matrix, A, for a graph.
#' If A[i,j] == 1, then nodes i and j are connected. If A[i,j] == 0, then they are not.
#'
#' @return an n x 2 matrix E, where n is the number of edges of the graph.
#' If there is a row with one element equal to i and one element equal to j, then there is an edge between nodes i and j.
#'
#' @examples
#' test <- matrix(rep(0),nrow=4,ncol=4)
#' test[1,2] <- 1
#' test[2,1] <- 1
#' test[2,3] <- 1
#' test[3,2] <- 1
#' test[3,4] <- 1
#' test[4,3] <- 1
#' test[4,1] <- 1
#' test[1,4] <- 1
#' edge_set(test)
#'
#' @export
edge_set <- function(adj_mat) {
  num_nodes <- ncol(adj_mat)
  all_edges <- which(adj_mat == 1, arr.ind = TRUE)
  doub_edges <- nrow(all_edges)
  num_edges <- doub_edges/2
  keep_indices <- list()
  for (i in 1:doub_edges) {
    this_edge <- all_edges[i,]
    if (this_edge[1] < this_edge[2]) {
      keep_indices <- append(keep_indices,i)
    }
  }
  edges <- unname(all_edges[unlist(keep_indices),])
  edges
}
#' Plot Graph of Function
#'
#'graph_plot plots the nodes of a graph, and the links between them based on their adjacency matrix.
#'
#' @param x_coords the x-coordinates of the graph nodes, as a vector.
#' @param y_coords the y-coordinates of the graph nodes, as a vector.
#' @param adj_mat the adjacency matrix, A, for a graph.
#' If A[i,j] == 1, then nodes i and j are connected. If A[i,j] == 0, then they are not.
#'
#' @examples
#' xs <- c(1,1,2,2)
#' ys <- c(1,2,2,1)
#' test <- rbind(c(0,1,0,1),c(1,0,1,0),c(0,1,0,1),c(1,0,1,0))
#' plot_graph(xs,ys,test)
#'
#' @export
graph_plot <- function(x_coords,y_coords,adj_mat) {
  if (length(x_coords)!=length(y_coords)) {
    stop('adjacency matrix must have same number of rows and columns as number of nodes')
  }
  if (length(x_coords)!=nrow(adj_mat)) {
    stop('adjacency matrix must have same number of rows and columns as number of nodes')
  }
  if (length(x_coords)!=ncol(adj_mat)) {
    stop('adjacency matrix must have same number of rows and columns as number of nodes')
  }
  edges_to_plot <- edge_set(adj_mat)
  num_edges <- nrow(edges_to_plot)
  plot(x_coords,y_coords,xlab='x',ylab='y',pch=19, col="#808080",cex=2)
  for (i in 1:num_edges) {
    lines(c(x_coords[edges_to_plot[i,1]],x_coords[edges_to_plot[i,2]]),c(y_coords[edges_to_plot[i,1]],y_coords[edges_to_plot[i,2]]), col="#007ba8", lwd=4)
  }
}
