library(igraph)

graph_from_ijfunc <- function(ijfunc = constant_one, n = 4) {
  graph_from_adjacency_matrix(adj_matrix_from_ijfunc(ijfunc, n))
}

# this only works if ijfunc has the correct dimensionality behavior
adj_matrix_from_ijfunc <- function(ijfunc = constant_one, n = 4) {
  firstRow <- sapply(1:n, ijfunc, j=1)
  sapply(1:n, ijfunc, j=firstRow)
}

checkerboard <- function(i, j) {
  (i+j) %% 2
}

constant_pij <- function(P) {
  function(i, j) {
    rep(P, length(j))
  }
}

random_np <- function(n_vertices, p_edge) {
  sample_gnp(n_vertices, p_edge);
  # aside re: package design. This function is a wrapper of an igraph function.
  # using the wrapper means we can change the implementation later, without
  # breaking old code that calls tgraph.random_np
}

constant_one <- function(i,j) {
  rep(1, length(j))
}


graphfunctest <- function(str) {
  str
}

test_adj <- adj_matrix_from_ijfunc()

test_graph <- graph_from_ijfunc()
