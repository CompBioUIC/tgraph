library(igraph)
#' Has constant Vertices
#'
#' Answers whether the graphs in a tgraph share a constant vertex set
#'
#' @param tgraph a dynamic network
#' @return whether all graphs in the tgraph have the same vertices.
#' @examples
#' dn <- constant_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)
#' time_ordered <- make_timeordered(dn)
has_constant_Vs <- function(tgraph) {

  vector_sets <- lapply(tgraph, V)
  equals_superset <- function(v_set) all(v_set == vector_sets[[1]])
  all(sapply(vector_sets, equals_superset))

}

#' Make Constant Vertices
#'
#' Makes the graphs in a tgraph share a constant vertex set
#'
#' @param tgraph a dynamic network
#' @return the same tgraph with constant vertices
#' @examples
#' dn <- constant_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)
#' make_constant_Vs(dn)
make_constant_Vs <- function(tgraph) {
  all_vertices <- vertex_superset(tgraph)

  modified_tgraph <- tgraph
  fix_ith_graph <- function(i) {
    missing_vs <- setdiff(vertex_superset, V(tgraph[[i]])) # set difference
    tgraph[[i]] + vertices(missing_vs) # returns tgraph_i with missing vs
  }
  lapply(tgraph, fix_ith_graph)

}

#' Vertex Superset
#'
#' Returns the set of all igraph vertices present at any time in the tgraph
#'
#' @param tgraph a dynamic network
#' @return a set of igraph vertices like the return of the igraph V function
#' @examples
#' dn <- constant_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)
#' make_constant_Vs(dn)
vertex_superset <- function(tgraph) {

  # make an actual set/list of manageable vertex objects
  #
  # do a real union on those sets
  make_vertex_set <- function(g) {
    #V(tgraph) stuff
  }

  unique(do.call(c, lapply(tgraph, make_vertex_set)))
}

#' Make Named tgraph
#'
#' If the igraphs making up a tgraph have unnamed vertices, applies trivial
#' names based on vertex index
#'
#' @param tgraph a dynamic network
#' @return a tgraph equal to the input tgraph but with all vertices named
#' @examples
#' dn <- constant_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)
#' make_named_tgraph(dn)
make_named_tgraph <- function(tgraph) {

}



#TODO
tgraph_is_equal <- function(gtm1, gtm2) {

}






