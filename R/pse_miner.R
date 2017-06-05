library(igraph)


#' Top-level function implementing the PSE-miner as presented in definition 6
#' of \url{http://compbio.cs.uic.edu/~tanya/research/pubs/LahiriBerger-Wolf_PeriodicSubgraph10.pdf}
#'
#' @param tgraph a dynamic network
#' @param min_support a positive integer defining how many times a
#' subgraph must appear to be considered periodic
#' @return a vector of graphs (just like a tgraph), containing each
#' subgraphs of tgraph with support >= min_support
#' @examples
#' dn <- constant_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)
#' subgraphs <- get_periodic_subgraphs(dn, 4)
#' time_ordered <- make_timeordered(dn)
periodic_subgraph_miner <- function(tgraph, min_support) {

  mining_env <- mining_environment(tgraph, min_support)



}

#' Since the PSE Mining algorithm is stateful, we need a defined miner
#' environment that is updated by the steps of the algorithm.
#'
#' @param tgraph a dynamic network
#' @param min_support a positive integer defining how many times a
#' subgraph must appear to be considered periodic
#' @return a vector of graphs (just like a tgraph), containing each
#' subgraphs of tgraph with support >= min_support
#' @examples
#' dn <- constant_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)
#' subgraphs <- get_periodic_subgraphs(dn, 4)
#' time_ordered <- make_timeordered(dn)
mining_environment <- function(tgraph, min_support) {

  node_tree <-

  mining_env <- structure(1, min_support=min_support, class="mining_environment")



}



#' Defines a periodic projection of a tgraph as defined in definition 8 of
#' \url{http://compbio.cs.uic.edu/~tanya/research/pubs/LahiriBerger-Wolf_PeriodicSubgraph10.pdf}
#'
#' @param tgraph a dynamic network
#' @param period the period of the projection; a positive nonzero integer
#' @param offset the initial offset of the projection; a positive integer
#' @return a vector of graphs (just like a tgraph), containing each
#' subgraphs of tgraph with support >= min_support
#' @examples
#' dn <- constant_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)
#' subgraphs <- get_periodic_subgraphs(dn, 4)
#' time_ordered <- make_timeordered(dn)
projection_indices <- function(tgraph, period, offset) {
  seq(offset, length(tgraph), period)
}

#' Determines how  a periodic projection of a tgraph as defined in definition 8 of
#' \url{http://compbio.cs.uic.edu/~tanya/research/pubs/LahiriBerger-Wolf_PeriodicSubgraph10.pdf}
#'
#' @param tgraph a dynamic network
#' @param period the period of the projection; a positive nonzero integer
#' @param offset the initial offset of the projection; a positive integer
#' @return a vector of graphs (just like a tgraph), containing each
#' subgraphs of tgraph with support >= min_support
#' @examples
#' dn <- constant_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)
#' subgraphs <- get_periodic_subgraphs(dn, 4)
#' time_ordered <- make_timeordered(dn)
projection_indices <- function(tgraph, period, offset) {
  seq(offset, length(tgraph), period)
}


#' A missing igraph utility function that determines if one graph is a subgraph
#' of another
#'
#' @param sub_g the subgraph in question; an igraph graph
#' @param g the graph in question; an igraph graph
#' @return true or false
#' @examples
#' dn <- constant_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)
#' subgraphs <- get_periodic_subgraphs(dn, 4)
#' time_ordered <- make_timeordered(dn)
is_subgraph <- function(sub_g, g) {
  intersect <- graph.intersection(sub_g, g)
  ecount(intersect) == ecount(sub_g)
}

#' Finds the support set of a graph in a tgraph
#'
#' @param sub_g the subgraph in question; an igraph graph
#' @param tg the tgraph in question
#' @return a vector of the indices in tgraph that contain sub_g as a subgraph
#' @examples
#' dn <- constant_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)
#' subgraphs <- get_periodic_subgraphs(dn, 4)
#' time_ordered <- make_timeordered(dn)
support_set <- function(sub_g, tg) {
  which(sapply(tg, is_subgraph, sub_g=sub_g))
}


#' Does one round of iteration of the PSE mining agorithm. This is Algorithm
#' 1 from \url{http://compbio.cs.uic.edu/~tanya/research/pubs/LahiriBerger-Wolf_PeriodicSubgraph10.pdf}
#'
#' @param sub_g the subgraph in question; an igraph graph
#' @param tg the tgraph in question
#' @return a vector of the indices in tgraph that contain sub_g as a subgraph
#' @examples
#' dn <- constant_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)
#' subgraphs <- get_periodic_subgraphs(dn, 4)
#' time_ordered <- make_timeordered(dn)
update_tree <- function(sub_g, tg) {
  which(sapply(tg, is_subgraph, sub_g=sub_g))
}




#' S3 generator for a node in the PSE-mining tree. A PSE miner node has a
#' subgraph and an array of closed periodic embeddings (descriptor objects)
#'
#' @param sub_g the subgraph in question; an igraph graph
#' @param tg the tgraph in question
#' @return a vector of the indices in tgraph that contain sub_g as a subgraph
#' @examples
#' dn <- constant_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)
#' subgraphs <- get_periodic_subgraphs(dn, 4)
#' time_ordered <- make_timeordered(dn)
pse_miner_node <- function(sub_g, descriptor) {
  return(structure(sub_g, cp_embeddings=list(descriptor), class="pse_miner_node"))
}


#' S3 generator for a description of a closed periodic embedding in the
#' PSE-mining tree,  as described in Section 5.1 of
#' \url{http://compbio.cs.uic.edu/~tanya/research/pubs/LahiriBerger-Wolf_PeriodicSubgraph10.pdf}
#'
#' @param p the period of the embedding, a positive int
#' @param support_set the set of time steps where the subgraph is embedded
#' @return a simple S3 object of class pse_description with p as its main value
#' and a support_set attribute.
#' @examples
#' dn <- constant_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)
#' subgraphs <- get_periodic_subgraphs(dn, 4)
#' time_ordered <- make_timeordered(dn)
pse_description <- function(p, support_set) {
  structure(1, support_set = support_set, p = p, class = "pse_description")
  ## how do you make a vector of classed items?
}

#' Finds the next expected time you'd expect to see a periodic pattern,
#' described in a pse_description S3 object.
#'
#' @param desc the pse_description describing the periodic pattern in question
#' @return the next time you'd expect to see the pattern appear
#' @examples
#' dn <- constant_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)
#' subgraphs <- get_periodic_subgraphs(dn, 4)
#' time_ordered <- make_timeordered(dn)
next_expected_t <- function(desc) {
  # note that support_set is a vector attribute and desc's value is the period
  desc[[length(desc)]] + attr(desc, "p")
}


#' S3 generator for a description of a closed periodic embedding in the
#' PSE-mining tree,  as described in Section 5.1 of
#' \url{http://compbio.cs.uic.edu/~tanya/research/pubs/LahiriBerger-Wolf_PeriodicSubgraph10.pdf}
#'
#' @param sub_g the subgraph in question; an igraph graph
#' @param tg the tgraph in question
#' @return a vector of the indices in tgraph that contain sub_g as a subgraph
#' @examples
#' dn <- constant_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)
#' subgraphs <- get_periodic_subgraphs(dn, 4)
with_description <- function(miner_node, desc) {
  old_descs <- attr(miner_node, "cp_embeddings")
  old_descs[[length(old_descs)+1]] <- desc
  pse_miner_node(miner_node[[1]], old_descs)
}

#' The update descriptors c as described in section 5.2 of
#' \url{http://compbio.cs.uic.edu/~tanya/research/pubs/LahiriBerger-Wolf_PeriodicSubgraph10.pdf}
#'
#' @param miner_node the node describing the subgraph in question; an S3 object
#' of the type returned by \link{pse_miner_node}.
#' @param t the current time step
#' @return an update to the pse_mining environment
#' @examples
#' dn <- constant_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)
#' subgraphs <- get_periodic_subgraphs(dn, 4)
#' time_ordered <- make_timeordered(dn)
update_descriptors <- function(miner_node, t) {


  which(sapply(tg, is_subgraph, sub_g=sub_g))
}

updated_descriptor <- function(desc, t, min_support) {
  next_t <- next_expected_t(desc)
  if (next_t == t) {

  }
}




