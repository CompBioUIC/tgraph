
library(igraph)
#' @include utility.R


# defaults for tgraph initialization
default_t_max         <- function() 12
default_n_vertices    <- function() 6
default_edge_P        <- function() 0.25
default_replication_w <- function() 2


# "A Dynamic Random Graph is defined as the
# probabilistic graph G(V, <E,T>, P), defined over the tuple
# of nodes V, the edge stream on those nodes (V, <E, T>),
# and a probability distribution function P, where P(i,j,t)
# is the probability of an edge E_(i,j) at time t.




#' Creates a uniform dynamic network with fixed edge probability.
#'
#' This function simply creates t random graphs with a fixed, uniform edge
#' probability.
#'
#' @param t the number of time steps, or length, of the dynamic network
#' @param n_vertices the number of vertices in the network
#' @param p_edge the uniform edge probability between two vertices at any time
#' @export
#' @return A dynamic network of length t
#' @examples
#' dn <- uniform_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)
uniform_rand_tgraph <- function(t, n_vertices, p_edge) {

  if (missing(t)) t <- default_t_max()
  if (missing(n_vertices)) n_vertices <- default_n_vertices()
  if (missing(p_edge)) p_edge <- default_edge_P()

  uniform_rand_graph_generator <- function(t) {
    random_np(n_vertices, p_edge)
  }

  lapply(1:t, uniform_rand_graph_generator)
}

#' Returns a copy of the input tgraph, but with each time step duplicated w
#' times
#'
#' @param tgraph a dynamic network
#' @param w the number of times each time step should be duplicated
#' @return A dynamic network of length w * length(tgraph)
#' @examples
#' dn <- uniform_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)
#' doubled_dn <- oversample_replicate(dn, 2)
oversample_replicate <- function(tgraph, w) {

  if (missing(w)) w <- default_replication_w()

  w_tgraphs_at <- function(t) {
    lapply(1:w, function(n) { tgraph[[t]] })
  }

  t_max <- length(tgraph)
  unlist( lapply(1:t_max, w_tgraphs_at), recursive = F )

}

#' Replaces each graph in a tgraph with w graphs, with the orignal graph's edges
#' randomly assigned.
#'
#' @param tgraph a dynamic network
#' @param w the number of times each time step should be duplicated
#' @return A dynamic network of length w * length(tgraph)
#' @examples
#' dn <- uniform_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)
#' same_edges_twice_as_long <- oversample_distribute(dn, 2)
oversample_distribute <- function(tgraph, w) {

  if (missing(w)) w <- default_replication_w()
  n_vertices <- tgraph_n_vertices(tgraph)

  w_tgraphs_at <- function(t) {

    dnet_edges <- get.edgelist(tgraph[[t]]) # all old edges
    n_edges <- length(dnet_edges) / 2

    edge_reassign_indexes <- sample(w, n_edges, replace=T) # randomly reassign edges to new tgraphs

    make_tgraph_subset <- function(dnet_index) {
      subset_ids <- which(edge_reassign_indexes %in% c(dnet_index))
      new_edges <- t(dnet_edges[subset_ids,]) # transpose b/c of R's weird conventions ?
      make_graph(new_edges, n = n_vertices)
    }

    lapply(1:w, make_tgraph_subset)
  }

  t_max <- length(tgraph)
  unlist( lapply(1:t_max, w_tgraphs_at), recursive = F )

}

#' A tgraph with no edges
#'
#' @param t the number of time steps
#' @param n_vertices the number of vertices
#' @return A tgraph with no edges, t time steps and n_vertices vertices
#' @examples
#' lonely_guy_ten_times <- edgeless_tgraph(10, 1)
#' five_isolated_people_six_times <- edgeless_tgraph(6, 5)
edgeless_tgraph <- function(t, n_vertices) {

  if (missing(t)) t <- default_t_max()
  if (missing(n_vertices)) n_vertices <- default_n_vertices()

  edgeless_graph <- function(t) {
    make_graph(c(), n = n_vertices)
  }

  lapply( 1:t, edgeless_graph )

}

# note: this is not quite a DynMix from the TWIN paper
test_periodic_graph <- function(t, n_vertices, p_on, p_off, full_period) {

  if (missing(t))           t           <- default_t_max()
  if (missing(n_vertices))  n_vertices  <- default_n_vertices()
  if (missing(p_on))        p_on        <- half_as_close_to_one(default_edge_P())
  if (missing(p_off))       p_off       <- default_edge_P()/2
  if (missing(full_period)) full_period <- default_replication_w()
  half_period <- full_period / 2

  on_graphs  = uniform_rand_tgraph(ceiling(t/2), n_vertices = n_vertices, p_on)
  off_graphs = uniform_rand_tgraph(  floor(t/2), n_vertices = n_vertices, p_off)

  alternating_uniform_rand_graph_generator <- function(t) {
    half_p_num <- ceiling(t/half_period)
    print(paste("half p num = ", half_p_num))
    is_on <- ((half_p_num+1) %% 2 == 0)
    print(paste("is_on = ", is_on))

    on_id <- floor(t/2)+1
    print(paste("on_id = ", on_id))

    off_id <- floor(t/2)
    print(paste("off_id = ", off_id))

    ifelse( is_on,
            {
              print("is ON!")
              print(on_graphs[[on_id]])
              return(on_graphs[[on_id]])
            }, {
              print("hoh no.......OFF!!")
              print(off_graphs[[off_id]])
              return(off_graphs[[off_id]])
            }
    )
  }

  lapply(1:t, alternating_uniform_rand_graph_generator)

}

downsample <- function(tgraph, w, randomize) {

  if (missing(w)) w <- default_replication_w()
  if (missing(randomize)) randomize <- FALSE
  new_length <- floor( length(tgraph) / w )
  rand_num <- function(n_min, n_max) sample(n_min:n_max, 1)
  rand_offset <- 0
  max_i <- length(tgraph)

  get_representative_graph <- function(i) {
    if (randomize) rand_offset <- rand_num(0,w-1)
    desired_index <- i*w + rand_offset
    oob_safe_index <- min(desired_index, max_i)
    tgraph[[ oob_safe_index ]]
  }

  lapply(1:new_length, get_representative_graph)

}


dyn_rand_graph <- function(t = 8, n_vertices = 4, tij_edge_func = tij_fully_connected) {

  ijfunc_at <- function(t) {
    function(i, j) {
      tij_edge_func(t, i, j)
    }
  }

  tgraph_generator <- function(t) {
    graph_from_ijfunc(ijfunc = ijfunc_at(t))
  }

  lapply(1:t, tgraph_generator)

}

two_constant_ptij <- function(t_on, p_on, t_off, p_off) {
  total_period <- t_on + t_off

  func_on  <- constant_pij( p_on)
  func_off <- constant_pij(p_off)

  function(t,i,j) {
    if ((t %% total_period) > t_on) {
      func_on( i,j)
    } else {
      func_off(i,j)
    }
  }

}


aggregation_tester_1 <- function(n=5, uniform_connection_p=0.4, agg_func=union) {

  graph1 <- random_np(n, uniform_connection_p)
  graph2 <- random_np(n, uniform_connection_p)

  aggregated <- agg_func(graph1, graph2)
  c(graph1, graph2, aggregated)

}

tij_fully_connected <- function(t = 10, i = 1, j = 1) {
  constant_one(i,j)
}

 tij_falloff <- function(t, i, j) {
  1/t
 }
 tij_checkerboard <- function(t, i, j) {
   (t + i + j) %% 2
 }

 half_as_close_to_one <- function(x) {
   1 - (1-x)/2
 }


