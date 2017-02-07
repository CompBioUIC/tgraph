library(timeordered)


#' Converts a dynamic network into a timeordered network
#' according the conventions of package timeordered.
#'
#' @param tgraph a dynamic network
#' @return An igraph graph representing tgraph as a time ordered network
#' @examples
#' dn <- uniform_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)
#' time_ordered <- make_timeordered(dn)
make_timeordered <- function(tgraph) {

  total_t  <- length(tgraph)
  total_vs <- total_t * length(V(tgraph[[1]]))

  # start by generating Name, Time, and full name attributes for timeordered
  # vertices
  tgraph_v_names <- get_tgraph_vertex_names(tgraph)
  all_Names <- rep(tgraph_v_names, total_t)

  t_for_each_name <- function(t) {rep(t, length(tgraph_v_names))}
  all_Times <- sapply(1:length(tgraph), t_for_each_name)

  all_full_names <- paste(all_Names, all_Times)

  # now build the time ordered network. first, add vertices and attributes.
  to <- graph.empty(n=total_vs, directed = TRUE)
  V(to)$name <- all_full_names
  V(to)$Name <- all_Names
  V(to)$Time <- all_Times

  # now do the same with edges
  make_to_edges <- function(t) { # make_to_edge for each edge in tgraph[[t]]
    sapply( E(tgraph[[t]]), make_to_edge, t=t)
  }
  all_v_froms_at_t <- function(t) {
    as_edgelist(tgraph[[t]])[,1]
  }
  all_v_tos_at_t   <- function(t) {
    as_edgelist(tgraph[[t]])[,2]
  }
  all_ts_at_t <- function(t) {
    n_edges <- ecount(tgraph[[t]])
    rep(t, n_edges)
  }
  orig_froms <- unlist(lapply(1:total_t, all_v_froms_at_t))
  orig_tos   <- unlist(lapply(1:total_t, all_v_tos_at_t))
  orig_times <- unlist(lapply(1:total_t, all_ts_at_t))

  new_froms <- paste(as.character(orig_froms), orig_times)
  new_tos   <- paste(as.character(orig_tos),   orig_times)

  edgelist <- c(rbind(new_froms, new_tos))
  to <- add_edges(to, edgelist)
  to <- set_edge_attr(to, "VertexFrom", value=as.character(orig_froms))
  to <- set_edge_attr(to, "VertexTo",   value=as.character(orig_tos))
  to <- set_edge_attr(to, "TimeStart",  value=orig_times)
  to <- set_edge_attr(to, "TimeCost",   value=0)
  to <- set_edge_attr(to, "HopCost",    value=0)
  to
}

make_tgraph_hard <- function(tonetwork) {
  vertex_names <- unique(vertex_attr(tonetwork, "Name"))
  n_vertices <- length(vertex_names)

  edgelist <- get.edgelist(tonetwork)
  remove_time <- function(vertex_name_str) {
    strsplit(vertex_name_str, ' (?=[^ ]+$)', perl=TRUE)[[1]][[1]]
  }
  cleaned_edgelist <- unlist(lapply(t(edgelist), remove_time))
  edge_timelist    <- get.edge.attribute(tonetwork, "TimeStart")
  t_max <- max(edge_timelist)

  # because edge i has two entries on the edgelist
  edge_i_to_vertex_is <- function(i) {c(2*i-1, 2*i)}

  empty_dn <- lapply(1:t_max, function(i){graph.empty(n=n_vertices)})

  make_graph_at <- function(t) {
    set_vertex_attr(empty_dn[[t]], "name", value=vertex_names)

    t_edge_indices <- which(edge_timelist == t)
    edgelist_entries_in_t <- sapply(edge_is_t, edge_i_to_vertex_is)
    edgelist_subset <- cleaned_edgelist[edgelist_entries_in_t]
    # todo: fix namespace issue preventing this from taking affect
    add_edges(empty_dn[[t]], edgelist_subset)
  }

  output_dn <- lapply(1:t_max, make_graph_at)

}

#' Converts a timeordered network into a dynamic network
#'
#' Uses the timeordered.generatenetworkslices function with a time window of 1.
#'
#' @param tgraph a dynamic network
#' @return An igraph graph representing tgraph as a time ordered network
#' @examples
#' dn <- uniform_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)
#' time_ordered <- make_timeordered(dn)
make_tgraph <- function(tonetwork) {

  total_time <- max(get.edge.attribute(tonetwork, "TimeStart"))
  unit_time_deltas <- generatetimedeltas(1,total_time+1,1)
  network_slices <- generatenetworkslices(tonetwork, unit_time_deltas)
  network_slices

}



#' Returns a string vector of a tgraph's vertices' names. If the vertices
#' are unnamed, returns the string vector of their indices.
#'
#' @param tgraph a dynamic network
#' @return A vector of length(dn) strings
#' @examples
#' dn <- uniform_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)
#' names <- get_tgraph_vertex_names(dn)
#' # [1] "1" "2" "3" "4" "5" "6"
get_tgraph_vertex_names <- function(tgraph) {
  tgraph_is_named <- is_named(tgraph[[1]])
  vertices <- V(tgraph[[1]])
  names <- if(tgraph_is_named) vertices$name else as.character(vertices)
}
