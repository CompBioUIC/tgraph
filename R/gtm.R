#' Group Members
#'
#' Generates the "GM" info (GTM without the T) for a single graph
#'
#' @param g an igraph graph
#' @param group_n_offset optional arg, an offset added to each
#' group number. Useful in group_time_members.
#' @return a "GM" list---a list of group-member vectors. Each
#' vector's first element is the group number, and the remaining
#' elements are the indices of the vertices that are in that group.
#' Note that for unlabeled igraph vertices the vertex index is
#' identical to the vertex.
#' @examples
#' dn <- uniform_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)
group_members <- function(g, group_n_offset=0) {

  g_clusters <- clusters(g)

  group_num_and_members <-function(group_n) {
    member_indices <- which(g_clusters$membership == group_n)
    # return the cluster number prepended to all vertex indices in that cluster
    append(member_indices, group_n+group_n_offset, after=0)
  }

  # for each group number apply the helper
  lapply(1:g_clusters$no, group_num_and_members)
}


#' Group Members Only
#'
#' Like group_members, but without explicitly naming the group numbers
#' (this info instead encoded in the index of each vertex list)
#' @param g an igraph graph
#' @return a list of vectors, where the ith vector indexes the vertices
#' in the ith group (connected subgraph) of g
#' @examples
#' dn <- uniform_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)
group_members_only <- function(g, group_n_offset=0) {

  g_clusters <- clusters(g)

  members <-function(group_n) {
    which(g_clusters$membership == group_n)
  }

  lapply(1:g_clusters$no, members)
}

#' Group Time Members
#'
#' Generates the group-time-member info for a tgraph, useful in such
#' community analysis algorithms as CommDy. Nests results by time.
#'
#' @param tgraph a tgraph
#' @return a list of vectors that is the group-time-member object
#' describing tgraph. Each vector's first element identifies a group
#' number in the tgraph. The second element identifies the time when
#' the group occurs. The rest of the vector indexes the vertices
#' present in that group.
#' @examples
#' tg <- uniform_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)
#' group_time_members(tg)
group_time_members <- function(tgraph) {
  unlist(group_time_members_time_nested(tgraph), recursive=F)
}

#' Group Time Members (nested)
#'
#' Generates the group-time-member info for a tgraph, useful in such
#' community analysis algorithms as CommDy. Nests results by time.
#'
#' @param tgraph a tgraph
#' @return a list of lists (of lists). Top level list is time, bottom
#' level is a line of gtm info
#' @examples
#' tg <- uniform_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)
#' group_time_members_time_nested(tg)
group_time_members_time_nested <- function(tgraph) {
  # just apply the above function, add the group things,
  # and the tricky part will be getting the time offsets?

  all_clusters <- lapply(tgraph, clusters)
  #all_num_groups[[i]] = number of clusters (groups) at tgraph[[i]]
  all_num_groups = sapply(all_clusters, function(c) c$no)
  num_groups_sofar <- cumsum(all_num_groups)

  gtms_for_t_gnum <- function(t,gnum_sofar) {
   gmems_only <- group_members(tgraph[[t]], group_n_offset = gnum_sofar)
   lapply(gmems_only, function(member_list) append(member_list,t,1))
  }

  gtms_for_t <- function(t) {
    if (t==1) return(gtms_for_t_gnum(t, 0))
    else return (gtms_for_t_gnum(t, num_groups_sofar[[t-1]]))
  }

  sapply(1:length(tgraph), gtms_for_t)

}

#TODO
gtm_is_equal <- function(gtm1, gtm2) {

}
