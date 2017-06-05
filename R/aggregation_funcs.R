library(igraph)


aggregate <- function(tgraph, step=2, agg_func = union) {

  t_total <- length(tgraph)
  n_steps <- t_total / step

  lapply(1:n_steps, make_aggregator(tgraph, step, agg_func))

}

# TODO: ask Tanya for help naming this function
# it duplicates the results of a many-to-one graph aggregation
# in order to make it many-to-many dimensionally symmetric
square_agg_func <- function(agg_func = union) {

  function(graphs) {

    res <- do.call(agg_func, graphs)

    f <- function(t) {
      res
    }

    lapply(1:length(graphs), f)
  }


}

saf <- function(graphs, agg_func = igraph::union) {

    res <- do.call(agg_func, graphs)

    f <- function(t) {
      graph_attr(res, "name") <- paste("Graph copy at time ",t)
      res
    }

    lapply(1:length(graphs), f)

}



squared_union <- function(graphs) {

  f <- square_agg_func(union)
  return(f(graphs))

}

make_aggregator <- function(tgraph, step=2, agg_func = union) {

  t_total <- length(tgraph)
  n_steps <- t_total / step

  aggregator <- function(step_num) {
    start_index <- (step_num-1)*step + 1
    end_index <- min(start_index + step - 1, t_total)
    do.call(agg_func, tgraph[start_index:end_index])
  }

  aggregator
}

aggregator <- function(tgraph, agg_func = union) {
  function(start, fin) {
    saf(tgraph[start:fin], agg_func)
  }
}

#' Applies an aggregation function such as union to a tgraph with a fixed
#' aggregation window
#'
#' This function divides tgraph into subgroups of length step_size, and applies
#' agg_func to each
#'
#' @param tgraph a dynamic network
#' @param step_size the aggregation step size
#' @param agg_func a function that transforms two or more igraphs into one
#' @return the dynamic network that is the described aggregation of tgraph
apply_aggregator <- function(tgraph, agg_func = union, step_size = 2) {

  agger <- aggregator(tgraph, agg_func)
  meta_agger <- function(step_num) {
    start <- (step_num-1) * step_size + 1
    end   <- min(length(tgraph), step_num*step_size)
    agger(start,end)
  }

  num_step_sizes = ceiling(length(tgraph)/step_size)
  unlist(lapply(1:num_step_sizes, meta_agger ), recursive = FALSE)

}


#' Applies an aggregation function such as union to a tgraph for a range of
#' possible aggregation windows
#'
#' This function applies apply_aggregator to a range of step_size values
#'
#' @param tgraph a dynamic network
#' @param agg_func a function that transforms two or more igraphs into one
#' @param max_stepsize the maximum aggregation step size
#' @return A list of max_stepsize dynamic networks

all_aggregations <- function(tgraph, agg_func, max_stepsize) {

  if (missing(agg_func)) agg_func <- union
  if (missing(max_stepsize)) max_stepsize <- length(tgraph)

  aggregate_with <- function(step_size) {
    apply_aggregator(tgraph, agg_func, step_size)
  }

  lapply(1:max_stepsize, aggregate_with)

}


