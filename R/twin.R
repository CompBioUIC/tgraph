library(igraph)



#' Apply a tgraph metric on a given tgraph and its time-step-wise aggregations
#'
#' @param tgraph a dynamic network
#' @param tgraph_metric a function mapping a dynamic network to a numeric value.
#' @param agg_func an aggregation function that takes two or more igraph graphs
#'  and returns a single graph, e.g. union.
#' @param max_step_size the largest number of time steps to be aggregated at
#'  once by the aggregation function.
#' @return A max_agg_step - length vector of tgraph_metric applied to the
#'  aggregations of tgraph.
#' @examples
#' dn <- uniform_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)
#' avg_degree_variance_vs_agg_step <- metric_vs_agg_step(
#'    tgraph = dn,
#'    tgraph_metric = var_avg_degree
#' )
metric_vs_agg_step <- function(tgraph, tgraph_metric, agg_func, max_agg_step)
{

  if (missing(tgraph_metric)) tgraph_metric = avg_avg_degree
  if (missing(agg_func)) agg_func = union

  all_aggs <- all_aggregations(tgraph, agg_func, max_agg_step)

  sapply(all_aggs, tgraph_metric)

}

# DEPRECATED: returns computed aggregation step
twin <- function(tgraph, tgraph_metric = avg_avg_degree, agg_func) {

  which.min(metric_vs_agg_step(tgraph, tgraph_metric, agg_func))

}


#' Implements the TWIN method, producing a plot from which we determine the
#' relevant time step size in a dynamic network.
#'
#' This plot compares the compression ratio and variance of average degree
#' of the various dynamic networks that result aggregating the input network
#' at various time step sizes.
#'
#' @inheritParams metric_vs_agg_step
#' @param tgraph_metric a function mapping a dynamic network to a numeric value.
#' @param agg_func an aggregation function that takes two or more igraph graphs
#'  and returns a single graph, e.g. union.
#' @param title_str the tile of the produced plot
#' @return A max_step_size - length vector of tgraph_metric applied to the
#'  aggregations of tgraph.
#' @examples
#' dn <- uniform_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)
#' triplicated_dn <- oversample_replicate(dn, 3)
#' twin_plot(triplicated_dn)
twin_plot <- function(tgraph, tgraph_metric, agg_func, title_str, ylab, max_agg_step) {


  # setting optional args
  if (missing(tgraph_metric)) tgraph_metric <- unit_var_avg_degree
  if (missing(ylab))         ylab         <- "variance of avg. degree"
  if (missing(agg_func))     agg_func     <- igraph::union
  if (missing(title_str))    title_str    <- "TWIN Plot"
  if (missing(max_agg_step)) max_agg_step <- length(tgraph)

  all_aggs <- all_aggregations(tgraph, agg_func, max_agg_step)
  metric_vec      <- sapply(all_aggs, tgraph_metric)
  compression_vec <- sapply(all_aggs, compression_ratio)


  metric_col_str <- "red"
  compression_col_str <- "darkblue"
  margin_val <- 6.0
  margin_vec <- c(margin_val, margin_val, margin_val, margin_val)


  par(mar=margin_vec)
  plot(metric_vec, type="l", col=metric_col_str, ylab = NA, xlab = "aggregation window", cex=5,  yaxt='n')
  title(title_str)

  #draw twin line
  #annotation_height <- mean(c(max(metric_vec),min(metric_vec)))
  #abline(v = twin_ans, lty=3)
  #text(twin_ans, annotation_height, "twin value", srt=90, pos=2)

  # fine-graned ticks & y axis label
  axis(1, at=c(1:length(tgraph)), labels=F)
  axis(2, col.ticks=metric_col_str, col.axis=metric_col_str)
  mtext(ylab, side=2, line=3, col=metric_col_str)

  par(new = T, mar=margin_vec)
  plot(compression_vec,type="l", col=compression_col_str, axes=F, xlab=NA, ylab=NA, pch=16)
  axis(side = 4, col.ticks=compression_col_str, col.axis=compression_col_str, tck=-0.04)
  y_range <- c(floor(min(compression_vec)):ceiling(max(compression_vec)))

  axis(4, at=y_range, col=compression_col_str, labels=F)
  mtext("compression ratio", side=4, line=3, col=compression_col_str)

}
