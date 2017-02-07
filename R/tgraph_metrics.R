# Functions here all operate on a Dynamic Network and return a number

library(igraph)

avg_netfunc <- function(tgraph, graph_func) {
  mean(sapply(tgraph, graph_func))
}

var_netfunc <- function(tgraph, graph_func) {
  var(sapply(tgraph, graph_func))
}

sd_netfunc <- function(tgraph, graph_func) {
  sd(sapply(tgraph, graph_func))
}

sd_avg_degree <- function(dg) {
  sd_netfunc(dg, avg_degree)
}

avg_avg_degree <- function(dg) {
  avg_netfunc(dg, avg_degree)
}

var_avg_degree <- function(tgraph) {
  var_netfunc(tgraph, avg_degree)
}

unit_var_avg_degree <- function(tgraph) {
  vars <- var_avg_degree(tgraph)
  sds  <- sd_avg_degree( tgraph)
  vars / sds
}

