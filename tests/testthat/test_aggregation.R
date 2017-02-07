library(compbioalpha)
library(igraph)
context("tgraph Aggregation")


#test_that()

test_that("Aggregate has correct dimensionality behavior in simple cases", {
  G12 <- dyn_rand_graph(t = 12, n_vertices = 4, tij_edge_func = tij_fully_connected)
  expect_equal( 12, length(G12) )
  expect_equal(  6, length(aggregate(G12, step=2)) )
  expect_equal(  4, length(aggregate(G12, step=3)) )
  expect_equal(  3, length(aggregate(G12, step=4)) )
  expect_equal(  2, length(aggregate(G12, step=6)) )
})
