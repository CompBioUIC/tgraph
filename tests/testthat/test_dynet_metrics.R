library(compbioalpha)
library(igraph)
context("tgraph metrics")


#test_that()

test_that("uniform_rand_tgraph has correct avg_avg_degree in extreme cases", {
  full_graphs <- uniform_rand_tgraph(t = 4, n_vertices = 4, p_edge = 1.0)
  empt_graphs <- uniform_rand_tgraph(t = 4, n_vertices = 4, p_edge = 0.0)
  expect_equal( avg_avg_degree(full_graphs), 3 )
  expect_equal( avg_avg_degree(empt_graphs), 0 )
})
