library(compbioalpha)
library(igraph)
context("Graph metrics")

test_that("avg_degree(G) is correct on basic graphs with integer avg. degrees", {
  expect_equal( avg_degree(graph.full(6)), 5 )
  expect_equal( avg_degree(graph.ring(4)), 2 )
  expect_equal( avg_degree(make_graph("cubical"    )), 3 )
  expect_equal( avg_degree(make_graph("octahedron" )), 4 )
  expect_equal( avg_degree(make_graph("icosahedron")), 5 )
})


test_that("uniform_rand_tgraph has correct avg_degree in extreme cases", {
  full_graphs <- uniform_rand_tgraph(t = 4, n_vertices = 4, p_edge = 1.0)
  empt_graphs <- uniform_rand_tgraph(t = 4, n_vertices = 4, p_edge = 0.0)
  expect_equal( avg_degree(full_graphs[[1]]), 3 )
  expect_equal( avg_degree(empt_graphs[[1]]), 0 )
})
