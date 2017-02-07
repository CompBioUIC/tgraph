library(compbioalpha);
context("Base test");

test_that("Something is happening", {
  expect_equal(ret_true(), TRUE)
})

test_that("I can make plots with tests", {
  g <- graph(edges=c(1,2, 2,3, 3, 1, 4,1), n=5, directed = F);
  simple_graph_plot(g);
})
