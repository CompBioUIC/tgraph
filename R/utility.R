library(igraph)

#' Calculate the dimensions of a rectangle with n items and n columns.
#'
#' This function tells you the number of rows and columns necessary to arrange
#' n items in a rectangle.
#'
#' The bottom row of the rectangle might have fewer items than above rows, but
#' never zero items.
#'
#' @param n_items the integer number of items to be placed in a rectangular grid
#' @param n_cols the number of columns (or desired width) of the rectangle
#' @return a vector of length 2 containing the (width, height) of the rectangle
#' @examples
#' rectangularize(3, 2) # c(2,2), a 3x2 grid could hold 3 objects in two columns
#' rectangularize(7, 2) # c(4,2), a 4x2 grid would hold 7 objects in two columns
#' rectangularize(8, 2) # c(4,2)
#' rectangularize(9, 3) # c(3,3)
rectangularize <- function(n_items, n_cols = 2) {
  n_rows <- ceiling(n_items / n_cols)
  c(n_rows, n_cols)
}


#' returns the number that is half as close to one as the input number
#'
#' @param x a number
#' @return 1 - (1-x)/2
#' @examples
#' zero_point_seven_five <- half_as_close_to_one(0.5)
#' two <- half_as_close_to_one(3)
half_as_close_to_one <- function(x) {
  1 - (1-x)/2
}

#' calculates the number of vertices in a dynamic network
#'
#' @param tgraph a dynamic network
#' @return the number of vertices in tgraph
#' @examples
#' dn <- uniform_rand_tgraph(t=2, n_vertices = 6)
#' six <- tgraph_n_vertices(dn)
tgraph_n_vertices <- function(tgraph) {
  vcount(tgraph[[1]])
  # as opposed to: max(sapply(vcount, tgraph))
}
