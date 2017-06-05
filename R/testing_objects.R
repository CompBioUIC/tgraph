#' @include tgraph_generators.R
#' @include timeordered_compatibility.R
#' @include tgraph_generators.R
#' @include twin.R

# some tgraphs
smallTG <- uniform_rand_tgraph(t=4, n_vertices = 4, p_edge = 0.5)
tg <- uniform_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)

# testing tgraph_generators' oversampling stuff
tg_duplicated <- oversample_replicate(tg, 4)
tg6 <- oversample_replicate(tg, 6)
tg_distributed <- oversample_distribute(tg, 2)

# copy below line into console to see plot (sans comment ofc)
# tp <- twin_plot(tg, title_str = "uniform random tgraph TWIN plot")

# testing the timeordered compatibility stuff
library(timeordered)
data(ants)
tonetwork <- generatetonetwork(ants)

to <- make_timeordered(tg)
to_to_tg <- make_tgraph(to)
