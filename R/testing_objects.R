library(igraph)
#require(tgraph)

smallDN <- tgraph::uniform_rand_tgraph(t=4, n_vertices = 4, p_edge = 0.5)



#dn <- uniform_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)

#dn_duplicated <- oversample_replicate(dn, 4)
#dn6 <- oversample_replicate(dn, 6)
#dn_distributed <- oversample_distribute(dn, 2)

#tp <- twin_plot(dn, title_str = "uniform random tgraph TWIN plot")


#dntotal <- saf(dn, union)
#dntotal_1_6 <- saf(dn[1:6], union)
#dntotal_7_12 <- saf(dn[7:12], union)
#dn_6 <- append (dntotal_1_6, dntotal_7_12)


library(timeordered)
data(ants)
tonetwork <- generatetonetwork(ants)

# to <- make_timeordered(dn)
# to_to_dn <- make_tgraph(to)
