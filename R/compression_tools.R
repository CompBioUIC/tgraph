library(igraph)

compress <- function(object) {

}

compression_ratio <-function(object, tmp_dir = "./tmp/") {

  nonce <- as.numeric(as.POSIXct(Sys.time())) # currrent time
  compressed_fname <- paste0(tmp_dir, nonce, ".zip")
  uncompressed_fname <- paste0(tmp_dir, nonce, ".RData")

  save(object, file = uncompressed_fname, compress=FALSE)
  save(object, file =   compressed_fname, compress=TRUE)

  uncompressed_size <- file.info(uncompressed_fname)$size
  compressed_size   <- file.info(  compressed_fname)$size

  file.remove(uncompressed_fname)
  file.remove(  compressed_fname)

  1 - compressed_size / uncompressed_size

}
