#' @include gtm.R

#' Save GTM
#'
#' Saves a .gtm file given a list of GTM vectors
#'
#' @param gtm a list where each item is a GTM vector
#' @param dir the path to the directory to be saved in.
#' Defaults to the working directory.
#' @param filename the saved file name. Defaults to <current date>.gtm
#' @examples
#' tg <- uniform_rand_tgraph(t=12, n_vertices = 6, p_edge = 0.4)
#' gtm <- group_time_members(tg)
#' save_gtm(gtm=gtm, filename="test.gtm")
save_gtm <- function(gtm, dir = NULL, filename = NULL) {
  if (is.null(dir)) {
    dir = getwd()
  }
  if (is.null(filename)) {
    filename = paste(Sys.time(),".gtm", sep = "")
  }
  fullname <- paste(dir, filename, sep= .Platform$file.sep)

  savefile <- file(fullname, 'w')
  gtm_lines <- lapply(gtm, function(line) paste(line, collapse=" "))
  for(line in gtm_lines) {
    write(line, savefile, append=TRUE)
  }
  print(paste("have saved", fullname))
  close(savefile)
}

#' Load GTM
#'
#' Loads a .gtm file into a list of GTM vectors
#'
#' @param filename the name of the .gtm file to be loaded.
#' @return a list of gtm-line vectors
load_gtm <- function(filename) {

  read_file <- file(filename, 'r')
  out_list <- list()

  while ( TRUE ) {
    this_line = readLines(read_file, n = 1)
    if ( length(this_line) == 0 ) break
    this_line_int_vector <- lapply(strsplit(this_line, " "), strtoi)
    out_list[[length(out_list) + 1]] <- this_line_int_vector
  }

  close(read_file)
  return(unlist(out_list, recursive = F))

}
