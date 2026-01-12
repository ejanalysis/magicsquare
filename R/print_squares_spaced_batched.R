################## ################### ################### #

# LOOK AT A TABLE OF MULTIPLE MAGIC SQUARES

print_squares_spaced_batched = function(hitstable, spaced = TRUE) {

  # if list of results provided, just get best portion
  found <- get_squares_or_largest_partial_squares_found(hitstable)

  # probably obsolete
  if (is.vector(found)) {found <- cbind(found)}

  # make it easier to read tables
  if (spaced) {
  found <- insertspaces(found)
  }

  # print in sets of 10 columns
  if (NCOL(found) %/% 10  > 0) { # at least 10 squares found
    for (batch in 1:(NCOL(found) %/% 10)) {
      print(found[, (1 + ((batch-1)*10)):(10 + ((batch-1)*10)), drop=FALSE])
    }
    batchesdone = (NCOL(found) %/% 10)
  } else {
    batchesdone = 0
  }
  if (NCOL(found) %% 10 > 0) {
    batch = 1 + batchesdone
    print(found[, (1 + ((batch-1)*10)):NCOL(found) , drop=FALSE])
  }
  return(NULL)
}
################## ################### ################### #

#  MAKE IT EASIER TO LOOK AT A TABLE OF MULTIPLE MAGIC SQUARES

#' @export
#'
insertspaces = function(datf) {

  stopifnot(!is.null(datf), all(!is.na(datf)))
  insertspaces_vec = function(vec) {
    x = sapply(vec, function(z) paste0(paste0( unlist(strsplit(z, split = NULL)), collapse = " "), " "))
    names(x) <- NULL
    x
  }
  x = apply(datf, 2, function(mycol) {  insertspaces_vec(mycol)  })
  colnames(x) <- NULL
  data.frame(x)
}
################## ################### ################### #
