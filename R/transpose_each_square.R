############################################################################

# xy transpose this 1 column of words into 1 column of words
# each row has 1 word we split up

transpose_1column = function(hitstable_column) {
  cbind(apply(
    data.frame(sapply(
      X = hitstable_column,
      FUN = function(z) {
        as.vector(strsplit(z, '')) # wordsincolumn2lettermatrix, split word into letters
      })),
    MARGIN = 1, # each row
    FUN = function(y) {
      paste0(y, collapse = "") # lettermatrix2wordsincolumn, collapse letters back together as a word (and transpose inherently this way)
    }
  ))
}
############################################################################

transpose_each_square = function(hitstable) {

  x =as.data.frame(
    apply(hitstable, MARGIN=2,
          FUN = transpose_1column)
  )
  colnames(x) <- NULL
  return(x)
}
############################################################################
