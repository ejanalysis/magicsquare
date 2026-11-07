library(magicsquare)

transpose <- function(rows) {
  apply(do.call(rbind, strsplit(rows, '')), 2L, paste, collapse='')
}
key <- function(rows) paste(rows, collapse='/')

# Deliberately interleave two pairs, a symmetric square, and an unpaired square.
a <- c('AIR', 'ROE', 'END')
b <- c('CAT', 'ARE', 'RED')
symmetric <- c('APE', 'PEA', 'EAR')
unpaired <- c('ABC', 'DEF', 'GHI')
input <- cbind(a, b, symmetric, transpose(a), unpaired, transpose(b))
expected <- input[, c(1, 4, 2, 6, 3, 5), drop=FALSE]
actual <- magicsquare:::group_transpose_pairs(input)
stopifnot(identical(actual, expected),
          identical(magicsquare:::group_transpose_pairs(actual), actual),
          identical(magicsquare:::group_transpose_pairs(NULL), NULL),
          identical(magicsquare:::group_transpose_pairs(input[,1,drop=FALSE]),
                    input[,1,drop=FALSE]))

# Exercise the actual search output for all supported sizes and both rules.
# Every available nonsymmetric partner must be immediately adjacent.
dictionaries <- list(c('AA', 'AB', 'BA', 'BB'), words3example,
                     words4small, words5example)
for (words in dictionaries) {
  for (shared in c(FALSE, TRUE)) {
    result <- make_squares(words, allow_shared_words=shared,
                           save_2row_hits=TRUE, save_3row_hits=TRUE, report=FALSE)
    n <- nchar(words[1])
    squares <- result[[paste0('found', n)]]
    if (is.null(squares)) next
    keys <- apply(squares, 2L, key)
    transpose_keys <- apply(squares, 2L, function(rows) key(transpose(rows)))
    partner <- match(transpose_keys, keys)
    paired <- which(!is.na(partner) & partner != seq_along(partner))
    stopifnot(all(abs(paired - partner[paired]) == 1L),
              ncol(squares) == result[[paste0('count_ok_', n, 'rows')]],
              !anyDuplicated(keys))
  }
}
# Symmetric and missing orientations are never inserted or duplicated.
x <- make_squares(c('BALL','AREA','LEAD','LADY'), allow_shared_words=TRUE,report=FALSE)
stopifnot(ncol(x$found4) == 1)
x <- make_squares(c('AA','BB','AB'), report=FALSE)
stopifnot(ncol(x$found2) == 1, identical(unname(x$found2[,1]), c('AA','BB')))
x <- make_squares(words3example, max_squares=1, report=FALSE)
stopifnot(ncol(x$found3) == 1, !x$complete)
