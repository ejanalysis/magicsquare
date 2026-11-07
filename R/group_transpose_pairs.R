# Group complete squares with their transposes, preserving the first encountered
# orientation and group order. Only reorder existing results: a symmetric square
# or a square with an absent transpose stays as a single result.
group_transpose_pairs = function(squares) {
  if (is.null(squares) || ncol(squares) < 2L) return(squares)
  n <- nrow(squares)
  keys <- apply(squares, 2L, paste, collapse = '/')
  transpose_keys <- rep('', ncol(squares))
  for (column in seq_len(n)) {
    word <- do.call(paste0, lapply(seq_len(n), function(row) {
      substr(squares[row, ], column, column)
    }))
    transpose_keys <- if (column == 1L) word else paste0(transpose_keys, '/', word)
  }
  partners <- match(transpose_keys, keys)
  seen <- rep(FALSE, ncol(squares))
  positions <- integer(ncol(squares))
  used <- 0L
  for (i in seq_len(ncol(squares))) {
    if (seen[i]) next
    used <- used + 1L
    positions[used] <- i
    seen[i] <- TRUE
    partner <- partners[i]
    if (!is.na(partner) && !seen[partner]) {
      used <- used + 1L
      positions[used] <- partner
      seen[partner] <- TRUE
    }
  }
  squares[, positions, drop = FALSE]
}
