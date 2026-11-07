library(magicsquare)

# Independent exhaustive oracle: enumerate every sequence of distinct row words,
# construct column strings, and scan the dictionary for each column prefix.
oracle <- function(words, shared = FALSE) {
  words <- sort(unique(toupper(words)))
  n <- nchar(words[1])
  counts <- numeric(n)
  hits <- list()
  visit <- function(rows) {
    for (word in setdiff(words, rows)) {
      next_rows <- c(rows, word)
      depth <- length(next_rows)
      if (depth >= 2) {
        columns <- apply(do.call(rbind, strsplit(next_rows, '')), 2, paste0, collapse = '')
        available <- if (shared) words else setdiff(words, next_rows)
        if (!all(vapply(columns, function(prefix) any(startsWith(available, prefix)), logical(1)))) next
        counts[depth] <<- counts[depth] + 1
      }
      if (depth == n) hits[[length(hits) + 1L]] <<- next_rows else visit(next_rows)
    }
  }
  visit(character())
  list(counts = counts[-1], squares = sort(vapply(hits, paste, '', collapse = '/')))
}
keys <- function(x) if (is.null(x)) character() else sort(apply(x, 2, paste, collapse = '/'))
set.seed(421)
for (n in 2:5) {
  pool <- expand.grid(rep(list(c('A', 'B')), n), stringsAsFactors = FALSE)
  pool <- apply(pool, 1, paste, collapse = '')
  for (iteration in 1:4) {
    words <- sample(pool, min(length(pool), 9L))
    for (shared in c(FALSE, TRUE)) {
      expected <- oracle(words, shared)
      actual <- make_squares(words, allow_shared_words = shared,
                             save_2row_hits = TRUE, save_3row_hits = TRUE)
      stopifnot(isTRUE(all.equal(unname(unlist(actual[seq_len(n-1)])), expected$counts)),
                identical(keys(actual[[paste0('found', n)]]), expected$squares),
                actual$complete)
    }
  }
}
# Both orientations of a general (asymmetric) square must be returned.
x <- make_squares(c('AIR', 'ROE', 'END', 'ARE', 'ION', 'RED'))
stopifnot(setequal(keys(x$found3), c('AIR/ROE/END', 'ARE/ION/RED')),
          is.na(x$count_ok_4rows), is.na(x$count_ok_5rows), is.null(x$found2))
# Default rule excludes symmetric squares; opt-in permits them.
w <- c('BALL', 'AREA', 'LEAD', 'LADY')
stopifnot(make_squares(w)$count_ok_4rows == 0,
          make_squares(w, allow_shared_words = TRUE)$count_ok_4rows == 1)
x <- make_squares(c('AIR', 'ROE', 'END', 'ARE', 'ION', 'RED'), max_squares = 1)
stopifnot(x$count_ok_3rows == 1, !x$complete,
          length(capture.output(invisible(make_squares(w)))) == 0)
# Normalization, tiny dictionaries, and malformed input.
stopifnot(identical(keys(make_squares(c(w, ' ball '), allow_shared_words=TRUE)$found4),
                    keys(make_squares(w, allow_shared_words=TRUE)$found4)),
          make_squares('ABCDE')$count_ok_5rows == 0)
for (bad in list(character(), NA_character_, c('AA', 'BBB'), 'A-B', '', 123, 'ABCDEF')) {
  stopifnot(inherits(try(make_squares(bad), silent=TRUE), 'try-error'))
}
for (bad in list(0, -1, 1.5, NA_real_, c(1, 2), '1', -Inf)) {
  stopifnot(inherits(try(make_squares(w, max_squares=bad), silent=TRUE), 'try-error'))
}
# The text source and installed data agree, and every entry is five A-Z letters.
stopifnot(length(words5music) == length(unique(words5music)),
          all(grepl('^[A-Z]{5}$', words5music)))
x <- make_squares(words5music, allow_shared_words=TRUE)
stopifnot(x$complete, 'ELGAR/LORDE/GRIEG/ADELE/REGER' %in% keys(x$found5))
for (i in seq_len(ncol(x$found5))) {
  rows <- x$found5[,i]
  columns <- apply(do.call(rbind, strsplit(rows, '')), 2, paste, collapse='')
  stopifnot(all(rows %in% words5music), all(columns %in% words5music), !anyDuplicated(rows))
}

# Repeated column words remain permitted under the original rule.
x <- make_squares(c('AA', 'BB', 'AB'))
stopifnot('AA/BB' %in% keys(x$found2))
# Native code allocates during search: exercise protection under forced GC.
gctorture(TRUE)
x <- make_squares(c('AIR', 'ROE', 'END', 'ARE', 'ION', 'RED'))
gc_words <- apply(expand.grid(LETTERS[1:3], LETTERS[1:3]), 1, paste, collapse='')
gc_result <- make_squares(gc_words, allow_shared_words=TRUE)
gctorture(FALSE)
stopifnot(x$count_ok_3rows == 2, gc_result$count_ok_2rows == 72)
