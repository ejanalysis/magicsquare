library(magicsquare)

# Every bundled word list follows its documented frequency order.
wordlists <- c('words3', 'words3example', 'words4', 'words4small', 'words4test',
               'words4example', 'words5', 'words5test', 'words5example',
               'words5useful', 'words5music', 'words5common')
for (name in wordlists) {
  words <- get(name)
  score <- word_frequencies$zipf[match(toupper(words), word_frequencies$word)]
  stopifnot(!anyNA(score), all(diff(score) <= 0),
            identical(order(-score, toupper(words), method='radix'), seq_along(words)),
            is.list(attr(words, 'source')))
}
stopifnot(is.character(words5common), length(words5common) == 518,
          !anyDuplicated(words5common), all(grepl('^[A-Z]{5}$', words5common)),
          all(words5common %in% toupper(words5)),
          identical(as.vector(words5common), words5common_frequency$word),
          identical(words5common_frequency$rank, seq_along(words5common)),
          all(words5common_frequency$zipf >= 4.5),
          sum(words5common_frequency$zipf >= 5) == 218,
          isTRUE(all.equal(words5common_frequency$per_million,
                           10^(words5common_frequency$zipf - 3))))
# The documented top-X example searches only its selected vocabulary.
selected <- head(words5, 500)
x <- make_squares(selected, allow_shared_words=TRUE, report=FALSE)
stopifnot(x$complete, x$count_ok_5rows == 3,
          all(toupper(x$found5) %in% toupper(selected)))

# Default report describes full solutions and normalized dictionary size.
words <- c('AIR', 'ROE', 'END', 'ARE', 'ION', 'RED', ' air ')
output <- capture.output(invisible(make_squares(words)))
stopifnot(identical(output, 'Found 2 complete 3x3 squares from 6 unique words.'))
output <- capture.output(invisible(make_squares(words, max_squares=1)))
stopifnot(identical(output[1], 'Found 1 complete 3x3 square from 6 unique words.'),
          grepl('stopped at max_squares; more squares may exist', output[2], fixed=TRUE))
output <- capture.output(invisible(make_squares('ABCDE')))
stopifnot(identical(output, 'Found 0 complete 5x5 squares from 1 unique word.'))
output <- capture.output(invisible(make_squares(words, report=FALSE)))
stopifnot(!length(output),
          inherits(try(make_squares(words, report=NA), silent=TRUE), 'try-error'))
