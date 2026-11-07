#' Find word squares using indexed prefix search
#'
#' @param wordlist Character vector of equal-length words, with 2 to 5 letters.
#'   Words are trimmed, uppercased, and deduplicated; only A-Z are accepted.
#' @param quiet Whether to suppress timing output. The console summary is
#'   controlled separately by \code{report}.
#' @param save_2row_hits Whether to retain plausible two-row partial squares.
#' @param save_3row_hits Whether to retain plausible three-row partial squares.
#' @param allow_shared_words Allow a word to appear both across and down.
#'   Defaults to FALSE, preserving the original search rule. Row words must
#'   always be distinct. Column words may repeat, as in the original function.
#' @param max_squares Stop after this many complete squares (default Inf).
#'   A finite limit can omit transposes and makes partial counts incomplete.
#' @param report Whether to print a console summary of the number of complete
#'   squares found and unique dictionary words searched (default TRUE).
#' @returns A list with the original count_ok_2rows through count_ok_5rows,
#'   found2 through found5, and timing fields. Each column of a found matrix
#'   contains the row words of one square. Unsaved partial hits are NULL.
#'   Counts beyond the word length are NA. Additional fields elapsed_seconds,
#'   complete, and candidates_checked describe the search. complete is FALSE
#'   whenever the result limit stops the search, even if no further hits exist.
#' @details
#' Column prefixes are indexed in a compiled trie. At each depth the search intersects
#' position bitsets to select only row words whose letters can extend every
#' column. It then checks that each extended column has an available dictionary
#' completion. This searches general across/down squares, including asymmetric
#' squares, rather than assuming the rows equal the columns.
#'
#' A complete unlimited search includes a transpose whenever its row words are
#' distinct and it satisfies the same rules. Partial counts describe
#' prefixes with an individually available completion for every column; they
#' do not guarantee that all columns can be completed together.
#' @examples
#' # Find music squares and print with spaces between letters:
#' x = make_squares(words5music, allow_shared_words = TRUE)
#' print_squares(x$found5, compact = FALSE)
#' # Show just the squares containing ELGAR:
#' print_squares(x$found5, query = 'ELGAR', compact = FALSE)
#'
#' # Search only the top X most frequent words in a specified bundled list:
#' wordlist = words5
#' X = 500
#' x_common = make_squares(head(wordlist, X), allow_shared_words = TRUE)
#' print_squares(x_common$found5, compact = FALSE)
#'
#' x3 = make_squares(c('AIR', 'ROE', 'END', 'ARE', 'ION', 'RED'))
#' x4 = make_squares(words4example)
#' x4$found4
#' # A symmetric square requires allowing words across and down:
#' make_squares(c('BALL', 'AREA', 'LEAD', 'LADY'), allow_shared_words = TRUE)
#' @useDynLib magicsquare, .registration = TRUE
#' @export
make_squares = function(wordlist,
                        quiet = TRUE,
                        save_2row_hits = FALSE,
                        save_3row_hits = FALSE,
                        allow_shared_words = FALSE,
                        max_squares = Inf,
                        report = TRUE) {
  started <- proc.time()[['elapsed']]
  if (!is.character(wordlist) || !length(wordlist) || anyNA(wordlist)) {
    stop('wordlist must be a nonempty character vector without NA')
  }
  wordlist <- sort(unique(toupper(trimws(wordlist))))
  sizes <- unique(nchar(wordlist))
  if (length(sizes) != 1L || !sizes %in% 2:5 ||
      any(!grepl('^[A-Z]+$', wordlist))) {
    stop('wordlist must contain only A-Z words of the same length (2 to 5)')
  }
  for (flag in list(quiet, save_2row_hits, save_3row_hits, allow_shared_words, report)) {
    if (!is.logical(flag) || length(flag) != 1L || is.na(flag)) {
      stop('search flags must be single TRUE or FALSE values')
    }
  }
  if (!is.numeric(max_squares) || length(max_squares) != 1L ||
      is.na(max_squares) || max_squares < 1 ||
      (is.finite(max_squares) && max_squares != floor(max_squares))) {
    stop('max_squares must be a positive integer or Inf')
  }
  n <- sizes
  retain <- c(FALSE, save_2row_hits || n == 2L,
              save_3row_hits || n == 3L, TRUE, TRUE)
  search <- .Call(C_search_squares, wordlist, allow_shared_words, retain,
                  as.double(max_squares))
  counts <- search[[1L]]
  found <- lapply(seq_len(5L), function(depth) {
    if (depth > n) return(NA)
    ids <- search[[2L]][[depth]]
    if (is.null(ids)) return(NULL)
    matrix(wordlist[ids], nrow = depth)
  })
  elapsed <- unname(proc.time()[['elapsed']] - started)
  timing <- paste0('Time elapsed: ', round(elapsed, 3L), ' seconds')
  if (report) {
    cat(sprintf('Found %s complete %dx%d %s from %s unique %s.\n',
                format(counts[n], scientific = FALSE, trim = TRUE), n, n,
                if (counts[n] == 1) 'square' else 'squares',
                format(length(wordlist), scientific = FALSE, trim = TRUE),
                if (length(wordlist) == 1) 'word' else 'words'))
    if (!search[[3L]]) {
      cat('Search stopped at max_squares; more squares may exist.\n')
    }
  }
  if (!quiet) cat(timing, '\n')
  result <- c(stats::setNames(as.list(counts[2:5]), paste0('count_ok_', 2:5, 'rows')),
              stats::setNames(found[2:5], paste0('found', 2:5)))
  c(result, list(timing = timing, elapsed_seconds = elapsed,
                 complete = search[[3L]], candidates_checked = search[[4L]]))
}
