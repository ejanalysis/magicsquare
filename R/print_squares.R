#' Print each square beside its transpose
#'
#' @param hitstable Output of [make_squares()], or a matrix/data frame with
#'   one square's row words in each column. Three to five saved rows are supported.
#' @param query A word or regular expression to match in either orientation,
#'   ignoring case. The default \code{"*"} displays all pairs.
#' @param compact Set FALSE to print spaces between letters.
#'
#' @returns Invisibly returns a data frame of matching input columns, or NULL
#'   if none match. Printing groups transpose pairs, without changing the input
#'   results or adding columns to the returned data frame.
#' @details
#' Each displayed block has two columns: the square and its transpose. If both
#' orientations occur in the input, their pair is displayed once. A symmetric
#' square is displayed with identical left and right columns. The right column
#' is computed for display even if that orientation is absent from the input,
#' for example after a search stops at max_squares. It does not add a found hit.
#'
#' Rectangular partial results are shown beside their transposes, with blank
#' padding as needed. The transposed strings are column prefixes, rather than
#' complete dictionary words, and are labeled accordingly.
#' @examples
#' # Each square is displayed next to its transpose, once per pair:
#' x = make_squares(head(words5, 1455), allow_shared_words = FALSE)
#' print_squares(x$found5, compact = FALSE)
#'
#' # Music squares, including symmetric ones:
#' x = make_squares(words5music, allow_shared_words = TRUE)
#' print_squares(x$found5, compact = FALSE)
#' print_squares(x$found5, query = 'ELGAR', compact = FALSE)
#' @export
print_squares = function(hitstable, query="*", compact = TRUE) {
  if (missing(hitstable) ||
      !(is.list(hitstable) || is.matrix(hitstable) || is.data.frame(hitstable))) {
    stop('hitstable must be make_squares() results or a matrix/data frame of row words')
  }
  if (!is.character(query) || length(query) != 1L || is.na(query)) {
    stop('query must be a single character string')
  }
  if (!is.logical(compact) || length(compact) != 1L || is.na(compact)) {
    stop('compact must be TRUE or FALSE')
  }
  squares <- get_squares_or_largest_partial_squares_found(hitstable)
  if (is.null(squares) || NROW(squares) == 0L || NCOL(squares) == 0L) {
    warning('no squares found')
    return(invisible(NULL))
  }
  squares <- as.matrix(squares)
  if (!is.character(squares) || anyNA(squares) || !nrow(squares) %in% 3:5 ||
      length(unique(as.vector(nchar(squares)))) != 1L ||
      nchar(squares[1L]) < nrow(squares) || nchar(squares[1L]) > 5L) {
    stop('provide three to five saved rows of equal-length words, each three to five letters')
  }
  seen <- new.env(hash = TRUE, parent = emptyenv())
  matched <- logical(ncol(squares))
  displayed <- 0L
  spaced <- function(words) {
    if (compact) return(words)
    vapply(strsplit(words, '', fixed = TRUE), paste, character(1L), collapse = ' ')
  }
  for (i in seq_len(ncol(squares))) {
    rows <- squares[, i]
    columns <- apply(do.call(rbind, strsplit(rows, '', fixed = TRUE)),
                     2L, paste, collapse = '')
    matched[i] <- identical(query, '*') || any(grepl(query, c(rows, columns), ignore.case = TRUE))
    if (!matched[i]) next
    key <- paste(rows, collapse = '/')
    if (exists(key, envir = seen, inherits = FALSE)) next
    assign(key, TRUE, envir = seen)
    assign(paste(columns, collapse = '/'), TRUE, envir = seen)
    displayed <- displayed + 1L
    partial <- length(rows) != length(columns)
    header <- paste(if (partial) 'Partial' else 'Square', displayed)
    left <- spaced(rows)
    right <- spaced(columns)
    height <- max(length(left), length(right))
    left <- c(left, rep('', height - length(left)))
    right <- c(right, rep('', height - length(right)))
    width <- max(nchar(c(header, left))) + 4L
    if (displayed > 1L) cat('\n')
    cat(format(header, width = width, justify = 'left'),
        if (partial) 'Transpose (prefixes)' else 'Transpose', '\n', sep = '')
    cat(paste0(format(left, width = width, justify = 'left'), right), sep = '\n')
  }
  if (!displayed) {
    cat('no matching squares found\n')
    return(invisible(NULL))
  }
  out <- as.data.frame(squares[, matched, drop = FALSE], stringsAsFactors = FALSE)
  colnames(out) <- seq_len(ncol(out))
  invisible(out)
}
