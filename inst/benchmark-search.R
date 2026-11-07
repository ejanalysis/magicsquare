# Run from the package root after installing/reloading magicsquare.
# Optional: set original_file to an archived pre-change make_squares.R to
# compare exact result sets and timings on the same machine.
benchmark_search = function(original_file = NULL) {
  reference <- NULL
  if (!is.null(original_file)) {
    reference <- new.env(parent = baseenv())
    sys.source(original_file, reference)
  }
  keys <- function(x) {
    if (is.null(x) || all(is.na(x))) return(character())
    sort(apply(x, 2, paste, collapse = '/'))
  }
  for (name in c('words3example', 'words4example', 'words5example',
                 'words5test', 'words4small', 'words5music', 'words5')) {
    dictionary <- get(name, envir = asNamespace('magicsquare'))
    now <- magicsquare::make_squares(dictionary,
                                    save_2row_hits = TRUE, save_3row_hits = TRUE)
    cat(name, length(dictionary), 'words;', now$elapsed_seconds, 'seconds;',
        now[[paste0('count_ok_', nchar(dictionary[1]), 'rows')]], 'squares\n')
    if (!is.null(reference)) {
      before_time <- system.time(invisible(capture.output(
        before <- reference$make_squares(dictionary,
                                         save_2row_hits = TRUE, save_3row_hits = TRUE)
      )))[['elapsed']]
      for (depth in 2:nchar(dictionary[1])) {
        count <- paste0('count_ok_', depth, 'rows')
        found <- paste0('found', depth)
        stopifnot(isTRUE(all.equal(before[[count]], now[[count]])),
                  identical(keys(before[[found]]), keys(now[[found]])))
      }
      cat('  Original:', before_time, 'seconds; counts and sets match\n')
    }
  }
  invisible(NULL)
}
