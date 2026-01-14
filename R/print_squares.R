#' look at results to see if a given word or string query was among the squares found
#'
#' @param hitstable output of [make_squares()] function
#' @param query word or partial word, query for grepl, ignoring case
#' @param compact set FALSE to print with spaces between letters - easier to read the tables
#'
#' @returns prints squares that had the query term
#'
#' @export
#'
print_squares = function(hitstable, query="*", compact = TRUE) {

  word <- query
  if (length(word) != 1 || class(word) != "character" || is.list(word) ||
      missing(hitstable) || "character" %in% class(hitstable)|| (!is.list(hitstable) && !is.data.frame(hitstable) && !is.array(hitstable)) ) {
    stop("must provide hitstable and optionally query, where query is single character string query and hitstable is list of results or single data.frame of results")
  }

  squares <- get_squares_or_largest_partial_squares_found(hitstable)

  if (NROW(squares) %in% 0) {
    warning("no squares found")
    return(NULL)
  }

  if (NROW(squares) %in% c(3, 4, 5)) {

    ########################################################################### #
    filter_using_query_term_across_only = function(z, word) {

      # search in both the regular tables and the transposed_versions_of_input
      # squares <- transpose_each_square(squares)

      if (NROW(z) == 5) {
        out = data.frame(z[ ,
                                  grepl(word, z[1,], ignore.case = TRUE) |
                                    grepl(word, z[2,], ignore.case = TRUE) |
                                    grepl(word, z[3,], ignore.case = TRUE) |
                                    grepl(word, z[4,], ignore.case = TRUE) |
                                    grepl(word, z[5,], ignore.case = TRUE)
        ])
      }

      if (NROW(z) == 4) {
        out = data.frame(z[ ,
                                  grepl(word, z[1,], ignore.case = TRUE) |
                                    grepl(word, z[2,], ignore.case = TRUE) |
                                    grepl(word, z[3,], ignore.case = TRUE) |
                                    grepl(word, z[4,], ignore.case = TRUE)
        ])
      }

      if (NROW(z) == 3) {
        out = data.frame(z[ ,
                                  grepl(word, z[1,], ignore.case = TRUE) |
                                    grepl(word, z[2,], ignore.case = TRUE) |
                                    grepl(word, z[3,], ignore.case = TRUE)
        ])
      }
      return(out)
    }
    ########################################################################### #

    out_found_in_across <- filter_using_query_term_across_only(
      squares, word
    )

    ##   the transposed versions are already in the output of make_squares()
    ## if we are looking at N x N but not if looking at partial results like the
    ## 5 x 3 squares where only 3 rows were found.
    ## But, dont bother to search for the query word in the partial squares.

    # sdown = transpose_each_square(squares)
    #
    # out_found_in_down <- filter_using_query_term_across_only(
    #   sdown, word
    # )

  } else {
    stop("can only work with results where 3, 4,or 5 rows worked, and also were saved (in the case of partial results, via save_3row_hits=T), in trying to create squares")
  }

  colnames(out_found_in_across) <- NULL
  # colnames(out_found_in_down) <- NULL
  n <- NCOL(out_found_in_across)

  if (NCOL(out_found_in_across) == 0) {
    cat('no matching squares found reading left to right\n')
    out_found_in_across <- NULL
  } else {
    print_squares_spaced_batched(out_found_in_across, spaced = !compact)

    # if (NROW(out_found_in_across) < NROW(out_found_in_down)) {
    #   extrarows = NROW(out_found_in_down) - NROW(out_found_in_across)
    #   for (i in 1:extrarows) {
    #     out_found_in_across = rbind(out_found_in_across, NA) # adds a row to make across and down results same NROW
    #   }
    # }
    colnames(out_found_in_across) <- 1:NCOL(out_found_in_across)
  }
  # if (NCOL(out_found_in_down) == 0) {
  #   cat('no matching squares found reading down each column\n')
  #   out_found_in_down <- NULL
  # } else {
  #   cat("\nTRANSPOSED VERSIONS OF THOSE", n,"SQUARES \n(i.e., swapping words reading across vs reading down in a column \n")
  #   print_squares_spaced_batched(out_found_in_down, spaced = !compact)
  #
  #   # if partial squares, NROW will differ between these 2
  #   if (NROW(out_found_in_across) > NROW(out_found_in_down)) {
  #     extrarows = NROW(out_found_in_across) - NROW(out_found_in_down)
  #     for (i in 1:extrarows) {
  #       out_found_in_down = rbind(out_found_in_down, NA) # adds a row to make across and down results same NROW
  #     }
  #   }
  #   colnames(out_found_in_down) <- paste0("T", 1:NCOL(out_found_in_down))
  # }
outall <- out_found_in_across
  # outall <- cbind(out_found_in_across, out_found_in_down)

  invisible(outall)
}
