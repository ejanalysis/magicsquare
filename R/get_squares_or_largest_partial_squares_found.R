
get_squares_or_largest_partial_squares_found = function(resultslist) {

  # if just the data.frame like found4 is provided, return that as-is
  # if the full list of out of make_squares() is provided, return the best data.frame of at least partial results

  if (is.list(resultslist) && !is.data.frame(resultslist)) {
    # get the squares or partial squares that are nxn, for the largest n where it found any squares of nxn
    if (is.na(resultslist$count_ok_5rows) || resultslist$count_ok_5rows %in% 0) {
      # no 5 row results (5x5 squares)
      if (is.na(resultslist$count_ok_4rows) || resultslist$count_ok_4rows %in% 0) {
        # no 4-row results (4x4 squares nor 5x4 partial squares)
        if (is.na(resultslist$count_ok_3rows) || resultslist$count_ok_3rows %in% 0) {
          # not even any 3-row results (3x3 squares nor partial squares of 4x3, 5x3)
          warning("no squares or partial squares found with at least 3 good rows")
          return(NULL)
        } else {
          # 3-row results were found
          squares <- resultslist$found3
        }
      } else {
        # 4-row results were found
        squares <- resultslist$found4
      }
    } else {
      # 5-row results were found
      squares <- resultslist$found5
    }
    # if (nchar(hitstable$found5[1,1]) %in% 5) {
    #
    # }
  } else {
    squares <- resultslist
  }
  return(squares)
}
