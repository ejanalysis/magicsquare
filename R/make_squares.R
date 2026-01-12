# WORD SQUARE MAKER
# ALGORITHMS TO HELP MAKE WORD SQUARES  -- VERY SLOW, INEFFICIENT AND HARDCODED TO 5 LETTER BOX SIZE
# e.g. it checks up to 2x what is needs to since any square or partial square checked means
#  you don't have to check the version that is reflected/rotated along the line from upper left to lower right.
################## ################### ################### #


#' try to create magic squares given a word list
#'
#' @param wordlist vector of character string words (all the same number of characters, either all with 4 or all with 5)
#' @param quiet whether to print partial successes, attempts
#' @param save_2row_hits whether to save/return partial successes of just 2 rows that work
#' @param save_3row_hits whether to save/return partial successes of just 3 rows that work
#'
#' @returns a list with counts of plausible combinations
#'   for 2,3,4,5 rows, and matrices of found partial or full squares
#'  The results include each valid table and the transposed version of it as well,
#'
#' @examples
#'  make_squares(c('AIR', 'ROE', 'END', 'ARE', 'ION', 'RED'))
#'
#'  x = make_squares(  )
#'  x$found4
#'  transpose_each_square(x$found4)
#'
#' @export
#'
make_squares = function(wordlist,
                    quiet = TRUE,
                    save_2row_hits = FALSE,
                    save_3row_hits = FALSE) {

  wordlist = sort(unique(toupper(wordlist)))

  n <- sapply(wordlist, nchar)
  if (2 %in% n) {save_2row_hits <- TRUE}
  if (3 %in% n) {save_3row_hits <- TRUE}
  if (length(unique(n)) > 1) {stop("not all have same number characters")}
  n <- unique(n)
  ######################### #  ######################### #
  hitcounts <- function(query, wordvector, ignore.case=TRUE) {
    # how many matches within each element of x vector?
    ######################### #
    grepn <- function(pattern, x, ignore.case=TRUE) {
      info <- gregexec(pattern = pattern, text = x, ignore.case = ignore.case)
      sapply(info, function(z) ifelse(z[1] == -1, 0, length(z)))
    }
    ######################### #
    colSums(sapply(query, grepn, wordvector, ignore.case) )
  }
  ######################### #  ######################### #
  count <- hitcounts(LETTERS, wordlist)
  letter_freq <- data.frame(letter=LETTERS, count = count)
  letter_freq <- letter_freq[order(letter_freq$count, decreasing = T), ]
  splitwords <- function(words) {strsplit(toupper(words), split = NULL)}
  ######################### #
  wordscores <- function(words) {
    xl <- splitwords(words)
    unlist(
      lapply(xl,
             function(vec) {
               sum(letter_freq$count[match(vec, letter_freq$letter)])
             }
      )
    )
  }
  ######################### #
  wordfreq <- wordscores(wordlist)
  # cbind(wordlist, wordfreq)
  wordlist <- wordlist[order(wordfreq, decreasing = T)]
  allowed_letters_by_slot <- lapply(1:n, function(z) {
    unique(substr(wordlist, z, z))
  } )

  # MATRIX OF LETTERS one row per word one col per slot

  wordletters <- splitwords(wordlist)
  wordletters <- matrix(unlist(wordletters), ncol = n,byrow = T)
  # setequal(unique(wordletters[ , 2]) , allowed_letters_by_slot[[2]])
  word_ok_in_col_i <- function(word, i) {
    ##  allowed_letters_by_slot is global precalculated
    wordletters <- splitwords(word)
    all(wordletters[[1]] %in% allowed_letters_by_slot[[i]])
  }
  allowed_words_by_col <- list()
  for (icol in 1:n) {
    allowed_words_by_col[[icol]] <- wordlist[sapply(wordlist, function(thisword) word_ok_in_col_i(thisword, icol))]
  }

  wordlist_first2letters = substr(wordlist,1,2)
  wordlist_first3letters = substr(wordlist,1,3)
  wordlist_first4letters = substr(wordlist,1,4)
  count_ok_2rows = 0
  count_ok_3rows = 0
  count_ok_4rows = 0
  count_ok_5rows = 0
  found2 = NULL
  found3 = NULL
  found4 = NULL
  found5 = NULL
  word2okmatrix <- matrix(0, nrow = length(wordlist), ncol = length(wordlist))
  rownames(word2okmatrix) <- colnames(word2okmatrix) <- wordlist

  cat("WORDLIST HAS ", length(wordlist), "unique words \n")
  ############################################################################# #
  ############################################################################# #

  for (w1 in 1:length(wordlist)) {

    if (!quiet) {cat(paste0("\nFor ", wordlist[w1], " in row 1 (", w1,"/",length(wordlist),"), checking row 2 candidates "))}
    ############################################################################# #
    # check 2d row (word) here

    for (w2 in 1:length(wordlist)) {

      if (w1 == w2) {next}## disallows same word to be used in 2 rows
      if (!quiet) {
        if (length(wordlist) < 200) {cat(paste0(wordlist[w2], ","))} else {if (w2 %% 100 == 0) cat(".")}
      } else {
        if (w2 %% 500 == 0) {cat(".")}
      }
      word2okmatrix[w1,w2] <-     if (
        ## disallows same word to be used in a row and a column
        (   paste0(wordletters[w1, 1], wordletters[w2, 1]) %in% wordlist_first2letters[c(-w1,-w2)])
        && (paste0(wordletters[w1, 2], wordletters[w2, 2]) %in% wordlist_first2letters[c(-w1,-w2)])
        && (paste0(wordletters[w1, 3], wordletters[w2, 3]) %in% wordlist_first2letters[c(-w1,-w2)])
        && ifelse(n<4, TRUE, paste0(wordletters[w1, 4], wordletters[w2, 4]) %in% wordlist_first2letters[c(-w1,-w2)])
        && ifelse(n<5, TRUE, paste0(wordletters[w1, 5], wordletters[w2, 5]) %in% wordlist_first2letters[c(-w1,-w2)])
      ) {TRUE} else {FALSE}
      count_ok_2rows <- count_ok_2rows + sum(word2okmatrix[w1,w2])

      if (sum(word2okmatrix[w1,w2]) > 0 ) {
        #cat("\n 1st 2 rows seemed ok: \n");  cat(paste0(c(wordlist[w1], wordlist[w2]), collapse = " \n")); cat("\n\n")
        if (save_2row_hits) {
          found2 <- cbind(  found2, cbind(c(wordlist[w1], wordlist[w2])))
        } else {
          found2 <- "0"
        }
        ############################################################################# #
        # check 3d row here

        word3ok <- vector()

        for (w3 in 1:length(wordlist)) {

          if (w3 %in% c(w1,w2)) {next}
          if (!quiet) {        if (length(wordlist) < 200) {cat(paste0(wordlist[w3], ","))}}

          word3ok[w3] <- if (
            (   paste0(wordletters[w1, 1], wordletters[w2, 1], wordletters[w3, 1]) %in% wordlist_first3letters[c(-w1,-w2,-w3)])
            && (paste0(wordletters[w1, 2], wordletters[w2, 2], wordletters[w3, 2]) %in% wordlist_first3letters[c(-w1,-w2,-w3)])
            && (paste0(wordletters[w1, 3], wordletters[w2, 3], wordletters[w3, 3]) %in% wordlist_first3letters[c(-w1,-w2,-w3)])
            && ifelse(n<4, TRUE, paste0(wordletters[w1, 4], wordletters[w2, 4], wordletters[w3, 4]) %in% wordlist_first3letters[c(-w1,-w2,-w3)])
            && ifelse(n<5, TRUE, paste0(wordletters[w1, 5], wordletters[w2, 5], wordletters[w3, 5]) %in% wordlist_first3letters[c(-w1,-w2,-w3)])
          ) {TRUE} else {FALSE}
          count_ok_3rows <- count_ok_3rows + (word3ok[w3])

          if (word3ok[w3]) {
            # cat(paste0("\n 1st 3 rows plausible: \n", paste0(c(wordlist[w1], wordlist[w2], wordlist[w3]), collapse = " \n")),  "\n\n")
            # found3 = c(found3, list(c(wordlist[w1], wordlist[w2], wordlist[w3])))
            if (save_3row_hits) {
              found3 <- cbind(found3, cbind(c(wordlist[w1], wordlist[w2], wordlist[w3])))
            } else {
              found3 <- 0
            }
            ############################################################################# #
            if (n > 3) {
              #  check 4th row here

              word4ok <- vector()

              for (w4 in 1:length(wordlist)) {

                if (w4 %in% c(w1,w2,w3)) {next()}
                word4ok[w4] <- if (
                  (   paste0(wordletters[w1, 1], wordletters[w2, 1], wordletters[w3, 1], wordletters[w4, 1]) %in% wordlist_first4letters[c(-w1,-w2,-w3, -w4)])
                  && (paste0(wordletters[w1, 2], wordletters[w2, 2], wordletters[w3, 2], wordletters[w4, 2]) %in% wordlist_first4letters[c(-w1,-w2,-w3, -w4)])
                  && (paste0(wordletters[w1, 3], wordletters[w2, 3], wordletters[w3, 3], wordletters[w4, 3]) %in% wordlist_first4letters[c(-w1,-w2,-w3, -w4)])
                  && ifelse(n<4, TRUE, paste0(wordletters[w1, 4], wordletters[w2, 4], wordletters[w3, 4], wordletters[w4, 4]) %in% wordlist_first4letters[c(-w1,-w2,-w3, -w4)])
                  && ifelse(n<5, TRUE, paste0(wordletters[w1, 5], wordletters[w2, 5], wordletters[w3, 5], wordletters[w4, 5]) %in% wordlist_first4letters[c(-w1,-w2,-w3, -w4)])
                ) {TRUE} else {FALSE}
                count_ok_4rows <- count_ok_4rows +  (word4ok[w4])
                if (word4ok[w4]) {
                  if (!quiet) {
                    cat(paste0("\n 1st 4 rows plausible: \n", paste0(c(wordlist[w1], wordlist[w2], wordlist[w3], wordlist[w4]), collapse = " \n")),  "\n")
                  }
                  found4 <- cbind(found4, cbind(c(wordlist[w1], wordlist[w2], wordlist[w3], wordlist[w4])))

                  ############################################################################# #
                  if (n > 4) {
                    #  check 5th row here

                    word5ok <- vector()

                    for (w5 in 1:length(wordlist)) {

                      if (w5 %in% c(w1,w2,w3,w4)) {next()}
                      word5ok[w5] <- if (
                        (   paste0(wordletters[w1, 1], wordletters[w2, 1], wordletters[w3, 1], wordletters[w4, 1], wordletters[w5, 1]) %in% wordlist[c(-w1,-w2,-w3, -w4, -w5)])
                        && (paste0(wordletters[w1, 2], wordletters[w2, 2], wordletters[w3, 2], wordletters[w4, 2], wordletters[w5, 2]) %in% wordlist[c(-w1,-w2,-w3, -w4, -w5)])
                        && (paste0(wordletters[w1, 3], wordletters[w2, 3], wordletters[w3, 3], wordletters[w4, 3], wordletters[w5, 3]) %in% wordlist[c(-w1,-w2,-w3, -w4, -w5)])
                        && ifelse(n<4, TRUE, paste0(wordletters[w1, 4], wordletters[w2, 4], wordletters[w3, 4], wordletters[w4, 4], wordletters[w5, 4]) %in% wordlist[c(-w1,-w2,-w3, -w4, -w5)])
                        && ifelse(n<5, TRUE, paste0(wordletters[w1, 5], wordletters[w2, 5], wordletters[w3, 5], wordletters[w4, 5], wordletters[w5, 5]) %in% wordlist[c(-w1,-w2,-w3, -w4, -w5)])
                      ) {TRUE} else {FALSE}
                      count_ok_5rows <- count_ok_5rows +  (word5ok[w5])
                      if (word5ok[w5]) {
                        cat(paste0("\n POSSIBLE FULL SQUARE: \n", paste0(c(wordlist[w1], wordlist[w2], wordlist[w3], wordlist[w4], wordlist[w5]), collapse = " \n")),  "\n\n")
                        found5 <- cbind(  found5, cbind(c(wordlist[w1], wordlist[w2], wordlist[w3], wordlist[w4], wordlist[w5])))
                      }
                    }
                  } else {found5 <- NA; count_ok_5rows <- NA}
                }
              } # next candidate for 4th row
            } else {found4 <- NA; count_ok_4rows <- NA}
          }
          # cat(sum(word4ok)) # only for that choice of first 3 rows
        } # next candidate for 3d row
        # cat("for this choice of first 2 rows, the count of pairs where 1st 3 rows seem OK= ", sum(word3ok), "\n")
      }

    } # next candidate for 2d row
  } # next candidate for 1st row
  ############################################################################# #

  # found2 = unique(found2)
  # found3 = unique(found3)
  # found4 = unique(found4)
  # found5 = unique(found5)

  cat("\n\n\n\n")
  cat(paste0(
    "Count of combos of 2 words that are plausible for 1st 2 rows: ", count_ok_2rows, "\n",
    "Count of combos of 3 words that are plausible for 1st 3 rows: ", count_ok_3rows, "\n",
    ifelse(n<4, "", paste0("Count of combos of 4 words that are plausible for 1st 4 rows: ", count_ok_4rows, "\n")),
    ifelse(n<5, "", paste0("Count of combos of 5 words that are plausible for 1st 5 rows: ", count_ok_5rows, "\n")),
    "\n"
  ))
  # print(word2okmatrix)
  return(list(
    count_ok_2rows = count_ok_2rows,
    count_ok_3rows = count_ok_3rows,
    count_ok_4rows = count_ok_4rows,
    count_ok_5rows = count_ok_5rows,
    found2 = found2,
    found3 = found3,
    found4 = found4,
    found5 = found5
  ))
  ######################### #

  ### not sure this is needed for speed if wordlist is fairly short
  #
  # indexes by 1 letter, 2d, etc. so that can search fast for word with X in slot N,
  # like if need a word with AB--- can search for only
  # where L1 is A & L2 is B.
  # How store that structure, index?
  #   **Faster to convert letters to integers, or to Factors in R:
  #   tibble or data table or data frame: 1 row per word,
  # 1 col for word & 5 factor cols (1 per slot or letter).
  #
  # slowest search algorithm:
  #   top row (for each word)
  # 2d row (for each word)
  # 1down (for each word) fit?
  #   if fits, try 2downs
  # if fits, try 3downs & row3?
  #
  #   faster algorithms:
  #
  #   sort or index by count of frequency of letters in each slot,
  # to start trying likely winners first, so
  # if T is most common first letter,
  # try those first as row1 so col1 is more likely to work too, and
  # also if N is most common in slot 2 then row1 makes that slot1 of col2, so
  # ***start by trying row1 words that have ALL their letters as common 1st letters ( so all 5 down words are easier to fit.)
  #
  # for (irow in 1:5) {
  #   # for row 1, try each word in row 1
  #   # for each word in row 1, try each word in col 1
  #   # if possible in col 1
  #
  # }

  # *** when trying row 2 words, start with words that have all 5 letters as commonly found in slot2.

  # pre-index by letter pairs or even trios too?
  #   if have rows 1&2, can quickly check up to 5 letter pairs made by  rows 1&2, in cols 1-5, to see if any words have that letter pair?
}
######################### ########################## ####### #
