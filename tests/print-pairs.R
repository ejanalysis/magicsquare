library(magicsquare)

rows <- c('START','THREE','ROOFS','ASSET','PEERS')
columns <- c('STRAP','THOSE','AROSE','REFER','TESTS')
input <- cbind(rows, columns)
output <- capture.output(returned <- print_squares(input, compact=FALSE))
stopifnot(sum(grepl('^Square ', output)) == 1,
          identical(output[1], 'Square 1     Transpose'),
          identical(output[2], 'S T A R T    S T R A P'),
          identical(output[6], 'P E E R S    T E S T S'),
          identical(unname(as.matrix(returned)), unname(input)))
# Compute the displayed transpose even if the input contains only one orientation.
limited <- capture.output(print_squares(input[,1,drop=FALSE], compact=FALSE))
stopifnot(identical(limited, output))
# A query can match the right-hand orientation alone.
filtered <- capture.output(returned <- print_squares(input[,1,drop=FALSE],
                                                     query='THOSE', compact=FALSE))
stopifnot(identical(filtered, output), ncol(returned) == 1)
# Symmetric squares display in both columns, with one block per input square.
symmetric <- c('ELGAR','LORDE','GRIEG','ADELE','REGER')
output <- capture.output(print_squares(cbind(symmetric, symmetric), compact=FALSE))
stopifnot(sum(grepl('^Square ', output)) == 1,
          identical(output[2], 'E L G A R    E L G A R'))
# Multiple pairs, even interleaved in external input, are displayed once each.
a <- c('AIR','ROE','END')
b <- c('CAT','ARE','RED')
at <- c('ARE','ION','RED')
bt <- c('CAR','ARE','TED')
output <- capture.output(print_squares(cbind(a,b,at,bt), compact=TRUE))
stopifnot(sum(grepl('^Square ', output)) == 2,
          any(grepl('AIR +ARE', output)), any(grepl('CAT +CAR', output)))
# Partial results retain all rows and clearly identify transposed prefixes.
output <- capture.output(print_squares(cbind(c('SCRAP','ALIBI','BEGAN')),compact=FALSE))
stopifnot(grepl('^Partial 1 +Transpose \\(prefixes\\)$', output[1]),
          any(grepl('S A B$', output)), length(output) == 6)
output <- capture.output(returned <- print_squares(input, query='ZZZZZ'))
stopifnot(identical(output, 'no matching squares found'), is.null(returned))
# Passing the result list works, with the same pairing and formatting.
x <- make_squares(head(words5,1455),report=FALSE)
output <- capture.output(print_squares(x,compact=FALSE))
stopifnot(sum(grepl('^Square ', output)) == 1)
