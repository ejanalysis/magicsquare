\name{wordlists}
\alias{words3}
\alias{words3example}
\alias{words4}
\alias{words4small}
\alias{words4test}
\alias{words4example}
\alias{words5}
\alias{words5test}
\alias{words5example}
\alias{words5useful}
\docType{data}
\title{Bundled Word Lists Ordered by Estimated English Frequency}
\description{Existing three-, four-, and five-letter vocabularies for word-square
searches. Each character vector is ordered from most to least frequent using
English wordfreq 3.1.1 estimates, with uppercase alphabetical tie breaks and
unestimated words last. Entries and original spelling have been preserved.}
\format{Character vectors: \code{words3} (500 words), \code{words3example} (6),
\code{words4} (5,526), \code{words4small} (998), \code{words4test} (300),
\code{words4example} (12), \code{words5} (3,103), \code{words5test} (300),
\code{words5example} (49), and \code{words5useful} (1,096).
Each has a \code{source} attribute documenting frequency provenance and licensing.}
\details{\code{head(wordlist, X)} selects up to X most frequent words within
that list, not the most frequent X words in all of English. For an explicitly
common five-letter subset, use \code{words5common}. Scores are available in
\code{word_frequencies}. Frequencies pool spelling across case and senses,
including names. The original broad lists can contain rare words.

Frequency-based ordering is derived from Robyn Speer's wordfreq 3.1.1 English
large-list estimates. These derived rankings use the CC BY-SA 4.0 data license;
see the embedded \code{source} attributes and \file{COMMON-WORDS.md} for credits,
source limitations, and reproduction instructions.}
\seealso{\code{\link{words5common}}, \code{\link{words5music}},
\code{\link{word_frequencies}}, \code{\link{make_squares}}}
\examples{
wordlist <- words5
X <- 500
x <- make_squares(head(wordlist, X), allow_shared_words = TRUE)
print_squares(x$found5, compact = FALSE)
}
