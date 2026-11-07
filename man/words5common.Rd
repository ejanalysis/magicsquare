\name{words5common}
\alias{words5common}
\alias{words5common_frequency}
\docType{data}
\title{Common Five-Letter English Words Ranked by Frequency}
\description{Common words from the bundled five-letter dictionary, ordered
from most to least frequent using English estimates from wordfreq 3.1.1.
Use the first N words to restrict a square search to the most frequent entries.}
\format{\code{words5common} is a character vector of 518 unique uppercase
five-letter words. \code{words5common_frequency} is a data frame with the same
518 words in the same order and four columns:
\describe{
\item{rank}{Position in this filtered five-letter vocabulary, starting at 1.}
\item{word}{Uppercase five-letter word.}
\item{zipf}{Estimated log10 occurrences per billion words.}
\item{per_million}{Approximate occurrences per million words, computed as
\code{10^(zipf - 3)} from the rounded Zipf score.}
}
Both objects have a \code{source} attribute containing provenance, attribution,
license information, filtering rules, and source-file checksums.}
\details{The list intersects \code{words5} with wordfreq's English large-list
estimates, retains only exactly five ASCII letters with Zipf scores of at least
4.5 (about 32 occurrences per million words), and sorts by decreasing score.
Tied scores are ordered alphabetically. The first 218 words meet the stricter
Zipf threshold of 5 (100 occurrences per million words).

This is an operational frequency cutoff for common words, not a guarantee of
individual familiarity. It is a ranking within the bundled dictionary, not all
possible English words. Plurals, inflections, and common words whose spellings
also occur as names are retained. Case and senses are pooled in the source
estimates. Source corpora cover several genres through approximately 2021;
rankings vary with corpus and dialect, and these are not current-year counts.
A small subset may be unable to form any complete square.

The data are derived from Robyn Speer's wordfreq and distributed under
Creative Commons Attribution-ShareAlike 4.0, separately from the package's
code license. See \file{COMMON-WORDS.md} in the installed package directory
and \code{attr(words5common, "source")} for credits and reproduction details.}
\source{\url{https://github.com/rspeer/wordfreq}

Robyn Speer (2022). rspeer/wordfreq: v3.0 (v3.0.2). Zenodo.
\doi{10.5281/zenodo.7199437}. Extracted with wordfreq version 3.1.1.}
\examples{
head(words5common, 20)
N <- 500
x <- make_squares(head(words5common, N), allow_shared_words = TRUE)
print_squares(x$found5, compact = FALSE)
head(words5common_frequency)
# The most frequent 218 entries:
very_common <- with(words5common_frequency, word[zipf >= 5])
}
