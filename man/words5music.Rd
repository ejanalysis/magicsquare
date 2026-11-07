\name{words5music}
\alias{words5music}
\docType{data}
\title{Five-letter music candidate vocabulary}
\description{A broad vocabulary of 824 five-letter music-related candidates,
including terms, instruments, performers, composers, brands, musical works,
regular noun plurals and verb forms, published abbreviations, and Italian
and French music or dance terminology.
Connections range from direct technical terms to first names and title components.
This is a curated candidate list, not a fully source-verified dictionary.}
\format{A character vector of 824 unique uppercase A-Z words, ordered by decreasing
estimated English frequency with alphabetical ties and unestimated words last.
Its source attribute records the frequency provenance and data licensing.}
\details{Frequency ordering uses wordfreq 3.1.1; see \code{\link{word_frequencies}}
and \file{COMMON-WORDS.md} for source credits and licensing. The editable source is \file{5-letter-words-music.txt} in the installed
package directory. \file{music-word-notes.txt} explains additions, and
\file{MUSIC-WORDS.md} describes inclusion policy, reference examples and searches.}
\examples{
x <- make_squares(words5music, allow_shared_words = TRUE, max_squares = 1)
x$found5
}
