\name{words5music}
\alias{words5music}
\docType{data}
\title{Five-letter music candidate vocabulary}
\description{A broad vocabulary of 607 five-letter music-related candidates,
including terms, instruments, performers, composers, brands, and musical works.
Connections range from direct technical terms to first names and title components.
This is a curated candidate list, not a fully source-verified dictionary.}
\format{A character vector of 607 unique uppercase A-Z words.}
\details{The editable source is \file{5-letter-words-music.txt} in the installed
package directory. \file{music-word-notes.txt} explains additions, and
\file{MUSIC-WORDS.md} describes inclusion policy, reference examples and searches.}
\examples{
x <- make_squares(words5music, allow_shared_words = TRUE, max_squares = 1)
x$found5
}
