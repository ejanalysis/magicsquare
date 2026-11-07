\name{word_frequencies}
\alias{word_frequencies}
\docType{data}
\title{English Frequency Estimates for Bundled Vocabulary Spellings}
\description{Estimates used to order the bundled word lists, from wordfreq
3.1.1's English large wordlist. Estimates pool case and word senses.}
\format{A data frame of 9,521 distinct uppercase spellings with columns:
\describe{
\item{word}{Uppercase spelling.}
\item{letters}{Number of letters.}
\item{zipf}{Log10 estimated occurrences per billion words; 0 means no available
estimate, not zero usage.}
\item{per_million}{Approximate occurrences per million words, calculated from the
rounded Zipf score; NA where no estimate is available.}
}
Rows are ordered by length, then decreasing Zipf score, then alphabetically.
The \code{source} attribute holds attribution, license, method, and source checksum.}
\details{Data and derived rankings are licensed under CC BY-SA 4.0. Source:
wordfreq by Robyn Speer, with freely available SUBTLEX data by Marc Brysbaert,
Boris New and collaborators, OpenSubtitles, Google Books Ngrams, Wikipedia,
and other sources acknowledged by wordfreq. See \file{COMMON-WORDS.md} and
\code{attr(word_frequencies, "source")} for credits and limitations.}
\source{\url{https://github.com/rspeer/wordfreq}}
\examples{
head(word_frequencies[word_frequencies$letters == 5, ])
}
