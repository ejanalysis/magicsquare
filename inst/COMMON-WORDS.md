# Common five-letter English words

`words5common` contains 518 unique uppercase words in descending estimated
English frequency order. `head(words5common, N)` selects the N most frequent
entries available in this vocabulary. `words5common_frequency` contains the
same entries with rank, Zipf score, and approximate occurrences per million.

## All bundled vocabularies

All bundled word-list vectors, including the full `words5`, music vocabulary,
and three- and four-letter lists, now use the same descending frequency order.
Original membership and case were preserved. No-estimate words have Zipf score
0 and are placed last, with uppercase alphabetical ties. The editable text
vocabularies use the same ordering. `word_frequencies` records the 9,521 unique
spellings scored across the bundled lists; unestimated per-million values are
NA. `squares5useful` is a set of search results and was not reordered.

Use `wordlist <- words5; X <- 500` followed by
`make_squares(head(wordlist, X), allow_shared_words = TRUE)` to search just the
top X entries of that specified list. Other bundled vectors can be substituted.

## Inclusion and ordering

- Start with the package's existing `words5` dictionary (the editable source
  is `inst/5-letter-words-3k.txt`). This limits raw corpus tokens and names.
- Score lowercase spellings with wordfreq 3.1.1, English `large` wordlist.
- Retain exactly five ASCII letters and a Zipf score of at least 4.5, equivalent
  to roughly 32 occurrences per million words. No manual frequency estimates
  or artificial padding are used.
- Uppercase and sort by decreasing Zipf score, breaking ties alphabetically.
  Source scores are binned to two decimal places; tie order is not evidence
  that one word is more frequent than another.

The first 218 words have Zipf scores of at least 5, equivalent to at least
100 occurrences per million. Use that threshold for an especially frequent
subset. Common inflections and plurals remain. The corpus aggregates spelling
across senses and case, so names can contribute to frequencies of words such
as BROWN or MILES. The cutoff measures frequency, not a reader's familiarity.
The vocabulary is limited to the bundled dictionary; this is not a complete
list of all English five-letter words.

The English estimates combine several text genres. The source describes its
data as a snapshot through about 2021. Ranking depends on the corpus and is
not a universal English order or a current-year measurement. The entire list
currently produces three symmetric squares with `allow_shared_words = TRUE`
and zero squares under the default across/down exclusion rule. The first 500
words also produce those three squares. Smaller subsets can produce none.

## Source, attribution, and data license

Source: [wordfreq](https://github.com/rspeer/wordfreq), by **Robyn Speer**,
version **3.1.1**. Citation: Robyn Speer (2022). *rspeer/wordfreq: v3.0*
(v3.0.2). Zenodo. <https://doi.org/10.5281/zenodo.7199437>.

These derived frequency rankings, common-word vocabulary, and frequency objects are distributed under
[Creative Commons Attribution-ShareAlike 4.0](https://creativecommons.org/licenses/by-sa/4.0/).
This data license is separate from the R package's code license. Changes are
filtering to the bundled dictionary and frequency cutoff, uppercasing, sorting,
and calculation of approximate per-million rates from rounded Zipf scores.
Attribution and licensing are embedded in the `source` attributes of the
ranked word lists and frequency objects as well as provided in this document.
Retain the appropriate credits,
license, and indication of modifications when redistributing derived data.

Underlying sources credited by wordfreq include Google Books Ngrams
(<https://books.google.com/ngrams>), Wikipedia, OPUS OpenSubtitles 2018 /
OpenSubtitles (<https://www.opensubtitles.org/>), SUBTLEX, and other web, news,
and social-media corpora described in wordfreq's README. SUBTLEX is freely
available data by Marc Brysbaert, Boris New, and collaborators; credit the
SUBTLEX authors as required by wordfreq's distribution terms. Relevant work:
Brysbaert & New (2009), *Behavior Research Methods*, 41(4), 977-990; and
Brysbaert et al. (2011), *Experimental Psychology*, 58, 412-424. See the
[upstream source and license notes](https://github.com/rspeer/wordfreq#license)
for full source acknowledgements. No wordfreq Python code is bundled here.

## Reproduction

From the source checkout, install `wordfreq==3.1.1` in a separate Python build
environment and run `python data-raw/build-word-frequencies.py` followed by
`python data-raw/build-words5common.py`. The first script reorders existing word
vectors and text sources without changing their entries; the second creates
both `.rda` files with base R and records SHA-256 hashes of the candidate
vocabulary and English frequency data inside the objects. Python and wordfreq
are build-only tools, not runtime dependencies of magicsquare. The packaged
R objects work offline.
