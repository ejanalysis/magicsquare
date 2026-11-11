# magicsquare

This package finds word squares from a vocabulary you choose. 
A word square is a square arrangement of words such that 
every row reads as a word from left to right, and 
every column reads as a word from top to bottom.


For example, the words "Bible ideal: elder, large beard." 
form a 5x5 symmetric square if arranged like this:
```text
  B I B L E
  I D E A L
  B E A R D
  L A R G E
  E L D E R 
```
Same for "These eight steel seats agree."
```text
  S E A T S
  E I G H T
  A G R E E
  T H E S E
  S T E E L
```

Some favorites where 10 different words are used to form a 5x5 square,
some of which sound like news headlines or poetry:

```text
  G R A P E
  R U L E D
  A L O N G
  F E N C E
  T R E E S

  S U G A R
  T R A D E
  R I S E S
  I N E P T
  P E S T S

  T A R T S
  A W A R E
  S A L O N
  K I L L S
  S T Y L E
  
  S C E N T
  C A N O E
  A R S O N
  R O U S E
  F L E E T

  T A S K S
  A W A I T
  R A L L Y
  T R O L L
  S E N S E

  S O A R S
  L I M I T
  A L O N E
  T E N S E
  E D G E D
```

If you allow a word to appear both across and down, 
you can find symmetric squares like these:
  
```text
  B R A V E
  R U L E D
  A L O N G
  V E N U E
  E D G E S
  
  D E L T A
  E M A I L
  L A R G E
  T I G E R
  A L E R T
```


`make_squares()` results show each square next to its transpose if both exist:
square 1, its transpose, square 2, its transpose, and so on. Symmetric squares
appear once because their transpose is identical. A transpose can be absent
when it would repeat a row word or when a result limit stops the search early.

`print_squares()` displays each pair once, in two columns: the square on the
left and its transpose on the right. For a symmetric square, both columns are
identical. The right-hand view is calculated even if that orientation was not
returned by the search; displaying it does not add a search result.

## Words limited to a specific topic such as music

If you have installed the package, you can just use `library(magicsquare)`. 
Otherwise, from the package checkout, use `pkgload::load_all()`
to load the current code and data.
```r
pkgload::load_all()
```

Find squares using music-related terms. 
Then print them with a space between each letter:

```r
x <- make_squares(words5music, allow_shared_words = TRUE)
print_squares(x$found5)
```

`compact = FALSE` is default and makes the squares easier to read. 
For example, to display
only squares containing ELGAR:

```r
print_squares(x$found5, query = "ELGAR")
```
Music-related word squares: 
  
```text
  C O R P S
  O P E R A
  R E M I X
  P R I C E * 
  S A X E S

  U S H E R
  S C O R E
  H O R N S
  E R N S T **
  R E S T S
```
\* PRICE could refer to either of these:

- Leontyne Price (born 1927): One of the most celebrated American 
operatic sopranos, particularly famous for her performances of Verdi.

- Lloyd Price (1933–2021): Influential R&B and early rock-and-roll singer,
known for “Stagger Lee,” “Personality,” and “Lawdy Miss Clawdy.” 
Inducted into the Rock & Roll Hall of Fame in 1998.

\*\* ERNST could refer to:

- Heinrich Wilhelm Ernst (1814–1865): A celebrated violinist and composer, 
considered one of the greatest virtuosos of the 19th century and 
a rival to Niccolò Paganini. Known for extremely difficult violin works, 
including his arrangement of Schubert's Erlkönig and his Six Polyphonic Studies.


`allow_shared_words = TRUE` permits a word to appear both across and down,
as in this symmetric square. The default is `FALSE`, which forbids that reuse.
Row words must always be distinct.

The bundled 824-word candidate vocabulary currently produces 66 squares with
reuse allowed. One contains the unverified entry ALFIN from the original list.
To exclude that entry and print the remaining 65 squares:

```r
x <- make_squares(setdiff(words5music, "ALFIN"), allow_shared_words = TRUE)
print_squares(x$found5, compact = TRUE)
```

See [vocabulary notes](inst/MUSIC-WORDS.md) and
[individual word connections](inst/music-word-notes.txt) for the inclusion policy
and explanations of names and less familiar terms.

## Use your own words

All bundled word-list vectors, including the full `words5` vocabulary and
`words5music`, are ordered from most to least frequent in English. To search
only the top X words from a specified bundled list:

```r
wordlist <- words5  # Or words5music, words4, words5common, etc.
X <- 500
selected_words <- head(wordlist, X)
x <- make_squares(selected_words, allow_shared_words = TRUE)
print_squares(x$found5, compact = FALSE)  # Use found4 for four-letter words
```

`head()` selects up to X available entries. Custom vectors you supply yourself
need to be frequency-ordered first; `make_squares()` does not rank them.
`word_frequencies` provides the estimates for spellings in the bundled lists.
Equal frequency scores are ordered alphabetically, and words with no estimate
go last. Original spellings and entries have been preserved.

For common English words ranked from most to least frequent, use
`words5common`. Select the first N with `head()`:

```r
head(words5common, 20)
N <- 500
x <- make_squares(head(words5common, N), allow_shared_words = TRUE)
print_squares(x$found5, compact = FALSE)
```

`make_squares()` reports the number of complete squares found and the number
of unique words searched. Add `report = FALSE` to suppress that summary. A
search stopped at `max_squares` reports that more results may exist.

The common list contains 518 five-letter words with wordfreq Zipf scores of
at least 4.5 (about 32 occurrences per million words). The most frequent 218
have scores of at least 5 (100 per million). These are corpus estimates rather
than a universal ordering of English words; ties are alphabetical. A smaller
vocabulary may have no valid square.

Inspect the ranks, scores, or a stricter frequency cutoff with:

```r
head(words5common_frequency)
very_common <- with(words5common_frequency, word[zipf >= 5])
```

See `?words5common` and the [source and licensing notes](inst/COMMON-WORDS.md).

## Smallest top-X vocabulary that makes a 5x5 square

For the current frequency-ordered `words5` list of 3,103 words:

| Across/down reuse | Smallest X | Squares found at X | Squares found at X - 1 | Word added at X |
| --- | ---: | ---: | ---: | --- |
| `allow_shared_words = TRUE` | 493 | 3 | 0 | SEATS |
| `allow_shared_words = FALSE` | 1,455 | 2 | 0 | ROOFS |

These minima were found by binary search and verified with complete searches
at X and X - 1. Adding words cannot eliminate an existing square, so the
zero-result X - 1 checks establish the minima. The findings depend on the
current dictionary and frequency ordering, including alphabetical tie breaks.

With reuse allowed, the top 493 words produce these three symmetric squares:

```text
S E A T S    S E A T S    S E A T S
E I G H T    E I G H T    E I G H T
A G R E E    A G R E E    A G R E E
T H E M E    T H E R E    T H E S E
S T E E L    S T E E L    S T E E L
```

With reuse forbidden, the top 1,455 words produce only these two squares. 
They are transposes of each other: the rows of one are the columns of the other.

```text
S T A R T    S T R A P
T H R E E    T H O S E
R O O F S    A R O S E
A S S E T    R E F E R
P E E R S    T E S T S
```

Reproduce the results and verify the immediately smaller prefixes:

```r
shared <- make_squares(head(words5, 493), allow_shared_words = TRUE)
print_squares(shared$found5, compact = FALSE)

disjoint <- make_squares(head(words5, 1455), allow_shared_words = FALSE)
print_squares(disjoint$found5, compact = FALSE)

shared_before <- make_squares(head(words5, 492), allow_shared_words = TRUE)
disjoint_before <- make_squares(head(words5, 1454), allow_shared_words = FALSE)
stopifnot(shared$complete, disjoint$complete,
          shared_before$complete, disjoint_before$complete,
          shared$count_ok_5rows == 3, disjoint$count_ok_5rows == 2,
          shared_before$count_ok_5rows == 0,
          disjoint_before$count_ok_5rows == 0)
```

## Supply a custom vocabulary

Supply a character vector of equal-length words, with two to five letters:

```r
x <- make_squares(c("AIR", "ROE", "END", "ARE", "ION", "RED"))
print_squares(x$found3, compact = FALSE)
```

For just one complete result, add `max_squares = 1`. See `?make_squares` and
`?print_squares` for the available options.
