# magicsquare

Find word squares from a vocabulary you choose: every row reads as a word
from left to right, and every column reads as a word from top to bottom.

## Try the music vocabulary

From the package checkout, load the current code and data:

```r
pkgload::load_all()
```

For an installed copy, use `library(magicsquare)` instead. Then find the music
squares and print them with a space between each letter:

```r
x <- make_squares(words5music, allow_shared_words = TRUE)
print_squares(x$found5, compact = FALSE)
```

`compact = FALSE` makes the squares easier to read. For example, to display
only squares containing ELGAR:

```r
print_squares(x$found5, query = "ELGAR", compact = FALSE)
```

The square's letters are displayed like this:

```text
E L G A R
L O R D E
G R I E G
A D E L E
R E G E R
```

`allow_shared_words = TRUE` permits a word to appear both across and down,
as in this symmetric square. The default is `FALSE`, which forbids that reuse.
Row words must always be distinct.

The bundled 607-word candidate vocabulary currently produces 17 squares with
reuse allowed. One contains the unverified entry ALFIN from the original list.
To exclude that entry and print the remaining 16 squares:

```r
x <- make_squares(setdiff(words5music, "ALFIN"), allow_shared_words = TRUE)
print_squares(x$found5, compact = FALSE)
```

See [vocabulary notes](inst/MUSIC-WORDS.md) and
[individual word connections](inst/music-word-notes.txt) for the inclusion policy
and explanations of names and less familiar terms.

## Use your own words

Supply a character vector of equal-length words, with two to five letters:

```r
x <- make_squares(c("AIR", "ROE", "END", "ARE", "ION", "RED"))
print_squares(x$found3, compact = FALSE)
```

For just one complete result, add `max_squares = 1`. See `?make_squares` and
`?print_squares` for the available options.
