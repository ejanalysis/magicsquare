# Five-letter music vocabulary

`5-letter-words-music.txt` is the editable source for the packaged `words5music`
vector. It contains 607 unique uppercase A-Z entries: the 176 original words
plus 431 additions. `music-word-notes.txt` explains each addition. The list is
intentionally broad and remains a candidate vocabulary, not a fully verified
lexicon. Original words remain included even where their connection is broad.

Names can be first names or surnames (e.g. MILES and DAVIS), and familiar name
components are permitted. Accents, spaces, and punctuation can be removed
(BJORK, FAURE, DRDRE, MYWAY). Plurals and musical inflections are allowed.
Generic performance/production words and some title components are included.
Explanations identify broad connections so they can be removed to taste.
No arbitrary five-letter truncations of longer surnames are intended.
STRAT refers to Fender's guitar/model nickname, rather than a maker's name.

## Checked reference examples

These sources support particular examples, not every word in the list:

- [Metropolitan Opera synopses](https://www.metopera.org/Discover/Synopses/):
  NORMA, TOSCA, MANON, MEDEA, FAUST, and THAIS (accent removed).
- [ABC Classic instrument glossary](https://www.abc.net.au/listen/classic/glossary-of-musical-instruments/102210426):
  REBAB, SHAWM, and SUONA.
- [Fender trademark reference](https://dealer.fender.com/documents/TermsofUse-DealerPortal.pdf): STRAT.
- [Recording Academy artist profile](https://www.grammy.com/artists/adele/528/): ADELE.
- [London Symphony Orchestra composer guide](https://www.lso.co.uk/news-and-stories/composer-guide/edward-elgar/): ELGAR.
- [Lorde's official site](https://www.lorde.co.nz/tour): LORDE.
- [Max Reger Institute](https://www.max-reger-institut.de/de/): REGER.

## Search results

A complete search of these 607 words finds **zero** squares under the original
rule forbidding a row word from occurring in any column, and **17** squares
when across/down reuse is allowed. Row words must still be distinct. Allowing
across/down reuse matches the ordinary symmetric word-square convention.
One particularly readable example uses only composers and recording artists:

```
ELGAR
LORDE
GRIEG
ADELE
REGER
```

All five columns equal the corresponding rows. No extra words outside the
music list were used. A lack of solutions for a specified dictionary is an
exhaustive result for that dictionary and those rules, not a claim that no
larger music dictionary could work.

```r
# Full enumeration under the original rule:
strict <- make_squares(words5music)
# First square under the broader rule:
first <- make_squares(words5music, allow_shared_words = TRUE, max_squares = 1)
print_squares(first$found5)
# Full enumeration including symmetric squares:
all_music <- make_squares(words5music, allow_shared_words = TRUE)
print_squares(all_music$found5)
```

To edit the list, update the text and the corresponding notes, then regenerate
package data from the package root (no extra package is required):

```r
words5music <- sort(unique(toupper(trimws(readLines('inst/5-letter-words-music.txt')))))
stopifnot(all(grepl('^[A-Z]{5}$', words5music)))
save(words5music, file = 'data/words5music.rda', compress = 'xz', version = 2)
```
