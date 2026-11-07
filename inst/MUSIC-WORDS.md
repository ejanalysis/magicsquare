# Five-letter music vocabulary

`5-letter-words-music.txt` is the editable source for the packaged `words5music`
vector. It contains 824 unique uppercase A-Z entries: the 176 original words,
431 earlier additions, and 217 additions in the latest expansion. `music-word-notes.txt` explains each addition. The list is
intentionally broad and remains a candidate vocabulary, not a fully verified
lexicon. Original words remain included even where their connection is broad.

Names can be first names or surnames (e.g. MILES and DAVIS), and familiar name
components are permitted. Accents, spaces, and punctuation can be removed
(BJORK, FAURE, DRDRE, MYWAY). Plurals and musical inflections are allowed.
Generic performance/production words and some title components are included.
Explanations identify broad connections so they can be removed to taste.
No arbitrary five-letter truncations of longer surnames are intended.
STRAT refers to Fender's guitar/model nickname, rather than a maker's name.

## Latest vocabulary expansion

Of the user's fourteen requested terms, BOMBA, PLENA, GLOCK and TUMBA were
new. SAMBA, CONGA, RUMBA, ALTOS, SONGS, TUNES, METER, BEATS, HORNS and FLUTE
were already present and were retained once.

The 217 additions comprise 68 regular four-letter noun + S forms, 41 regular
four-letter verb + S forms, 19 published music/audio abbreviations, 50 Italian
or French music/dance terms, 34 musician nicknames or stage-name components,
the early-music note value LONGA, and the four missing requested terms. Categories overlap in meaning; for
example, CLAPS and VAMPS are both nouns and verbs. Examples include:

- Nouns: CLAPS, DISCS, DIVAS, DYADS, FILLS, FROGS (bow fittings), HEADS, KNOBS,
  NECKS, PIPES, REELS, SKINS, TAKES, TUBES and VAMPS.
- Verbs: BLOWS, DAMPS, DRAWS, EDITS, FADES, HEARS, LILTS, PEALS, READS,
  SNAPS, SPINS, TOLLS, TOOTS, WAILS and WALKS (a bass line).
- Abbreviations: ACCEL, ANDTE, CRESC, GLISS, MODTO, RITEN, SMORZ and STACC.
  Periods and superscript formatting are removed from published spellings;
  no arbitrary truncation is used to force longer terms into five letters.
- Italian/French terms: ADAGE, CANTO, CEDEZ, CORPS, COUPE, FONDU, LEGER,
  PASSE, PIQUE, PLIES, QUASI, SAUTE, TENDU, TOMBE, VALSE and VOLTA. Accents
  are removed, and regular plural forms such as JETES and PLIES are allowed.
- Nicknames/stage names: BONZO (John Bonham), CHICK (Chick Corea), LEMMY
  (Ian Kilmister), SATCH (Louis Armstrong), SKUNK (Jeff Baxter), SNOOP,
  BUSTA, NELLY, QUAVO and SUGGS. Existing names such as DIZZY, RINGO, STING,
  SLASH, BUDDY and MUDDY remain included once.

Four-letter words were screened rather than blindly given an S. For example,
BASS does not become BASSS, and short names such as FLEA were not padded to
five letters. Some additions have broad performance or production connections
(e.g. ROOMS or MAKES); their notes state the context so they can be excluded
if a narrower vocabulary is preferred.

## Checked reference examples

These sources support particular examples, not every word in the list:

- [Smithsonian Folkways: bomba and plena](https://folkways.si.edu/puerto-rican-bomba-plena-shared-traditions-distinct-rhythms/latin-world/music/article/smithsonian).
- [Latin Percussion tumba](https://www.lpmusic.com/products/lp807t-pm-pedrito-12-1-2-tumba/).
- [Contemporary Music Centre instrumentation abbreviations](https://www.cmc.ie/instrumentation-help): GLOCK.
- [Towson University musical abbreviations](https://webapps.towson.edu/ows/abbrevMusic.aspx):
  ACCEL, ANDTE, BRILL, CRESC, DELIC, ENERG, MODTO, MOVTO, OBBLI, PIANG,
  RECIT, RITEN, SMORZ, STACC, STENT, TRATT and TROMB.
- [The Australian Ballet vocabulary](https://australianballet.com.au/ballet-101/ballet-vocabulary):
  ADAGE, COUPE, FONDU, PASSE, SAUTE, TENDU, and the bases of PLIES and JETES.
- [Ballet Manila: piqué](https://balletmanilaarchives.squarespace.com/home/2019/5/1/ballet-dictionary-piqu).
- [Chick Corea's official biography](https://chickcorea.com/about-chick/),
  [John Bonham biography](https://johnbonham.co.uk/index.html),
  [Louis Armstrong House exhibits](https://www.louisarmstronghouse.org/virtual-exhibits/),
  [Jeff Skunk Baxter's official site](https://www.jeffskunkbaxter.com/), and
  [Smithsonian Bucky Pizzarelli archive](https://www.americanhistory.si.edu/ne/collections/archival-item/sova-nmah-ac-1222-ref1151).


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

A complete search of these 824 words finds **zero** squares under the original
rule forbidding a row word from occurring in any column, and **66** squares
when across/down reuse is allowed. All 66 are symmetric. One uses the
unverified original entry ALFIN; excluding it leaves 65. Row words must still be distinct. Allowing
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
package data from the package root:

```r
words5music <- toupper(trimws(readLines('inst/5-letter-words-music.txt')))
stopifnot(all(grepl('^[A-Z]{5}$', words5music)), !anyDuplicated(words5music))
save(words5music, file = 'data/words5music.rda', compress = 'xz', version = 2)
```

Then run `python data-raw/build-word-frequencies.py` in a Python environment
with `wordfreq==3.1.1` to restore frequency order, refresh `word_frequencies`,
and embed the source attribution. The text file and packaged data must have
the same order. Python is only needed for rebuilding, not for using the list.
