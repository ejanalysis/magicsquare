# Indexed search implementation and checks

`make_squares()` now delegates to a compiled C backtracking search. A prefix
trie represents possible down words; positional bitsets represent across-word
candidates. Each recursive step intersects the bitsets allowed by all down-word
prefixes, then checks that each extended prefix still has a completion after
excluding selected row words. Impossible branches stop immediately. It does
not allocate a dictionary-size-squared matrix or assume a symmetric square.
No additional R package dependency is required. Installing from source or using
`pkgload::load_all()` now requires an R-compatible C compiler.

The original across/down exclusion remains the default. Set
`allow_shared_words = TRUE` to permit symmetric squares. Row words remain
distinct; duplicate column words remain permitted, matching the original code.
`max_squares = 1` finds just one result. `complete = FALSE` marks searches
stopped by a finite result limit; their counts are lower bounds. An unlimited
completed search includes both orientations of asymmetric squares when both
satisfy the distinct-row rule. Repeated columns can prevent a transpose from
satisfying that rule.

The existing result fields and matrix layout remain. Intentional cleanups:
`quiet = TRUE` suppresses timing output; the console count summary is controlled
separately by `report` (TRUE by default). Unsaved partial hits are NULL rather than
placeholder zeroes, counts beyond N are consistently NA, and invalid input
raises an informative error. Word case, duplicates, and surrounding whitespace
are normalized. Result order is deterministic alphabetical traversal rather
than the original frequency-based order.

## Measurements

Measured locally on November 6, 2026; elapsed times vary by machine and load.

| Dictionary | Original nested loops | Intermediate R index | Compiled index | Complete squares |
| --- | ---: | ---: | ---: | ---: |
| words4small, 998 words | 108.874 s | 0.340 s | 0.004 s | 740 |
| words5test, 300 words | 0.859 s | 0.018 s | under 0.01 s | 0 |
| words5, 3,103 words | not timed | 125.392 s | 2.222 s | 2,279 |
| expanded music, 607 words, shared allowed | not timed | 0.193 s | about 0.002 s | 17 |

The original and intermediate searches returned identical count and result sets
for the bundled 3-, 4-, and 5-letter examples, original 176-word music list,
300-word five-letter test dictionary, and 998-word four-letter dictionary.
The compiled implementation was separately compared against the original on
all three example dictionaries and the 300-word test dictionary. The compiled
and intermediate searches both checked 3,442,858 candidate rows and found 2,279
complete squares for the 3,103-word dictionary. The compiled 998-word search
returned the same partial counts (8,420 and 2,024) and 740 complete squares.

`tests/search.R` compares the compiled search against a separate exhaustive R
oracle on 32 small dictionary/rule combinations for sizes 2 through 5. It also
checks asymmetric transposes, shared-word rules, result limits, quiet output,
invalid inputs, vocabulary membership, and forced garbage collection.
`benchmark-search.R` provides a callable benchmark, with an optional path to
an archived original function for direct comparison. The exhaustive oracle
validates result sets, rather than just counts or a single pleasing example.

Package installation, examples, compiled-code checks, and search tests passed.
`R CMD check --no-manual --no-vignettes` finished with zero errors, two existing
warnings (a script filename containing spaces and undocumented older objects),
and one existing metadata note (Description punctuation and the MIT template).
The stale `hello.Rd` scaffold was removed because its nonexistent `hello()`
example previously caused a package-check error. The user's pre-existing
DESCRIPTION edit was preserved.

Search remains exponential in difficult dictionaries. A finite result limit
helps when only one answer is needed. Unlimited searches with many hits can
still consume substantial time and output memory.
