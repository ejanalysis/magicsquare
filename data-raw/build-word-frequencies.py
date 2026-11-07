"""Reorder bundled word-list vectors and text sources using wordfreq==3.1.1.

Run before build-words5common.py when rebuilding both. Entries and spelling
are preserved. No Python dependency is needed to use the resulting R package.
"""
import hashlib
import importlib.metadata
import json
from pathlib import Path
import subprocess
import tempfile

import wordfreq
from wordfreq import zipf_frequency

assert importlib.metadata.version('wordfreq') == '3.1.1'
root = Path(__file__).resolve().parents[1]
source_path = Path(wordfreq.__file__).parent / 'data/large_en.msgpack.gz'
metadata = {
    'author': 'Robyn Speer',
    'version': '3.1.1',
    'url': 'https://github.com/rspeer/wordfreq',
    'citation': 'Robyn Speer (2022). rspeer/wordfreq: v3.0 (v3.0.2). Zenodo. https://doi.org/10.5281/zenodo.7199437',
    'license': 'CC BY-SA 4.0',
    'license_url': 'https://creativecommons.org/licenses/by-sa/4.0/',
    'attribution': 'English frequency estimates from wordfreq by Robyn Speer; includes freely available SUBTLEX data by Marc Brysbaert, Boris New, and collaborators, OPUS OpenSubtitles 2018 / OpenSubtitles, Google Books Ngrams, Wikipedia, and other sources credited by wordfreq. See COMMON-WORDS.md for credits.',
    'changes': 'Ranked existing bundled vocabulary by decreasing English large-list Zipf frequency. Case-insensitive lookup; uppercase alphabetical tie breaks; original entries and spelling preserved. Missing estimates have score 0 and go last.',
    'scope': 'Corpus estimates through about 2021, not universal or current-year English frequencies; names and senses share frequencies for the same spelling.',
    'frequency_sha256': hashlib.sha256(source_path.read_bytes()).hexdigest(),
}
with tempfile.TemporaryDirectory(prefix='magicsquare-rank-') as directory:
    directory = Path(directory)
    dump = directory / 'words.txt'
    # All tokens are A-Z only in the current package, so | is unambiguous here.
    export = '''args <- commandArgs(TRUE)
for (file in list.files('data', pattern='^words.*rda$', full.names=TRUE)) {
  e <- new.env(); load(file, e)
  for (name in ls(e)) {
    x <- e[[name]]
    if (is.character(x) && !startsWith(name, 'words5common')) {
      stopifnot(all(grepl('^[A-Za-z]+$', x)))
      cat(paste(name, x, sep='|'), file=args[1], sep='\\n', append=TRUE)
    }
  }
}
'''
    script = directory / 'export.R'
    script.write_text(export)
    subprocess.run(['Rscript', '--vanilla', str(script), str(dump)], cwd=root, check=True)
    tokens = {line.split('|', 1)[1].upper() for line in dump.read_text().splitlines()}
    # Include the editable text sources too, even if a future source adds words.
    text_sources = [root / 'inst' / name for name in (
        '4-letter-words-6k.txt', '4-letter-words-1k.txt',
        '5-letter-words-3k.txt', '5-letter-words-music.txt')]
    for path in text_sources:
        tokens.update(word.upper() for word in path.read_text().splitlines())
    tokens = sorted(tokens)
    scores = {word: zipf_frequency(word.lower(), 'en', wordlist='large') for word in tokens}
    quote = json.dumps
    code = '\n'.join([
        'frequency <- stats::setNames(c(' + ','.join(f'{scores[w]:.2f}' for w in tokens) + '), c(' + ','.join(quote(w) for w in tokens) + '))',
        'source <- list(' + ','.join(f'{key}={quote(value)}' for key, value in metadata.items()) + ')',
        '''for (file in list.files('data', pattern='^words.*rda$', full.names=TRUE)) {
  e <- new.env(); load(file, e)
  for (name in ls(e)) {
    x <- e[[name]]
    if (is.character(x) && !startsWith(name, 'words5common')) {
      spelling <- toupper(x)
      x <- as.vector(x)[order(-unname(frequency[spelling]), spelling, method='radix')]
      attr(x, 'source') <- source
      assign(name, x, envir=e)
      save(list=name, file=file, envir=e, compress='xz', version=2)
    }
  }
}
word_frequencies <- data.frame(word=names(frequency), letters=nchar(names(frequency)),
                               zipf=unname(frequency), stringsAsFactors=FALSE)
word_frequencies <- word_frequencies[order(word_frequencies$letters,
    -word_frequencies$zipf, word_frequencies$word, method='radix'), ]
rownames(word_frequencies) <- NULL
word_frequencies$per_million <- ifelse(word_frequencies$zipf > 0,
                                      10^(word_frequencies$zipf - 3), NA_real_)
attr(word_frequencies, 'source') <- source
save(word_frequencies, file='data/word_frequencies.rda', compress='xz', version=2)
''',
    ])
    script = directory / 'rank.R'
    script.write_text(code)
    subprocess.run(['Rscript', '--vanilla', str(script)], cwd=root, check=True)
    for path in text_sources:
        words = path.read_text().splitlines()
        words.sort(key=lambda word: (-scores[word.upper()], word.upper()))
        path.write_text('\n'.join(words) + '\n')
print(f'Ranked bundled word lists using {len(tokens)} distinct spellings.')
