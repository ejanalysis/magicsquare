"""Build the packaged common-word objects from pinned wordfreq estimates.

Run from the repository root after installing wordfreq==3.1.1 in a Python
build environment. Python and wordfreq are not runtime package dependencies.
The generated objects retain attribution and licensing in a 'source' attribute.
"""
import hashlib
import importlib.metadata
import json
from pathlib import Path
import re
import subprocess
import tempfile

import wordfreq
from wordfreq import zipf_frequency

VERSION = '3.1.1'
MIN_ZIPF = 4.5
assert importlib.metadata.version('wordfreq') == VERSION
root = Path(__file__).resolve().parents[1]
word_source = root / 'inst/5-letter-words-3k.txt'
frequency_source = Path(wordfreq.__file__).parent / 'data/large_en.msgpack.gz'
candidates = sorted(set(word_source.read_text().lower().split()))
rows = []
for word in candidates:
    if not re.fullmatch('[a-z]{5}', word):
        continue
    score = zipf_frequency(word, 'en', wordlist='large')
    if score >= MIN_ZIPF:
        rows.append((word.upper(), score))
rows.sort(key=lambda row: (-row[1], row[0]))

# JSON string quoting is compatible with R for these ASCII metadata strings.
def r_string(value):
    return json.dumps(value, ensure_ascii=True)

metadata = {
    'title': 'English wordfreq estimates, filtered to common five-letter words',
    'author': 'Robyn Speer',
    'version': VERSION,
    'url': 'https://github.com/rspeer/wordfreq',
    'citation': 'Robyn Speer (2022). rspeer/wordfreq: v3.0 (v3.0.2). Zenodo. https://doi.org/10.5281/zenodo.7199437',
    'license': 'CC BY-SA 4.0',
    'license_url': 'https://creativecommons.org/licenses/by-sa/4.0/',
    'attribution': 'Derived from wordfreq by Robyn Speer. Includes freely available SUBTLEX data by Marc Brysbaert, Boris New, and collaborators; OPUS OpenSubtitles 2018 / OpenSubtitles; Google Books Ngrams; Wikipedia and other sources credited by wordfreq.',
    'changes': 'Filtered to the bundled words5 dictionary, exactly five ASCII letters, Zipf >= 4.5; uppercased; sorted by descending Zipf, alphabetically within ties. Per-million estimates calculated from rounded Zipf scores.',
    'scope': 'Frequency estimates aggregated from English corpora through about 2021; not a universal or current-year English frequency ranking. Frequencies pool case and senses, including names with common-word spellings.',
    'wordlist_sha256': hashlib.sha256(word_source.read_bytes()).hexdigest(),
    'frequency_sha256': hashlib.sha256(frequency_source.read_bytes()).hexdigest(),
}
r_code = '\n'.join([
    'words5common <- c(' + ', '.join(r_string(word) for word, _ in rows) + ')',
    'scores <- c(' + ', '.join(f'{score:.2f}' for _, score in rows) + ')',
    'source <- list(' + ', '.join(f'{key} = {r_string(value)}' for key, value in metadata.items()) + ', minimum_zipf = 4.5)',
    'attr(words5common, "source") <- source',
    'words5common_frequency <- data.frame(rank = seq_along(words5common), word = as.vector(words5common), zipf = scores, per_million = 10^(scores - 3), stringsAsFactors = FALSE)',
    'attr(words5common_frequency, "source") <- source',
    'stopifnot(!anyDuplicated(words5common), all(grepl("^[A-Z]{5}$", words5common)), all(diff(scores) <= 0), all(scores >= 4.5))',
    'save(words5common, file = "data/words5common.rda", compress = "xz", version = 2)',
    'save(words5common_frequency, file = "data/words5common_frequency.rda", compress = "xz", version = 2)',
])
with tempfile.TemporaryDirectory(prefix='magicsquare-common-') as directory:
    script = Path(directory) / 'build.R'
    script.write_text(r_code)
    subprocess.run(['Rscript', '--vanilla', str(script)], cwd=root, check=True)
print(f'Built {len(rows)} common words, including {sum(score >= 5 for _, score in rows)} with Zipf >= 5.')
