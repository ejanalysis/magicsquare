library(magicsquare)

requested <- toupper(c('bomba','plena','glock','samba','conga','rumba','tumba',
                       'altos','songs','tunes','meter','beats','horns','flute'))
examples <- c('DIVAS','DYADS','FILLS','FROGS','HEADS','KNOBS','PIPES','REELS',
              'SKINS','TAKES','TUBES','VAMPS','BLOWS','DAMPS','DRAWS','EDITS',
              'FADES','HEARS','LILTS','PEALS','READS','SINGS','SNAPS','SPINS',
              'TOLLS','TOOTS','WAILS','WALKS','ACCEL','CRESC','GLOCK','RITEN',
              'SMORZ','STACC','ADAGE','CEDEZ','FONDU','PIQUE','PLIES','TENDU',
              'VALSE','VOLTA','BONZO','CHICK','LEMMY','SATCH','SKUNK','SNOOP')
stopifnot(all(c(requested, examples) %in% words5music),
          all(grepl('^[A-Z]{5}$', words5music)), !anyDuplicated(words5music),
          !any(c('BASSS','HUMSS') %in% words5music))
# The installed editable source, annotations and packaged object stay in sync.
text <- readLines(system.file('5-letter-words-music.txt',package='magicsquare'))
notes <- readLines(system.file('music-word-notes.txt',package='magicsquare'))
notes <- notes[grepl('^[A-Z]{5} [|]',notes)]
entries <- strsplit(notes,' | ',fixed=TRUE)
words <- vapply(entries,`[`,character(1),1)
stopifnot(identical(text,as.vector(words5music)),setequal(words,words5music),
          !anyDuplicated(words))
for (entry in entries) {
  if (entry[2] %in% c('four-letter noun plus S','four-letter verb plus S')) {
    base <- substr(entry[3],1,4)
    stopifnot(identical(entry[1],paste0(base,'S')),
              startsWith(entry[3],paste0(base,' + S:')))
  }
}
# New vocabulary has the same completeness and dictionary-membership guarantees.
x <- make_squares(words5music,allow_shared_words=TRUE,report=FALSE)
stopifnot(x$complete, x$count_ok_5rows >= 66)
for (i in seq_len(ncol(x$found5))) {
  rows <- x$found5[,i]
  columns <- apply(do.call(rbind,strsplit(rows,'')),2,paste,collapse='')
  stopifnot(all(c(rows,columns) %in% words5music),!anyDuplicated(rows))
}
