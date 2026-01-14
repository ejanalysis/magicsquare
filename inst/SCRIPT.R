
############ ############# ############# ############# #

words3example <- c("AIR", "ROE", "END", "ARE", "ION", "RED")
usethis::use_data(words3example, overwrite = TRUE)
############ #

words4example <- c("blat", "ROBE", "ISLE", "SHES",
                   "BRIS", "LOSH", "ABLE", "TEES",
                   "more", "four", "five", "nine")
usethis::use_data(words4example, overwrite = TRUE)
############ #

words5example <- unique(  c(
  'ducat', 'opera', 'penny', 'tress',
  'adopt', 'super', 'scene', 'earns', 'stays' ,

  'seals', 'taboo', 'aglow', 'geese', 'erred',
  'stage',  'eager', 'abler', 'loose', 'sowed',

  'ASSES' ,
  'STEAL' ,
  'PARSE' ,
  'EGGED' ,
  'NEEDS' ,

  'ASPEN',
  'STAGE',
  'SERGE',
  'EASED',
  'SLEDS',

  'asses',
  'banal',
  'abase',
  'fried',
  'tells',

  'abaft','sabre','snail', 'eases'  , 'sleds',

  'asset', 'clear', 'ridge', 'edges', 'seers',
  'aster', 'shale', 'sixes', 'erect', 'tests'
))
usethis::use_data(words5example, overwrite = TRUE)
############ ############# ############# ############# #


## read text files and save as datasets for package

# 3-letter word list from
# https://copylists.com/words/list-of-3-letter-words/#CVS

words3 <- read.csv('https://copylists.com/downloads/words/3_letter_words/3_letter_words.csv')
words3 <- words3$X
usethis::use_data(words3, overwrite = TRUE)

# # 4-letter word list from https://gist.github.com/raspberrypisig/cc18b0f4fbc0c79ffd667d06adc0a190
words4 = readLines("./inst/4-letter-words-6k.txt")  ## excessive, since many of these words are VERY OBSCURE !
usethis::use_data(words4)
words4small = readLines("./inst/4-letter-words-1k.txt") ## even this list has a lot of OBSCURE words !
usethis::use_data(words4small)
words4test = sample(words4, 300)
usethis::use_data(words4test)

############ ############# ############# ############# #

# # 5-letter word list of 3k words, from https://gist.github.com/shmookey/b28e342e1b1756c4700f42f17102c2ff
words5  = readLines("./inst/5-letter-words-3k.txt")
usethis::use_data(words5)
words5test = sample(words5, 300)
usethis::use_data(words5test)
# 5-letter words related to music, from me
words5music = readLines("./inst/5-letter-words-music.txt")
usethis::use_data(words5music)
############ ############# ############# ############# #

######################### # .................................................................

## check speed, timing

system.time({x5test = make_squares(words5test, save_2row_hits = F, save_3row_hits = F, quiet = F)})
# 300 5-letter words takes <1 second on M5 mbpro
## zero possible 5x5 squares with this particular sample

nn=300; system.time({x4_300 = make_squares(words4test, quiet = T, save_2row_hits = F, save_3row_hits = T)} )
## 300 4-letter words takes < 2 seconds on M5 mbpro
## zero possible 4x4 squares with this particular sample

nn=500; system.time({x4_500 = make_squares(sample(words4, nn), quiet = T, save_2row_hits = F, save_3row_hits = T)} )
## 500 4-letter words takes < 20 seconds on M5 mbpro
## found 4 possible 4x4 squares with this particular sample

# n is 1,000 seeking 4x4 squares
system.time({s4 = make_squares(words4small)})
## 998 unique 4-letter words takes 1.5 to 5 minutes, depending on new vs old computer
## 90 seconds on M5 mbpro
## Count of combos of 4 words that are plausible for 1st 4 rows: 740
## Found 740 good squares for this word list.
## s4$count_ok_3rows # [1] 2024
## s4$count_ok_4rows # [1] 740
## View of all 740 partial squares:
#   print_squares( s4$found4  )


#####################

# SEARCH FOR MAGIC SQUARES OF 4 LETTERS - 1k wordlist

stop("VERY SLOW even 1k words of 4 letters takes 1.5 minutes on M5 mbpro, but a few minutes on an old computer -  very SLOOOOWWW")

system.time({
  squares4_1k = make_squares(words4small, save_2row_hits = F, save_3row_hits = T, quiet = T)
})
# print(squares4_1k )
print_squares( squares4_1k, query="love")
save(squares4_1k, file = "./inst/squares4_1k.rda")

#####################


stop("VERY SLOW - 6k words of 4 letters")

system.time({
  squares4_6k = make_squares(words4, save_2row_hits = F, save_3row_hits = T, quiet = F)
})
# print(squares4_6k )
print_squares( squares4_6k$found4)
save(squares4_6k, file = "./inst/squares4_6k.rda")

#####################

# SEARCH FOR MAGIC SQUARES OF 5 LETTERS

## 3k words of 5 letters is EXTREMELY SLOW

stop("VERY SLOW to use 5letter words")

system.time({squares5 = make_squares(words5, save_2row_hits = F, save_3row_hits = F, quiet = F)})
# print(squares5 )
print_squares( squares5$found5)
save(squares5, file = "./inst/squares5.rda")
######################### #
