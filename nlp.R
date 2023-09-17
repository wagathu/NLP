

# Natural Language Processing ---------------------------------------------

library(pacman)
p_load(gutenbergr, dplyr, tm, wordcloud, stringr, SnowballC, tidytext, RWeka, udpipe, textstem)


# Downloading the novels --------------------------------------------------

novels <- gutenberg_works(author == c("Austen, Jane", "Stoker, Bram", "Melville, Herman"))
novels2 <- gutenberg_works(title == "The Adventures of Sherlock Holmes")
novels3 <- gutenberg_works(title == "Wuthering Heights")

nov1 <- gutenberg_download(768)
nov2 <- gutenberg_download(1661)
nov3 <- gutenberg_download(345)

novels <- paste(nov1$text, nov2$text, nov3$text)

# novels <- gutenberg_download(c(768, 1661, 345)) #2701, 1342
# 
# novels <- novels %>% 
#   select(,-c("gutenberg_id"))

# Creating a Corpus -------------------------------------------------------

# novelCorpus <- novels %>% 
#   unnest_tokens(word, text)  # Here, I removed all the stop words

novelCorpus <- Corpus(VectorSource(novels))

# Cleaning the Corpus -----------------------------------------------------

corpus <- tm_map(novelCorpus, tolower)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)

# Stemming the words in the corpus ----------------------------------------

# corpus <- tm_map(corpus, stemDocument, language = "english")
# inspect(corpus[1])

# lemmatization  -----------------------------------------------------------

corpus <- tm_map(corpus, lemmatize_strings)
inspect(corpus)


# Tokenizing into letters -------------------------------------------------

wordT <- function(x)
{
  unlist(strsplit(as.character(x), ""))
}

corpus_letters <- lapply(corpus, wordT)

# Converting back to corpus

corpus_tokens <- Corpus(VectorSource(sapply(corpus_letters, paste, collapse = " ")))
inspect(corpus_tokens[1])


# Creating the DocumentTermMatrix -----------------------------------------

corpusdtm <- DocumentTermMatrix(corpus)









