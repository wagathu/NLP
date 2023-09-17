
# Natural Language Processing ---------------------------------------------
  
library(pacman)
p_load(gutenbergr, rpart,randomForest, text2vec, keras,ModelMetrics, torch, quanteda, dplyr, tm, wordcloud, stringr, SnowballC, tidytext, RWeka, udpipe, textstem)

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

# Tokenizing into individual words ----------------------------------------

corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, WordTokenizer)
inspect(corpus[1:10])


# Creating ngrams ---------------------------------------------------------

novels2 <- novels %>% 
  tolower() %>% 
  removePunctuation() %>% 
  stripWhitespace() %>% 
  removeNumbers() %>% 
  removeWords(words = stopwords())

digramms <- NGramTokenizer(novels2, Weka_control(min = 2, max = 2)) 
trigramms <- NGramTokenizer(novels2, Weka_control(min = 3, max = 3))

# Creating a DocumentTermMatrix -------------------------------------------

dtm <- DocumentTermMatrix(corpus)

# Obatining the Individual words ------------------------------------------

words <- (dtm$dimnames)$Terms

# Creating a dataframe of words less than 10 letters ----------------------

df <- data.frame(words = words)%>% 
  filter(nchar(words) < 10) # I decided 10 letters because all words were 9 letters except for abnormal sentences with 44 and 27 letters

# Splitting the words dataset into letters ---------------------------------------------

df$letters <- str_split(df$words, "")

# Putting the letters into a df and filling shorter with NA ---------------

df_new <- data.frame(matrix(NA, nrow=nrow(df), ncol= 9 ))
for (i in 1:nrow(df)) {
  letters <- strsplit(df[i, "words"], "")[[1]]
  df_new[i, 1:length(letters)] <- letters
}

# Naming the columns ------------------------------------------------------

colnames(df_new) <- paste0("letter_", 1:ncol(df_new))

# Creating a df of letters and the corresponding word ---------------------

wordsdata <- cbind(df, df_new) %>% 
  select(, -c("letters")) %>% # Removing the letters column
  filter(nchar(words) > 2)  # This is because there were characherts such as ., _

write.csv(wordsdata, "E:/Desktop/NLP/wordsdata.csv")

# Selecting df for words with 9 letters only ------------------------------

wordsdata9 <- wordsdata %>% 
  na.omit()

# Selecting df for words with 8 letters only ------------------------------

wordsdata8 <- wordsdata %>% 
  filter(nchar(words) == 8) %>% 
  select( -c("letter_9"))

# Selecting df for words with 7 letters only ------------------------------

wordsdata7 <- wordsdata %>% 
  filter(nchar(words) == 7) %>% 
  select( -c("letter_8", "letter_9"))

# Selecting df for words with 6 letters only ------------------------------

wordsdata6 <- wordsdata %>% 
  filter(nchar(words) == 6) %>% 
  select( -c("letter_8", "letter_9", "letter_7"))

# Selecting df for words with 5 letters only ------------------------------

wordsdata5 <- wordsdata %>% 
  filter(nchar(words) == 5) %>% 
  select( -c("letter_8", "letter_9", "letter_7", "letter_6"))

# Selecting df for words with 4 letters only ------------------------------

wordsdata4 <- wordsdata %>% 
  filter(nchar(words) == 4) %>% 
  select( -c("letter_8", "letter_9", "letter_7", "letter_6", "letter_5"))

# Selecting df for words with 3 letters only ------------------------------

wordsdata3 <- wordsdata %>% 
  filter(nchar(words) == 3) %>% 
  select( -c("letter_8", "letter_9", "letter_7", "letter_6", "letter_5", "letter_4"))


# Modelling -----------------------------------------------------------

# 1, Partitioning into testing and training

sample_size = 0.8
sample_size <- floor(sample_size * nrow(wordsdata9))
set.seed(123)
trainIndex <- sample(seq_len(nrow(wordsdata9)), size = sample_size)

trainData <- wordsdata9[trainIndex,]
testData <- wordsdata9[-trainIndex,] %>% 
  select(, -c("words"))


# training the model ------------------------------------------------------

model <- randomForest(as.factor(words)~., data = trainData)


# Predicting words --------------------------------------------------------

pred <- data.frame(letter_1 = "s", letter_2 = "t",
                   letter_3 = "a", letter_4 = "t",
                   letter_5 = "e", letter_6 = "m",
                   letter_7 = "e", letter_8 = "n",
                   letter_9 = "t")
mine <- predict(model, (pred))


predicted <- predict(model, testData) %>% 
  as.data.frame()

View(predicted)

predict(model, preddata)



# Putting the columns into ASCII range of 1:26 ----------------------------

wordsdata9[, 2:10] <- apply(wordsdata9[, 2:10], 2, function(x) {
  sapply(x, function(y) {
    ascii <- as.integer(charToRaw(y))
    if(length(ascii) == 0) {
      NA
    } else {
      ascii[1] - 96
    }
  })
})






























