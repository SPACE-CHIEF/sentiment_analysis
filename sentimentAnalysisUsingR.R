library(textdata)
library(tidyverse)
library(wordcloud)
library(wordcloud2)
library(tm)
library(SentimentAnalysis)
library(tidytext)
library(dplyr)
library(purrr)
library(ggplot2)
library(reshape2)
library(tidyquant)
library(topicmodels)
library(quanteda)
library(ichimoku)
library(lubridate)
apple <- read_csv("C://Users/rober/Downloads/apple_news.csv")
apple <- apple %>%
  mutate(date = as.POSIXct(datetime, origin="1970-01-01"))
# Load financial dictionary
fin_dict <- textdata::lexicon_loughran()

# Load Harvard IV dictionary and convert it to same structure as fin_dict
psych_dict <- SentimentAnalysis::DictionaryGI
psych_dict <- as.data.frame(bind_rows(psych_dict[1], psych_dict[2]))
t <- data.frame(word = unlist(psych_dict, use.names = F))
t <- tidyr::drop_na(t)
t$sentiment <- t$word %in% SentimentAnalysis::DictionaryGI$positive
psych_dict <- t %>%
  mutate(sentiment = ifelse(sentiment == 1, 'positive', 'negative'))

# Combine both dictionaries into one
dictionary <- rbind(fin_dict, psych_dict)

# Create a corpus for financial news headline
corpus <- iconv(apple$headline)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# remove numbers from corpus
corpus <- tm::tm_map(corpus, tolower)
# remove special characters
corpus <- tm::tm_map(corpus, removePunctuation)
# remove stop words
cleanset <- tm::tm_map(corpus, removeWords, stopwords('english'))
# remove URLs
removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm::tm_map(cleanset, content_transformer(removeURL))
# Text stemming
cleanset <- tm_map(cleanset, removeWords, c('aapl', 'apple'))
cleanset <- tm_map(cleanset, gsub,
                   pattern = 'stocks',
                   replacement = 'stock')
cleanset <- tm_map(cleanset, stemDocument)
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])
# Term document matrix
tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm)
# Bar Plot
w <- rowSums(tdm)
w <- subset(w, w>=25)
barplot(w,
        las = 2,
        col = rainbow(50))
# Word cloud
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
wordcloud2(w,
           size = 0.7,
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)

#---------------------------------------------#
data("stop_words")
tidy_apple <- apple %>%
  group_by(headline, datetime) %>%
  mutate(text = headline,
         datetime = date) %>%
  dplyr::select(text)

tidy_apple <- tidy_apple %>%
  unnest_tokens(word, text)

tidy_apple <- tidy_apple %>%
  dplyr::anti_join(stop_words)

tidy_apple %>%
  count(word, sort = TRUE)

apple_sentiment <- tidy_apple %>%
  inner_join(get_sentiments("loughran")) %>%
  count(headline, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = (negative + litigious + positive
         + constraining + uncertainty) / length(headline))

loughran_word_counts <- tidy_apple %>%
  inner_join(get_sentiments("loughran")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
loughran_word_counts$date <- as.Date(ymd_hms(loughran_word_counts$datetime))
loughran_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 5) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

tidy_apple %>%
  inner_join(get_sentiments("loughran")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0, fun.aggregate = sum) %>%
  comparison.cloud(colors = c("gray", "gray", "red", "green"),
                   max.words = 100, scale = c(3.5, 0.25))

tidy_apple %>%
  inner_join(get_sentiments("loughran")) %>%
  filter(sentiment == "negative") %>%
  count(word, sort = T) %>%
  with(wordcloud(word, n, max.words = 100, rot.per = 0.35))

# Download APPLE Price Data
#-----------------------------#
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)

getSymbols("AAPL", from = '2021-03-24',
           to = "2022-03-24",warnings = FALSE,
           auto.assign = TRUE)

head(AAPL)
chart_Series(AAPL)

cloud <- ichimoku(AAPL)
AAPL <- xts_df(cloud, keep.attrs = TRUE)
AAPL <- AAPL %>%
  dplyr::select(index, open, high, low, close) %>%
  mutate(datetime = index)

AAPL$index <- NULL
AAPL$date <- as.Date(ymd_hms(AAPL$datetime))

# Join price and words
price_news <- AAPL %>%
  inner_join(loughran_word_counts, by = "date")

# TODO: Feature transformation based on the research paper.