library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(textdata)
library(reshape2)
library(wordcloud)
library(wordcloud2)

# generate wordclouds of all words 
generate_wordcloud2 <- function(tbl){
    wordcloud2(tbl, size = 0.7, shape = 'star', color = brewer.pal(8, "Dark2"))
}

# Top 20 words in the data
top_words <- function(tbl) {
    tbl %>%
      slice_head(n=15) %>%
      mutate(word=reorder(word, n)) %>%
      ggplot(aes(x=word, y=n, fill = -n)) + geom_col(show.legend = FALSE) + xlab(NULL) + coord_flip() + 
      labs(x="Count", y="Unique words", title="Unique words found") + theme_linedraw()
}

# bing sentiment analysis
bing_sentiments <- function(tbl) {
  bing_tweet <- tbl %>%
    inner_join(get_sentiments("bing"), by = "word")
}

# afinn sentiment analysis
afinn_sentiments <- function(tbl) {
  afinn_tweet <- tbl %>%
    inner_join(get_sentiments("afinn"), by = "word")
}

# nrc sentiment analysis
emotion_sentiments <- function(tbl) {
  emotion_tweet <- tbl %>%
    inner_join(get_sentiments('nrc'), by = "word") %>%
    filter(!grepl('positive|negative', sentiment))

}

# generate top 10 positive and negative words
pn_plot <- function(tbl) {
  tbl %>%
    group_by(word) %>%
    summarize(contribution = sum(n * value)) %>%
    slice_max(abs(contribution), n = 15) %>%
    mutate(word = reorder(word, contribution)) %>%
    ggplot(aes(word, contribution)) + geom_col() + labs(x = "total contribution of each word", y = NULL) + coord_flip() + theme_linedraw()
}

# generate barplot of sentiments
bar_sentiments <- function(tbl) {
    tbl %>%
      mutate(sentiment = reorder(sentiment, n)) %>%
      ggplot(aes(sentiment, n, fill = sentiment)) + geom_col() + labs(x = NULL, y = "word_count", title="Sentiments") + theme_linedraw()
}

