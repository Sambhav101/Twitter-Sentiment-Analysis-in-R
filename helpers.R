library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(textdata)
library(reshape2)
library(wordcloud2)
library(packcircles)

# generate wordclouds of all words 
generate_wordcloud2 <- function(tbl){
    wordcloud2(tbl, size = 0.7, shape = 'cirlce', ellipticity = 1, rotateRatio = 0.5, color = "random-dark")
}

# generate bubble chart
generate_bubbles <- function(tbl) {
  rows <- sample(nrow(tbl))
  tbl <- tbl[rows, ]
  packing <- circleProgressiveLayout(tbl$n, sizetype="radius")
  data <- cbind(tbl, packing)
  dat.gg <- circleLayoutVertices(packing, npoints=100)
  # Make the plot
  ggplot() + 
    
    # Make the bubbles
    geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.5) +
    
    # Add text in the center of each bubble + control its size
    geom_text(data = data, aes(x, y, size=n, label = word)) +
    scale_size_continuous(range = c(0.2,6.5)) +
    
    # General theme:
    theme_void() + 
    theme(legend.position="none")
}

# Top 20 words in the data
top_words <- function(tbl) {
    tbl %>%
      slice_head(n=15) %>%
      mutate(word=reorder(word, n)) %>%
      ggplot(aes(x=word, y=n, fill = -n)) + geom_col(show.legend = FALSE) + xlab(NULL) + coord_flip() + 
      labs(x="Words", y="Count", title="Top Unique words found") + theme_linedraw() + theme(text = element_text(size=15))
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
    ggplot(aes(word, contribution, fill = contribution > 0)) + geom_col(show.legend = FALSE) + labs(x = "top words from both sentiments", y = "contribution of each word", title="Most used words from both sentiments") + 
    coord_flip() + theme_linedraw() + theme(text = element_text(size=15))
}

# generate barplot of sentiments
bar_sentiments <- function(tbl) {
    tbl %>%
      mutate(sentiment = reorder(sentiment, n)) %>%
      ggplot(aes(sentiment, n, fill = sentiment)) + geom_col() + labs(x = "Emotion", y = "word_count", title="Sentiment Ratio") + 
    theme_linedraw() + theme(text = element_text(size=15))
}

