library(rtweet)
library(dplyr)
library(textclean)
library(tokenizers)
library(tidyr)
library(tidytext)

# store api keys
app_name <- "Analyzer-sentiment"
api_key <- "vdkDo0185ZjwEz3iWy10zd85T"
api_secret_key <- "T6mvqWA218ZRZ6nmHY9N3OcRN4GH4zmgGxXA22erpfA04EMU4K"
access_token <- "4323850282-D4XBXcDcKbHcXzfazFwknpkZqZPYCYlZZwFlwRZ"
access_secret_token <- "EzzgBuUsn1d8tCqa1XattBT2vYVtLFgZImJSXzEQVQOPQ"

# authenticate via access token
token <- create_token(
  app = app_name,
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_secret_token
)

get_tweets_data <- function(var, n) {
  tweet_data <- search_tweets(
    var,
    n, 
    include_rts = FALSE,
    type = 'mixed',
    lang = "en"
  )
  
  tweet_text <- tweet_data %>% select(text)
  
  tweet_text <- replace_url(tweet_text, replacement = "") %>%
    replace_contraction() %>%
    tokenize_tweets()
  
  tweet_tab <- table(tweet_text[[1]])
  tweet_words <- tibble(word = names(tweet_tab), n = as.numeric(tweet_tab)) %>%
    arrange(desc(n)) %>% 
    anti_join(stop_words) %>%
    filter(!grepl("\\d|@|amp|nigg", word)) %>%
    filter(!grepl(gsub(" ", "|", var), word, ignore.case = TRUE)) %>%
    subset(subset = nchar(word) != 1)
  
  return(tweet_words)
}


