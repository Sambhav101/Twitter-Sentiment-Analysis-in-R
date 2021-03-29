
# import basic libraries
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(textdata)
library(reshape2)
library(tokenizers)
library(stopwords)


# preprocess our text data

get_text_data <- function(text) {
  
  # tokenize our text into words
  text_data <- text %>% 
    replace_url(replacement = "") %>%
    replace_date(replacement = "") %>%
    replace_contraction() %>%
    replace_emoji() %>%
    replace_emoticon() %>%
    replace_grade() %>%
    replace_html(symbol = FALSE) %>%
    replace_internet_slang() %>%
    replace_number(remove= TRUE) %>%
    replace_word_elongation() %>%
    tokenize_words(stopwords = stopwords::stopwords("en"))
  
  # create a table of words
  text_tab <- table(text_data[[1]])
  text_words <- tibble(word = names(text_tab), n = as.numeric(text_tab)) %>%
    arrange(desc(n))
  
  return (text_words)
}


