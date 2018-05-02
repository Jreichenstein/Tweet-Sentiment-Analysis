library(twitteR)
library(tidyr)
library(tibble)
library(dplyr)
library(tidyverse)
library(ggjoy)
library(RColorBrewer)
library(wordcloud)

# Connecting to twitter 
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#searching for tweets
tw <- searchTwitter('@airbnb', n = 8000)

#creating tibble
tweets <- tibble(
  screen_name = tw %>% map_chr(~.x$screenName),
  tweetid = tw %>% map_chr(~.x$id),
  created_timestamp = seq_len(length(tw)) %>% map_chr(~as.character(tw[[.x]]$created)),
  is_retweet = tw %>% map_chr(~.x$isRetweet),
  text = tw %>% map_chr(~.x$text)
) %>%
  mutate(created_date = as.Date(created_timestamp)) %>%
  filter(is_retweet == FALSE,
         substr(text, 1,2) != "RT")

#breaking down all tweets into individual words
tweets_by_word <- tweets %>%
  select(tweetid,
         screen_name,
         text,
         created_date) %>%
  unnest_tokens(word, text)

#common words to remove before sentiment analysis 
my_stop_words <- tibble(
  word = c(
    "https",
    "t.co",
    "rt",
    "amp",
    "rstats",
    "gt"
  ),
  lexicon = "twitter"
)

#combining custom list with stop_words
all_stop_words <- stop_words %>%
  bind_rows(my_stop_words)

suppressWarnings({
  no_numbers <- tweets_by_word %>%
    filter(is.na(as.numeric(word)))
})

no_stop_words <- no_numbers %>%
  anti_join(all_stop_words, by = "word")

#how many words were removed
tibble(
  total_words = nrow(tweets_by_word),
  after_cleanup = nrow(no_stop_words)
)

#Calculating top words used in tweets
top_words <- no_stop_words %>%
  group_by(word) %>%
  tally %>%
  arrange(desc(n)) %>%
  head(10)

#using NRC Sentiment analysis
nrc_words <- no_stop_words %>%
  inner_join(get_sentiments("nrc"), by = "word")

#tallying sentiments in tweets
nrc_words %>%
  group_by(sentiment) %>%
  tally %>%
  arrange(desc(n))

#plotting sentiments over time
ggplot(nrc_words) +
  geom_joy(aes(
    x = created_date,
    y = sentiment, 
    fill = sentiment),
    rel_min_height = 0.01,
    alpha = 0.7,
    scale = 3) +
  theme_joy() +
  labs(title = "Sentiment analysis in Tweets including '@airbnb'",
       x = "Tweet Date",
       y = "Sentiment") + 
  scale_fill_discrete(guide=FALSE)

set.seed(10)

cloud_words <- nrc_words %>%
  group_by(word) %>%
  tally

#creating wordcloud of top sentiments
cloud_words %>%
  with(wordcloud(word, n, max.words = 50, colors =  c("#56B4E9", "#E69F00")))