install.packages("rtweet")
install.packages("twitteR")
install.packages("ROAuth")
install.packages("hms")
install.packages("lubridate") 
install.packages("tidytext")
install.packages("tm")
install.packages("wordcloud")
install.packages("igraph")
install.packages("glue")
install.packages("networkD3")
install.packages("plyr")
install.packages("stringr")
install.packages("ggplot2")
install.packages("ggeasy")
install.packages("plotly")
install.packages("dplyr")  
install.packages("magrittr")
install.packages("tidyverse")
install.packages("janeaustenr")
install.packages("widyr")
library(twitteR)
library(ROAuth)
library(hms)
library(lubridate) 
library(tidytext)
library(tm)
library(wordcloud)
library(igraph)
library(glue)
library(networkD3)
library(rtweet)
library(plyr)
library(stringr)
library(ggplot2)
library(ggeasy)
library(plotly)
library(dplyr)  
library(hms)
library(lubridate) 
library(magrittr)
library(tidyverse)
library(janeaustenr)
library(widyr)
api_key <- "zbYQsOb0jFp3XiN3Wxzho4Cb4"
api_secret <- "tEbDcHwdRsa4AVb5XSKcdc0Cmj659dNi0Q3MpEJwBnzhOUpjPO"
access_token <- "1496765351215964161-OUdq8uwksz01Bm1rKPfT0ug0NuGlhX"
access_token_secret <- "l7dBK3KMtOFrDyCzL7welnS8yZgpBmR0qSb8raukA3J3m"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
tweetsbr <- searchTwitter("vacina", n=4000, lang="pt")
n.tweetbr <- length(tweetsbr)
tweetsbr.df <- twListToDF(tweetsbr)
tweetsbr.txt <- sapply(tweetsbr, function(t)t$getText())
tweets <- searchTwitter("#vaccine", n=4000, lang="en")
n.tweet <- length(tweets)
tweets.df <- twListToDF(tweets)
tweets.txt <- sapply(tweets, function(t)t$getText())
tweets.txt <- str_replace_all(tweets.txt,"[^[:graph:]]", " ")
clean.text = function(x)
{
  # convert to lower case
  x = tolower(x)
  # remove rt
  x = gsub("rt", "", x)
  # remove at
  x = gsub("@\\w+", "", x)
  # remove punctuation
  x = gsub("[[:punct:]]", "", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove links http
  x = gsub("http\\w+", "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  # some other cleaning text
  x = gsub('https://','',x)
  x = gsub('http://','',x)
  x = gsub('[^[:graph:]]', ' ',x)
  x = gsub('[[:punct:]]', '', x)
  x = gsub('[[:cntrl:]]', '', x)
  x = gsub('\\d+', '', x)
  x = str_replace_all(x,"[^[:graph:]]", " ")
  return(x)
}
cleanText <- clean.text(tweets.txt)
idx <- which(cleanText == " ")
cleanText <- cleanText[cleanText != " "]
tweets.df %<>% 
  mutate(
    created = created %>% 
      # Remove zeros.
      str_remove_all(pattern = '\\+0000') %>%
      # Parse date.
      parse_date_time(orders = '%y-%m-%d %H%M%S')
  )
tweets.df %<>% 
  mutate(Created_At_Round = created%>% round(units = 'hours') %>% as.POSIXct())

tweets.df %>% pull(created) %>% min()
tweets.df %>% pull(created) %>% max()
plt <- tweets.df %>% 
  dplyr::count(Created_At_Round) %>% 
  ggplot(mapping = aes(x = Created_At_Round, y = n)) +
  theme_light() +
  geom_line() +
  xlab(label = 'Date') +
  ylab(label = NULL) +
  ggtitle(label = 'Number of Tweets per Hour')

plt %>% ggplotly()

