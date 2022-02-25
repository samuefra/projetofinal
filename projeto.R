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
install.packages("httr")
install.packages("lattice")
install.packages("naivebayes")
install.packages("lexiconPT")
library(lexiconPT)
library(naivebayes)
library(lattice)
library(httr)
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
#configurando API do twitter
api_key <- "zbYQsOb0jFp3XiN3Wxzho4Cb4"
api_secret <- "tEbDcHwdRsa4AVb5XSKcdc0Cmj659dNi0Q3MpEJwBnzhOUpjPO"
access_token <- "1496765351215964161-OUdq8uwksz01Bm1rKPfT0ug0NuGlhX"
access_token_secret <- "l7dBK3KMtOFrDyCzL7welnS8yZgpBmR0qSb8raukA3J3m"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#obtendo tweets em portugues contendo bolsonaro e russia
tweets <- searchTwitter("Bolsonaro russia", n=10000, lang="pt")
n.tweets <- length(tweets)
tweets.df <- twListToDF(tweets)
tweets.txt <- sapply(tweets, function(t)t$getText())

##### tweets contendo Bolsonaro pré- invasão
tweetsantes <- searchTwitter("Bolsonaro", n=5000, lang="pt",since ="2022-02-10" ,until = "2022-02-22"  )
n.tweetsa <- length(tweetsantes)
tweetsantes.df <- twListToDF(tweetsantes)
tweetsantes.txt <- sapply(tweetsantes, function(t)t$getText())
#### checando horario se +10000 tweets depois da invasão não preciso usar since e until
tweets.df %<>% 
  mutate(
    created = created %>% 
      str_remove_all(pattern = '\\+0000') %>%
      parse_date_time(orders = '%y-%m-%d %H%M%S')
  )

tweets.df %<>% 
  mutate(Created_At_Round = created%>% round(units = 'hours') %>% as.POSIXct())

tweets.df %>% pull(created) %>% min()

tweets.df %>% pull(created) %>% max()
##pós invasão 
tweetsdepois <- searchTwitter("Bolsonaro", n=5000, lang="pt" )
n.tweetsdepois <- length(tweetsdepois)
tweetsdepois.df <- twListToDF(tweetsdepois)
tweetsdepois.txt <- sapply(tweetsdepois, function(t)t$getText())


# função limpando os tweets
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

#aplicando clean text com russia
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
#aplicando clean text antes da invasão
cleanTexta <- clean.text(tweetsantes.txt)
idy <- which(cleanTexta == " ")
cleanTexta <- cleanTexta[cleanTexta != " "]
tweetsantes.df %<>% 
  mutate(
    created = created %>% 
      # Remove zeros.
      str_remove_all(pattern = '\\+0000') %>%
      # Parse date.
      parse_date_time(orders = '%y-%m-%d %H%M%S')
  )

#aplicando clean text depois da invasão
cleanTextd <- clean.text(tweetsdepois.txt)
idz <- which(cleanTextd == " ")
cleanTextd <- cleanTextd[cleanTexta != " "]
tweetsdepois.df %<>% 
  mutate(
    created = created %>% 
      # Remove zeros.
      str_remove_all(pattern = '\\+0000') %>%
      # Parse date.
      parse_date_time(orders = '%y-%m-%d %H%M%S')
  )




##cloud com russia
text_corpus <- Corpus(VectorSource(cleanText))
text_corpus <- tm_map(text_corpus, content_transformer(tolower))
text_corpus <- tm_map(text_corpus, function(x)removeWords(x,stopwords("portuguese")))
text_corpus <- tm_map(text_corpus, removeWords, c("messias" , "jair", "rússia","bolsonaro", "brasileiros", "brasil", "que", "foi", "para", "rú", "rúss" ))
tdm <- TermDocumentMatrix(text_corpus)
tdm <- as.matrix(tdm)
tdm <- sort(rowSums(tdm), decreasing = TRUE)
tdm <- data.frame(word = names(tdm), freq = tdm)
set.seed(113)
dev.off()
wordcloud(text_corpus, min.freq = 10, max.words = 200, scale = c(2,0.5,0.25),
          colors=brewer.pal(7, "Dark2"), random.color = T, random.order = F)
#cloud antes
text_corpusa <- Corpus(VectorSource(cleanTexta))
text_corpusa <- tm_map(text_corpusa, content_transformer(tolower))
text_corpusa <- tm_map(text_corpusa, function(x)removeWords(x,stopwords("portuguese")))
text_corpusa <- tm_map(text_corpusa, removeWords, c("messias" , "jair","rússia","bolsonaro", "brasileiros", "brasil", "que", "foi", "para", "rú", "rúss"))
tdma <- TermDocumentMatrix(text_corpusa)
tdma <- as.matrix(tdma)
tdma <- sort(rowSums(tdma), decreasing = TRUE)
tdma <- data.frame(word = names(tdma), freq = tdma)
dev.off()
set.seed(123)
wordcloud(text_corpusa, min.freq = 10, max.words = 200, scale = c(2,0.5,0.25),
          colors=brewer.pal(7, "Dark2"), random.color = T, random.order = F)
#cloud depois

text_corpusd <- Corpus(VectorSource(cleanTextd))
text_corpusd <- tm_map(text_corpusd, content_transformer(tolower))
text_corpusd <- tm_map(text_corpusd, function(x)removeWords(x,stopwords("portuguese")))
text_corpusd <- tm_map(text_corpusd, removeWords, c("messias" , "jair","rússia","bolsonaro", "brasileiros", "brasil", "que", "foi", "para", "rú", "rúss"))
tdmd <- TermDocumentMatrix(text_corpusd)
tdmd <- as.matrix(tdmd)
tdmd <- sort(rowSums(tdmd), decreasing = TRUE)
tdmd <- data.frame(word = names(tdmd), freq = tdmd)
dev.off()
set.seed(123)
wordcloud(text_corpusd, min.freq = 10, max.words = 200, scale = c(2,0.5,0.25),
          colors=brewer.pal(7, "Dark2"), random.color = T, random.order = F)


###frquency com russia
ggplot(tdm[1:20,], aes(x=reorder(word, freq), y=freq)) + 
  geom_bar(stat="identity") +
  xlab("Termos") + 
  ylab("frequência") + 
  coord_flip() +
  theme(axis.text=element_text(size=7)) +
  ggtitle('Termos mais frequentes') +
  ggeasy::easy_center_title()

###frquency antes

ggplot(tdma[1:20,], aes(x=reorder(word, freq), y=freq)) + 
  geom_bar(stat="identity") +
  xlab("Termos") + 
  ylab("frequência") + 
  coord_flip() +
  theme(axis.text=element_text(size=7)) +
  ggtitle('Termos mais frequentes antes da invasão') +
  ggeasy::easy_center_title()

###frquency depois

ggplot(tdmd[1:20,], aes(x=reorder(word, freq), y=freq)) + 
  geom_bar(stat="identity") +
  xlab("Termos") + 
  ylab("frequência") + 
  coord_flip() +
  theme(axis.text=element_text(size=7)) +
  ggtitle('Termos mais frequentes depois da invasão') +
  ggeasy::easy_center_title()

###começo da analise de sentimentos
positive = scan("D:/documentos/rstudio/projetofinal/positive_words_pt.txt", what = 'character', comment.char = ';')
negative = scan("D:/documentos/rstudio/projetofinal/negative_words_pt.txt", what = 'character', comment.char = ';')

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    sentence = gsub('https://','',sentence)
    sentence = gsub('http://','',sentence)
    sentence = gsub('[^[:graph:]]', ' ',sentence)
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = str_replace_all(sentence,"[^[:graph:]]", " ")
    sentence = tolower(sentence)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}
##### agora aplicando a função score para cada uma
analysis <- score.sentiment(cleanText, positive, negative)
table(analysis$score)
# antes
analysisa <- score.sentiment(cleanTexta, positive, negative)
table(analysisa$score)
#depois
analysisd <- score.sentiment(cleanTextd, positive, negative)
table(analysisd$score)
###grafico de analise de senteimentos
analysis %>%
  ggplot(aes(x=score)) + 
  geom_histogram(binwidth = 1, fill = "black")+ 
  ylab("Frequência") + 
  xlab("score de sentimentos") +
  ggtitle("Distribuição de sentimentos por tweet") +
  ggeasy::easy_center_title()
##antes
dev.off()
analysisa %>%
  ggplot(aes(x=score)) + 
  geom_histogram(binwidth = 1, fill = "black")+ 
  ylab("Frequência") + 
  xlab("score de sentimentos") +
  ggtitle("Distribuição de sentimentos por tweet, antes da invasão") +
  ggeasy::easy_center_title()
##depois
analysisd %>%
  ggplot(aes(x=score)) + 
  geom_histogram(binwidth = 1, fill = "black")+ 
  ylab("Frequência") + 
  xlab("score de sentimentos") +
  ggtitle("Distribuição de sentimentos por tweet, depois da invasão") +
  ggeasy::easy_center_title()
###sentimento barplot 

neutral <- length(which(analysis$score == 0))
positive <- length(which(analysis$score > 0))
negative <- length(which(analysis$score < 0))
Sentiment <- c("Positivo","Neutro","Negativo")
Count <- c(positive,neutral,negative)
output <- data.frame(Sentiment,Count)
output$Sentiment<-factor(output$Sentiment,levels=Sentiment)
ggplot(output, aes(x=Sentiment,y=Count))+
  geom_bar(stat = "identity", aes(fill = Sentiment))+
  ylab("Frequência") + 
  xlab("Score de sentimentos") +
  ggtitle("Barplot de sentimentos")

#antes da invasão
neutrala <- length(which(analysisa$score == 0))
positivea <- length(which(analysisa$score > 0))
negativea <- length(which(analysisa$score < 0))
Sentiment <- c("Positivo","Neutro","Negativo")
Count <- c(positivea,neutrala,negativea)
outputa <- data.frame(Sentiment,Count)
outputa$Sentiment<-factor(outputa$Sentiment,levels=Sentiment)
ggplot(outputa, aes(x=Sentiment,y=Count))+
  geom_bar(stat = "identity", aes(fill = Sentiment))+
  ylab("Frequência") + 
  xlab("Score de sentimentos") +
  ggtitle("Barplot de sentimentos, antes da invasão")
##depois da invasão
neutrald <- length(which(analysisd$score == 0))
positived <- length(which(analysisd$score > 0))
negatived <- length(which(analysisd$score < 0))
Sentiment <- c("Positivo","Neutro","Negativo")
Count <- c(positived,neutrald,negatived)
outputd <- data.frame(Sentiment,Count)
outputd$Sentiment<-factor(outputd$Sentiment,levels=Sentiment)
ggplot(outputd, aes(x=Sentiment,y=Count))+
  geom_bar(stat = "identity", aes(fill = Sentiment))+
  ylab("Frequência") + 
  xlab("Score de sentimentos") +
  ggtitle("Barplot de sentimentos, depois da invasão")
#table
write.table(output, file = "output.txt", sep = ",", quote = FALSE, row.names = F)
write.table(outputa, file = "outputa.txt", sep = ",", quote = FALSE, row.names = F)
write.table(outputd, file = "outputd.txt", sep = ",", quote = FALSE, row.names = F)
#saving datasets




