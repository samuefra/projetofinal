library(twitteR)
library(ROAuth)
library(tidytext)
library(tm)
library(wordcloud)
library(glue)
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
load("tweets.Rda")
load("tweetsantes.Rda")
load("tweetsdepois.Rda")

#prepatando bases
n.tweets <- length(tweets)
tweets.df <- twListToDF(tweets)
tweets.txt <- sapply(tweets, function(t)t$getText())

##### tweets contendo Bolsonaro pré- invasão 10 a 22 de fevereiro
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
dev.off()
ggplot(tdm[1:20,], aes(x=reorder(word, freq), y=freq)) + 
  geom_bar(stat="identity") +
  xlab("Termos") + 
  ylab("frequência") + 
  coord_flip() +
  theme(axis.text=element_text(size=7)) +
  ggtitle('Termos mais frequentes') +
  ggeasy::easy_center_title()
ggsave("freqcom.png")

###frquency antes
dev.off()
ggplot(tdma[1:20,], aes(x=reorder(word, freq), y=freq)) + 
  geom_bar(stat="identity") +
  xlab("Termos") + 
  ylab("frequência") + 
  coord_flip() +
  theme(axis.text=element_text(size=7)) +
  ggtitle('Termos mais frequentes antes da invasão') +
  ggeasy::easy_center_title()
ggsave("freqantes.png")
###frquency depois
dev.off()
ggplot(tdmd[1:20,], aes(x=reorder(word, freq), y=freq)) + 
  geom_bar(stat="identity") +
  xlab("Termos") + 
  ylab("frequência") + 
  coord_flip() +
  theme(axis.text=element_text(size=7)) +
  ggtitle('Termos mais frequentes depois da invasão') +
  ggeasy::easy_center_title()
ggsave("freqdepois.png")
###começo da analise de sentimentos
load("positive.rda")
load("negative.rda")



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
dev.off()
analysis %>%
  ggplot(aes(x=score)) + 
  geom_histogram(binwidth = 1, fill = "black")+ 
  ylab("Frequência") + 
  xlab("score de sentimentos") +
  ggtitle("Distribuição de sentimentos por tweet") +
  ggeasy::easy_center_title()
ggsave("sentimentocom.png")
##antes
dev.off()
analysisa %>%
  ggplot(aes(x=score)) + 
  geom_histogram(binwidth = 1, fill = "black")+ 
  ylab("Frequência") + 
  xlab("score de sentimentos") +
  ggtitle("Distribuição de sentimentos por tweet, antes da invasão") +
  ggeasy::easy_center_title()
ggsave("sentimentoantes.png")
##depois
dev.off()
analysisd %>%
  ggplot(aes(x=score)) + 
  geom_histogram(binwidth = 1, fill = "black")+ 
  ylab("Frequência") + 
  xlab("score de sentimentos") +
  ggtitle("Distribuição de sentimentos por tweet, depois da invasão") +
  ggeasy::easy_center_title()
ggsave("sentimentodepois.png")
###sentimento barplot 
dev.off()
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
ggsave("barplotcom.png")

#antes da invasão
dev.off()
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
ggsave("barplotantes.png")

##depois da invasão
dev.off()
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
ggsave("barplotdepois.png")
#table
write.table(output, file = "output.txt", sep = ",", quote = FALSE, row.names = F)
write.table(outputa, file = "outputa.txt", sep = ",", quote = FALSE, row.names = F)
write.table(outputd, file = "outputd.txt", sep = ",", quote = FALSE, row.names = F)