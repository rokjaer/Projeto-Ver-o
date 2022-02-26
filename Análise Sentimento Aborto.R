rm(list=ls())
##Instalação de pacotes necessários
library(twitteR)
library(ROAuth)
library(hms)
library(lubridate) 
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
library(magrittr)
library(tidyverse)
library(janeaustenr)
library(widyr)


###APIS's
##O setup para API's do twitter:
#api_key <- "MINHA-CHAVE-API"
#api_secret <- "MINHA-CHAVE-API-SECRETA"
#access_token <- "MEU-TOKEN-DE-ACESSO"
#access_token_secret <- "MEU-TOKEN-DE-ACESSO-SECRETO"
#setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

##Raspagem de dados com a palavra-chave
##Com filtro de data para 3 dias antes do anúncio na Colômbia

##O código utilizava API's, porém por motivo de segurança de dados, as bases foram salvas e importadas para o R.
#As linhas de código utilizadas foi: 

#abort_pt <- searchTwitter("aborto", n=15000,since="2022-02-19",until="2022-02-21",lang="pt")
#abort.txt_pt <- sapply(abort_pt, function(t)t$getText())
#abort.txt_pt <- str_replace_all(abort.txt_pt,"[^[:graph:]]", " ")
#save(abort.txt_pt,file="tweets_pt.rda")

#Carregando as bases:
load("tweets_pt.Rda")
load("tweets_es.Rda")
load("tweets_pt_pos.Rda")
load("tweets_es_pos.Rda")

#Carregando os dicionários:
load("pos.words_pt.Rda")
load("neg.words_pt.Rda")
load("pos.words_es.Rda")
load("neg.words_es.Rda")

##Função para Data Wrangling
clean.text = function(x)
{ x = tolower(x)
  x = gsub("rt", "", x)
  x = gsub("@\\w+", "", x)
  x = gsub("[[:punct:]]", "", x)
  x = gsub("[[:digit:]]", "", x)
  x = gsub("http\\w+", "", x)
  x = gsub("[ |\t]{2,}", "", x)
  x = gsub("^ ", "", x)
  x = gsub(" $", "", x)
  x = gsub('https://','',x)
  x = gsub('http://','',x)
  x = gsub('[^[:graph:]]', ' ',x)
  x = gsub('[[:punct:]]', '', x)
  x = gsub('[[:cntrl:]]', '', x)
  x = gsub('\\d+', '', x)
  x = str_replace_all(x,"[^[:graph:]]", " ")
  return(x)
}

limpo_abort_pt <- clean.text(abort.txt_pt)
idx <- which(limpo_abort_pt == " ")
limpo_abort_pt <- limpo_abort_pt[limpo_abort_pt != " "]


##Setando os dicionários de palavras positivas e negativas
#Novamente, os dicionários carregados serão salvos e importados para o R
#pos.words_pt = scan('C:/Users/rosal/OneDrive/Documentos/Mestrado PUC-Rio/Verão/Ciências de Dados/Projeto/positive_words_pt.txt', what = 'character', comment.char = ';')
#neg.words_pt = scan('C:/Users/rosal/OneDrive/Documentos/Mestrado PUC-Rio/Verão/Ciências de Dados/Projeto/negative_words_pt.txt',what = 'character', comment.char = ';')
#save(pos.words_pt,file=("pos.words_pt.rda"))
#save(neg.words_pt,file=("neg.words_pt.rda"))

score.sentimento = function(sentences, pos.words, neg.words, .progress='none')
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


analysis_abort_pt <- score.sentimento(limpo_abort_pt, pos.words_pt, neg.words_pt)

analysis_abort_pt %>%
  ggplot(aes(x=score)) + 
  geom_histogram(binwidth = 1, fill = "lightblue")+ 
  ylab("Frequência") + 
  xlab("Score de Sentimento para Lusófonos - Antes da aprovação") +
  ggtitle("Distribuição") +
  ggeasy::easy_center_title()
dev.off()
  ggsave("score1.png")

##Distribuição dos tweets coletados em relação ao posicionamento do autor
neutral_pt <- length(which(analysis_abort_pt$score == 0))
positive_pt <- length(which(analysis_abort_pt$score > 0))
negative_pt <- length(which(analysis_abort_pt$score < 0))
Sentimento <- c("Positivo","Neutro","Negativo")
Count_pt <- c(positive_pt,neutral_pt,negative_pt)
output_pt <- data.frame(Sentimento,Count_pt)
output_pt$Sentimento<-factor(output_pt$Sentimento,levels=Sentimento)
ggplot(output_pt, aes(x=Sentimento,y=Count_pt))+
  geom_bar(stat = "identity", aes(fill = Sentimento))+
  ggtitle("Distribuição dos tweets lusófonos em relação ao sentimento - Antes da aprovação")+
  ylab("Quantidade")
dev.off()
ggsave("sentimento1.png")

##Nuvem de Palavras
text_corpus_pt <- Corpus(VectorSource(limpo_abort_pt))
text_corpus_pt <- tm_map(text_corpus_pt, content_transformer(tolower))
text_corpus_pt <- tm_map(text_corpus_pt, function(x)removeWords(x,stopwords("portuguese")))
text_corpus_pt <- tm_map(text_corpus_pt, removeWords, c("aboo","aboar"))
tdm_pt <- TermDocumentMatrix(text_corpus_pt)
tdm_pt <- as.matrix(tdm_pt)
tdm_pt <- sort(rowSums(tdm_pt), decreasing = TRUE)
tdm_pt <- data.frame(word = names(tdm_pt), freq = tdm_pt)
set.seed(123)
dev.off()
wordcloud(text_corpus_pt, min.freq = 1, max.words = 100, scale = c(4.2,1),
          colors=brewer.pal(10, "Dark2"), random.color = T, random.order = F)


## Gráfico de Frequência das Palavras mais Usadas
ggplot(tdm_pt[1:20,], aes(x=reorder(word, freq), y=freq)) + 
  geom_bar(stat="identity") +
  xlab("Termos") + 
  ylab("Quantidade") + 
  coord_flip() +
  theme(axis.text=element_text(size=7)) +
  ggtitle('Palavras Mais Usadas') +
  ggeasy::easy_center_title()
dev.off()
ggsave("freq1.png")

##Agora para a língua espanhola
##Linhas de código utilizadas para limpeza inicial e raspagem de dados
#tweets_es <- searchTwitter("aborto", n=15000,since="2022-02-19",until="2022-02-21",lang="es")
#tweets.txt_es <- sapply(tweets_es, function(t)t$getText())
#tweets.txt_es <- str_replace_all(tweets.txt_es,"[^[:graph:]]", " ")
#cleanText_es <- clean.text(tweets.txt_es)
#idx <- which(cleanText_es == " ")
#cleanText_es <- cleanText_es[cleanText_es != " "]
#save(cleanText_es,file="tweets_es.rda")

## Setando os dicionários de palavras positivas e negativas em espanhol
#Novamente, os dicionários carregados serão salvos e importados para o R
#pos.words_es = scan('C:/Users/rosal/OneDrive/Documentos/Mestrado PUC-Rio/Verão/Ciências de Dados/Projeto/positive_words_es.txt', what = 'character', comment.char = ';')
#neg.words_es = scan('C:/Users/rosal/OneDrive/Documentos/Mestrado PUC-Rio/Verão/Ciências de Dados/Projeto/negative_words_es.txt',what = 'character', comment.char = ';')
#save(pos.words_es,file=("pos.words_es.rda"))
#save(neg.words_es,file=("neg.words_es.rda"))

analysis_es <- score.sentimento(cleanText_es, pos.words_es, neg.words_es)
table(analysis_es$score)

analysis_es %>%
  ggplot(aes(x=score)) + 
  geom_histogram(binwidth = 1, fill = "lightblue")+ 
  ylab("Frequência") + 
  xlab("SCore de Sentimento") +
  ggtitle("Distribuição") +
  ggeasy::easy_center_title()
dev.off()
ggsave("score2.png")



##Gráfico de distribuição de sentimento
neutral_es <- length(which(analysis_es$score == 0))
positive_es <- length(which(analysis_es$score > 0))
negative_es <- length(which(analysis_es$score < 0))
Sentiment_es <- c("Positivo","Neutro","Negativo")
Count_es <- c(positive_es,neutral_es,negative_es)
output_es <- data.frame(Sentiment_es,Count_es)
output_es$Sentiment_es<-factor(output_es$Sentimento,levels=Sentimento)
ggplot(output_es, aes(x=Sentimento,y=Count_es))+
  geom_bar(stat = "identity", aes(fill = Sentimento))+
  ggtitle("Distribuição dos tweets hispanófolos em relação ao sentimento - Antes da aprovação")+
  ylab("Quantidade")+
  xlab("Sentimento")
dev.off()
ggsave("sentimento2.png")

##Nuvem de palavras
text_corpus_es <- Corpus(VectorSource(cleanText_es))
text_corpus_es <- tm_map(text_corpus_es, content_transformer(tolower))
text_corpus_es <- tm_map(text_corpus_es, function(x)removeWords(x,stopwords("spanish")))
text_corpus_es <- tm_map(text_corpus_es, removeWords, c("aboar","aboo"))
tdm_es <- TermDocumentMatrix(text_corpus_es)
tdm_es <- as.matrix(tdm_es)
tdm_es <- sort(rowSums(tdm_es), decreasing = TRUE)
tdm_es <- data.frame(word = names(tdm_es), freq = tdm_es)
set.seed(1235)
dev.off()
wordcloud(text_corpus_es, min.freq = 1, max.words = 100, scale = c(4.2,1),
          colors=brewer.pal(10, "Dark2"), random.color = T, random.order = F)


#Gráfico de palavras mais usadas
ggplot(tdm_es[1:20,], aes(x=reorder(word, freq), y=freq)) + 
  geom_bar(stat="identity") +
  xlab("Palavras") + 
  ylab("Quantidade") + 
  coord_flip() +
  theme(axis.text=element_text(size=7)) +
  ggtitle('Palavras mais Usadas') +
  ggeasy::easy_center_title()
dev.off()
ggsave("freq2.png")



##Com filtro para a data posterior ao anúncio da legalização do aborto na Colômbia
#Lingua portuguesa
#abort_pt_pos <- searchTwitter("aborto", n=15000,since="2022-02-22",until="2022-02-25",lang="pt")
#abort.txt_pt_pos <- sapply(abort_pt_pos, function(t)t$getText())
#abort.txt_pt_pos <- str_replace_all(abort.txt_pt_pos,"[^[:graph:]]", " ")
#limpo_abort_pt_pos <- clean.text(abort.txt_pt_pos)
#idx_pt_pos <- which(limpo_abort_pt_pos == " ")
#limpo_abort_pt_pos <- limpo_abort_pt_pos[limpo_abort_pt_pos != " "]
#save(limpo_abort_pt_pos,file="tweets_pt_pos.rda")

analysis_abort_pt_pos <- score.sentimento(limpo_abort_pt_pos, pos.words_pt, neg.words_pt)
table(analysis_abort_pt_pos$score)

analysis_abort_pt_pos %>%
  ggplot(aes(x=score)) + 
  geom_histogram(binwidth = 1, fill = "lightblue")+ 
  ylab("Frequência") + 
  xlab("Score de Sentimento") +
  ggtitle("Distribuição de c") +
  ggeasy::easy_center_title()
dev.off()
ggsave("score3.png")



##Gráfico de distribuição de sentimento
neutral_pt_pos <- length(which(analysis_abort_pt_pos$score == 0))
positive_pt_pos <- length(which(analysis_abort_pt_pos$score > 0))
negative_pt_pos <- length(which(analysis_abort_pt_pos$score < 0))
Sentimento <- c("Positivo","Neutro","Negativo")
Count_pt_pos <- c(positive_pt_pos,neutral_pt_pos,negative_pt_pos)
output_pt_pos <- data.frame(Sentimento,Count_pt_pos)
output_pt_pos$Sentimento<-factor(output_pt_pos$Sentimento,levels=Sentimento)
ggplot(output_pt_pos, aes(x=Sentimento,y=Count_pt_pos))+
  geom_bar(stat = "identity", aes(fill = Sentimento))+
  ggtitle("Distribuição dos tweets lusófonos em relação ao sentimento - Depois da aprovação")+
  ylab("Quantidade")
dev.off()
ggsave("sentimento3.png")



##Nuvem de palavras
text_corpus_pt_pos <- Corpus(VectorSource(limpo_abort_pt_pos))
text_corpus_pt_pos <- tm_map(text_corpus_pt_pos, content_transformer(tolower))
text_corpus_pt_pos <- tm_map(text_corpus_pt_pos, function(x)removeWords(x,stopwords("portuguese")))
text_corpus_pt_pos <- tm_map(text_corpus_pt_pos, removeWords, c("aboo","aboar"))
tdm_pt_pos <- TermDocumentMatrix(text_corpus_pt_pos)
tdm_pt_pos <- as.matrix(tdm_pt_pos)
tdm_pt_pos <- sort(rowSums(tdm_pt_pos), decreasing = TRUE)
tdm_pt_pos <- data.frame(word = names(tdm_pt_pos), freq = tdm_pt_pos)
set.seed(1234)
dev.off()
wordcloud(text_corpus_pt_pos, min.freq = 1, max.words = 100, scale = c(4.2,1),
          colors=brewer.pal(10, "Dark2"), random.color = T, random.order = F)


##Gráfico das palavras mais usadas
ggplot(tdm_pt_pos[1:20,], aes(x=reorder(word, freq), y=freq)) + 
  geom_bar(stat="identity") +
  xlab("Palavras") + 
  ylab("Quantidade") + 
  coord_flip() +
  theme(axis.text=element_text(size=7)) +
  ggtitle('Palavras Mais Usadas') +
  ggeasy::easy_center_title()
dev.off()
ggsave("freq3.png")


#Por fim, para a língua espanhola
#abort_es_pos <- searchTwitter("aborto", n=15000,since="2022-02-18",until="2022-02-21",lang="es")
#abort.txt_es_pos <- sapply(abort_es_pos, function(t)t$getText())
#abort.txt_es_pos <- str_replace_all(abort.txt_es_pos,"[^[:graph:]]", " ")
#limpo.abort_es_pos <- clean.text(abort.txt_es_pos)
#idx_es_pos <- which(limpo.abort_es_pos == " ")
#limpo.abort_es_pos <- limpo.abort_es_pos[limpo.abort_es_pos != " "]
#save(limpo.abort_es_pos,file="tweets_es_pos.rda")

analysis_es_pos <- score.sentimento(limpo.abort_es_pos, pos.words_es, neg.words_es)
table(analysis_es_pos$score)

analysis_es_pos %>%
  ggplot(aes(x=score)) + 
  geom_histogram(binwidth = 1, fill = "lightblue")+ 
  ylab("Frequência") + 
  xlab("Score de Sentimento") +
  ggtitle("Distribuição") +
  ggeasy::easy_center_title()
dev.off()
ggsave("score4.png")



#Distribuição de sentimento 
neutral_es_pos <- length(which(analysis_es_pos$score == 0))
positive_es_pos <- length(which(analysis_es_pos$score > 0))
negative_es_pos <- length(which(analysis_es_pos$score < 0))
Sentimento <- c("Positivo","Neutro","Negativo")
Count_es_pos <- c(positive_es_pos,neutral_es_pos,negative_es_pos)
output_es_pos <- data.frame(Sentimento,Count_es_pos)
output_es_pos$Sentimento<-factor(output_es_pos$Sentimento,levels=Sentimento)
ggplot(output_es_pos, aes(x=Sentimento,y=Count_es_pos))+
  geom_bar(stat = "identity", aes(fill = Sentimento))+
  ggtitle("Distribuição dos tweets hispanófonos em relação ao sentimento - Após a aprovação")+
  ylab("Quantidade")
dev.off()
ggsave("sentimento4.png")



#Nuvem de palavras
text_corpus_es_pos <- Corpus(VectorSource(limpo.abort_es_pos))
text_corpus_es_pos <- tm_map(text_corpus_es_pos, content_transformer(tolower))
text_corpus_es_pos <- tm_map(text_corpus_es_pos, function(x)removeWords(x,stopwords("spanish")))
text_corpus_es_pos <- tm_map(text_corpus_es_pos, removeWords, c("aboar","aboo"))
tdm_es_pos <- TermDocumentMatrix(text_corpus_es_pos)
tdm_es_pos <- as.matrix(tdm_es_pos)
tdm_es_pos <- sort(rowSums(tdm_es_pos), decreasing = TRUE)
tdm_es_pos <- data.frame(word = names(tdm_es_pos), freq = tdm_es_pos)
set.seed(1236)
dev.off()
wordcloud(text_corpus_es_pos, min.freq = 1, max.words = 100, scale = c(4.2,1),
          colors=brewer.pal(10, "Dark2"), random.color = T, random.order = F)

#Gráfico de palavras mais usadas
ggplot(tdm_es_pos[1:20,], aes(x=reorder(word, freq), y=freq)) + 
  geom_bar(stat="identity") +
  xlab("Palavras") + 
  ylab("Quantidade") + 
  coord_flip() +
  theme(axis.text=element_text(size=7)) +
  ggtitle('Palavras mais Usadas') +
  ggeasy::easy_center_title()
dev.off()
ggsave("freq4.png")

