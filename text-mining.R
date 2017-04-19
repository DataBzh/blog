#Partie 1

##Chargement des packages  
library("tm")
library("tidyverse")
library("tidytext")
library("gutenbergr")

##Charger le document
frenchcourt <- gutenberg_download("50052") %>%
  gutenberg_strip()

##Tidytext

tidytext <- data_frame(line = 1:nrow(frenchcourt), text = frenchcourt$text) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) 

barplot(height=head(tidytext, 10)$n, names.arg= head(tidytext, 10)$word, xlab="Mots", ylab="Fréquence", col="#973232", main="Analyse de 'Pictures of the old French court'")

##Méthode classique

##Créer un corpus
corpus <- frenchcourt %>%
  VectorSource()%>%
  VCorpus()
##Nettoyer le corpus
corpus <- corpus %>%
  tm_map(content_transformer(tolower))%>%
  tm_map(stripWhitespace) %>%
  tm_map(removeNumbers)%>%
  tm_map(removePunctuation)%>%
  tm_map(removeWords, stopwords("english"))
##Créer une matrice des fréquences
TDM <- corpus %>%
  TermDocumentMatrix() %>%
  as.matrix()
TDM <- sort(rowSums(TDM),decreasing=TRUE)
TDM <- data.frame(word = names(TDM),freq=TDM)
#Visualisation de ces 10 mots
barplot(height=head(TDM,10)$freq, names.arg=head(TDM,10)$word, xlab="Mots", ylab="Fréquence", col="#973232", main="Analyse de 'Pictures of the old French court'")

#Partie 2 

#racinisation

corpus2 <- tm_map(corpus1, stemDocument)
TDM2 <- TermDocumentMatrix(corpus2)
TDM2 <- as.matrix(TDM2)

#Partie 3

##Création du nuage de mot
library(wordcloud)
wordcloud(frequence$word, frequence$freq, max.words=100, rot.per=FALSE, colors= c("#973232", "#1E5B5B", "#6D8D2F", "#287928"))

#Partie 4 

##Vecteur de termes associés 
library(qdapTools)
associations <- findAssocs(TDM, c("king","queen"), corlimit = 0.05)
associations_df <-data.frame(list_vect2df(associations))
names(associations_df) <- c("source", "terme", "corr")
library(dplyr)
associations_df <- arrange(associations_df, desc(corr))
library(ggplot2)
ggplot(associations_df[1:50,], aes(x = terme, y = corr, fill=source)) + 
  geom_bar(size = 4, stat="identity", position = "dodge") + 
  coord_flip()+
  xlab("Terme") + 
  ylab("Corrélation") + 
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=15),
        axis.title.x=element_text(margin=margin(20,0,0,0)),
        axis.title.y=element_text(margin=margin(0,20,0,0)),
        plot.margin=margin(20,20,20,20))
