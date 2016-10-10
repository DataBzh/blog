#Partie 1

##Charger tm 
library("tm")
##Charger le document
frenchcourt <- readLines("https://www.gutenberg.org/files/50052/50052-0.txt")
##Créer un corpus
frenchcourtpVS <- VectorSource(frenchcourt)
corpus <- VCorpus(frenchcourtpVS)
##Nettoyer le corpus
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
##Créer une matrice des fréquences
TDM <- TermDocumentMatrix(corpus)
matrixFrenchCourt <- as.matrix(TDM)
sortedmatrix <- sort(rowSums(matrixFrenchCourt),decreasing=TRUE)
frequence <- data.frame(word = names(sortedmatrix),freq=sortedmatrix)
#Les 10 mots les plus présents
tete <- head(frequence, 10)
#Visualisation de ces 10 mots
barplot(height=tete$freq, names.arg=tete$word, xlab="Mots", ylab="Fréquence", col="#973232", main="Analyse de 'Pictures of the old French court'")

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
