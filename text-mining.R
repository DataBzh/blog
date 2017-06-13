#Partie 1

##Chargement des packages  
library("tm")
library("tidytext")
library("proustr")
library("tidyverse")
devtools::install_github("ThinkRstat/stopwords")
library("stopwords")

##Charger le document
books <- proust_books()

##Tidytext

books_tidy <- proust_books() %>%
  mutate(text = stringr::str_replace_all(.$text, "’", " ")) %>% 
  unnest_tokens(word, text) %>%
  filter(!word %in% stopwords_iso$fr) %>%
  count(word, sort = TRUE) %>%
  head(10)

barplot(height=books_tidy$n, names.arg= books_tidy$word, xlab="Mots", ylab="Fréquence", col="#973232", main="À la recherche du temps perdu")

##Méthode classique

##Créer un corpus
corpus <- proust_books() %>%
  mutate(text = stringr::str_replace_all(.$text, "’", " ")) %>% 
  .$text %>% 
  VectorSource()%>%
  VCorpus()
##Nettoyer le corpus
corpus <- corpus %>%
  tm_map(content_transformer(tolower))%>%
  tm_map(stripWhitespace) %>%
  tm_map(removeNumbers)%>%
  tm_map(removePunctuation)%>%
  tm_map(removeWords, stopwords("french"))
##Créer une matrice des fréquences
TDM <- corpus %>%
  TermDocumentMatrix() %>%
  as.matrix()
TDM <- sort(rowSums(TDM),decreasing=TRUE)
TDM <- data.frame(word = names(TDM),freq=TDM)
#Visualisation de ces 10 mots
barplot(height=head(TDM,10)$freq, names.arg=head(TDM,10)$word, xlab="Mots", ylab="Fréquence", col="#973232", main="À la recherche du temps perdu")

#Partie 2 

#racinisation

library("SnowballC")
library("tidyverse")
library("proustr")
library("tidytext")
books_tidy <- proust_books() %>%
  mutate(text = stringr::str_replace_all(.$text, "’", " ")) %>% 
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE)

nrow(books_tidy)

stem <- wordStem(books_tidy$word, language = "french")
length(unique(stem))

#Partie 3

##Création du nuage de mot
library(wordcloud)
books_tidy <- proust_books() %>%
  mutate(text = stringr::str_replace_all(.$text, "’", " ")) %>% 
  unnest_tokens(word, text) %>%
  filter(!word %in% stopwords_iso$fr) %>%
  count(word, sort = TRUE)
wordcloud(books_tidy$word, books_tidy$n, max.words=100, rot.per=FALSE, colors= c("#973232", "#1E5B5B", "#6D8D2F", "#287928"))

#Partie 4 

##Vecteur de termes associés 
corpus <- proust_books() %>%
  mutate(text = stringr::str_replace_all(.$text, "’", " ")) %>% 
  .$text %>% 
  VectorSource()%>%
  VCorpus()%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(stripWhitespace) %>%
  tm_map(removeNumbers)%>%
  tm_map(removePunctuation)%>%
  tm_map(removeWords, stopwords("french"))%>%
  TermDocumentMatrix() 

library(qdapTools)

associations <- findAssocs(corpus, "temps", corlimit = 0.05) %>% 
  list_vect2df() %>%
  data.frame()
names(associations) <- c("source", "terme", "corr")
associations <- arrange(associations, desc(corr))
ggplot(associations[1:10,], aes(x = terme, y = corr)) + 
  geom_bar(stat="identity") + 
  coord_flip()+
  xlab("Terme") + 
  ylab("Corrélation") + 
  theme_minimal()
