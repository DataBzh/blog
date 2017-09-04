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
library("wordcloud")
library("tidyverse")
library("proustr")
library("tidytext")
books_tidy <- proust_books() %>%
  mutate(text = stringr::str_replace_all(.$text, "’", " ")) %>% 
  unnest_tokens(word, text) %>%
  filter(!word %in% stopwords_iso$fr) %>%
  count(word, sort = TRUE)
wordcloud(books_tidy$word, books_tidy$n, max.words=100, rot.per=FALSE, colors= c("#973232", "#1E5B5B", "#6D8D2F", "#287928"))

#Partie 4 

##Vecteur de termes associés 
library("wordcloud")
library("tidyverse")
library("proustr")
library("tidytext")
library("qdapTools")
library("tm")

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

# Partie 5

library(proustr)
library(stringr)
library(dplyr)
proust <- proust_books()
txt <- sample_n(proust, 1) %>% 
 select(text) %>%
 unlist

txt
"Comme nous y rencontrions parfois M. Vinteuil, très sévère pour 
« le genre déplorable des jeunes gens négligés, dans les idées de 
l’époque actuelle », ma mère prenait garde que rien ne clochât dans
ma tenue, puis on partait pour l’église. C’est au mois de Marie que
je me souviens d’avoir commencé à aimer les aubépines. N’étant pas 
seulement dans l’église, si sainte, mais où nous avions le droit 
d’entrer, posées sur l’autel même, inséparables des mystères à la 
célébration desquels elles prenaient part, elles faisaient courir 
au milieu des flambeaux et des vases sacrés leurs branches attachées 
horizontalement les unes aux autres en un apprêt de fête, et 
qu’enjolivaient encore les festons de leur feuillage sur lequel étaient 
semés à profusion, comme sur une traîne de mariée, de petits bouquets de 
boutons d’une blancheur éclatante. Mais, sans oser les regarder qu’à 
la dérobée, je sentais que ces apprêts pompeux étaient vivants et que
c’était la nature elle-même qui, en creusant ces découpures dans les 
feuilles, en ajoutant l’ornement suprême de ces blancs boutons, avait
rendu cette décoration digne de ce qui était à la fois une réjouissance
populaire et une solennité mystique. Plus haut s’ouvraient leurs 
corolles çà et là avec une grâce insouciante, retenant si négligemment
comme un dernier et vaporeux atour le bouquet d’étamines, fines comme 
des fils de la Vierge, qui les embrumait tout entières, qu’en suivant,
qu’en essayant de mimer au fond de moi le geste de leur efflorescence,
je l’imaginais comme si ç’avait été le mouvement de tête étourdi et rapide, 
au regard coquet, aux pupilles diminuées, d’une blanche jeune fille, 
distraite et vive. M. Vinteuil était venu avec sa fille se placer à 
côté de nous. D’une bonne famille, il avait été le professeur de piano 
des sœurs de ma grand’mère et quand, après la mort de sa femme et un 
héritage qu’il avait fait, il s’était retiré auprès de Combray, on le 
recevait souvent à la maison. Mais d’une pudibonderie excessive, il cessa 
de venir pour ne pas rencontrer Swann qui avait fait ce qu’il appelait « un 
mariage déplacé, dans le goût du jour ». Ma mère, ayant appris qu’il composait, 
lui avait dit par amabilité que, quand elle irait le voir, il faudrait qu’il lui 
fît entendre quelque chose de lui. M. Vinteuil en aurait eu beaucoup de joie, 
mais il poussait la politesse et la bonté jusqu’à de tels scrupules que, se 
mettant toujours à la place des autres, il craignait de les ennuyer et de 
leur paraître égoïste s’il suivait ou seulement laissait deviner son désir. 
Le jour où mes parents étaient allés chez lui en visite, je les avais 
accompagnés, mais ils m’avaient permis de rester dehors, et comme la maison 
de M. Vinteuil, Montjouvain, était en contre-bas d’un monticule buissonneux, 
où je m’étais caché, je m’étais trouvé de plain-pied avec le salon du second 
étage, à cinquante centimètres de la fenêtre. Quand on était venu lui annoncer 
mes parents, j’avais vu M. Vinteuil se hâter de mettre en évidence sur le 
piano un morceau de musique. Mais une fois mes parents entrés, il l’avait 
retiré et mis dans un coin. Sans doute avait-il craint de leur laisser 
supposer qu’il n’était heureux de les voir que pour leur jouer de ses 
compositions. Et chaque fois que ma mère était revenue à la charge au 
cours de la visite, il avait répété plusieurs fois : « Mais je ne sais 
qui a mis cela sur le piano, ce n’est pas sa place », et avait détourné 
la conversation sur d’autres sujets, justement parce que ceux-là l’intéressaient 
moins. Sa seule passion était pour sa fille, et celle-ci, qui avait l’air 
d’un garçon, paraissait si robuste qu’on ne pouvait s’empêcher de sourire 
en voyant les précautions que son père prenait pour elle, ayant toujours des 
châles supplémentaires à lui jeter sur les épaules. Ma grand’mère faisait 
remarquer quelle expression douce, délicate, presque timide passait souvent
dans les regards de cette enfant si rude, dont le visage était semé de taches
de son. Quand elle venait de prononcer une parole, elle l’entendait avec 
l’esprit de ceux à qui elle l’avait dite, s’alarmant des malentendus possibles
et on voyait s’éclairer, se découper comme par transparence, sous la figure
hommasse du « bon diable », les traits plus fins d’une jeune fille éplorée."
# Tous les Monsieur quelque chose 
# Ici, le pattern est M, un point (échappé avec \\), un espace, et un ou plusieurs 
str_extract_all(txt, pattern = "M. [:alpha:]*")
[1] "M. Vinteuil" "M. Vinteuil" "M. Vinteuil" "M. Vinteuil" "M. Vinteuil"

# Tous les mots de cinq lettres
# Un espace, cinq caractères, un espace
str_extract_all(txt, pattern = " [:alpha:]{5} ")
 [1] " genre " " idées " " garde " " Marie " " aimer " " droit " " elles " " elles "
 [9] " vases " " leurs " " semés " " comme " " avait " " cette " " digne " " était "
[17] " leurs " " grâce " " comme " " atour " " fines " " mimer " " geste " " comme "
[25] " jeune " " était " " fille " " bonne " " avait " " piano " " sœurs " " après "
[33] " femme " " avait " " cessa " " venir " " Swann " " avait " " ayant " " avait "
[41] " quand " " irait " " chose " " bonté " " place " " allés " " avais " " comme "
[49] " était " " salon " " Quand " " était " " hâter " " piano " " doute " " jouer "
[57] " était " " cours " " avait " " avait " " parce " " seule " " était " " avait "
[65] " ayant " " jeter " " cette " " était " " Quand " " comme " " jeune "

# Tous les mots avant une virgule
str_extract_all(txt, pattern = "[:alpha:]+,")
 [1] "Vinteuil," "négligés," "tenue," "église," "sainte," 
 [6] "entrer," "même," "part," "fête," "profusion," 
[11] "mariée," "Mais," "dérobée," "qui," "feuilles," 
[16] "boutons," "insouciante," "étamines," "Vierge," "entières," 
[21] "suivant," "efflorescence," "rapide," "coquet," "diminuées," 
[26] "fille," "famille," "quand," "fait," "Combray," 
[31] "excessive," "déplacé," "mère," "composait," "que," 
[36] "voir," "joie," "que," "autres," "visite," 
[41] "accompagnés," "dehors," "Vinteuil," "Montjouvain," "buissonneux," 
[46] "caché," "étage," "parents," "entrés," "visite," 
[51] "piano," "sujets," "fille," "ci," "garçon," 
[56] "elle," "douce," "délicate," "rude," "parole," 
[61] "dite," "éclairer," "transparence,"
