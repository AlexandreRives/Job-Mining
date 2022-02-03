#============================================#
#     Chargement des donnn?es
#     pr? nettoyage (tolower, accent, \n)
#============================================#
#library(tidyverse)
#library(SnowballC)
#library(tidytext)
#library(tm)

library(quanteda)
library(caret)
library(lsa)

#r?partoire de travail 
setwd("C:\\Users\\In?s\\Cours M2 SISE\\Texte Ming - Ricco R\\Projet -- pole emploie")

d <- etiquetage_LDA

# import fichier ?tiquett?  
d <- readxl::read_excel("etiquetage.xlsx")

# tolower label 
d$label <- tolower(d$label)

#============================================#
#     Cr?ation du jeu train et du jeu test 
#============================================#

#concate titre et description
d$doc <- paste(d$intitule,d$description,sep = " ")


#Cr?ation de dTrain et dTest 
training.idx <- createDataPartition(d$label, p=0.7, list = FALSE)

training <- d[training.idx,] # creation du jeu de donn?es "train" 
testing <- d[-training.idx,] # creation du jeu de donn?es "test"

#============================================================================#
#     Fonction de nettoyage : commune ? chaque jeu de donn?es 
#     IMPORTANT d'utilis? les m?mes fonction sur le jeu test et train
#     Cr?ation d'un fonction est donc utile pour ne pas oublier des ?tapes
#============================================================================#

#fonction de nettoyage des donn?es 
nettoyage <- function(document){
  #passe en miniature 
  document <- tolower(document)
  #retire les sauts de lignes
  document <- gsub("[\n\r/+]","",document)
  #retire la ponctuation
  document <- gsub("[\\(,);:.?!'-]"," ",document)
  #retire les chiffres
  document <- gsub("[0-9]","",document)
  #retire les accents
  document <- gsub("[????]","e",document)
  document <- gsub("[???]","a",document)
  document <- gsub("[??]","i",document)
  document <- gsub("[??]","u",document)
  document <- gsub("[??]","o",document)
}

d$doc <- nettoyage(d$doc)

#==================================================#
#       Modification du df pour qu'il soit adapt? 
#       au format de quantedas 
#==================================================#

train.corpus <- quanteda::corpus(d,text_field='doc')

#=====================================#
#       Tokennization des termes
#       Suppression des stopwords
#       Stemining
#=====================================#

mots_vides <- quanteda::stopwords(language = 'fr')
mots_vides <- c(mots_vides,c("data","donnee","donnees","tant","que","hf","fh","en","a","e"))

word_data <- c("python","r","java","javascript","scala","cloud","spark","analyst","analyse",
               "scientist","science","azure","talend","qlik","ia","engineering","engineer",
               "learning","deep","analyser","aide","algorithme","decision","predictive","predictif",
               "predictives","alimenter","alimente","datawarehouse","etl","bi","gestion","modelisation",
               "backlog","business","management","manager","powerbi","entrepot","entrepots","reporting",
               "powerplateform","saas","sas","api","lake","dynamics","sql","excel","reportings","gouvernace",
               "jira","confluence","kafka","spec","mongodb","oracle","hadoop","SAP","datastage","MicroStrategy",
               "informatica","kpi","kpis","scikit","learn","pandas","datalab","datalakes","batch",
               "pipelines","hdfs","hive","hbase","cloudera","mapr","awz","gcp","docker","cassandra",
               "elasticsearch","datacenter","it",'agile',"srum","pilotage","si")

quanteda_options(verbose = TRUE)

#tokenize : v?rifie que tout les nombres et carat?re sp?ciaux on bien ?t? supprim?
train.tokens <- tokens(train.corpus,remove_numbers = TRUE,remove_punct =TRUE,remove_symbols = TRUE)

#suppression des stopswords
train.tokens_spw <- tokens_select(train.tokens,pattern = mots_vides,selection = "remove")

#applique un alog de stemining (r?cup?re la racine des mots)
train.tokens_spw_stem <- tokens_wordstem(train.tokens_spw,language = 'fr')


#=========================================================#
#       Cr?ation de la matrice documents termes 
#       Mise en place de la fr?quence a d?quois pour mdt
#=========================================================#

#cr?ation mdt
dmtTrain <- dfm(train.tokens_spw_stem)

print(dmtTrain)
#print(quanteda::topfeatures(dmtTrain,10))
#print(head(sort(colSums(dmtTrain),decreasing=TRUE),10))

#applique la fr?quence la plus ad?quois 
dfm_tfidf()
dfm_smooth()
dfm_weight()

dmtTrain_freq

#fait un trie -- les mots qui apparaisent dans moins de n document son vir? 

#dmtTrain_sel <- dfm_trim(dmtTrain,min_docfreq = n)
#print(head(dmtTrain_sel))

#=========================================================#
#               Convertion en data.frame : 
#       pour appliqu? des techniques d'algo suppervis? 
#=========================================================#

data.train <- convert(dmtTrain,to="data.frame")
print(head(colnames(data.train)))

#supprime la colonne indexe

data.train$doc_id <- NULL

#rajoute le labell 
data.train$LABEL <- training$label
print(tail(colnames(data.train),10))

#============================================#
#       Flitre python count offre 
#============================================#

data.train[data.train$python > 0,c("LABEL")]

data.train[data.train$r > 0,c("LABEL")]
data.train[data.train$jav > 0,c("LABEL")]


#==============================#
#     Test arbre de d?cision 
#==============================#


library(quanteda.textmodels)
mylsa <- textmodel_lsa(dmtTrain)
plot(mylsa$docs[,1:2])

plot(mylsa$features[,1:2])


mylsa$matrix_low_rank


# Cr?er un dictionnaire qui se rapproche du m?tier de data scientist : R, Python, Machine, Learning, SQL ..................

# Matrice document terme => sur chaque document dico / mdt => si y'a une offre o? tu n'as pas un mot du dictionnaire => CIAO L'OFFRE !

# MDT

library(RSQLite)
library(DBI)

db <- dbConnect(RSQLite::SQLite(), "corpusOffreData.sqlite")
on.exit(dbDisconnect(db), add = TRUE)
offreResume <- dbGetQuery(db, paste0("SELECT id_offre, intitule, date_parution, partenaire, logo FROM offre INNER JOIN partenaire ON offre.id_partenaire = partenaire.id_partenaire;"))

offres <- list(id = offreResume$id_offre, intitule = offreResume$intitule, date_parution = offreResume$date_parution, logo = offreResume$logo, partenaire = offreResume$partenaire)
class(offres) <- "offres"

