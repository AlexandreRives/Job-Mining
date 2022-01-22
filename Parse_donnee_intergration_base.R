# chemin repertoire de travail 
setwd("C:\\Users\\Inès\\Cours M2 SISE\\Texte Ming - Ricco R\\Projet -- pole emploie")

# chargement des librairies 
library(lubridate)
library(jsonlite)
library(tidyverse)
library(tidytext)
library(RMySQL)
library(httr)
library(tm)

# mise en place accès token 
client_id = "PAR_projettextmining_8c23292b85bd39841eee81f31aacfdbb703b8fb078a132a7ccdbff5a5933f949"
client_secret = "5d1c87fcfe5e20d59b1cf74c299bfcedb4da5e6200bef0faf624e7cceb22f94b"

request_body <- list(grant_type = "client_credentials",
                     client_id = client_id,
                     client_secret = client_secret,
                     scope = paste("api_offresdemploiv2","o2dsoffre", paste0("application_",client_id), sep = " "))

result_auth <- POST("https://entreprise.pole-emploi.fr/connexion/oauth2/access_token",
                    query = list(realm = "/partenaire"),
                    body = request_body,
                    encode = "form")
api_char <-  base::rawToChar(result_auth$content)
api_JSON <-  jsonlite::fromJSON(api_char, flatten = TRUE)
api_JSON

token = paste("Bearer",api_JSON$access_token)

# requête qui permet de récupéré les offres emploie avec mots clé : Data

#test nouvelle technique 
request <- GET("https://api.emploi-store.fr/partenaire/offresdemploi/v2/offres/search?motsCles=Data", add_headers(Authorization = token))
request_utf <- httr::content(request, as = 'text', encoding="UTF-8")
api_JSON <- jsonlite::fromJSON(request_utf, flatten = TRUE)

#création dataframe analyse des offres
id <- api_JSON$resultats$id
description <- api_JSON$resultats$description
intituler <- api_JSON$resultats$intitule
date_parution <- api_JSON$resultats$dateCreation
type_contrat <- api_JSON$resultats$typeContrat
entreprise <- api_JSON$resultats$origineOffre.partenaires
longitude <- api_JSON$resultats$lieuTravail.longitude
latitude <- api_JSON$resultats$lieuTravail.latitude


#=============================================================================#
#         Transformation des offres récupérés par l'api 
#         extraction des informations des liste, insertion des cote simples,
#         transformation date,...
#=============================================================================#

#recodage date
date <- strsplit(date_parution,"T",fixed = T)
date2 <- unlist(lapply(date, function(x){return(x[1])}))

#creation des listes
partenaire <- list()
offre_partenaire <- list()
link <- list()

#extraction des informations
for(i in 1:length(entreprise)){
  nom <- entreprise[[i]]$nom
  logo <- entreprise[[i]]$logo
  if(length(nom) == 0){
    nom <- paste("'","non connu","'",sep="")
    logo <- ''
  }
  if(nom == "'non connu'"){
    offre_partenaire <- c(offre_partenaire,1)
  }
  if(nom == "JOBINTREE"){
    offre_partenaire <- c(offre_partenaire,2)
  }
  if(nom == "WE_RECRUIT"){
    offre_partenaire <- c(offre_partenaire,3)
  }
  if(nom == "JOBIJOBA"){
    offre_partenaire <- c(offre_partenaire,4)
  }
  if(nom == "ENEDIS"){
    offre_partenaire <- c(offre_partenaire,5)
  }
  if(nom == "CAREERBUILDER"){
    offre_partenaire <- c(offre_partenaire,6)
  }
  if(nom == "INZEJOB"){
    offre_partenaire <- c(offre_partenaire,7)
  }
  if(nom == "PMEJOB"){
    offre_partenaire <- c(offre_partenaire,8)
  }
  if(nom == "PEP"){
    offre_partenaire <- c(offre_partenaire,9)
  }
  if(nom == "AEROCONTACT"){
    offre_partenaire <- c(offre_partenaire,10)
  }
  if(nom == "GRD_SEANERGIC"){
    offre_partenaire <- c(offre_partenaire,11)
  }
  if(nom == "STUDYRAMA"){
    offre_partenaire <- c(offre_partenaire,12)
  }
  if(nom == "DIRECTEMPLOI"){
    offre_partenaire <- c(offre_partenaire,13)
  }
  if(nom == "APEC"){
    offre_partenaire <- c(offre_partenaire,14)
  }
  if(nom == "INDEED"){
    offre_partenaire <- c(offre_partenaire,15)
  }
  if(!(nom %in% partenaire)){
    partenaire <- c(partenaire,nom)
  }
  if(!(logo %in% link)){
    link <- c(link,logo)
  }
}

#mise au format chaine de caractère
offre_partenaire <- unlist(offre_partenaire)
partenaire <- unlist(partenaire) 
link <- unlist(link)

#nettoyage de la variable description (pré-traitement)
nettoyage <- function(doc){
  doc <- tolower(doc)
  doc <- gsub("\n","",doc)
  doc <- gsub("'"," ",doc)
  doc <- gsub("[0-9]","",doc)
  
  return(doc)
}

description_net <- nettoyage(description)
intituler_net <- nettoyage(intituler)

#========================================================#
#       Test mettre des cote simple dans description 
#       après transformation Data.frame données qualitative
#========================================================#

# itégration de simple cote pour requete SQL sur les données qualitatives
quali_df <- data.frame(cbind(id,intituler_net,description_net,date2,type_contrat))
quali_df <- apply(quali_df,2,FUN =  function(x){return(paste("'",x,"'",sep = ""))})
class(quali_df)

# création des bases de données
offres <- data.frame(cbind(quali_df,latitude,longitude,offre_partenaire))
partenaire <- data.frame(cbind(1:length(partenaire),partenaire,link))

# types 
class(offres)
class(partenaire)

#supprime les offres longitude/ latitude => NA
offres <- offres[!is.na(offres$latitude),]

#================================================#
#           Insersiton des data.frames 
#           dans la base de données
#================================================#


#connection a la base de données 

conn <- dbConnect(
      drv = RMySQL::MySQL(),
       dbname = "corpus_offres_data",
       host = "localhost",
       username = "root",
       password = "")

query <- paste("INSERT INTO offre VALUES(",apply(offres, 1, paste0, collapse = ","), ")")
#query
res <- dbSendQuery(conn = conn,query)
dbGetStatement(res)