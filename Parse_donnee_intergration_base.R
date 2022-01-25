# chemin repertoire de travail 
setwd("C:\\Users\\Inès\\Cours M2 SISE\\Texte Ming - Ricco R\\Projet -- pole emploie")

# chargement des librairies 
library(lubridate)
library(jsonlite)
#library(tidyverse)
#library(tidytext)
library(RSQLite)
library(DBI)
library(httr)
#library(tm)

# ==========================================#
#     Connection à l'api et récupération
#               des offres 
# ==========================================#

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
id_offre <- api_JSON$resultats$id
description <- api_JSON$resultats$description
intitule <- api_JSON$resultats$intitule
date_parution <- api_JSON$resultats$dateCreation
type_contrat <- api_JSON$resultats$typeContrat
partenaire_API <- api_JSON$resultats$origineOffre.partenaires
longitude <- api_JSON$resultats$lieuTravail.longitude
latitude <- api_JSON$resultats$lieuTravail.latitude
statut <- api_JSON$resultats$qualificationLibelle
experience <- api_JSON$resultats$experienceLibelle

#=============================================================================#
#         Transformation des offres récupérés par l'api 
#         extraction des informations des liste, insertion des cote simples,
#         transformation date,...
#=============================================================================#

#recodage date
date <- strsplit(date_parution,"T",fixed = T)
date_parution <- unlist(lapply(date, function(x){return(x[1])}))

#creation des listes
offre_partenaire <- list()
logo <- list()

#extraction des informations
for(i in 1:length(partenaire_API)){
  nom <- partenaire_API[[i]]$nom
  logos <- partenaire_API[[i]]$logo
  if(length(nom) == 0){
    nom <- "non connu" #paste("'","non connu","'",sep="")
    logos <- ''
  }
  offre_partenaire <- c(offre_partenaire,nom)
  logo <- c(logo,logos)
}

#mise au format chaine de caractère
partenaire <- unlist(offre_partenaire)
logo <- unlist(logo)

#========================================================#
#       Test mettre des cote simple dans description 
#       après transformation Data.frame données qualitative
#========================================================#

#itégration de simple cote pour requete SQL sur les données qualitatives
#quali_df <- data.frame(cbind(id_offre,intitule,description,date_parution,type_contrat))
#quali_df <- apply(quali_df,2,FUN =  function(x){return(paste("'",x,"'",sep = ""))})
#class(quali_df)

# création des bases de données
offre <- data.frame(cbind(id_offre,intitule,description,date_parution,latitude,longitude,type_contrat,statut,experience,partenaire,logo))
partenaire <- data.frame(cbind(partenaire,logo))


#================================================#
#           Insersiton des data.frames 
#           dans la base de données
#================================================#

#connection 
db <- dbConnect(RSQLite::SQLite(), "corpusOffreData.sqlite")

#insertion 
dbWriteTable(db,name = "offre",value = offre,append=TRUE)

#close
dbDisconnect(db)

  
  
  
  
  
  
  
  
  
  
  


#connection a la base de données 
# query <- paste("INSERT INTO offre (id_offre,intitule,description,date_parution,latitude,longitude,type_contrat,statut,experience) VALUES( SELECT ",",") ")")
# 
# query <- paste("INSERT INTO offre (id_offre,intitule,description,date_parution,latitude,longitude,type_contrat,statut,experience) VALUES(",apply(offres, 1, paste0, collapse = ",")
#       ,",(SELECT id_partenaire from partentaire where partenaire.partenaire=offre.id_partenaire))") 
# 
# INSERT INTO  offre 
# SELECT o.id_offre,o.intitule,o.description,o.date_parution,o.latitude,o.longitude,o.type_contrat,o.statut,o.experience,p.id_partenaire
# FROM offre as o
# LEFT JOIN partenaire as p 
# ON o.id_partenaire = p.partenaire;

