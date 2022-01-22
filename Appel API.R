# Librairie
require(jsonlite)
require(httr)
library(RSQLite)
library(DBI)

# Stockage des identifiants
client_id = "PAR_jobmining_ed402730d0bb4b6d057f41779bb1bab6b08a49e3317ad626c547dc3b055d94cc"
client_secret = "aad6d0b817f01e18114af8dc89c0188a4922043e119b59a467bc2b20a2f88967"

# Création de la liste des options pour la requête POST du token
request_body <- list(grant_type = "client_credentials",
  client_id = client_id,
  client_secret = client_secret,
  scope = paste("api_offresdemploiv2", "o2dsoffre", paste0("application_",client_id), sep = " "))

# POST pour récupérer le token
result_auth <- POST("https://entreprise.pole-emploi.fr/connexion/oauth2/access_token",
  query = list(realm = "/partenaire"),
  body = request_body,
  encode = "form")

# Stockage du token
api_char_token <- base::rawToChar(result_auth$content)
api_JSON_token <- jsonlite::fromJSON(api_char_token, flatten = TRUE)
token = paste("Bearer ",api_JSON_token$access_token)

# GET sur les offres
request <- GET("https://api.emploi-store.fr/partenaire/offresdemploi/v2/offres/search?motsCles=Data", add_headers(Authorization = token))
request_utf <- httr::content(request, as = 'text', encoding="UTF-8")
api_JSON <- jsonlite::fromJSON(request_utf, flatten = TRUE)
data.frame(api_JSON$resultats$id, api_JSON$resultats$intitule)

# Récupérer le code des régions
region <- GET("https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel/regions", add_headers(Authorization = token))
region$content
api_JSON <-  jsonlite::fromJSON(api_char, flatten = TRUE)
regionApi <- data.frame(api_JSON$code, api_JSON$libelle)
colnames(regionApi) <- c("Code", "Libelle")
regionApi


library(leaflet)
library(shinydashboard)
library(shinycssloaders)
library(shiny)
library(DT)
library(leaflet.extras)
library(DBI)
library(tidytext)
library(tidyverse)
library(tm)


#Script creation base de donnees
db <- dbConnect(RSQLite::SQLite(), "corpusOffreData.sqlite")
dbExecute(db, "CREATE TABLE offre (id_offre TEXT PRIMARY KEY, intitule TEXT, description TEXT, date_parution TEXT, poste_predit TEXT, pourc_appartenance TEXT, latitude REAL, longitude REAL, id_partenaire INTEGER, FOREIGN KEY(id_partenaire) REFERENCES partenaire(id_partenaire))")
dbGetQuery(db, "SELECT * from offre")
dbExecute(db, "CREATE TABLE partenaire (id_partenaire INTEGER PRIMARY KEY, partenaire TEXT, logo TEXT)")
dbGetQuery(db, "Select * from partenaire")
dbDisconnect(db)
#unlink("corpus_offre_data.sqlite")

query <- paste("INSERT INTO offre VALUES(",paste(entreprises[1,], collapse =","), ")")
query





