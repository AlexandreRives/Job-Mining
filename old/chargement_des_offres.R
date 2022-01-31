

# chargement des librairies 
library(lubridate)
library(jsonlite)

library(RSQLite)
library(DBI)
library(httr)

library(data.table)

# ==========================================#
#     Connection ? l'api et r?cup?ration
#               des offres 
# ==========================================#

# mise en place acc?s token 
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

# requ?te qui permet de r?cup?r? les offres emploie avec mots cl? : Data

#test nouvelle technique 
request <- GET("https://api.emploi-store.fr/partenaire/offresdemploi/v2/offres/search?motsCles=Data", add_headers(Authorization = token))
request_utf <- httr::content(request, as = 'text', encoding="UTF-8")
api_JSON <- jsonlite::fromJSON(request_utf, flatten = TRUE)

#creation dataframe analyse des offres
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
#         Transformation des offres recuperes par l'api 
#         extraction des informations des liste,
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

#mise au format chaine de caract?re
partenaire <- unlist(offre_partenaire)
logo <- unlist(logo)


# dataframe des offres recuperees
offre <- data.frame(cbind(id_offre,intitule,description,date_parution,latitude,longitude,type_contrat,statut,experience,partenaire,logo))

#enlever les offres dupliquées en se basant sur l'ID

offre<-offre[!duplicated(offre$id_offre), ]

#================================================#
#           Insertion des offres
#           dans la base de donnees
#================================================#

# connexion 
db <- dbConnect(RSQLite::SQLite(), "corpusOffreData.sqlite")
data<-dbGetQuery(db,paste0("SELECT * FROM offre;"))

# comparaison des nouvelles offres avec celles dans la base

New_offers<-setDT(offre)[!data, on="id_offre"]

# insertion 
if(nrow(data>500)){
  # supprimer les 150 premières lignes
  dbExecute(db,paste0("DELETE FROM offre WHERE id_offre IN (
    SELECT id_offre FROM offre ORDER BY id_offre LIMIT 150);"))
}

dbWriteTable(db,name = "offre",value = New_offers,append=TRUE)

#close
dbDisconnect(db)

db <- dbConnect(RSQLite::SQLite(), "corpusOffreData.sqlite")
query_offres <- dbGetQuery(db, paste0("SELECT id_offre, intitule, date_parution, partenaire, logo, experience, type_contrat FROM offre;"))
print(query_offres)
dbDisconnect(db)









