require(jsonlite)
require(httr)

library(leaflet)
library(shinydashboard)
library(shinycssloaders)

client_id = "PAR_jobmining_ed402730d0bb4b6d057f41779bb1bab6b08a49e3317ad626c547dc3b055d94cc"
client_secret = "aad6d0b817f01e18114af8dc89c0188a4922043e119b59a467bc2b20a2f88967"

request_body <- list(grant_type = "client_credentials",
  client_id = client_id,
  client_secret = client_secret,
  scope = paste("api_offresdemploiv2", "o2dsoffre", paste0("application_",client_id), sep = " "))

result_auth <- POST("https://entreprise.pole-emploi.fr/connexion/oauth2/access_token",
  query = list(realm = "/partenaire"),
  body = request_body,
  encode = "form")

result_auth

api_char <-  base::rawToChar(result_auth$content)
api_JSON <-  jsonlite::fromJSON(api_char, flatten = TRUE)
api_JSON$access_token

token = paste("Bearer ",api_JSON$access_token)
token

request <- GET("https://api.emploi-store.fr/partenaire/offresdemploi/v2/offres/search?motsCles=Data", add_headers(Authorization = token))
request$content

#Récupérér le code des régions
region <- GET("https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel/regions", add_headers(Authorization = token))
region$content
api_char <-  base::rawToChar(region$content)
api_JSON <-  jsonlite::fromJSON(api_char, flatten = TRUE)
regionApi <- data.frame(api_JSON$code, api_JSON$libelle)
colnames(regionApi) <- c("Code", "Libelle")
regionApi

api_char <-  base::rawToChar(request$content)
api_JSON <-  jsonlite::fromJSON(api_char, flatten = TRUE)
data.frame(api_JSON$resultats$lieuTravail.libelle)

data.frame(api_JSON$resultats$id, api_JSON$resultats$intitule)

