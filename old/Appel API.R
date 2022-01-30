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

api_JSON$resultats$salaire.libelle
api_JSON$resultats$intitule

api_JSON$resultats$intitule

dfOffres <- data.frame(api_JSON$resultats$id, api_JSON$resultats$intitule, api_JSON$resultats$dateCreation, api_JSON$resultats$typeContrat)

# Récupérer le code des régions
region <- GET("https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel/regions", add_headers(Authorization = token))
region$content
api_JSON <-  jsonlite::fromJSON(api_char, flatten = TRUE)
regionApi <- data.frame(api_JSON$code, api_JSON$libelle)
colnames(regionApi) <- c("Code", "Libelle")
regionApi

#Script creation base de donnees
db <- dbConnect(RSQLite::SQLite(), "corpusOffreData.sqlite")
dbExecute(db, "CREATE TABLE offre (id_offre TEXT PRIMARY KEY, intitule TEXT, description TEXT, date_parution TEXT, latitude REAL, longitude REAL, type_contrat TEXT, statut TEXT, experience TEXT, partenaire TEXT, logo TEXT)")
dbGetQuery(db, "SELECT * from offre")
dbDisconnect(db)
#unlink("corpus_offre_data.sqlite")

# Script pour insertion des données.
query <- paste("INSERT INTO offre VALUES(",paste(entreprises[1,], collapse =","), ")")
query

db <- dbConnect(RSQLite::SQLite(), "corpusOffreData.sqlite")
dbGetQuery(db, "SELECT * from offre")
dbDisconnect(db)


df <- data.frame(matrix(nrow=5, ncol = 2))

names(df) <- c("variable", "percentage")
df$variable <- c("Carbohydrates", "Warming", "NGTnotPresent", "DrainNotPresent", "DrEaMing")
df$percentage <- c(0.67,0.33,0.86,0.78,0.58)

df$variable[1]

df <- df %>% mutate(group=ifelse(percentage <0.6, "red",
                                 ifelse(percentage>=0.6 & percentage<0.8, "orange","green")),
                    label=paste0(percentage*100, "%"),
                    title=dplyr::recode(variable, `Carbohydrates`="Preoperative\ncarbohydrate loading",
                                        `Warming`="Intraoperative\nwarming",
                                        `NGTnotPresent`="Patients without a\nnasogastric tube\non arrival in recovery",
                                        `DrainNotPresent`="Patients without an\nabdominal drain\non arrival in recovery",
                                        `DrEaMing`=paste0("Patients DrEaMing on\npostoperative day 1")))


ggplot(df, aes(fill = group, ymax = percentage, ymin = 0, xmax = 2, xmin = 1)) +
  geom_rect(aes(ymax=1, ymin=0, xmax=2, xmin=1), fill ="#ece8bd") +
  geom_rect() + 
  coord_polar(theta = "y",start=-pi/2) + xlim(c(0, 2)) + ylim(c(0,2)) +
  geom_text(aes(x = 0, y = 0, label = label, colour=group), size=6.5, family="Poppins SemiBold") +
  geom_text(aes(x=1.5, y=1.5, label=title), family="Poppins Light", size=4.2) + 
  facet_wrap(~title, ncol = 5) +
  theme_void() +
  scale_fill_manual(values = c("red"="#C9146C", "orange"="#DA9112", "green"="#129188")) +
  scale_colour_manual(values = c("red"="#C9146C", "orange"="#DA9112", "green"="#129188")) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  guides(fill=FALSE) +
  guides(colour=FALSE)
