##################################
# Job Mining                     #
#                                #
# by Ines Kara                   #
#    Deffa Ndiaye                #
#    Alexandre Rives             #
#                                #
##################################

# # vidage de la memoire
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# 
# # Fonction de verification pour installation des packages
packages = c("leaflet", "shinydashboard", "shinycssloaders", "shiny", "DT", "leaflet.extras", "DBI", "tidytext", "tidyverse", "tm", "RSQLite", "httr", "jsonlite", "quanteda", "quanteda.textstats", "dplyr")

package.check <- lapply(
    packages,
    FUN = function(x) {
        if (!require(x, character.only = TRUE)) {
            install.packages(x, dependencies = TRUE)
            library(x, character.only = TRUE)
        }
    }
)

# library(leaflet)
# library(shinydashboard)
# library(shinycssloaders)
# library(shiny)
# library(DT)
# library(leaflet.extras)
# library(DBI)
# library(tidytext)
# library(tm)
# library(RSQLite)
# library(httr)
# library(jsonlite)
# library(quanteda)
# library(dplyr)

# Partie front
ui <- shinyUI(fluidPage(
    
    # Charger le style
    includeCSS("www/style.css"),

    # Page complete
    dashboardPage(
        
        skin = "yellow",
        
        # Header
        dashboardHeader(title="Job Mining", titleWidth = 300),
        
        dashboardSidebar(width = 300,
            sidebarMenu(
                menuItem("Résume des offres", tabName = "resume", icon = icon("table")),
                menuItem("Carte des offres", tabName = "carte", icon = icon("map-marked-alt")),
                menuItem("Analyse d'un document", tabName = "offre", icon = icon("file")),
                menuItem("Analyse d'un corpus", tabName = "corpus", icon = icon("archive")),
                HTML(paste0(
                    "<br><br><br><br><br><br><br><br><br>",
                    "<table style='margin-left:auto; margin-right:auto;'>",
                    "<tr>",
                    "<p style = 'text-align: center;'><small>LinkedIn :</a></small></p>",
                    "<p style = 'text-align: center;'><small><a href='https://www.linkedin.com/in/deffa-ndiaye' target='_blank'>Deffa Ndiaye</a></small></p>",
                    "<p style = 'text-align: center;'><small><a href='https://www.linkedin.com/in/in%C3%A8s-kara-2923b3224/' target='_blank'>In&#232;s Kara</a></small></p>",
                    "<p style = 'text-align: center;'><small><a href='https://www.linkedin.com/in/rives-alexandre/' target='_blank'>Alexandre Rives</a></small></p>",
                    "</tr>",
                    "</table>",
                    "<br>"),
            )
        )),
        
        dashboardBody(
            
            tabItems(
                
                #Tableau récapitulatif des offres
                tabItem(
                    tabName = "resume",
                    # Bouton rafraÃ®chir
                    fluidRow(box(width = 12, title = "Résume des offres", actionButton(inputId = "update", label = "Charger les données", icon(name = "sync", class = "fas fa-sync"), width = "20%"))),
                    fluidRow(
                        DT::DTOutput("resumeOffres")
                    )
                ),

                #Carte
                tabItem(tabName = "carte",

                    leafletOutput("carte")
                    
                ),
                
                #Analyse d une offre
                tabItem(
                    tabName = "offre"
                ),
                
                #Analyse du corpus
                tabItem(
                    tabName = "corpus", 
                )
                    
            )
            
        )
        
    )
    
))

# Partie serveur
server <- shinyServer(function(input, output, session) {
    
    # Fonction nettoyage de donnÃ©es :
    nettoyage <- function(document){
        #passe en miniature 
        document <- tolower(document)
        #retire les sauts de lignes
        document <- gsub("[\n\r/+]","",document)
        #retire la ponctuation
        #document <- gsub("[\\(,);:.?!'-*]"," ",document)
        #retire les chiffres
        document <- gsub("[0-9]","",document)
        #retire les accents
        document <- gsub("[éèëê]","e",document)
        document <- gsub("[àäâ]","a",document)
        document <- gsub("[îï]","i",document)
        document <- gsub("[üû]","u",document)
        document <- gsub("[öô]","o",document)
    }
    
    ######################################
    #             Partie API             #
    ######################################
    
    # # Stockage des identifiants
    # client_id = "PAR_jobmining_ed402730d0bb4b6d057f41779bb1bab6b08a49e3317ad626c547dc3b055d94cc"
    # client_secret = "aad6d0b817f01e18114af8dc89c0188a4922043e119b59a467bc2b20a2f88967"
    # 
    # # Creation de la liste des options pour la requete POST du token
    # request_body <- list(grant_type = "client_credentials",
    #                      client_id = client_id,
    #                      client_secret = client_secret,
    #                      scope = paste("api_offresdemploiv2", "o2dsoffre", paste0("application_",client_id), sep = " "))
    # 
    # # POST pour recuperer le token
    # result_auth <- POST("https://entreprise.pole-emploi.fr/connexion/oauth2/access_token",
    #                     query = list(realm = "/partenaire"),
    #                     body = request_body,
    #                     encode = "form")
    # 
    # # Stockage du token
    # api_char_token <- base::rawToChar(result_auth$content)
    # api_JSON_token <- jsonlite::fromJSON(api_char_token, flatten = TRUE)
    # token = paste("Bearer ",api_JSON_token$access_token)
    # 
    # # GET sur les offres
    # request <- GET("https://api.emploi-store.fr/partenaire/offresdemploi/v2/offres/search?motsCles=Data", add_headers(Authorization = token))
    # request_utf <- httr::content(request, as = 'text', encoding="UTF-8")
    # api_JSON <- jsonlite::fromJSON(request_utf, flatten = TRUE)
    # dfOffres <- data.frame(api_JSON$resultats$id, api_JSON$resultats$intitule, api_JSON$resultats$dateCreation, api_JSON$resultats$typeContrat, api_JSON$resultats$experienceLibelle, api_JSON$resultats$qualificationLibelle)
    # dfPartenaires <- data.frame()
    #RÃ©cupÃ©rer tous les partenaires
    #RÃ©cupÃ©rer tous les logos
    #Faire dfOffres final
    #print(dfOffres)
    
    # Faire controle ici avant insertion Select id offre from offre check si id de l'API different ou egal a un id present dans la table si NON INSERT
    # db <- dbConnect(RSQLite::SQLite(), "corpusOffreData.sqlite")
    # on.exit(dbDisconnect(db), add = TRUE)
    # query_offres <- dbGetQuery(db, paste0("SELECT id_offre FROM offre"))
    # for(i in query_offres$id_offre){
    #     if(i != dfOffres$id){
    #         query <- paste("INSERT INTO offre VALUES(",paste(dfOffres[1,], collapse =","), ")")
    #         dbExecute(db, query)
    #     }
    # }
    
    ######################################
    #        Partie résumé des offres    #
    ######################################
    
    # Affichage des offres :
    db <- dbConnect(RSQLite::SQLite(), "corpusOffreData.sqlite")
    query_offres <- dbGetQuery(db, paste0("SELECT id_offre, intitule, date_parution, partenaire, logo, experience, type_contrat FROM offre;"))

    output$resumeOffres <- DT::renderDT({data=query_offres}, filter = 'top', options = list(lengthMenu = c(5, 10, 20), pageLength = 20, scrollX = TRUE))
    
    # Connexion à la base de donnÃ©es
    query_carte <- dbGetQuery(db, paste0("SELECT id_offre, intitule, type_contrat, experience, latitude, longitude, description FROM offre;"))
    dbDisconnect(db)
    
    # Filtres sur les coordonnées erronées
    query_carte_filtered <- query_carte[query_carte$longitude < 10,]
    
    # Select des descriptions pour affichage sur la popup de la carte.
    query_carte_filtered$description <- nettoyage(query_carte_filtered$description)
    corpus <- corpus(query_carte_filtered,text_field='description')
    
    # Stop words
    mots_vides <- quanteda::stopwords("french")
    mots_vides <- c(mots_vides,c("data","donnee","donnees","tant","que","hf","fh","en","a","e",""))
    
    # CrÃ©ation de la matrice documents termes
    corpus.token <- corpus %>%
        tokens(remove_numbers = TRUE,remove_symbols = TRUE,remove_url = TRUE)%>%
        tokens_remove(mots_vides) %>%
        dfm()
    
    # Liste des maîtrises à matcher avec la matrice documents-termes.
    maitrise <- c("python","r","sql","nosql","knime","tableau","powerbi","sas","azure","aws","statistique","mongodb","hadoop","spark","matlab","scala","java","git","github","gitlab","qliksense","cloud","excel","gcp","hive","qlikview","qlik","talend")

    # map.dmt <- quanteda::dfm_match(dfm(corpus.token),maitrise)
    dfMap <- convert(corpus.token,to="data.frame")
    
    dfMap$doc_id <- NULL
    
    dfMap <- dfMap %>% select(one_of(maitrise))

    liste_maitrise <- apply(dfMap, 1, function(x){
        index <- which(x>0)
        return(names(dfMap)[index])
    })

    df_maitrise <- c()
    for(i in liste_maitrise){
        if(length(i) == 0){
            i <- "Non renseigné"
        }
        df_maitrise <- rbind(df_maitrise, toString(i))
    }

    query_carte_filtered <- cbind(query_carte_filtered, df_maitrise)
    
    ######################################
    #           Partie carte             #
    ######################################
    
    # Préparation des données à afficher sur la carte
    map_data <- paste("Id offre : ", query_carte_filtered$id, "<br/>", "Intitulé : ", query_carte_filtered$intitule, "<br/>", "Type contrat : ", query_carte_filtered$type_contrat, "<br/>", "Expérience requise : ", query_carte_filtered$experience, "<br/>", "Compétences requises : ", query_carte_filtered$df_maitrise)
    
    # Carte
    output$carte <- renderLeaflet({leaflet(map_data) %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Open Street Map", options = providerTileOptions(noWrap = TRUE)) %>%
        addMarkers(lat = query_carte_filtered$latitude, lng = query_carte_filtered$longitude, popup = map_data, clusterOptions = markerClusterOptions()) %>%
        addLayersControl(
            baseGroups = c("Open Street Map"),
            position = c("topleft"),
            options = layersControlOptions(collapsed = TRUE)
        )
    })
    
    ######################################
    #        Partie analyse du corpus    #
    ######################################
    
    
})

shinyApp(ui = ui, server = server)
