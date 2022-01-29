##################################
# Job Mining                     #
#                                #
# by Ines Kara                   #
#    Deffa Ndiaye                #
#    Alexandre Rives             #
#                                #
##################################

# vidage de la memoire
rm(list=ls()) 

# Fonction de verification pour installation des packages
packages = c("leaflet", "shinydashboard", "shinycssloaders", "shiny", "DT", "leaflet.extras", "DBI", "tidytext", "tidyverse", "tm", "RSQLite", "httr", "jsonlite", "quanteda")

package.check <- lapply(
    packages,
    FUN = function(x) {
        if (!require(x, character.only = TRUE)) {
            install.packages(x, dependencies = TRUE)
            library(x, character.only = TRUE)
        }
    }
)

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
                menuItem("Resume des offres", tabName = "resume", icon = icon("table")),
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
                    # Bouton rafraichir
                    fluidRow(box(width = 12, title = "Résume des offres", actionButton(inputId = "update", label = "Charger les donnees", icon(name = "refresh", class = "fa-2x"), width = "20%"))),
                    fluidRow(
                        DT::DTOutput("resumeOffres")
                    )
                ),
                
                #Carte
                tabItem(tabName = "carte",

                    leafletOutput("carte") %>% withSpinner(color="black")
                    
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
    
    # # Stockage des identifiants
    client_id = "PAR_jobmining_ed402730d0bb4b6d057f41779bb1bab6b08a49e3317ad626c547dc3b055d94cc"
    client_secret = "aad6d0b817f01e18114af8dc89c0188a4922043e119b59a467bc2b20a2f88967"

    # Creation de la liste des options pour la requete POST du token
    request_body <- list(grant_type = "client_credentials",
                         client_id = client_id,
                         client_secret = client_secret,
                         scope = paste("api_offresdemploiv2", "o2dsoffre", paste0("application_",client_id), sep = " "))

    # POST pour recuperer le token
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
    # dfOffres <- data.frame(api_JSON$resultats$id, api_JSON$resultats$intitule, api_JSON$resultats$dateCreation, api_JSON$resultats$typeContrat, api_JSON$resultats$experienceLibelle, api_JSON$resultats$qualificationLibelle)
    # dfPartenaires <- data.frame()
    # #Récupérer tous les partenaires
    # #Récupérer tous les logos
    # #Faire dfOffres final
    # print(dfOffres)
    
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
    
    # Affichage des offres :
    db <- dbConnect(RSQLite::SQLite(), "corpusOffreData.sqlite")
    query_offres <- dbGetQuery(db, paste0("SELECT id_offre, intitule, date_parution, logo, type_contrat,experience FROM offre;"))
    query_offres$logo<-paste0('<img src="',query_offres$logo, '"/>')
    #renommer la table des offres 
    query_offres <-rename(query_offres, c("N° Offre"="id_offre", "Intitule de l'offre"="intitule", "Date de parution"="date_parution",
                                          "Partenaire"="logo","Type de contrat"="type_contrat","Expérience"="experience"))
    dbDisconnect(db)
    output$resumeOffres <- DT::renderDT({DT::datatable(query_offres,escape = FALSE)}, filter = 'top', options = list(lengthMenu = c(5, 10, 20), pageLength = 20, scrollX = TRUE))
    
    # Connexion à la base de données
    db <- dbConnect(RSQLite::SQLite(), "corpusOffreData.sqlite")
    query_coordonnees <- dbGetQuery(db, paste0("SELECT latitude, longitude FROM offre;"))
    dbDisconnect(db)

    # Carte
    output$carte <- renderLeaflet({leaflet() %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Open Street Map", options = providerTileOptions(noWrap = TRUE)) %>%
        addFullscreenControl() %>%
        addMarkers(lat = query_coordonnees$latitude, lng = query_coordonnees$longitude, label = c(query_coordonnees$latitude)) %>%
        addLayersControl(
            baseGroups = c("Open Street Map"),
            position = c("topleft"),
            options = layersControlOptions(collapsed = TRUE)
        )
    })
})

shinyApp(ui = ui, server = server)
