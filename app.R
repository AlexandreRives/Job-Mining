##################################
# Job Mining                     #
#                                #
# by Ines Kara                   #
#    Deffa Ndiaye                #
#    Alexandre Rives             #
#                                #
##################################

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
library(RSQLite)

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
                
                #Tableau recapitulatif des offres
                tabItem(
                    tabName = "resume",
                    # Bouton rafraichir
                    fluidRow(box(width = 12, title = "Resume des offres")),
                    fluidRow(
                        #fluidRow(actionButton(inputId = "update", label = "Rafraichir", icon(name = "refresh", class = "fa-2x"))),
                        HTML(paste0("<br>")),
                        DT::DTOutput("resumeOffres"))
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
    
    # Offres
    output$resumeOffres <- DT::renderDT({data=mtcars}, filter = 'top', options = list(lengthMenu = c(5, 10), pageLength = 10, scrollX = TRUE))
    
    # # Connection a la base de donnees
    db <- dbConnect(RSQLite::SQLite(), "corpusOffreData.sqlite")
        on.exit(dbDisconnect(db), add = TRUE)

    # Requete sur offre + creation objet S3 pour insertion dans le front
    #   offre <- dbGetQuery(db, paste0("SELECT * FROM offre;"))
    # 
    # 
    # latitude <- offre$latitude
    # longitude <- offre$longitude
    # 
    # offre <- list(latitude = latitude, longitude = longitude)
    # class(offre) <- "offre"
    
    latitude = c(45.5, 47.5)
    longitude = c(7.5, 7.8)

    # Carte
    output$carte <- renderLeaflet({leaflet() %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Open Street Map", options = providerTileOptions(noWrap = TRUE)) %>%
        addFullscreenControl() %>%
        addMarkers(lat = latitude, lng = longitude) %>%
        addLayersControl(
            baseGroups = c("Open Street Map"),
            position = c("topleft"),
            options = layersControlOptions(collapsed = TRUE)
        )
    })
})

shinyApp(ui = ui, server = server)
