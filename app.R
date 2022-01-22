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
                    mainPanel(fluidRow(
                        #fluidRow(actionButton(inputId = "update", label = "Rafraichir", icon(name = "refresh", class = "fa-2x"))),
                        HTML(paste0("<br>")),
                        DT::dataTableOutput("resumeOffres"))
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
    
    # Offres
    output$resumeOffres <- DT::renderDataTable({data=mtcars}, filter = 'top', options = list(lengthMenu = c(10, 15), pageLength = 10))
    
    # # Connection a la base de donnees
    # db <- dbConnect(
    #     drv = RMySQL::MySQL(),
    #     dbname = "corpus_offres_data",
    #     host = "localhost",
    #     username = "root",
    #     password = "")
    # on.exit(dbDisconnect(db), add = TRUE)
    # entreprise <- dbGetQuery(db, paste0("SELECT * FROM offre;"))
    # 
    # # Stockage offre
    # latitude <- offre$latitude
    # longitude <- offre$longitude
    # 
    # ent <- list(latitude = latitude, longitude = longitude)
    # class(ent) <- "offre"
    
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
