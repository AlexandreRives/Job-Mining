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

# Partie front
ui <- shinyUI(fluidPage(
    
    # Charger le style
    includeCSS("www/style.css"),

    # Chargement de la page
    dashboardPage(
        
        skin = "yellow",
        
        dashboardHeader(title="Job Mining", titleWidth = 300),
        
        dashboardSidebar(width = 300,
            sidebarMenu(
                menuItem("Resume des offres", tabName = "resume", icon = icon("table")),
                menuItem("Carte des offres", tabName = "carte", icon = icon("map marked alt")),
                menuItem("Analyse d'un document", tabName = "offre", icon = icon("file")),
                menuItem("Analyse d'un corpus", tabName = "corpus", icon = icon("archive"))
            )
        ),
        
        dashboardBody(
            
            tabItems(
                
                #Tableau recapitulatif des offres
                tabItem(
                    tabName = "resume",
                    # Bouton rafraichir
                    mainPanel(
                        fluidRow(actionButton(inputId = "update" ,"Rafraichir", icon("refresh"), class = "btn btn-primary")),
                        HTML(paste0("<br>")),
                        fluidRow(DT::datatable(data=iris, filter = 'top', options = list(lengthMenu = c(10, 15, 20), pageLength = 10)))
                    )
                ),
                
                #Carte
                tabItem(tabName = "carte",
                        
                    # carte
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

    # Carte
    output$carte <- renderLeaflet({leaflet() %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Open Street Map", options = providerTileOptions(noWrap = TRUE)) %>%
        addFullscreenControl() %>%
        addMarkers(lat = 45.764043, lng = 4.835659) %>%
        addLayersControl(
            baseGroups = c("Open Street Map"),
            position = c("topleft"),
            options = layersControlOptions(collapsed = TRUE)
        )
    })
})

shinyApp(ui = ui, server = server)
