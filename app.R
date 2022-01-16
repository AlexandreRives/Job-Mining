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
                menuItem("Carte des offres", tabName = "carte", icon = icon("map marked alt")),
                menuItem("Analyse d'un document", tabName = "offre", icon = icon("file")),
                menuItem("Analyse d'un corpus", tabName = "corpus", icon = icon("archive")),
                HTML(paste0(
                    "<br><br><br><br><br><br><br><br><br>",
                    "<table style='margin-left:auto; margin-right:auto;'>",
                    "<tr>",
                    "<p style = 'text-align: center;'><small>LinkedIn :</a></small></p>",
                    "<p style = 'text-align: center;'><small><a href='https://www.linkedin.com/in/deffa-ndiaye' target='_blank'>Deffa Ndiaye</a></small></p>",
                    "<p style = 'text-align: center;'><small><a href='https://www.linkedin.com/in/ines-kara' target='_blank'>Ines Kara</a></small></p>",
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
                    mainPanel(
                    fluidRow(actionButton(inputId = "update", label = "Rafraichir", icon(name = "refresh", class = "fa-2x"))),
                    HTML(paste0("<br>")),
                    DT::datatable(data=mtcars, filter = 'top', options = list(lengthMenu = c(10, 15, 20), pageLength = 10))
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
    
    
    # Connection a la base de donnees
    # output$tbl <- renderTable({
    #     conn <- dbConnect(
    #         drv = RMySQL::MySQL(),
    #         dbname = "shinydemo",
    #         host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
    #         username = "guest",
    #         password = "guest")
    #     on.exit(dbDisconnect(conn), add = TRUE)
    #     dbGetQuery(conn, paste0(
    #         "SELECT * FROM City LIMIT ", input$nrows, ";"))
    # })
    
    #Coordonnees
    Latitude <- c(45.764043, 45.764050)
    Longitude <- c(4.835659, 4.835870)

    # Carte
    output$carte <- renderLeaflet({leaflet() %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Open Street Map", options = providerTileOptions(noWrap = TRUE)) %>%
        addFullscreenControl() %>%
        addMarkers(lat = Latitude, lng = Longitude) %>%
        addLayersControl(
            baseGroups = c("Open Street Map"),
            position = c("topleft"),
            options = layersControlOptions(collapsed = TRUE)
        )
    })
})

shinyApp(ui = ui, server = server)
