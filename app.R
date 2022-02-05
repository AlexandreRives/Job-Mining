##################################
# Job Mining                     #
#                                #
# by Ines Kara                   #
#    Deffa Ndiaye                #
#    Alexandre Rives             #
#                                #
##################################

# vider de la mémoire, sélection du chemin par défaut.
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Fonction de vérification pour installation des packages
packages = c("leaflet", "shinydashboard", "shinycssloaders", "shiny","shinyWidgets", "DT", "leaflet.extras", "DBI", "tidytext", "tidyverse", "tm", "RSQLite", "httr", "jsonlite", "quanteda", "quanteda.textstats", "dplyr", "data.table","quanteda.textmodels")



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
# library(shinyWidgets)
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
# library(data.table)
# library(quanteda.textstats)
# library(quanteda.textmodels)
# library(ggplot2)

# Liste des mots qui nous intéressent
word_data <- c("python","r","java","javascript","scala","cloud","spark","analyst","analyse",
               "scientist","science","azure","talend","qlik","ia","engineering","engineer",
               "learning","deep","analyser","aide","algorithme","decision","predictive","predictif",
               "predictives","alimenter","alimente","datawarehouse","etl","bi","gestion","modelisation",
               "backlog","business","management","manager","powerbi","entrepot","entrepots","reporting",
               "powerplateform","saas","sas","api","lake","dynamics","sql","excel","reportings","gouvernace",
               "jira","confluence","kafka","spec","mongodb","oracle","hadoop","SAP","datastage","MicroStrategy",
               "informatica","kpi","kpis","scikit","learn","pandas","datalab","datalakes","batch",
               "pipelines","hdfs","hive","hbase","cloudera","mapr","awz","gcp","docker","cassandra",
               "elasticsearch","datacenter","it",'agile',"scrum","pilotage","si", "tensorflow", "keras")

VB_style <- function(msg = 'Hello', style="font-size: 100%;"){
  tags$p( msg , style = style )
}

# Affichage des offres
db <- dbConnect(RSQLite::SQLite(), "corpusOffreData.sqlite")
query_offres <- dbGetQuery(db, paste0("SELECT id_offre, intitule, date_parution, logo, experience, type_contrat FROM offre;"))

# Retraitement des adresses web des logos
query_offres$logo<-paste0('<img src="',query_offres$logo, '"/>')

# Retraitement des adresses web des offres
query_offres$id_offre<-paste0('<a href=https://candidat.pole-emploi.fr/offres/recherche/detail/',query_offres$id_offre,'/>',query_offres$id_offre,'</a>')

#renommer les colonnes de la table des offres
query_offres <-rename(query_offres, c("N° Offre"="id_offre", "Intitulé de l'offre"="intitule", "Date de parution"="date_parution",
                                      "Partenaire"="logo","Type de contrat"="type_contrat","Expérience"="experience"))
dbDisconnect(db)

affichage_data <- reactiveValues(data = query_offres)

# Partie front
ui <- shinyUI(fluidPage(
    
    # Charger le style
    includeCSS("www/style.css"),

    # Page complète
    dashboardPage(
        
        skin = "yellow",
        
        # Header
        dashboardHeader(title="Job Mining", titleWidth = 300),
        
        dashboardSidebar(width = 300,
            sidebarMenu(
                menuItem("Résume des offres", tabName = "resume", icon = icon("table")),
                menuItem("Carte des offres", tabName = "carte", icon = icon("map-marked-alt")),
                menuItem("Analyse d'un corpus", tabName = "corpus", icon = icon("archive")),
                menuItem("Analyse sémantique latente", tabName = "lsa", icon = icon("file")),
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
                    
                    # Bouton rafraîchir
                    fluidRow(box(width = 12, title = "Résume des offres", 
                                 actionButton(inputId = "update", label = "Charger les données", icon(name = "sync", class = "fas fa-sync"), width = "20%" ))),

                    fluidRow(
                        addSpinner(DT::DTOutput("resumeOffres"), spin = "circle", color="black")
                    )
                ),

                #Carte
                tabItem(tabName = "carte",

                    addSpinner(leafletOutput("carte"), spin = "circle", color = "black")
                    
                ),
                
                #Analyse du corpus
                tabItem(
                  tabName = "corpus",
                  fluidRow(box(title = "Analyse du corpus", width = 12)),
                  fluidRow(column(width=4,h2("Offres")),  
                           column(width=4 ,h2("Top métier")),
                           column(width=4,h2("Offre débutant"))),
                  fluidRow(
                    valueBoxOutput("nbOffre"),
                    valueBoxOutput("topmetier"),
                    valueBoxOutput("jobdeb")),
                  fluidRow(
                    column(
                      width=6,sliderInput("slider","Nombre de mots : ",min = 1,max = 25 ,value = 10, width="70%"),
                      plotOutput("plot_freq", height = "800px")),
                    column(
                      width=6,
                      
                          selectInput("select","Axe d'analyse :",choices = c("Type contrat","Expériences","Statut")),
                          multiInput(inputId = "multinput",label = "Sélectionner au maximum 5 mots :",selected = c("python","r"),choices = word_data,width = "auto"),
                          plotOutput("plot2", height = "600px"))
                  )
                ),
                tabItem(tabName = "lsa",
                        fluidRow(box(title = "Analyse sémantique latente", width = 12)),
                        fluidRow(
                          column(width = 6, plotOutput("plot_doc",height = "900px")),
                          column(width=6,plotOutput("plot_txt",height = "900px")))
                )
            )
        )
    )
))

# Partie serveur
server <- shinyServer(function(input, output, session) {
  
    #rafraichisement des données 
    refresh <- reactive({
      session$reload()
      Sys.sleep(3)
      Sys.time()
    })
    
    # Fonction nettoyage de données
    nettoyage <- function(document){
        #passe en miniature 
        document <- tolower(document)
        #retire les sauts de lignes
        document <- gsub("[\n\r/+]"," ",document)
        #retire la ponctuation
        document <- gsub("[\\(,);:.?!'-*]"," ",document)
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
    
    observeEvent(input$update, {
      
      # Stockage des identifiants
      client_id = "PAR_projettextmining_8c23292b85bd39841eee81f31aacfdbb703b8fb078a132a7ccdbff5a5933f949"
      client_secret = "5d1c87fcfe5e20d59b1cf74c299bfcedb4da5e6200bef0faf624e7cceb22f94b"
      
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
      
      # Création dataframe analyse des offres
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
      
      # Recodage date
      date <- strsplit(date_parution,"T",fixed = T)
      date_parution <- unlist(lapply(date, function(x){return(x[1])}))
      
      # Création des listes
      offre_partenaire <- list()
      logo <- list()
      
      # Extraction des informations
      for(i in 1:length(partenaire_API)){
        nom <- partenaire_API[[i]]$nom
        logos <- partenaire_API[[i]]$logo
        if(length(nom) == 0){
          nom <- "NON CONNU"
          logos <- ''
        }
        offre_partenaire <- c(offre_partenaire,nom)
        logo <- c(logo,logos)
      }
      
      # Mise au format chaine de caractère
      partenaire <- unlist(offre_partenaire)
      logo <- unlist(logo)
      
      # dataframe des offres récupérées
      offre <- data.frame(cbind(id_offre,intitule,description,date_parution,latitude,longitude,type_contrat,statut,experience,partenaire,logo))
      
      # Enlever les offres dupliquées en se basant sur l'ID
      offre <- offre[!duplicated(offre$id_offre), ]
      
      #================================================#
      #           Insertion des offres
      #           dans la base de donnees
      #================================================#
      
      # Connexion 
      db <- dbConnect(RSQLite::SQLite(), "corpusOffreData.sqlite")
      data <- dbGetQuery(db,paste0("SELECT * FROM offre;"))
      
      # Comparaison des nouvelles offres avec celles dans la base
      New_offers <- setDT(offre)[!data, on="id_offre"]
      
      # Insertion 
      if(nrow(data)>500){
        # Supprimer les 150 premières lignes
        dbExecute(db,paste0("DELETE FROM offre WHERE id_offre IN (SELECT id_offre FROM offre ORDER BY id_offre LIMIT 150);"))
      }

      dbWriteTable(db, name ="offre", value = New_offers,append=TRUE)
      
      query_offres_maj <- dbGetQuery(db, paste0("SELECT id_offre, intitule, date_parution, logo, experience, type_contrat FROM offre;"))
      
      # Retraitement des adresses web des logos
      query_offres_maj$logo <- paste0('<img src="',query_offres_maj$logo, '"/>')
      
      # Retraitement des adresses web des offres
      query_offres_maj$id_offre<-paste0('<a href=https://candidat.pole-emploi.fr/offres/recherche/detail/',query_offres_maj$id_offre,'/>',query_offres_maj$id_offre,'</a>')
      
      #renommer les colonnes de la table des offres 
      query_offres_maj <-rename(query_offres_maj, c("N° Offre"="id_offre", "Intitulé de l'offre"="intitule", "Date de parution"="date_parution",
                                            "Partenaire"="logo","Type de contrat"="type_contrat","Expérience"="experience"))
      # Déconnexion de la bdd
      dbDisconnect(db)
      
      # Mis à jour des données
      affichage_data$data <- query_offres_maj
      
      refresh()
      
    })
    
    
    ######################################
    #        Partie résumé des offres  #
    ######################################
    
    # Affichage de tous les éléments
    
    output$resumeOffres <- DT::renderDT({DT::datatable(affichage_data$data,escape = FALSE)}, options = list(lengthMenu = c(5, 10, 20), pageLength = 20, scrollX = TRUE))
    
    ######################################
    #           Partie carte             #
    ######################################
    
    db <- dbConnect(RSQLite::SQLite(), "corpusOffreData.sqlite")
    
    # Connexion à la base de données
    query_carte <- dbGetQuery(db, paste0("SELECT id_offre, intitule, type_contrat, experience, latitude, longitude, description FROM offre;"))
    
    # Filtres sur les coordonnées erronées
    query_carte_filtered <- query_carte[query_carte$longitude < 10,]
    
    # Select des descriptions pour affichage sur la popup de la carte.
    query_carte_filtered$description <- nettoyage(query_carte_filtered$description)
    corpus <- corpus(query_carte_filtered,text_field='description')
    
    # Stop words
    mots_vides <- quanteda::stopwords("french")
    mots_vides <- c(mots_vides,c("data","donnee","donnees","tant","que","hf","fh","en","a","e",""))
    
    # Création de la matrice documents termes
    corpus.token <- corpus %>%
      tokens(remove_numbers = TRUE,remove_symbols = TRUE,remove_url = TRUE)%>%
      tokens_remove(mots_vides) %>%
      dfm()
    
    # Liste des maîtrises à matcher avec la matrice documents-termes.
    maitrise <- c("python","r","sql","nosql","knime","tableau","powerbi","sas","azure","aws","statistique","mongodb","hadoop","spark","matlab","scala","java","git","github","gitlab","qliksense","cloud","excel","gcp","hive","qlikview","qlik","talend","tensorflow","keras", "dax", "etl", "reporting", "management")
    
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
        i <- "Non renseignée"
      }
      df_maitrise <- rbind(df_maitrise, toString(i))
    }
    
    query_carte_filtered <- cbind(query_carte_filtered, df_maitrise)

    # Préparation des données à afficher sur la carte
    map_data <- paste("Id Offre : ", query_carte_filtered$id_offre, "<br/>", "Intitulé : ", query_carte_filtered$intitule, "<br/>", "Type contrat : ", query_carte_filtered$type_contrat, "<br/>", "Expérience requise : ", query_carte_filtered$experience, "<br/>", "Compétences requises : ", query_carte_filtered$df_maitrise)
   
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
    
    dbDisconnect(db)
    
    ######################################
    #        Partie analyse du corpus    #
    ######################################
    
    # connexion base de données
    conn <- dbConnect(RSQLite::SQLite(), "corpusOffreData.sqlite")
    
    df <- dbGetQuery(conn,"SELECT * FROM offre")
    
    dbDisconnect(conn)
    
    #nettoyage de la description
    df$description <- nettoyage(df$description)
    
    #création du corpus 
    corpus <- quanteda::corpus(df,text_field='description')
    
    #création liste des mots intéressants
    
    #tokenize : vérifie que tous les nombres et caratère spéciaux ont bien été supprimés
    tokens <- tokens(corpus,remove_numbers = TRUE,remove_symbols = TRUE)
    
    #sélection des mots dans corpus 
    tokens_spw <- tokens %>%
      tokens_select(pattern = word_data,selection = "keep")
    
    #création de la matrice doc-termes
    dmt <- dfm(tokens_spw)
    
    #============================#
    #       Sortie graphique 
    #============================#
    
    output$plot_freq <- renderPlot({
      
      #df fréquence des mots du corpus
      freq_terms <- textstat_frequency(dmt)
      
      n = input$slider
      
      titre <- paste("Top",n,"des mots présents dans le corpus")
      
      #ggplot fréquence d'apparition dans le corpus (unique)
      ggplot(data=freq_terms[1:n,], aes(x=reorder(feature,docfreq), y=docfreq)) +
        geom_bar(stat="identity",fill="lightblue")+
        theme(axis.text.x=element_text(angle = -90, hjust = 0), axis.text.y = element_text(size = 15, face = "bold"))+
        geom_text(aes(label=docfreq), vjust=1.6, color="black", size=3.5)+
        ggtitle(titre) + coord_flip()+
        xlab("Mots du corpus") + ylab("Doc freq")
      
    })
    
    #deuxième graphique en fonction des autres axes 
    output$plot2 <- renderPlot({
      
      #df freq en fonction var 
      if(input$select == "Type contrat"){
        freq_terms_var <- textstat_frequency(dmt,groups = type_contrat)
      } else if (input$select == "Expériences") {
        freq_terms_var <- textstat_frequency(dmt,groups = experience)
      } else if (input$select == "Statut") {
        freq_terms_var <- textstat_frequency(dmt,groups = statut)
      }
      
      #liste des termes saisis par l'utilisateur
      var <- input$multinput
      #bloque pour afficher uniquement 5 variables
      if(length(var)> 5){
        var <- var[1:5]
      }
      selecvar <- freq_terms_var[freq_terms_var$feature %in% var,]
      
      #graphique sur la fréquence des mots du corpus
      ggplot(data=selecvar, aes(x=feature, y=docfreq, fill=group)) +
        geom_bar(stat="identity", position=position_dodge())+
        ggtitle("Fréquence des mots du corpus par axe") +
        geom_text(aes(x=feature, y=docfreq, label=docfreq,group=group), position = position_dodge(width = 1),
                  color="black", size=3.5)+
        theme(axis.text.x=element_text(angle = -60, hjust = 0,size = 15, face = "bold"))+
        xlab("Mots du corpus") + ylab("Doc freq")
      
      
      
    })
    
    #=======================#
    #   Analyse des KPI 
    #=======================#
    
    #gestion des décimals
    options(digits = 2)
    
    # nombre d'offres analysées
    nb_offre <- nrow(df)
    
    output$nbOffre <- renderValueBox({
      valueBox(
        VB_style(paste0(nb_offre), "font-size: 60%;"),
        "total des offres récupérées", 
        color = "teal"
      )
    })
    
    # métier le plus représenté
    metier <- c("analyst","scientist","engin","manager","consult")
    
    df$intitule <- nettoyage(df$intitule)
    
    #création du corpus
    corpus_int <- quanteda::corpus(df,text_field='intitule')
    
    #création liste des mots intéressants
    
    #tokenize : vérifie que tous les nombres et caratères spéciaux ont bien été supprimés
    tokens_int <- tokens(corpus_int,remove_numbers = TRUE,remove_symbols = TRUE)%>%
      tokens_wordstem()
    
    #sélection des mots dans corpus
    tokens_int_spw <- tokens_int %>%
      tokens_select(pattern = metier,selection = "keep")
    
    
    #création de la matrice doc-termes
    dmt_int <- dfm(tokens_int_spw)
    
    #compte fréquence des intitulés
    freq_terms_int <- textstat_frequency(dmt_int)
    
    top1metier <- freq_terms_int[order(freq_terms_int$docfreq,decreasing = TRUE)][1]
    
    pourcent <- (top1metier$docfreq/nrow(df))*100
    
    output$topmetier <- renderValueBox({
      valueBox(
        VB_style( paste0("Data ",top1metier$feature), "font-size: 60%;"  ),
        paste0(" est le metier le plus présent à ",round(pourcent,2),"%"),
        color = "teal"
      )
    })
    
    # nombre d'offre avec % d'offre pour les débutants
    freq_terms_var_expe <- textstat_frequency(dmt,groups = experience)
    
    deb <- freq_terms_var_expe[grep("Début.",freq_terms_var_expe$group),]
    
    deb_pourcent <- (nrow(deb)/nrow(df))*100
    
    output$jobdeb <- renderValueBox({
      valueBox(
        VB_style( paste0(round(deb_pourcent,2),"%"), "font-size: 60%;"),
        "sont des offres pour les débutants",
        color = "teal"
      )
    })
    
    #####################################
    #       Analyse du corpus           #
    #               LSA                 #
    #####################################
    
    mylsa <- textmodel_lsa(dmt)
    
    
    dfs <- data.frame(mylsa$docs[,1:2])
    
    output$plot_doc <- renderPlot({
      ggplot(dfs, aes(X1, X2, label = rownames(dfs)))+
        geom_point()+ geom_text() +
        ggtitle("Projection des documents sur les deux premiers axes")+
        xlab("Axe 1") + ylab("Axe 2")
    })
    
    var <- data.frame(mylsa$features[,1:2])
    
    output$plot_txt <- renderPlot({
      ggplot(var, aes(X1, X2, label = rownames(var)))+
        geom_point()+ geom_text()+
        ggtitle("Projection des termes sur les deux premiers axes")+
        xlab("Axe 1") + ylab("Axe 2")
    })
    
})

shinyApp(ui = ui, server = server)


