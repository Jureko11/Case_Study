#Installationsroutine
list_of_req_packages <- c("shiny", "raster", "tidyverse", "mapproj", "DT", "leaflet", 
                          "leaflet.minicharts", "geosphere", "bslib", "RColorBrewer", "mapview", "miceadds")
needed_packages <- list_of_req_packages[!(list_of_req_packages %in% installed.packages()[,"Package"])]
if(length(needed_packages) > 0) install.packages(needed_packages)

#Library-Import
library(shiny)
library(raster)
library(tidyverse)
library(mapproj)
library(DT)
library(leaflet)
library(leaflet.minicharts)
library(geosphere)
library(bslib)
library(RColorBrewer)
library(mapview)
library(miceadds)

#Import Datei mit Package "miceadds"
shiny_data <- load.Rdata2("Datensatz.RData")

# Datensatz fuer die Form Deutschlands um diese auf der Karte dunkel umranden zu koennen
DEU <- getData("GADM", country = "DEU", level = 0)

# Funktion, die abhaengig von der Hoehe der Variable x eine Farbe zurueckgibt
getColor <- function(x){
  if (x >= 0 && x < 10000){
    return("#ff603b")
  }
  else if(x >= 10000 && x < 20000){
    return("#ff0000")
  }
  else if(x >= 20000 && x < 30000){
    return("#bb0000")
  }
  else if(x >= 30000 && x < 40000){
    return("#9a0000")
  }
  else if(x >= 40000 && x < 50000){
    return("#790001")
  }
  else if(x >= 50000 && x < 75000){
    return("#5a0004")
  }
  else if(x >= 75000 && x < 100000){
    return("#3a0002")
  }
  else if(x >= 100000){
    return("black")
  }
}



# Hier wird die  Benutzeroberflaeche erstellt
ui <- fluidPage(
  # Erstellung des Bootstrap themes
  # Ein Design Package 
  theme =  bs_theme(
    bg = "lightsteelblue", 
    fg = "#305679",
    primary  = "#305679",
    base_font = "Calibri", code_font = "Calibri", heading_font = "Calibri"),
  
  sidebarLayout(
    # Aufeilung der App in sidabarPanel() und mainPanel()
    
    sidebarPanel(
      img(src = "./Zusaetzliche_Dateien/LOGO.png", height = 300, width = 300),
      selectInput(
        "Komponente_Herstellernummer", 
        "Komponenten-Hersteller:", 
        choices = unique(shiny_data$Komponente_Herstellernummer))),
  
  mainPanel(h1("Lieferbeziehungen"),
  # Erstllung von  2 Tabs mit Titel: Plot, Table
  # Aufsplitten des Tabs graphische Darstellung
  tabsetPanel(type = "tabs",
              tabPanel("Graphische Darstellung", splitLayout(
                cellWidths = c("60%", "40%"),
                leafletOutput("mymap"),
                plotOutput("barchart"))),
              
              tabPanel("Tabelle", DTOutput("Beweistabelle"))
            ))))


server <- function(input, output, session) {
  
  # Berechnung der Anzahl der gelierferten Einzelteile pro Einzeilteilhersteller
  Anzahl_der_Komponenten <- reactive({shiny_data %>%
      filter(Komponente_Herstellernummer == input$Komponente_Herstellernummer) %>%
      group_by(Werksnummer) %>%
      mutate(Anzahl = n())
    
  })
  
  # Erstellung von Datansatz mit nur Laengengrad und Breitengrad
  filtered_location <- reactive({
    shiny_data %>%
      filter(Komponente_Herstellernummer == input$Komponente_Herstellernummer) %>%
      group_by(Werksnummer) %>%
      mutate(Anzahl = n()) %>%
      select(c(-Komponente_ID, -ID)) %>%
      distinct() %>%
      mutate(color = getColor(Anzahl))
  })
  
  
  # Bestimmung der Distanz zwischen den Einzelteilherstellern und Komponentenherstellern
  distance_ETH_KH <- reactive({
    filtered_location() %>% 
      rowwise() %>%
      mutate(Distanz = round(distHaversine(c(Laengengrad, Breitengrad), 
            c(Komponente_Laengengrad, Komponente_Breitengrad))/1000, digits = 0))
  })
  
  # Erstellen von Tabelle mit jeweiligen Einzelteilherstellern inkl. ihrer gesamten Liefermenge
  Unique_Anzahl <- reactive({
    unique(Anzahl_der_Komponenten()[, c("Werksnummer", "Anzahl")])
  })
  
  # Erstellung einer Karte mit den Lieferbeziehungen, Anzahl der gelieferten Einzelteile,
  # Erstellung von Pfeilen(addFlows()) die die gelieferten Volumina darstellen.
  # Erstellung von Popups mit Informationen zu Einzelteilherstellern, gelieferte Menge, Werksnummer
  output$mymap <- renderLeaflet({
    leaflet(data = filtered_location()) %>%
      addTiles() %>%
      addPolylines(
        data = DEU,
        stroke = TRUE,
        color = "black", opacity = 1, weight = 2) %>%
      addPolygons(
        data = DEU, 
        stroke = TRUE, smoothFactor = 0.5, fillOpacity = 0.2,
        color = "#FFFFE0") %>%
    
      addCircleMarkers(~Laengengrad, ~Breitengrad, radius = 6, fillOpacity = 0.8, 
                       popup = paste("<div>",
                                     "<h5>","Einzelteilhersteller","</h5>",
                                     "<h6>","Werksname:",filtered_location()$Werksnummer,"</h6>",
                                     "<h6>","Komponentenhersteller:",input$Komponente_Herstellernummer,"</h6>",
                                     "<h6>", "Menge:",Unique_Anzahl()$Anzahl,"</h6>",
                                     "<h6>", "Entfernung:",distance_ETH_KH()$Distanz, "km","</h6>",
                                     "</div>")) %>%
      addCircleMarkers(~Komponente_Laengengrad, ~Komponente_Breitengrad, radius =  10, color = "orange", 
                       popup = paste("<div>","<h5>","Komponentenhersteller","</h5>",
                                      "<h6>","Name:",input$Komponente_Herstellernummer,"</h6>",
                                      "</div>")) %>%
      # Pfeile erstellen, die die Lieferbeziehungen darstellen
      addFlows(lng0 = filtered_location()$Laengengrad, lat0 = filtered_location()$Breitengrad,
               lng1 = filtered_location()$Komponente_Laengengrad, 
               lat1 =  filtered_location()$Komponente_Breitengrad, 
               color = filtered_location()$color, opacity = 0.9, maxThickness = 6, 
               minThickness = 2, flow = Unique_Anzahl()$Anzahl,
               popup = popupArgs(labels  = paste("Anzahl glieferter Komponenten"))) %>%
    
    addLegend(position = "bottomright",title = "Hersteller",
              pal = colorFactor(palette = c("orange","blue"),
              domain = c("Komponentenhersteller","Einzelteilhersteller"), 
              ordered = T), 
              values = c("Komponentenhersteller","Einzelteilhersteller"),  
              opacity = 1) %>%
      
    addLegend(position = "topright",title = "Volumenstrom",
              pal = colorFactor(palette = c("#ff603b", "#ff0000", "#bb0000", "#9a0000", 
                                            "#790001", "#5a0004", "#3a0002", "black"),
              levels = c("0 - 10.000", "10.000 - 20.000", "20.000 - 30.000", "30.000 - 40.000", 
                         "40.000 - 50.000", "50.000 - 75.000", "75.000 - 100.000", "> 100.000")), 
              values = c("0 - 10.000", "10.000 - 20.000", "20.000 - 30.000", 
                          "30.000 - 40.000", "40.000 - 50.000", "50.000 - 75.000", "75.000 - 100.000", 
                          "> 100.000"),  
              opacity = 1)
  })
  
 
  


  # Erstellung eines Balkendiagrammes mit der Anzahl der gelieferten Einzelteile pro Einzelteilhersteller
  output$barchart <- renderPlot({
    
    ggplot(Unique_Anzahl(), aes(x= factor(Werksnummer), y = Anzahl)) +
      geom_bar(stat =  "identity", fill = "#09143C", alpha = 0.8) + 
      labs(x = "Einzelteilhersteller", y = "Anzahl der gelieferten Einzelteile") + 
    
      theme(axis.line=element_blank(),
            panel.background=element_rect(fill  = "lightsteelblue", color = "lightsteelblue"),
            plot.background = element_rect(fill = "lightsteelblue", color = "lightsteelblue"),
            panel.border=element_blank(),
            panel.grid.minor=element_blank(),
            panel.grid.major=element_blank())
  })
  
  # Erstellung einer Tabelle mit den Daten zum Balkendiagramm
  output$Beweistabelle <- renderDT({
    Unique_Anzahl()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


