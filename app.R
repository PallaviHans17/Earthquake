# Loading Required Packages

library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)
library(dplyr)
library(shinydashboard)
library(shinyBS)
library(basictabler)
library(shinydashboardPlus)
library(leaflet.extras)
library(shinycssloaders)
library(shinyWidgets)
library(dashboardthemes)

#!/usr/bin/env Rscript
rm(list=ls())
path <- "./"
setwd(path)

# Loading USGS dataset
dataset_usgs <- "161107_eq_list_USGS.csv"
dataset <- read.csv(file = dataset_usgs, header = T, sep = ",")

# Data Manipulation
library(dplyr)
dataset <- arrange(dataset, -row_number())

# Removing columns with NA values
dataset <- dataset[,c(-7,-8,-9,-10,-16,-17,-18,-19)]

# Extracting year from time column
v <- as.POSIXlt(dataset$time)
Year <- format(v, '%Y')

# Combining year extracted with the dataset
dataset <- cbind(dataset, Year)

# Extracting date from the time column
e <- as.POSIXlt(dataset$time)
date <- format(e, '%Y-%m-%d')

# Combining date with the dataset
dataset <- cbind(dataset, date)

dataset$date <- as.Date(dataset$date, format = "%Y-%m-%d")

# Extracting Month from the time column
Month <- format(v, '%B')
dataset <- cbind(dataset, Month)

# Introducing energy released column as the exponential of the magnitude column
dataset$Energy = exp(dataset$mag)

# Extracting country from the place 
dataset$country <- sub('.*,\\s*', '', dataset$place)

library(countrycode)

# Storing continent in the continent column 
dataset$continent <- countrycode(sourcevar = dataset[, "country"],
                                 origin = "country.name",
                                 destination = "continent")

# Replacing NA with "Ocean" in the dataset 
dataset$continent[is.na(dataset$continent)] <- "Ocean"

# Log transform the Magnitude value 
dataset$mag <- log(dataset$mag)
dataset$mag <- round(dataset$mag,1)

# Log transform the Depth value
dataset$depth <- log(dataset$depth)

# Removing the rows containing "nuclear explosion" type from the dataset and columns which includes "net", "updated", "type", "status", "MagSource", "Year", and "Month"
quak <- dataset[c(-5132,-9735,-10792),c(-7,-9,-11,-12,-14)]

# Introducing the depth type according to the depth values
quak$depth_type <- ifelse(quak$depth <= 4.25, "shallow", ifelse(quak$depth > 4.25 & quak$depth <= 5.70, "intermediate", ifelse(quak$depth > 5.70, "deep", "other")))

# Reading the predicted energy dataset
data2 <- read.csv("dataset_new1.csv", header = T, sep = ",")

# Reading the High energy released dataset
risky <- read.csv("tot_final_2.csv", header=TRUE)

# Reading the risk zones dataset
high_energy <- read.csv("tot_final_best.csv", header=TRUE)


# UI 

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Visual Analysis of Earthquakes", titleWidth = 350, tags$li(class = "dropdown", actionButton("show", "", icon = icon("gear")))),
  
 
  
  
  dashboardSidebar(collapsed = TRUE, sidebarMenu()),
 
  
  dashboardBody(shinyDashboardThemes(theme = "grey_dark"),
    
    shinyjs::useShinyjs(),
   
    tabsetPanel(type = "tabs",
                tabPanel("Maps",
                         fluidRow(
                           box(title = "Earthquake Map", width = 12,
                               leafletOutput('map01'),
                               absolutePanel(top=200, left=20,
                                             tags$li(class = "dropdown", checkboxInput("markers", "Depth", FALSE), tags$style("{font-size:60px;}")),
                                             checkboxInput("heat", "Heatmap", FALSE),
                                             checkboxInput("dark", "Night Map", FALSE)
                                              )),
                           box(width = 12,  tags$style(HTML("
                    .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate {
                    color: #ffffff;
                    }

                    thead {
                    color: #ffffff;
                    }

                     tbody {
                    color: #ffffff;
                     }

                   " )), DT::dataTableOutput("table")))),
                tabPanel("Plots",
                         fluidRow(
                           box(width=4, plotlyOutput('hist01')),
                           box(width=4, plotlyOutput('hist02')),
                           box(width=4, plotlyOutput('freq01')),
                         fluidRow(
                           box(width = 4, plotlyOutput('hist03')),
                           box(width = 4, plotlyOutput('mag02')),
                           box(title = "Statistical Measures for Energy Released per Month", width = 4, 
                               tableOutput("Magnitude"), solidHeader = TRUE, collapsible = TRUE, 
                               align="center", status = "primary")))),
                
                tabPanel("Prediction",
                         fluidRow(
                           box("Predicted Energy Released", width = 12,
                               leafletOutput('map_energy', width = "100%", height = "800px"),
                               absolutePanel(top=200, left=20,
                                             # checkboxInput("risk", "High Predicted Energy Release", FALSE),
                                             # checkboxInput("energy", "Risk Zones", FALSE)
                                             radioButtons("predictions", "Model Predictions:",
                                                          c("Predicted Energy Released" = "p",
                                                            "High Predicted Energy Release" = "r",
                                                            "Risk Zones" = "h"))
                                             ))
                         ))),
    
    
    setBackgroundColor("black", shinydashboard = TRUE)
  
  ))



# Server Side

server <- function(input,output, session) {


  RA_s <- reactiveValues()
  
  observeEvent(input$show, {showModal(modalDialog(
    
     tags$style(HTML("
                    .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate {
                    color: #000000;
                    }

                    thead {
                    color: #000000;
                    }

                     tbody {
                    color: #000000;
                     }

                   " )),

    if(is.null(input$sld01_Mag[1])){
      sliderInput(inputId = "sld01_Mag",label = "Show Earthqaukes of Magnitude:",
                min = min(quak$mag), max = max(quak$mag),
                value = c(min, max), step=0.1, width = "1000px")
    }
    else{
      sliderInput(inputId = "sld01_Mag",label = "Show Earthqaukes of Magnitude:",
                  min = min(quak$mag), max = max(quak$mag),
                  value = c(input$sld01_Mag[1], input$sld01_Mag[2]), step=0.1, width = "1000px")
    },
    if(is.null(input$date[1])){
      tags$style('color: #000000')
      dateRangeInput("Date range", inputId = "date", start = "2010-10-31", end = "2016-11-07", format = "yyyy-mm-dd")
    }
    else{
       dateRangeInput("Date range", inputId = "date", start = input$date[1], end = input$date[2], format = "yyyy-mm-dd")
    },
       checkboxGroupInput(inputId= "location", "Select Continents:", choices = c("Americas","Oceania","Asia","Europe", "Africa", "Ocean"), selected = input$location),
    size ="m",
    easyClose = TRUE

    
   ))
 
    
  })
  
  
  # Updating the dataset acccording to the magnitude and date choosen by the user
 
  
  qSub <- reactive({
    
    inputs_to_save <- c(input$sld01_Mag[1], input$sld01_Mag[2], input$date[1], input$date[2])
    conts <- c("Americas","Oceania","Asia","Europe", "Africa", "Ocean")
    conts1 <- c()
    for (i in input$location){
      conts1 <- c(conts1, i)
    }

    if (is.null(inputs_to_save)) {
      inputs_to_save <- c(min(quak$mag), max(quak$mag), min(quak$date), max(quak$date))
    }

    if(is.null(conts1))
    {
      subset <- subset(quak, quak$mag>=inputs_to_save[1] &
                         quak$mag<=inputs_to_save[2] & quak$date>=inputs_to_save[3] & quak$date <= inputs_to_save[4] & quak$continent == conts) 
    }
    else{
      subset <- subset(quak, quak$mag>=inputs_to_save[1] &
                         quak$mag<=inputs_to_save[2] & quak$date>=inputs_to_save[3] & quak$date <= inputs_to_save[4] & quak$continent == conts1)
    }
    
    
  })
  
  
  
  # histogram giving location of the subset of earthquake dataset as per user inputs
  output$hist01 <- renderPlotly({
    ggplot(data=qSub()) + 
      geom_histogram(aes(x = mag), fill = "#adbce6", bins = 10) +
      xlab('Magnitude') +
      ylab('Count') +
      ggtitle('Earthquake Magnitude') +
      theme(# get rid of panel grids
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Change plot and panel background
        plot.background=element_rect(fill = "grey30"),
        panel.background = element_rect(fill = 'grey30'),
        axis.line.x = element_line(color = "white"),
        axis.text.x = element_text(face="plain", color="white", 
                                   size=10),
        axis.text.y = element_text(face="plain", color="white", 
                                   size=10),
        plot.title = element_text(color="white", size=12, face="bold", hjust = 0.5),
        axis.title.x = element_text(color="white", size=10, face="plain"),
        axis.title.y = element_text(color="white", size=10, face="plain"))
        
    
  })
  

  output$hist02 <- renderPlotly({
    ggplot(data=qSub()) + 
      geom_histogram(aes(x = depth), fill = "#adbce6", bins = 10) +
      xlab('Depth') +
      ylab('Count') +
      ggtitle('Earthquake Depth') +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # Change plot and panel background
            plot.background=element_rect(fill = "grey30"),
            panel.background = element_rect(fill = 'grey30'),
            axis.line.x = element_line(color = "white"),
            axis.text.x = element_text(face="plain", color="white", 
                                       size=10),
            axis.text.y = element_text(face="plain", color="white", 
                                       size=10),
            plot.title = element_text(color="white", size=12, face="bold", hjust = 0.5),
            axis.title.x = element_text(color="white", size=10, face="plain"),
            axis.title.y = element_text(color="white", size=10, face="plain")) 
  })
  
  output$freq01 <- renderPlotly({
    ggplot(data=qSub()) +
      geom_bar(aes(x=Month), fill="#adbce6") +
      xlab('Month') +
      ylab('Number of Earthquakes') +
      ggtitle('Frequency of Earthquake Each Month') +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # Change plot and panel background
            plot.background=element_rect(fill = "grey30"),
            panel.background = element_rect(fill = 'grey30'),
            axis.line.x = element_line(color = "white"),
            axis.text.x = element_text(face="plain", color="white", 
                                       size=10, angle=90, hjust=1),
            axis.text.y = element_text(face="plain", color="white", 
                                       size=10),
            plot.title = element_text(color="white", size=12, face="bold", hjust = 0.5),
            axis.title.x = element_text(color="white", size=10, face="plain"),
            axis.title.y = element_text(color="white", size=10, face="plain"))
   
  }
  )
  
  
  
  output$hist03 <- renderPlotly({
    ggplot(data=qSub(), aes(x=mag,y=depth)) + 
      geom_point(col="#adbce6") +
      xlab('Magnitude of Earthquake') +
      ylab('Depth') +
      ggtitle('Earthquake Magnitude and Depth') +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # Change plot and panel background
            plot.background=element_rect(fill = "grey30"),
            panel.background = element_rect(fill = 'grey30'),
            axis.line.x = element_line(color = "white"),
            axis.text.x = element_text(face="plain", color="white", 
                                       size=10),
            axis.text.y = element_text(face="plain", color="white", 
                                       size=10),
            plot.title = element_text(color="white", size=12, face="bold", hjust = 0.5),
            axis.title.x = element_text(color="white", size=10, face="plain"),
            axis.title.y = element_text(color="white", size=10, face="plain")) 
  })
  
  output$mag02 <- renderPlotly({
    ggplot(data=qSub(), aes(x=Month,y=Energy)) + 
      geom_point(col="#adbce6") +
      xlab('Month of Earthquake') +
      ylab('Energy Released') +
      ggtitle('Estimated Total Energy Released') +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # Change plot and panel background
            plot.background=element_rect(fill = "grey30"),
            panel.background = element_rect(fill = 'grey30'),
            axis.line.x = element_line(color = "white"),
            axis.text.x = element_text(face="plain", color="white", 
                                       size=10, angle=90, hjust=1),
            axis.text.y = element_text(face="plain", color="white", 
                                       size=10),
            plot.title = element_text(color="white", size=12, face="bold", hjust = 0.5),
            axis.title.x = element_text(color="white", size=10, face="plain"),
            axis.title.y = element_text(color="white", size=10, face="plain"))
    
  })
  
  # returning dataset with the datapoints as per user selection
  output$table01 <- renderDataTable({
    
    DT::datatable(qSub(), selection = "single",options=list(stateSave = TRUE))
  })
  
  output$table = DT::renderDataTable({
    if (isTruthy(input$map01_bounds)) {
      bounds = input$map01_bounds
      qSub() %>% filter(
        between(longitude, bounds$west, bounds$east),
        between(latitude, bounds$south, bounds$north)
      )
    } else
      qSub()
  })
  
  output$Magnitude <- renderTable(
    qSub() %>%
      group_by(Month) %>%
      summarise(Mean_Energy_Released= mean(Energy), standard_deviation = sd(Energy)))
  
  pal <- colorNumeric(
    palette = c('gold', 'orange', 'dark orange', 'orange red', 'red', 'dark red'),
    domain = c(min(quak$mag), max(quak$mag)))
  
 
    
  # updated map
  output$map01 <- renderLeaflet({
    pal0 <- colorFactor(
      palette = c('gold', 'orange', 'dark orange', 'orange red', 'red', 'dark red'),
      domain = quak$mag)
    qMap <- leaflet(data = qSub()) %>%
      setView(lng=42, lat=23, zoom = 2) %>%
      addProviderTiles(provider = providers$GeoportailFrance.orthos) %>%
      addCircles(data = qSub(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~sqrt(mag)*80000, 
                 popup=~paste("Magnitude:", mag, "<br>",
                                    "Depth:", depth, "<br>",
                                    "Place:", place, "<br>",
                                    "Date and Time:", time), 
                 label = ~as.character(paste0("Magnitude: ", sep = " ", mag)), 
                 color = ~pal0(mag), fillOpacity = 0.5) %>%
      addLegend("bottomright", pal = pal0, values = qSub()$mag,
                title = "Magnitude",
                opacity = 1)
    
    qMap
  })
  
  output$map_energy <- renderLeaflet({
    pal0 <- colorNumeric(
      palette = c('gold', 'orange', 'dark orange', 'orange red', 'red', 'dark red'),
      domain = c(min(data2$Predicted), max(data2$Predicted)))
    qMap11 <- leaflet(data = data2) %>%
      setView(lng=42, lat=23, zoom = 2) %>%
      addProviderTiles(provider = providers$CartoDB.DarkMatter) %>%
      addCircles(data = data2, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~sqrt(Predicted)*100, 
                 popup=~paste("Predicted Energy Released:", Predicted), 
                 label = ~as.character(paste0("Predicted Energy Released: ", sep = " ", Predicted)), 
                 color = ~pal0(Predicted), fillOpacity = 0.5) %>%
      addLegend("bottomright", pal = pal0, values = round(data2$Predicted,2),
                title = "Predicted Energy Released", opacity = 1)
    
    qMap11
  })
  

  #define the color of for the depth of the earquakes
  pal2 <- colorFactor(
    palette = c('blue', 'yellow', 'red'),
    domain = quak$depth_type
  )
  
  
  #next we use the observe function to make the checkboxes dynamic. If you leave this part out you will see that the checkboxes, when clicked on the first time, display our filters...But if you then uncheck them they stay on. So we need to tell the server to update the map when the checkboxes are unchecked.

  observeEvent(input$markers, {proxy <- leafletProxy("map01", data = qSub())
  pal0 <- colorFactor(
    palette = c('gold', 'orange', 'dark orange', 'orange red', 'red', 'dark red'),
    domain = quak$mag)
  proxy %>% clearMarkers()

  if (input$markers) {
    proxy %>% clearControls()
    proxy %>% addCircleMarkers(stroke = FALSE, color = ~pal2(depth_type), fillOpacity = 0.2, group = "Toner", label = ~as.character(paste0("Magnitude: ", sep = " ", mag)))
    proxy %>% addLegend("bottomright", pal = pal2, values = qSub()$depth_type, group = "Toner",
                        title = "Depth Type",
                        opacity = 1)
    
  }
  else{
    proxy %>% clearControls()
    proxy %>% addLegend("bottomright", pal = pal0, values = qSub()$mag,
                        title = "Magnitude",
                        opacity = 1)}
  
  })
  

  
  observe({
    proxy <- leafletProxy("map01", data = qSub())
    proxy %>% clearMarkers()

    if (input$heat) {
      proxy %>%  addHeatmap(lng=~longitude, lat=~latitude, intensity = ~mag, blur =  10, max = 0.05, radius = 15) 
    }
    else{
      proxy %>% clearHeatmap()
    }
    
    
  })
  
  observe({
    proxy <- leafletProxy("map01", data = qSub())
    if (input$dark) {
      proxy %>% addProviderTiles(provider = providers$CartoDB.DarkMatter)}
    else {
      proxy %>% addProviderTiles(providers$GeoportailFrance.orthos)
    }
  })
  
  pal3 <- colorFactor(
    palette = c('blue', 'yellow', 'red', 'orange', 'red', 'dark red', 'gold'),
    domain = round(risky$Predicted, 2)
  )
  
  pal6 <- colorFactor(
    palette = c('blue', 'yellow', 'red', 'orange', 'red', 'dark red', 'gold'),
    domain = round(high_energy$Predicted, 2)
  )
  
  pal08 <- colorNumeric(
    palette = c('gold', 'orange', 'dark orange', 'orange red', 'red', 'dark red'),
    domain = c(min(data2$Predicted), max(data2$Predicted)))
  
  observeEvent(input$predictions, {proxy <- leafletProxy("map_energy")
    pal0 <- colorNumeric(
      palette = c('gold', 'orange', 'dark orange', 'orange red', 'red', 'dark red'),
      domain = c(min(data2$Predicted), max(data2$Predicted)))
    if (input$predictions == "r") {
      proxy %>% clearShapes()
      proxy %>% clearControls()
      proxy %>% addCircles(data = risky, lat = ~ y, lng = ~ x, weight = 1, radius = ~sqrt(Predicted)*10000,
                           popup=~paste("Predicted Energy Released:", round(Predicted,2), "<br>",
                                        "Population Count:", round(r1,2)),
                           label = ~as.character(paste0("Predicted Energy Released: ", sep = " ", round(Predicted,2))),
                           color = ~pal3(round(Predicted, 2)), fillOpacity = 0.5)
      proxy %>% addLegend("bottomright", pal = pal3, values = round(risky$Predicted, 2),
                  title = "Predicted Energy Released",
                  opacity = 1)}
    
    else if (input$predictions == "h") {
      proxy %>% clearShapes()
      proxy %>% clearControls()
      proxy %>% addCircles(data = high_energy, lat = ~ y, lng = ~ x, weight = 1, radius = ~sqrt(Predicted)*10000,
                           popup=~paste("Predicted Energy Released:", round(Predicted,2), "<br>",
                                        "Population Count:", round(r1,2)),
                           label = ~as.character(paste0("Predicted Energy Released: ", sep = " ", round(Predicted,2))),
                           color = ~pal6(round(Predicted, 2)), fillOpacity = 0.5)
      proxy %>% addLegend("bottomright", pal = pal6, values = round(high_energy$Predicted, 2),
                          title = "Predicted Energy Released",
                          opacity = 1)}
    
    else if(input$predictions == "p"){
      proxy %>% clearShapes()
      proxy %>% clearControls()
      proxy %>% addCircles(data = data2, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~sqrt(Predicted)*100,
                 popup=~paste("Predicted Energy Released:", Predicted),
                 label = ~as.character(paste0("Predicted Energy Released: ", sep = " ", Predicted)),
                 color = ~pal0(Predicted), fillOpacity = 0.5) 
      proxy %>% addLegend("bottomright", pal = pal0, values = round(data2$Predicted,2),
                  title = "Predicted Energy Released", opacity = 1)
    }

  })
  
 
  
  pal <- colorNumeric(
    palette = c('gold', 'orange', 'dark orange', 'orange red', 'red', 'dark red'),
    domain = quak$mag)
  

}

shinyApp(ui = ui, server = server)




