setwd("C:/Users/HK/Desktop/RA/Dataset/NYC EPA Harbor Water Sampling Data/shiny working log")
rm(list = ls(all = TRUE))

options(bitmapType='cairo')

library(shiny)
library(ggplot2)
library(leaflet)
library(plotly)
library(dplyr)

load("Shiny_Data_1204.RData")

set.seed(2017)
stations_full <- stations_full %>%
  filter(Site %in% stations_full$Site[sample(nrow(stations_full), 30)])

results_full <- results_full %>%
  filter(Site %in% stations_full$Site) %>%
  arrange(Site, Parameter, Date)


#############################################################################
# UI
#############################################################################

ui <- fluidPage(
  
  headerPanel('Water Quality Visualization Tool'),
  fluidRow(column(width = 10,
                  leafletOutput("Map", width = "100%", height = 600),
                  offset = 1)
  ), 
  
  sidebarPanel(
    titlePanel("View in Plot"),
    selectInput('site', 'Site', choices = results_full$Site, multiple = FALSE),
    #selectInput('para', 'Parameter', choices = results_full$Parameter), --> secondSelect
    uiOutput("secondSelect"),
    checkboxInput("loess", "Show Trend", F),
    sliderInput("date", 'Date Range', min = min(results_full$Date), max = max(results_full$Date), 
                                      value = c(min(results_full$Date), max(results_full$Date))),
    
    titlePanel("Download Data"),
    selectInput('dsite', 'Download: Site', choices = results_full$Site, multiple = TRUE), 
    selectInput('dpara', 'Download: Parameter', choices = results_full$Parameter, multiple = TRUE),
    sliderInput("ddate", 'Download: Date Range', min = min(results_full$Date), max = max(results_full$Date), 
                                                 value = c(min(results_full$Date), max(results_full$Date))),
    downloadButton("downloadData", "Download")
  ),
  
  mainPanel(
    column(width = 8, textOutput("date"),
    plotlyOutput('ts_plot'), 
    offset = 1)
  )
  
)

###################################################################################
# SERVER
###################################################################################

server <- function(input, output, session) {

  selectedData <- reactive({
    results_full %>%
      filter(Site == input$site, Parameter == input$para) %>%
      select("Date", "Parameter", "Result")
  })

  SelectedSite <- reactiveValues(clickedMarker = NULL)

  output$secondSelect <- renderUI({
    selectInput('para', 'Parameter', choices = results_full$Parameter[which(results_full$Site == input$site)])
  })
    
  output$ts_plot <- renderPlotly({
    ts_data <- results_full %>%
      filter(Site == input$site, 
             Parameter == input$para,
             Date %in% as.Date(input$date[1]):as.Date(input$date[2])) %>%
      select("Date", "Parameter", "Result")
    
    ts_plot <- ggplot(ts_data, aes(x = as.Date(Date), y = as.numeric(Result))) + 
      geom_point(labels = input$para) + theme_classic() + ylab(input$para) + xlab("Date") 
    
    if(input$loess) {
      ts_plot <- ts_plot + geom_smooth(se = FALSE)
    }  
    ggplotly(ts_plot)
  })
  
  output$Map <- renderLeaflet({
    leaflet(stations_full, options = leafletOptions(minzoom = 9, maxzoom = 9)) %>% 
      addTiles() %>%
      setView(lng = -72.947542, lat = 41.016421, zoom = 9) %>% 
      addCircleMarkers(~ Lng, ~ Lat, color = ~ Source, popup = ~ Site, layerId = ~ Site)})

  observeEvent(input$Map_marker_click, { # update the location selectInput on map clicks
    click_on_site <- input$Map_marker_click
    if(!is.null(click_on_site$id)){
      if(is.null(input$site) || input$site != click_on_site$id) updateSelectInput(session, "site", selected = click_on_site$id)
    }
  })
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste("download", ".csv", sep = "")  # need improve
    },
    
    content = function(file) {
      download_data <- results_full %>%
        filter(Site %in% input$dsite,
               Parameter %in% input$dpara,
               Date %in% as.Date(input$ddate[1]):as.Date(input$ddate[2])) %>%
        select(Site, Date, Parameter, Result)
      
    write.csv(download_data, file, row.names = FALSE)
    }
  )
}



########################################################################################
# RUN
########################################################################################


shinyApp(ui = ui, server = server)
