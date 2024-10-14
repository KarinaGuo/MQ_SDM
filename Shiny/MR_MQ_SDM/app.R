library(shiny)
library(shinycssloaders) # Loading button
library(markdown)
library(leaflet) # Map
library(sf) # Map
library(tidyverse) # data wrangling
library(raster) # Map
library(plotly) # Histogram/figures interactive
library(shinyWidgets)

setwd("~/Uni/Doctorate/SDM/Shiny/MR_MQ_SDM")

# Load current projection data
maxent_MQuin <- raster("data/maxent_MQuin.tiff")
maxent_MR <- raster("data/maxent_MR.tiff")
maxent_intersection <- raster("data/maxent_intersection.tiff")
maxent_studyarea <- st_read("data/maxent_studyarea.gpkg")

MQ_observations <- read.csv("data/MQ_locs_clean.csv"); MQ_observations <- MQ_observations[,2:3]
MR_observations <- read.csv("data/MR_locs_clean.csv"); MR_observations <- MR_observations[,2:3]

# Load future projection data
years_clust <- c("2021-2040", "2041-2060", "2061-2080")
ssp_list <- c("126", "245", "370", "585")

for (species in c("MQuin", "MR")){
  for (cluster in years_clust){
    for (ssp in ssp_list){
      read_file <- raster(paste0("data/maxent_",species, cluster, "_", ssp, ".tiff"))
      assign(paste0("maxent_",species, cluster, "_", ssp), read_file)
    }
  }
}

for (cluster in years_clust){
  for (ssp in ssp_list){
    read_file <- raster(paste0("data/future_maxent_intersection", cluster, "_", ssp, ".tiff"))
    assign(paste0("maxent_intersection", cluster, "_", ssp), read_file)
  }
}


# Colour palettes
pal_mr <- colorRampPalette(c("grey95","#C6DC96","#C6DC96","darkolivegreen","yellow"))
pal_mq <- colorRampPalette(c("grey95","#EAC8BF","#EAC8BF","orange","forestgreen"))
pal_int <- colorRampPalette(c("grey95","#C6DC96","#C6DC96","#45B055","#4870BD", "deeppink4"))

ui <- navbarPage("SDM",
                 # First Tab: Current Projections
                 tabPanel("Current projections",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("layer_curr", "Choose Layer to Display:",
                                           choices = c("Melaleuca quinquenervia" = "MQuin",
                                                       "Myrtle rust" = "MR",
                                                       "Intersection" = "Intersection"),
                                           selected = "MQuin"),
                              textInput("latitude", "Enter latitude:", value = "-33.8688"),
                              textInput("longitude", "Enter longitude:", value = "151.2093"),
                              actionButton("go", "Go to Location")
                            ),
                            mainPanel(
                              withSpinner(
                                leafletOutput("map")),
                              plotlyOutput("freqplot")
                            )
                          )
                 ),
                 
                 # Second Tab: Future Projections
                 tabPanel("Future projections",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("layer_fut", "Choose Layer to Display:",
                                           choices = c("Melaleuca quinquenervia" = "MQuin_fut",
                                                       "Myrtle rust" = "MR_fut",
                                                       "Intersection" = "intersection_fut"),
                                           selected = "MQuin_fut"),
                              
                              br(),  # Add space between inputs
                              
                              tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), # Removing the minor ticks in the slider input
                              sliderTextInput("year_clust", "Average climate year cluster:",
                                              choices = years_clust,
                                              selected = "2021-2040",
                                              grid = TRUE),
                              
                              br(),  # Add space between inputs
                              
                              tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), # Removing the minor ticks in the slider input
                              sliderTextInput("ssp", "Climate scenario (SSP):",
                                              choices = ssp_list,
                                              selected = "126",
                                              grid = TRUE),
                              
                              br(),
                              radioButtons("fut_show_both", "Show present distribution?",
                                           choices = c("Show" = "fut_show_pres",
                                                       "Hide" = "fut_hide_pres"),
                                           selected = "fut_hide_pres")
                            ),
                            
                            mainPanel(
                              withSpinner(
                                leafletOutput("map_2", height = 600))
                            )
                          )
                 )
)




server <- function(input, output, session) {
  
  # Main current tab
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = maxent_studyarea, color = "black", weight = 1, fill = FALSE, group = "Outline") %>%
      addRasterImage(maxent_MQuin, colors = pal_mq(20), opacity = 0.8, group = "MQuin") %>%
      addCircleMarkers(data = MQ_observations, lng = ~decimalLongitude, lat = ~decimalLatitude, color = "#1c4027", group = "MQuin") %>% 
      addRasterImage(maxent_MR, colors = pal_mr(20), opacity = 0.8, group = "MR") %>%
      addCircleMarkers(data = MR_observations, lng = ~decimalLongitude, lat = ~decimalLatitude, color = "#5f4417", group = "MR") %>% 
      addRasterImage(maxent_intersection, colors = pal_int(20), opacity = 0.8, group = "Intersection")
    #addLegend(position = "topright", pal = pal_mq, values = values(maxent_MQuin), title = "SDM value", group = "MQuin") %>% 
    #addLegend(position = "topright", pal = pal_mr, values = values(maxent_MR), title = "SDM value", group = "MR") %>% 
    #addLegend(position = "topright", pal = pal_mq, values = values(maxent_intersection), title = "SDM value", group = "Intersection")
  })
  
  # Observe input and update map based on radio button selection
  observe({
    selected_raster <- switch(input$layer_curr,
                              "MQuin" = maxent_MQuin,
                              "MR" = maxent_MR,
                              "Intersection" = maxent_intersection)
    
    selected_observations <- switch(input$layer_curr,
                                    "MQuin" = MQ_observations,
                                    "MR" = MR_observations,
                                    "Intersection" = NULL) 
    
    selected_palette <- switch(input$layer_curr,
                               "MQuin" = pal_mq(20),
                               "MR" = pal_mr(20),
                               "Intersection" = pal_int(20))
    color_pal <- colorNumeric(palette = selected_palette, domain = values(selected_raster), na.color = "transparent")
    
    # Clear previous layers and add the selected layer
    leafletProxy("map") %>%
      clearImages() %>%
      clearMarkers() %>%
      clearControls() %>% 
      addRasterImage(selected_raster, colors = selected_palette, opacity = 0.8, group = input$layer_curr) %>%
      addLegend(position = "topright", pal = color_pal, 
                values = values(selected_raster), title = "SDM value", group = input$layer_curr)
    
    if (!is.null(selected_observations)) {
      leafletProxy("map") %>%
        addCircleMarkers(data = selected_observations, 
                         lng = ~decimalLongitude, lat = ~decimalLatitude, 
                         color = ifelse(input$layer_curr == "MQuin", "#1c4027", "#5f4417"), 
                         group = input$layer_curr, stroke = FALSE, radius = 1, fillOpacity = 0.6)
    }
  })
  
  observeEvent(input$go, {
    req(input$latitude); req(input$longitude)
    inp_latitude <- as.numeric(input$latitude)
    inp_longitude <- as.numeric(input$longitude)
    
    leafletProxy("map") %>%
      flyToBounds(lng1 = inp_longitude-0.15, lng2 = inp_longitude+0.15, lat1 = inp_latitude-0.15, lat2 = inp_latitude+0.15, options = c(duration=0.3))
    
  })
  
  observeEvent(input$map_bounds, {
    bounds <- input$map_bounds
    req(bounds)  # Ensure bounds are available
    
    # Define the cropping extent from map bounds
    ext <- extent(bounds$west, bounds$east, bounds$south, bounds$north)
    
    # Crop the raster to the current map view
    cropped_raster <- crop(maxent_intersection, ext)
    
    # Plot frequency distribution if raster is not empty
    if (!is.null(cropped_raster)) {
      freq <- cropped_raster[!is.na(values(cropped_raster))]
      output$freqplot <-  renderPlotly({
        plot_ly(x=~freq, type=~"histogram", marker = list(color = pal_int(15)), nbinsx = 20)  %>%
          layout(xaxis = list(range = c(0,1.2), title = 'Degree of intersection'), 
                 yaxis = list(title = 'Frequency'))
      })
    }
  })
  
  #######################################################################
  
  # Future current tab
  output$map_2 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = maxent_studyarea, color = "black", weight = 1, fill = FALSE, group = "Outline")
  })
  
  # Observe input and update map based on radio button selection
  observe({
    req(input$layer_fut, input$year_clust, input$ssp)  # Ensure inputs are available
    
    species_fut <- gsub("_fut", "",input$layer_fut) # Changes species name to make it work 
    
    raster_name_fut <- paste0("maxent_", species_fut, input$year_clust, "_", input$ssp) 
    selected_raster_fut <- get(raster_name_fut, envir = .GlobalEnv)  # Use global environment to get raster
    
    raster_name_curr <- paste0("maxent_", species_fut) 
    selected_raster_curr <- get(raster_name_curr, envir = .GlobalEnv)  # Use global environment to get raster
    
    selected_palette_fut <- switch(input$layer_fut,
                                   "MQuin_fut" = pal_mq(20),
                                   "MR_fut" = pal_mr(20),
                                   "intersection_fut" = pal_int(20))
    
    color_pal_fut <- colorNumeric(palette = selected_palette_fut, domain = values(selected_raster_fut), na.color = "transparent")
    
    # Clear previous layers and add the selected layer
    
    if (!is.null(input$fut_show_both) && input$fut_show_both == "fut_hide_pres") {
      leafletProxy("map_2") %>%
        clearImages() %>%
        clearMarkers() %>%
        clearControls() %>%
        addRasterImage(selected_raster_fut, colors = selected_palette_fut, opacity = 0.8, group = input$layer_fut) %>%
        addLegend(position = "topright", pal = color_pal_fut, 
                  values = values(selected_raster_fut), title = "SDM value", group = input$layer_fut)
    }
    
    if (!is.null(input$fut_show_both) && input$fut_show_both == "fut_show_pres") {
      
      # Raster of difference (future - current)
      fut_pres_diff <- selected_raster_fut - selected_raster_curr
      
      # Color palette for difference (centered around 0)
      pal_int_fut <- colorRampPalette(c('#261323', 'deeppink4', "white", "darkolivegreen", "#1b2613"))
      color_pal_fut <- colorNumeric(palette = pal_int_fut(20), domain = c(-1.3,1.2), na.color = "transparent") # Range set with biggest diff observed, manually set for static color scale
      
      # Plot the raster of differences
      leafletProxy("map_2") %>%
        clearImages() %>%
        clearMarkers() %>%
        clearControls() %>%
        addRasterImage(fut_pres_diff, colors = color_pal_fut, opacity = 0.8, group = input$layer_fut) %>%
        addLegend(position = "topright", pal = color_pal_fut, 
                  values = c(-.71,1.2), title = "Difference in SDM value", group = input$layer_fut)
    }
  })
  
}


shinyApp(ui = ui, server = server)

