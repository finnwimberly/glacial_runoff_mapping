#install.packages("leaflet.extras")
#install.packages("shiny")
#install.packages("zoo")
#install.packages("webshot")
#install.packages("shinyjs")
#install.packages('rsconnect')
#install.packages("shinylive")
#install.packages("shiny")
#install.packages("httpuv", update = TRUE)

library(sf)
library(tidyverse)
#library(htmlwidgets)
#library(webshot)
#library(zoo)
library(leaflet)
library(shiny)
library(leaflet.extras)
#library(shinyjs)
library(rsconnect)
library(here)
#library(shinylive)
#library(httpuv)
library(stringi)
library(data.table)


options(encoding = "UTF-8")

load("merged_data_list.R")
load("rf_dataframes.R")
load("basin_info_dataframes.R")



# Define the color palette based on the range of years
my_colors1 <- colorNumeric(palette = "magma", domain = range(2000, 2100))
my_colors2 <- colorNumeric(palette = "viridis", domain = range(0, 82))

# # Creating our UI
# ui <- fluidPage(
#   titlePanel("Projected 21st Century Glacier Runoff"),
#   sidebarLayout(
#     sidebarPanel(
#       selectInput("colorfill", "Select Color Fill:", choices = 
#                     c("Year of Peak Water", "Average Annual Runoff")),
#       selectInput("ssp", "Select SSP:", choices = names(merged_data_list)),
#       selectInput("glacier_model", "Select Glacier Model:", choices = c("Multi-Model Median", "GloGEM", "PyGEM", "OGGM")),
#       tags$div(
#         class = "welcome-message",
#         tags$p("Welcome to the MINT (Middlebury Ice Numerical Team) interactive
#                glacier model runoff intercomparison map. This app presents century-scale 
#                runoff projections from 3 state-of-the-art glacier evolution models. The 
#                basins can be colored according to their (across-century) average annual 
#                runoff or year of projected peak water (year of maximum annual runoff). 
#                You can select any of the 3 glacier models, or the multi-model median, for 
#                4 different emission scenarios from the drop-down menus. Clicking on any 
#                individual basin will result in a pop-up containing more information and 
#                figures relevant to that basin. The code used to create this application is 
#                available on GitHub at finnwimberly/glacial_runoff_mapping"),
#         style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-top: 20px; border: 1px solid #ddd;"
#       )
#     ),
#     mainPanel(
#       leafletOutput("map"),
#       tags$style(HTML(".legend-superscript { line-height: 0.8; }"))  # CSS for adjusting line height
#     )
#   )
# )

ui <- fluidPage(
  titlePanel("Projected 21st Century Glacier Runoff"),
  sidebarLayout(
    sidebarPanel(
      selectInput("colorfill", "Select Color Fill:", choices = 
                    c("Year of Peak Water", "Average Annual Runoff")),
      selectInput("ssp", "Select SSP:", choices = names(merged_data_list)),
      selectInput("glacier_model", "Select Glacier Model:", choices = c("Multi-Model Median", "GloGEM", "PyGEM", "OGGM")),
      tags$div(
        class = "welcome-message",
        tags$p("Welcome to the MINT (Middlebury Ice Numerical Team) interactive
               glacier runoff model-intercomparison map. This app presents century-scale 
               runoff projections from 3 state-of-the-art glacier evolution models. The 
               basins can be colored according to their year of projected peak water (year 
               of maximum annual runoff) or (across-century) average annual runoff. 
               You can select any of the 3 glacier models, or the multi-model median, for 
               4 different emission scenarios from the drop-down menus. Clicking on any 
               individual basin will result in a pop-up containing more information and 
               figures relevant to that basin. The code used to create this application is 
               available in", tags$a(href="https://github.com/finnwimberly/glacial_runoff_mapping", "this repository"),
               "on GitHub."),
        style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-top: 20px; border: 1px solid #ddd;"
      )
    ),
    mainPanel(
      leafletOutput("map"),
      tags$style(HTML(".legend-superscript { line-height: 0.8; }"))  # CSS for adjusting line height
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive expression to get the selected glacier model data
  get_glacier_model_data1 <- reactive({
    if (input$glacier_model == "Multi-Model Median") {
      return(merged_data_list[[input$ssp]]$Median_PeakWater)
    } else {
      return(merged_data_list[[input$ssp]][[input$glacier_model]])
    }
  })
  
  get_glacier_model_data2 <- reactive({
    if (input$glacier_model == "Multi-Model Median") {
      return(rf_dataframes[[input$ssp]]$Median_RF)
    } else {
      return(rf_dataframes[[input$ssp]][[input$glacier_model]])
    }
  })
  
  # Function to display the plot
  display_plot <- function(basin_name, ssp, glacier_model) {
    # Construct the file path to the saved plot image
    if (glacier_model == "Multi-Model Median") {
      plot_file_path <- here("Data&Figures", "App Figs", ssp, paste0("RF_", basin_name, "_", ssp, ".png"))
    } else {
      plot_file_path <- here("Data&Figures", "App Figs", ssp, paste0("RF_", basin_name, "_", ssp, "_", glacier_model, ".png"))
    }
    
    # Return the plot file path
    return(plot_file_path)
  }
  
  output$map <- renderLeaflet({
    # Predefine the addPolygons function call based on the condition
    polygons_function <- if (input$colorfill == "Year of Peak Water") {
      function(map) {
        map |>
          addPolygons(data = merged_data_list[[input$ssp]],
                      opacity = 1,
                      fillOpacity = 1,
                      fillColor = ~my_colors1(get_glacier_model_data1()),
                      color = "black",
                      weight = 1,
                      label = ~RIVER_BASI)
      }
    } else {
      function(map) {
        map %>%
          addPolygons(data = merged_data_list[[input$ssp]],
                      opacity = 1,
                      fillOpacity = 1,
                      fillColor = ~my_colors2(get_glacier_model_data2()),
                      color = "black",
                      weight = 1,
                      label = ~RIVER_BASI)
      }
    }
    
    # Define legend parameters based on the selected color fill
    legend_pal <- if (input$colorfill == "Year of Peak Water") {
      my_colors1
    } else {
      my_colors2
    }
    legend_title <- if (input$colorfill == "Year of Peak Water") {
      "Year of Peak Water"
    } else {
      HTML("Average Annual<br>Runoff [km<sup>3</sup>]")
    }
    legend_values <- if (input$colorfill == "Year of Peak Water") {
      seq(2000, 2100, length.out = 5)
    } else {
      seq(0, 82, length.out = 5)
    }
    
    # Create leaflet map and add tiles
    leaflet() %>%
      addTiles() %>%
      # Call the predefined polygons_function
      polygons_function() %>%
      addPolygons(data = merged_data_list[[input$ssp]],
                  fillColor = "transparent",
                  stroke = FALSE,
                  weight = 0,
                  label = ~RIVER_BASI,
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                  layerId = ~RIVER_BASI,
                  group = "basin_group") %>%
      addLayersControl(overlayGroups = c("basin_group")) %>%
      addLegend(position = "bottomright",
                pal = legend_pal,
                values = legend_values,
                title = legend_title,
                opacity = 1,
                labFormat = labelFormat(big.mark = "")
      )
  })
  
  # Display basin information and plot on click
  observeEvent(input$map_shape_click, {
    event <- input$map_shape_click
    if (!is.null(event)) {
      basin_name <- event$id
      selected_basin_data <- merged_data_list[[input$ssp]][merged_data_list[[input$ssp]]$RIVER_BASI == basin_name,][1, ]
      if (input$glacier_model == "Multi-Model Median") {
        supp_basin_data <- basin_info_dataframes[["Median"]][basin_info_dataframes[["Median"]]$Basin_Name == basin_name,][1, ]
        pw_year <- selected_basin_data$Median_PeakWater
      } else {
        supp_basin_data <- basin_info_dataframes[[input$glacier_model]][basin_info_dataframes[[input$glacier_model]]$Basin_Name == basin_name,][1, ]
        pw_year <- selected_basin_data[[input$glacier_model]]
      }
      
      # Explicitly calling columns specific to single SSPs
      ssp <- input$ssp
      supp_basin_data$Mean_Initial_Volumes <- supp_basin_data[[paste0("Mean_Initial_Volumes_", ssp)]]
      
      # Use output$basin_table to render a table
      output$basin_table <- renderTable({
        data.frame(
          "Basin" = basin_name,
          "MRB ID" = as.character(selected_basin_data$MRBID),
          "Percent Area Initially Glaciated" = 100 * supp_basin_data$GlacierAreaFrac,
          "Initial Ice Volume (Km³)" = supp_basin_data$Mean_Initial_Volumes,
          "Year of Peak Water" = pw_year,
          "Number of Glaciers" = supp_basin_data$`#glaciers`,
          check.names = FALSE
        )
      })
      
      # Display plot for the selected basin, SSP, and glacier model
      plot_file_path <- display_plot(basin_name, input$ssp, input$glacier_model)
      
      # Render the plot image
      output$plot_image <- renderImage({
        list(src = plot_file_path, alt = "Runoff Plot", width = "85%")
      }, deleteFile = FALSE)
      
      showModal(modalDialog(
        title = "Basin Information",
        fluidRow(
          column(12, tableOutput("basin_table")),
          column(12, div(style = "text-align: center;", imageOutput("plot_image")))  # Center the image
        ),
        easyClose = TRUE,
        size = "l"  # Adjust the size of the modal dialog window
      ))
    }
  })
}

shinyApp(ui = ui, server = server)
