#install.packages("leaflet.extras")
#install.packages("shiny")
#install.packages("zoo")
#install.packages("webshot")
#install.packages("shinyjs")
#install.packages('rsconnect')
library(sf)
library(tidyverse)
library(htmlwidgets)
library(webshot)
library(zoo)
library(leaflet)
library(shiny)
library(leaflet.extras)
library(shinyjs)
library(rsconnect)
library(here)

basepath <- "~/Documents/GitHub/glacial_runoff_mapping/Data&Figures/"
outline_path <- paste0(basepath, "Outlines/glacier_basins.shp")
basin_outlines <- st_read(outline_path)

#Reading in peak water data
#Lets define all of our dimensions
scenarios <- c('ssp126','ssp245','ssp370','ssp585')

pw_path <- paste0(basepath, "CSV Outputs/Median PW/Median_PeakWater")
# Read the CSV files into R dataframes
ssp126_df <- read.csv(paste0(pw_path,"_ssp126.csv"))
ssp245_df <- read.csv(paste0(pw_path,"_ssp245.csv"))
ssp370_df <- read.csv(paste0(pw_path,"_ssp370.csv"))
ssp585_df <- read.csv(paste0(pw_path,"_ssp585.csv"))

# Create a list to store the dataframes
ssp_dataframes <- list(ssp126_df, ssp245_df, ssp370_df, ssp585_df)

# Set names for easier indexing
names(ssp_dataframes) <- c("ssp126", "ssp245", "ssp370", "ssp585")

# Function to calculate median for each row
calculate_median <- function(row) {
  return(median(row, na.rm = TRUE))
}

# Apply the function to each dataframe
for (ssp in names(ssp_dataframes)) {
  ssp_df <- ssp_dataframes[[ssp]]
  ssp_dataframes[[ssp]]$Median_PeakWater <- apply(ssp_df[, c("OGGM", "PyGEM", "GloGEM")], 1, calculate_median)
}

# Function to extract year from date and convert to integer
extract_year <- function(date) {
  return(as.integer(format(as.Date(date), "%Y")))
}

# Apply the function to each dataframe
for (ssp in names(ssp_dataframes)) {
  ssp_df <- ssp_dataframes[[ssp]]
  ssp_dataframes[[ssp]]$Median_PeakWater <- sapply(ssp_df$Median_PeakWater, extract_year)
  ssp_dataframes[[ssp]]$GloGEM <- sapply(ssp_df$GloGEM, extract_year)
  ssp_dataframes[[ssp]]$PyGEM <- sapply(ssp_df$PyGEM, extract_year)
  ssp_dataframes[[ssp]]$OGGM <- sapply(ssp_df$OGGM, extract_year)
}

# Extract all the Median_PeakWater values from all dataframes
all_median_years <- unlist(lapply(ssp_dataframes, function(df) df$Median_PeakWater))

# Remove text within parentheses in RIVER_BASI so that indices match
basin_outlines$RIVER_BASI <- gsub("\\s*\\([^\\)]+\\)", "", basin_outlines$RIVER_BASI)

# Create a list to store merged data sets
merged_data_list <- list()

# Loop over each element in ssp_dataframes and merge with basin_outlines
for (ssp_name in names(ssp_dataframes)) {
  merged_data_list[[ssp_name]] <- merge(basin_outlines, ssp_dataframes[[ssp_name]], 
                                        by.x = "RIVER_BASI", by.y = "X", all.x = TRUE)}

#Loading in runoff data
rf_path <- paste0(basepath, 'CSV Outputs/RF Values/')
# Read runoff data for SSPs
ssp126_rfdf <- read.csv(paste0(rf_path,"ssp126_avgRF.csv"))
ssp245_rfdf <- read.csv(paste0(rf_path,"ssp245_avgRF.csv"))
ssp370_rfdf <- read.csv(paste0(rf_path,"ssp370_avgRF.csv"))
ssp585_rfdf <- read.csv(paste0(rf_path,"ssp585_avgRF.csv"))

# Create a list to store the dataframes
rf_dataframes <- list(ssp126_rfdf, ssp245_rfdf, ssp370_rfdf, ssp585_rfdf)

# Set names for easier indexing
names(rf_dataframes) <- c("ssp126", "ssp245", "ssp370", "ssp585")

# Reorder rows in each rf_dataframes dataframe alphabetically by "X"
for (ssp in names(rf_dataframes)) {
  rf_dataframes[[ssp]] <- rf_dataframes[[ssp]][order(rf_dataframes[[ssp]]$X), ]
}

#Reading in dataframe that contains additional basin data
supp_path <- paste0(basepath, "/CSV Outputs/Parameters/MASTERS/")

# Define the glacier models
gmodels <- c("GloGEM", "PyGEM", "OGGM")

# Read the CSV files into R dataframes and store them in a list
basin_info_dataframes <- lapply(gmodels, function(gmodel) {
  read.csv(paste0(supp_path, "MASTER_", gmodel, ".csv"))
})

# Name the list elements with the glacier model names
names(basin_info_dataframes) <- gmodels


# Iterate over each dataframe
for (gmodel in gmodels) {
  # Rename the first column to "Basin_Name"
  basin_info_dataframes[[gmodel]] <- basin_info_dataframes[[gmodel]] %>%
    rename(Basin_Name = X)
}

# Initialize an empty dataframe for median values
median_df <- data.frame(matrix(NA, nrow = nrow(basin_info_dataframes[[gmodels[1]]]), ncol = ncol(basin_info_dataframes[[gmodels[1]]])))
names(median_df) <- names(basin_info_dataframes[[gmodels[1]]])

# Iterate over the rows and columns to calculate the median
for (row_index in 1:nrow(median_df)) {
  for (col_index in 1:ncol(median_df)) {
    # Extract corresponding elements from each dataframe
    values <- sapply(gmodels, function(gmodel) {
      value <- basin_info_dataframes[[gmodel]][row_index, col_index]
      if (is.numeric(value)) value else NA  # Set non-numeric values to NA
    })
    # Compute median
    median_value <- median(values, na.rm = TRUE)
    # Assign median to the median dataframe
    median_df[row_index, col_index] <- median_value
  }
}

#Refill basin names which were converted to NaNs
median_df$Basin_Name <- basin_info_dataframes[[gmodels[1]]]$Basin_Name

# Store the median dataframe in the list
basin_info_dataframes[["Median"]] <- median_df

# Define the color palette based on the range of years
my_colors1 <- colorNumeric(palette = "magma", domain = range(2000, 2100))
my_colors2 <- colorNumeric(palette = "viridis", domain = range(0, 82))

# Creating our UI
ui <- fluidPage(
  titlePanel("Projected 21st Century Glacier Runoff"),
  sidebarLayout(
    sidebarPanel(
      selectInput("colorfill", "Select Color Fill:", choices = 
                    c("Average Annual Runoff", "Year of Peak Water")),
      selectInput("ssp", "Select SSP:", choices = names(merged_data_list)),
      selectInput("glacier_model", "Select Glacier Model:", choices = c("Multi-Model Median", "GloGEM", "PyGEM", "OGGM"))
    ),
    mainPanel(
      leafletOutput("map"),
      tags$style(HTML(".legend-superscript { line-height: 0.8; }"))  # CSS for adjusting line height
    )
  )
)


# Server
server <- function(input, output, session) {
  # Show notification on initial load
  showNotification("Welcome to the MINT (Middlebury Ice Numerical Team) interactive
  glacier model runoff intercomparison map. This app presents century-scale runoff
  projections from 3 state-of-the-art glacier evolution models. The basins
  can be colored according to their (across-century) average annual runoff or year 
  of projected peak water (year of maximum annual runoff).
  You can select any of the 3 glacier models, or the multi-model median, for 4
  different emission scenarios from the drop-down menus. Clicking on any
  individual basin will result in a pop-up containing more information and
  figures relevant to that basin. Enjoy!",
                   duration = NULL, type = "message")
  
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
  
  # Function to retrieve and display plot images
  display_plot <- function(basin_name, ssp, glacier_model) {
    # Construct the file path to the saved plot image
    if (input$glacier_model == "Multi-Model Median") {
      plot_file_path <- paste0(basepath, "App Figs/", ssp, "/RF_", basin_name, "_", ssp, ".png")
    } else {
      plot_file_path <- paste0(basepath, "App Figs/", ssp, "/RF_", basin_name, "_", ssp, "_", glacier_model, ".png")
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
                opacity = 1
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
      
      #Explicitly calling columns specific to single SSPs
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
          "Number of Glaciers" = supp_basin_data$X.glaciers,
          check.names = FALSE
        )
      })
      
      # Display plot for the selected basin, SSP, and glacier model
      plot_file_path <- display_plot(basin_name, input$ssp, input$glacier_model)
      
      # Render the plot image
      output$plot_image <- renderImage({
        list(src = plot_file_path, alt = "Runoff Plot", width = "100%")
      }, deleteFile = FALSE)
      
      showModal(modalDialog(
        title = "Basin Information",
        fluidRow(
          column(12, tableOutput("basin_table")),
          column(12, imageOutput("plot_image"))
        ),
        easyClose = TRUE,
        size = "l"  # Adjust the size of the modal dialog window
      ))
    }
  })
}

shinyApp(ui = ui, server = server)


#Code to save image of single SSP map

# # Merge the datasets using 'RIVER_BASI' and 'X'
# merged_data <- merge(basin_outlines, ssp_dataframes[['ssp370']], by.x = "RIVER_BASI", by.y = "X", all.x = TRUE)

# # Format the legend labels as strings
# legend_labels <- sprintf("%d", all_median_years)
# 
# # Create a color palette
# my_colors <- colorNumeric(palette = "magma", domain = range(all_median_years))
# 
# 
# # Create the leaflet map (assuming you already have the leaflet code)
# leaflet() |>
#   addTiles() |>  # Add a base map
#   addPolygons(data = combined_merged_data,
#               opacity = 1,
#               fillOpacity = 1,
#               fillColor = ~my_colors(combined_merged_data$Median_PeakWater),
#               color = "black",  # Set boundary line color to black
#               weight = 1,       # Set boundary line thickness to 1
#               label = ~RIVER_BASI) |> 
#  addLegend(position = "bottomright", 
#             pal = my_colors, 
#             values = range(all_median_years),  # Corrected values parameter
#             title = "Median Peak Water Year",
#             opacity = 1,
#             labels = legend_labels)  # Specify the legend labels as character strings

# # Set boundaries and zoom level
# my_map <- setView(my_map, lng = 5, lat = 0, zoom = 1.8)
# 
# # Set the working directory to your desired location
# setwd('/Users/finnwimberly/Desktop/Lizz Research/Paper Figs')
# 
# # Save the map as an HTML widget
# widget_file <- "my_map.html"
# saveWidget(my_map, file.path(getwd(), widget_file), selfcontained = TRUE)
# 
# # Capture a screenshot and save as PDF
# webshot::install_phantomjs()
# webshot(file.path(getwd(), widget_file), "my_map.pdf", delay = 3)