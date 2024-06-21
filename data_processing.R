install.packages("stringi")

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


# Paths to data and figures directories relative to the project root
outline_path <- here("Data&Figures", "Outlines", "glacier_basins.shp")
print(outline_path)
basin_outlines <- st_read(outline_path) 

# Define all of our dimensions
scenarios <- c('ssp126','ssp245','ssp370','ssp585')

# Path for peak water data relative to the project root
pw_path <- here("Data&Figures", "CSV Outputs", "Median PW", "Median_PeakWater")

# Read the CSV files into R dataframes
ssp126_df <- fread(paste0(pw_path,"_ssp126.csv"), encoding = "UTF-8")
colnames(ssp126_df)[1]<-"X"
ssp245_df <- fread(paste0(pw_path,"_ssp245.csv"), encoding = "UTF-8")
colnames(ssp245_df)[1]<-"X"
ssp370_df <- fread(paste0(pw_path,"_ssp370.csv"), encoding = "UTF-8")
colnames(ssp370_df)[1]<-"X"
ssp585_df <- fread(paste0(pw_path,"_ssp585.csv"), encoding = "UTF-8")
colnames(ssp585_df)[1]<-"X"

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
#rf_path <- paste0(basepath, 'CSV Outputs/RF Values/')
rf_path <- here("Data&Figures", "CSV Outputs", "RF Values")


# Read runoff data for SSPs
ssp126_rfdf <- fread(paste0(rf_path,"/ssp126_avgRF.csv"), encoding = "UTF-8")
colnames(ssp126_rfdf)[1]<-"X"
ssp245_rfdf <- fread(paste0(rf_path,"/ssp245_avgRF.csv"), encoding = "UTF-8")
colnames(ssp245_rfdf)[1]<-"X"
ssp370_rfdf <- fread(paste0(rf_path,"/ssp370_avgRF.csv"), encoding = "UTF-8")
colnames(ssp370_rfdf)[1]<-"X"
ssp585_rfdf <- fread(paste0(rf_path,"/ssp585_avgRF.csv"), encoding = "UTF-8")
colnames(ssp585_rfdf)[1]<-"X"


# Create a list to store the dataframes
rf_dataframes <- list(ssp126_rfdf, ssp245_rfdf, ssp370_rfdf, ssp585_rfdf)

# Set names for easier indexing
names(rf_dataframes) <- c("ssp126", "ssp245", "ssp370", "ssp585")

# Reorder rows in each rf_dataframes dataframe alphabetically by "X"
for (ssp in names(rf_dataframes)) {
  rf_dataframes[[ssp]] <- rf_dataframes[[ssp]][order(rf_dataframes[[ssp]]$X), ]
}

#Reading in dataframe that contains additional basin data
supp_path <- here("Data&Figures", "CSV Outputs", "Parameters", "MASTERS")

# Define the glacier models
gmodels <- c("GloGEM", "PyGEM", "OGGM")

# Read the CSV files into R dataframes and store them in a list
basin_info_dataframes <- lapply(gmodels, function(gmodel) {
  fread(paste0(supp_path, "/MASTER_", gmodel, ".csv"), encoding = "UTF-8") 
})

# Name the list elements with the glacier model names
names(basin_info_dataframes) <- gmodels


# Iterate over each dataframe
for (gmodel in gmodels) {
  # Rename the first column to "Basin_Name"
  basin_info_dataframes[[gmodel]] <- basin_info_dataframes[[gmodel]] %>%
    rename(Basin_Name = V1)
}

# Initialize an empty dataframe for median values
median_df <- data.frame(matrix(NA, nrow = nrow(basin_info_dataframes[[gmodels[1]]]), ncol = ncol(basin_info_dataframes[[gmodels[1]]])))
names(median_df) <- names(basin_info_dataframes[[gmodels[1]]])

# Iterate over the rows and columns to calculate the median
for (row_index in 1:nrow(median_df)) {
  for (col_index in 1:ncol(median_df)) {
    # Extract corresponding elements from each dataframe
    values <- sapply(gmodels, function(gmodel) {
      value <- basin_info_dataframes[[gmodel]][[row_index, col_index]]
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


#Processing df text
save(merged_data_list, file = "merged_data_list.R")
save(rf_dataframes, file = "rf_dataframes.R")
save(basin_info_dataframes, file = "basin_info_dataframes.R")

getwd()
