#-------------------------------------------------------------------------------
# Global code for the Viz Tool  
#   Loads pacakges and data for use in the UI
#-------------------------------------------------------------------------------


# load packages-----------------------------------------------------------------
library(shiny)
library(leaflet)
library(DT)
library(plotly)
library(tidyverse)
library(leaflet.extras2)
library(shinythemes)
library(shinydashboard)
library(sf)
library(viridisLite)
library(glue)
library(billboarder)
library(treemap)
library(RColorBrewer)
library(htmlwidgets)

#-read in usable data from national costing consultant spreadsheet--------------

# ribbon data
national_ribbon_data <- read.csv("working-data/amen_national_ribbon_data.csv")

# cost summary data (total cost)
national_total_cost_summary <- read.csv("working-data/amen_national_total_cost_summary.csv")

# Shape files
country_outline <- sf::st_read("working-data/shapefiles/country_shapefile.shp")
state_outline   <- sf::st_read("working-data/shapefiles/state_shapefile_simp.shp")
lga_outline     <- sf::st_read("working-data/shapefiles/lga_shapefile_simp.shp")

state_outline$state[which(state_outline$state == "Akwa-Ibom")] <- "Akwa Ibom"

# intervention mix map 
intervention_mix_map <-
  sf::st_read("working-data/shapefiles/amen_intervention_mix_map.shp")|> 
  rename(intervention_summary = intrvn_)

# intervention facet map 
intervention_mix_plot <- 
  sf::st_read("working-data/shapefiles/amen_intervention_mix_plot.shp")

# plan comparison data
plan_comparison_costs <- read.csv("working-data/plan_comparison_costs.csv")
plan_comparison_mixes <- sf::st_read("working-data/shapefiles/amen_plan_comparison_mixes.shp")|> 
  rename(intervention_summary = intrvn_)
total_cost_comparisons <- read.csv("working-data/total_cost_comparisons.csv")

# Extract unique plans and their descriptions, excluding 'baseline'
unique_plans <- unique(plan_comparison_costs$plan[plan_comparison_costs$plan != "Scenario 1"])
plan_descriptions <- unique(plan_comparison_costs$plan_description[plan_comparison_costs$plan != "Scenario 1"])
plan_details <- unique(plan_comparison_costs$plan_details[plan_comparison_costs$plan != "Scenario 1"])

# Combine plan and plan_description for checkbox labels
plan_labels <- paste(unique_plans, "-", plan_descriptions, "(", plan_details, ")")