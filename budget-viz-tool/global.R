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
#-read in usable data from national costing consultant spreadsheet--------------

# total data 
national_data_total_cost <- read.csv("working-data/national_data_total_cost.csv")
state_data_total_cost <- read.csv("working-data/state_data_total_cost.csv")
lga_data_total_cost <- read.csv("working-data/lga_data_total_cost.csv")

# intervention mix data 
intervention_mix <- read.csv("working-data/intervention_mix.csv") |> 
  select(state, lga, intervention_summary) |> 
  distinct() 

# prevalence data 
prevalence_data <- read.csv("working-data/prevalence_data.csv") 

# ribbon data 
national_ribbon_data <- read.csv("working-data/national_ribbon_data.csv")
state_ribbon_data <- read.csv("working-data/state_ribbon_data.csv")
lga_ribbon_data <- read.csv("working-data/lga_ribbon_data.csv")

# cost summary data (total cost)
national_total_cost_summary <- read.csv("working-data/national_total_cost_summary.csv")
state_total_cost_summary <- read.csv("working-data/state_total_cost_summary.csv")
lga_total_cost_summary <- read.csv("working-data/lga_total_cost_summary.csv")

# cost summary data (elemental)
national_intervention_chart_data <- read.csv("working-data/national_intervention_chart_data.csv")
state_intervention_chart_data <- read.csv("working-data/state_intervention_chart_data.csv")
lga_intervention_chart_data <- read.csv("working-data/lga_intervention_chart_data.csv")

# add in plan comparison data 
plan_comparison_mixes <- read.csv("plan-comparisons/viz-data/plan-comparison-mixes.csv")
plan_comparison_costs <- read.csv("plan-comparisons/viz-data/plan-comparison-costs.csv")

plan_comparison_mixes$plan <- str_to_title(plan_comparison_mixes$plan)
plan_comparison_costs$plan <- str_to_title(plan_comparison_costs$plan)

# Extract unique plans and their descriptions, excluding 'baseline'
unique_plans <- unique(plan_comparison_mixes$plan[plan_comparison_mixes$plan != "Baseline"])
plan_descriptions <- unique(plan_comparison_mixes$plan_description[plan_comparison_mixes$plan != "Baseline"])

# Combine plan and plan_description for checkbox labels
plan_labels <- paste(unique_plans, "-", plan_descriptions)

total_cost_comparisons <- 
  plan_comparison_costs |> 
  group_by(plan, plan_description, 
           currency) |> 
  summarise(full_cost = sum(total_cost, na.rm=TRUE))


#-read in other data sources----------------------------------------------------
# Shape files
country_outline <- sf::st_read("working-data/shapefiles/country_shapefile.shp")
state_outline   <- sf::st_read("working-data/shapefiles/state_shapefile_simp.shp")
lga_outline     <- sf::st_read("working-data/shapefiles/lga_shapefile_simp.shp")

state_outline$state[which(state_outline$state == "Akwa-Ibom")] <- "Akwa Ibom"

#-merge the different data sets and load them as items into the environment-----
# intervention mix map  
# Join intervention mix data with LGA outline
# intervention_mix_map <- left_join(lga_outline, intervention_mix, by = c("state","lga"))
# sf::st_write(intervention_mix_map, "budget-viz-tool/working-data/shapefiles/intervention_mix_map.shp")
intervention_mix_map <- sf::st_read("working-data/shapefiles/intervention_mix_map.shp") |> 
  rename(intervention_summary = intrvn_)

# # intervention single map 
# interactive_map <-
#   intervention_mix_map |> 
#   mutate(
#     unique_interventions = intervention_summary,
#   ) |>
#   separate_rows(
#     unique_interventions, sep = "\\+ "
#   ) |>
#   mutate(unique_interventions = trimws(unique_interventions))

# sf::st_write(interactive_map, "budget-viz-tool/working-data/shapefiles/interactive_map.shp")
interactive_map <- 
  sf::st_read("working-data/shapefiles/interactive_map.shp") |> 
  rename(intervention_summary = intrvn_, 
         unique_interventions = unq_ntr)

# prevalence data 
# prev_outline <- 
#   state_outline |> 
#   left_join(prevalence_data |> filter(year == 2021))
# sf::st_write(prev_outline, "budget-viz-tool/working-data/shapefiles/prev_outline.shp")
prev_outline <- 
  sf::st_read("working-data/shapefiles/prev_outline.shp") |> 
  rename(prev_u5_state = prv_5_s)

# intervention mix map data for plan comparisons 
# intervention_mix_map_plan_comp <- 
#   left_join(lga_outline, 
#             plan_comparison_mixes |> select(state, lga, intervention_summary, plan, plan_description) |> 
#               distinct(), 
#             multiple="all", by=c("state", "lga"))
# 
# sf::st_write(intervention_mix_map_plan_comp, "budget-viz-tool/working-data/shapefiles/intervention_mix_map_plan_comp.shp")

intervention_mix_map_plan_comp <- 
<<<<<<< HEAD
  left_join(lga_outline, plan_comparison_mixes, multiple="all", by=c("state", "lga"))

=======
>>>>>>> acdd73321c44d2ab72c0a784f18772bd1cefe8df
  sf::st_read("working-data/shapefiles/intervention_mix_map_plan_comp.shp") |> 
  rename(intervention_summary = intrvn_, 
         plan_description = pln_dsc)
