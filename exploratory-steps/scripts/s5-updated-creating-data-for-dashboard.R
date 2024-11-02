# packages 
library("tidyverse")

#---------------------------------------------------------------------------------------
# Creating cost summary data for the dashboard using the same methods as for the plan 
# comparison page 
# 
# This is because there were a couple of errors with just extracting the state and lga 
# values from the spreadsheet: 
#         - PMC costs were calculated for every LGA not just the targeted LGA so 
#           national cost was > than expected 
#         - SMC national costs were miscounting and not including all states/LGAs 
#           that were targeted with SMC so slightly < estimating costs 
#         - ITN routine distribution was double counting the distribution cost 
#           of nets so cost was > than expected 
#-----------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
# data set needs: 
# totals data
#   - total cost value 
#   
# intervention mix data 
#   - baseline intervention mix coded for the maps  
#  
#   
# ribbon data
#   - for the icons 
#   
#
# cost summary data (total cost)
# - data for the tables and to population donut chart / treemap 
#
# 
# cost summary data (elemental)
#   - elementatal breakdown of interventions for each intervention type 
#------------------------------------------------------------------------------------------

# source useful functions 
source("budget-viz-tool/plan-comparisons/scripts/f1-functions.R")
source("exploratory-steps/scripts/f1-functions.R")

#-baseline intervention mix---------------------------------------------------------------- 
intervention_mix <- 
  read.csv("budget-viz-tool/plan-comparisons/modifiable-intervention-mix.csv")

intervention_mix_map <- 
  create_intervention_mix_for_map(
    intervention_mix
  )

write.csv(intervention_mix_map, "budget-viz-tool/working-data/intervention_mix.csv", row.names = FALSE)

#-Cost summary data------------------------------------------------------------------------
# national cost data 
national_total_cost_summary <- 
  create_plan_cost_summary_grouped(
    intervention_mix_map, 
    grouping="national"
    )

write.csv(national_total_cost_summary, "budget-viz-tool/working-data/national_total_cost_summary.csv", row.names = FALSE)

# state cost data 
state_total_cost_summary <- 
  create_plan_cost_summary_grouped(
    intervention_mix_map, 
    grouping="state"
  )

write.csv(state_total_cost_summary, "budget-viz-tool/working-data/state_total_cost_summary.csv", row.names = FALSE)

# state cost data 
lga_total_cost_summary <- 
  create_plan_cost_summary_grouped(
    intervention_mix_map, 
    grouping="lga"
  )

write.csv(lga_total_cost_summary, "budget-viz-tool/working-data/lga_total_cost_summary.csv", row.names = FALSE)

#-Cost summary data elemental---------------------------------------------------------------
national_intervention_chart_data <- 
  create_plan_cost_elements_grouped(intervention_mix_map, 
                                    grouping = "national")

write.csv(national_intervention_chart_data, "budget-viz-tool/working-data/national_intervention_chart_data.csv", row.names = FALSE)

state_intervention_chart_data <- 
  create_plan_cost_elements_grouped(intervention_mix_map, 
                                    grouping="state")

write.csv(state_intervention_chart_data, "budget-viz-tool/working-data/state_intervention_chart_data.csv", row.names = FALSE)

lga_intervention_chart_data <- 
  create_plan_cost_elements_grouped(intervention_mix_map, 
                                    grouping="lga")

write.csv(lga_intervention_chart_data, "budget-viz-tool/working-data/lga_intervention_chart_data.csv", row.names = FALSE)

#-total cost data--------------------------------------------------------------------------
# National 
national_data_total_cost <- 
  read.csv("budget-viz-tool/working-data/national_data_total_cost.csv") |> 
  select(-full_cost, -cost_per_person) |> 
  left_join(national_total_cost_summary |> group_by(currency) |> summarise(full_cost = sum(total_cost))) |> 
  mutate(cost_per_person = full_cost / pop_2025_projected)

write.csv(national_data_total_cost, "budget-viz-tool/working-data/national_data_total_cost.csv", row.names = FALSE)

# State 
state_data_total_cost <- 
  read.csv("budget-viz-tool/working-data/state_data_total_cost.csv") |> 
  select(-full_cost, -cost_per_person) |> 
  left_join(state_total_cost_summary |> group_by(state, currency) |> summarise(full_cost = sum(total_cost, na.rm=TRUE))) |> 
  mutate(cost_per_person = full_cost / pop_2025_projected)

write.csv(state_data_total_cost, "budget-viz-tool/working-data/state_data_total_cost.csv", row.names = FALSE)

# LGA 
lga_data_total_cost <- 
  read.csv("budget-viz-tool/working-data/lga_data_total_cost.csv") |> 
  select(-full_cost, -cost_per_person) |> 
  left_join(lga_total_cost_summary |> group_by(state, lga, currency) |> summarise(full_cost = sum(total_cost, na.rm=TRUE))) |> 
  mutate(cost_per_person = full_cost / pop_2025_projected)

write.csv(lga_data_total_cost, "budget-viz-tool/working-data/lga_data_total_cost.csv", row.names = FALSE)



#-Ribbon data------------------------------------------------------------------------------
national_ribbon_data <- 
  read.csv("budget-viz-tool/working-data/national_ribbon_data.csv") |> 
  select(-full_cost, -cost_per_person) |> 
  left_join(national_data_total_cost)

write.csv(national_ribbon_data, "budget-viz-tool/working-data/national_ribbon_data.csv", row.names = FALSE)


state_ribbon_data <- 
  read.csv("budget-viz-tool/working-data/state_ribbon_data.csv") |> 
  select(-full_cost, -cost_per_person) |> 
  left_join(state_data_total_cost)

write.csv(state_ribbon_data, "budget-viz-tool/working-data/state_ribbon_data.csv", row.names = FALSE)


lga_ribbon_data <- 
  read.csv("budget-viz-tool/working-data/lga_ribbon_data.csv") |> 
  select(-full_cost, -cost_per_person) |> 
  left_join(lga_data_total_cost)

write.csv(lga_ribbon_data, "budget-viz-tool/working-data/lga_ribbon_data.csv", row.names = FALSE)
