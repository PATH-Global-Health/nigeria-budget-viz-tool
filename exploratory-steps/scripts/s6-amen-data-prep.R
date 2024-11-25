#-------------------------------------------------------------------------------
# Data Prep for Amen Version of the Tool  
#-------------------------------------------------------------------------------

#-read in scenario total cost data per intervention-----------------------------
scenario_cost_data <- 
  readxl::read_xlsx("budget-viz-tool/working-data/cost_data_HS_inc1b.xlsx") |> 
  janitor::clean_names() |> 
  crossing(currency = c("USD", "Naira")) |> 
  rename(number_of_lgas = number_of_lg_as)

#-convert to Naira assuming same exchange rate as before 1600------------------- 
scenario_cost_data <- 
  scenario_cost_data |> 
  mutate(total_cost = case_when(currency == "Naira" ~ total_cost * 1600, 
                                TRUE ~ total_cost)) |> 
  rename(state_count = number_of_states, 
         lga_count = number_of_lgas, 
         title = intervention) |> 
  mutate(intervention_type = case_when(
    title %in% c("Capacity Building",
                 "Entomological surveillance",
                 "Governance & Coordination", 
                 "Monitoring & Evaluation", 
                 "Resource Mobilisation",
                 "Social Behaviour Change") ~ "Support Services", 
    TRUE ~ "Malaria Interventions"
  )) 

#-total cost data--------------------------------------------------------------- 
total_cost <- 
  scenario_cost_data |> 
  group_by(scenario, currency) |> 
  summarise(full_cost = sum(total_cost, na.rm=TRUE))

#-Intervention mix Data---------------------------------------------------------
# Function to convert intervention dataframe with 1's and 0's into intervention mix dataframe 
mix_map <- function(data, 
                    plan= NULL, 
                    plan_description = NULL){
  mix_map <- 
    data |> 
    # removing urban rural distinction from dashboard 
    select(-code_itn_urban, -pop_2025_projected, -pop_number_children_u5, 
         -percentage_urban_pop, -chemoprevention_split, - prev_u5_dhs_2021) |> 
    # Pivot the data from wide to long format
    pivot_longer(cols = starts_with("code_"), 
                 names_to = "intervention", 
                 values_to = "value") |> 
    # Filter to keep only rows where the intervention is set to 1
    filter(value == 1)  |> 
    mutate(intervention = str_remove(intervention, "code_")) |> 
    # create a new column for nicely coded intervention names  
    mutate(
      intervention_mix_to_show = str_to_upper(intervention)) |> 
    mutate( 
      intervention_mix_to_show = case_when(
        intervention == "cm_public" ~ "CM",
        intervention == "iptp" ~ "IPTp", 
        intervention == "vacc" ~ "Vaccine", 
        intervention == "itn_routine" ~ "ITN Routine", 
        intervention == "itn_campaign" ~ "ITN Campaign", 
        intervention == "irs" ~ "Targeted IRS", 
        intervention == "lsm" ~ "Targeted LSM",
        TRUE ~ intervention_mix_to_show)
    ) |> 
    group_by(state, lga)  |> 
    # Concatenate interventions with "+" separator
    mutate(intervention_summary = 
             paste(intervention_mix_to_show, collapse = " + ")
           ) |> 
    # remove case management private from the mix  
    mutate(intervention_summary =
             str_remove_all(intervention_summary, "\\s*\\+ CM_PRIVATE$")
           ) |> 
    mutate(intervention_summary =
             str_remove_all(intervention_summary, "CM_PRIVATE\\s*\\+\\s*")
           ) |> 
    # add plan code 
    mutate(plan = plan, 
           plan_description = plan_description
           ) |> 
    select(-value) 
  
  return(mix_map)
}


# intervention_mix data
intervention_mix <- 
  mix_map(data = read.csv("budget-viz-tool/working-data/2024-11-21-final-scen-1-v1.csv"), 
          plan = "Baseline", 
          plan_description = "Scenario 1 - Fully scaled up plan")

write.csv(intervention_mix, "budget-viz-tool/working-data/amen_intervention_mix.csv")
    
#-National Ribbon Data---------------------------------------------------------- 
national_ribbon_data <-
  read.csv("budget-viz-tool/working-data/national_ribbon_data.csv") |> 
  select(-full_cost, -cost_per_person) |> 
  left_join(total_cost |> filter(scenario == "scenario 1")) |> 
  mutate(cost_per_person = full_cost / pop_2025_projected)

write.csv(national_ribbon_data, "budget-viz-tool/working-data/amen_national_ribbon_data.csv")

#-national total cost breakdown-------------------------------------------------
national_total_cost_data <- 
  scenario_cost_data |> 
  filter(scenario == "scenario 1")

write.csv(national_total_cost_data, "budget-viz-tool/working-data/amen_national_total_cost_summary.csv")

#-pre-join data to shapefile----------------------------------------------------
lga_outline     <- sf::st_read("budget-viz-tool/working-data/shapefiles/lga_shapefile_simp.shp")
intervention_mix_map <- left_join(lga_outline, intervention_mix |> distinct(), by = c("state","lga"))
sf::st_write(intervention_mix_map, "budget-viz-tool/working-data/shapefiles/amen_intervention_mix_map.shp")

#-Individual intervention mix plot----------------------------------------------
intervention_mix_plot <-
    lga_outline %>% 
    left_join(intervention_mix |> 
                select(-intervention_summary, -plan, -plan_description),
              by = c("state", "lga"), 
              multiple = "all"
              ) |>   
    filter(intervention != "cm_private") |> 
    mutate(mix = 
             case_when(intervention_mix_to_show == "CM" ~ "Case management\n(public sector)",    
                       intervention_mix_to_show == "ITN Campaign" ~ "Mass ITN campaign",         
                       intervention_mix_to_show == "ITN Routine" ~ "Routine ITN distribution", 
                       intervention_mix_to_show == "Vaccine" ~ "Malaria Vaccine",
                       TRUE ~ intervention_mix_to_show)) %>%     
    mutate(mix = 
             factor(mix, 
                    levels = c("Mass ITN campaign", 
                               "Routine ITN distribution",  
                               "Targeted LSM", 
                               "Targeted IRS",  
                               "Malaria Vaccine",
                               "SMC",
                               "PMC", 
                               "IPTp",   
                               "Case management\n(public sector)"))) |> 
  select(country, state, lga, mix, geometry)

sf::st_write(intervention_mix_plot, "budget-viz-tool/working-data/shapefiles/amen_intervention_mix_plot.shp")
  
  ggplot() +   
    geom_sf(data = lga_outline, fill = "grey90", col="grey", alpha=0.5)+
    geom_sf(data = intervention_mix_plot, fill = "#1B7339", color = "grey") +
    geom_sf(data = state_outline, fill = NA, linewidth = 0.7, color = "black") + 
    facet_wrap(vars(mix)) +  
    theme_void() +  
    theme(legend.position = "none")  
  

#-plan comparison mixes---------------------------------------------------------
plan_comparison_costs <- 
    scenario_cost_data |> 
    mutate(plan = str_to_title(scenario), 
           plan_description = 
             case_when(plan == "Scenario 1" ~ "Fully scaled-up plan", 
                       plan == "Scenario 2" ~ "Prioritized plan A", 
                       plan == "Scenario 3" ~ "Prioritized plan B", 
                       plan == "Scenario 4" ~ "As is – prior plan"), 
           plan_details = 
             case_when(plan == "Scenario 1" ~ " ", 
                       plan == "Scenario 2" ~ "Targeted mass campaigns, reduced IRS and LSM, 22 state vaccine rollout, targeted PMC, medium private sector CM", 
                       plan == "Scenario 3" ~ "Targeted mass campaigns, reduced IRS and LSM, 2 state vaccine rollout, 2 state PMC, low private sector CM", 
                       plan == "Scenario 4" ~ "Targeted mass campaign, reduced LSM, no IRS, 2 state vaccine rollout, 2 state PMC, no private sector CM")
             
             )
  
write.csv(plan_comparison_costs, "budget-viz-tool/working-data/plan_comparison_costs.csv")
  

plan_comparison_mixes <- 
    mix_map(data = read.csv("budget-viz-tool/working-data/2024-11-21-final-scen-1-v1.csv"), 
            plan = "Scenario 1", 
            plan_description = "Fully scaled up plan") |> 
  bind_rows( mix_map(data = read.csv("budget-viz-tool/working-data/2024-11-21-final-scen-2-v1.csv"), 
                     plan = "Scenario 2", 
                     plan_description = "Prioritized plan A")) |> 
  bind_rows( mix_map(data = read.csv("budget-viz-tool/working-data/2024-11-21-final-scen-3-v1.csv"), 
                     plan = "Scenario 3", 
                     plan_description = "Prioritized plan B")) |> 
  bind_rows( mix_map(data = read.csv("budget-viz-tool/working-data/2024-11-21-final-scen-4-v1.csv"), 
                     plan = "Scenario 4", 
                     plan_description = "As is – prior plan")) |> 
  select(state, lga, intervention_summary, plan, plan_description) |> 
  distinct()

  
plan_comparison_mixes <- 
  lga_outline |> 
  left_join(plan_comparison_mixes, by=c("state", "lga"), 
            multiple="all")


sf::st_write(plan_comparison_mixes, "budget-viz-tool/working-data/shapefiles/amen_plan_comparison_mixes.shp")

total_cost_comparisons <- 
  plan_comparison_costs |> 
  group_by(plan, plan_description, plan_details, 
           currency) |> 
  summarise(full_cost = sum(total_cost, na.rm=TRUE))

write.csv(total_cost_comparisons, "budget-viz-tool/working-data/total_cost_comparisons.csv")

