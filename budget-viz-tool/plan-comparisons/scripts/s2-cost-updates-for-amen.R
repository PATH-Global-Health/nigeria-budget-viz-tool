
library(tidyverse)

# unit cost data 
# Add in unit costs and calculate costs for these interventions  
unit_cost_data <- 
  read.csv("exploratory-steps/data/working-data/codable-unit-costs-october.csv") |> 
  # remove support services cost 
  filter(intervention != "Support Services", 
         intervention != "Entomological Surveillance") 


# costs that need to be updated with GLOBAL FUND UPDATES

# USD
unit_cost_data$usd_cost[which(unit_cost_data$resource == "ITN Campaign-Procurement per ITN (Dual AI)")] <- 4.42
unit_cost_data$usd_cost[which(unit_cost_data$resource == "ITN Campaign-Cost of distribution from State to LGA and from LGA to DHs")] <- 4.94
unit_cost_data$usd_cost[which(unit_cost_data$resource == "ITN Routine Distribution-Pocurement cost per ITN (Dual AI)")] <- 4.42
unit_cost_data$usd_cost[which(unit_cost_data$resource == "ITN Routine Distribution-Operational cost per ITN")] <- 13.53 # this is the cost per bale - bale assumes 50 nets per bale 
unit_cost_data$resource[which(unit_cost_data$resource == "ITN Routine Distribution-Operational cost per ITN")] <- "ITN Routine Distribution-Operational cost per bale" # this is the cost per bale - bale assumes 50 nets per bale 
unit_cost_data$unit[which(unit_cost_data$resource == "ITN Routine Distribution-Operational cost per Bale")] <- "per bale" # this is the cost per bale - bale assumes 50 nets per bale 
unit_cost_data$usd_cost[which(unit_cost_data$resource == "SMC-SPAQ-3-11 months-Procurement cost per SPAQ")] <- 0.29
unit_cost_data$usd_cost[which(unit_cost_data$resource == "SMC-SPAQ-12-59 months-Procurement cost per SPAQ")] <- 0.31
unit_cost_data$usd_cost[which(unit_cost_data$resource == "PMC-SP-Procurement cost")] <- 0.39
unit_cost_data$usd_cost[which(unit_cost_data$resource == "PMC-SP-Routine Distribution cost")] <- 0
unit_cost_data$usd_cost[which(unit_cost_data$resource == "IPTp-SP-Procurement cost per SP")] <- 0.42
unit_cost_data$usd_cost[which(unit_cost_data$resource == "IPTp-SP-Routine Distribution cost")] <- 0
unit_cost_data$usd_cost[which(unit_cost_data$resource == "Case Management-AL-Procurement cost per AL")] <- 0.43
unit_cost_data$usd_cost[which(unit_cost_data$resource == "Case Management-AL-Routine Distribution cost per AL")] <- 0
unit_cost_data$usd_cost[which(unit_cost_data$resource == "Case Management-Artesunate injections-Procurement cost")] <- 1.49
unit_cost_data$usd_cost[which(unit_cost_data$resource == "Case Management-Artesunate injections-Routine Distribution cost")] <- 0
unit_cost_data$usd_cost[which(unit_cost_data$resource == "Case Management-Rectal Artesunate Suppositories (RAS)-Procurement cost per RAS")] <- 0.77
unit_cost_data$usd_cost[which(unit_cost_data$resource == "Case Management-RAS-Routine Distribution cost per RAS")] <- 0
unit_cost_data$usd_cost[which(unit_cost_data$resource == "Case Management-RDT kits-Procurement cost per kit & consumables")] <- 0.34
unit_cost_data$usd_cost[which(unit_cost_data$resource == "Case Management-RDT kits-Distribution cost per kit & consumables")] <- 0

# NGN 
unit_cost_data$ngn_cost <- unit_cost_data$usd_cost * 1600

# new cost lines to be added 
# PMC-Cost per child per annum
to_add_pmc <- 
  data.frame(resource = "PMC-Cost per child per annum", 
             intervention = "PMC", 
             cost_category = "Operational", 
             unit = "per child", 
             ngn_cost = 5232, 
             usd_cost = 3.27
  )

# Entomological surveillance data has changed  
to_add_ento <- 
  data.frame(resource = "Entomological Surveillance-Cost per state", 
             intervention = "Entomological Surveillance", 
             cost_category = "Operational", 
             unit = "per state", 
             ngn_cost = 270769231, 
             usd_cost = 169230.77
  )

# add in additional changes
unit_cost_data <- 
  unit_cost_data |> 
  bind_rows(to_add_pmc, to_add_ento)

rm("to_add_pmc")
rm("to_add_ento")

# values that won't change between scenarios  
gf_data <- data.frame(
  intervention = "gf_wb",
  title = "Global Fund Warehousing & Distribution Activities", 
  currency = c("Naira", "USD"), 
  total_cost = c(4086518400, 2554074)
)


#-SCENARIO 1B FROM THE SPREADSHEET---------------------------------------------------------------  
scenario1b <- 
  read.csv("exploratory-steps/data/working-data/codable-national-data-october.csv") |> 
  select(contains("total_cost"), 
         -cm_rdt_kits_total_cost, -cm_al_total_cost,
         -cm_iv_artesunate_total_cost, -cm_ras_total_cost, 
         -gf_wd_total_cost) |> 
  pivot_longer(cols = everything(),
               names_to = "intervention",
               values_to = "total_cost"
  ) |> 
  mutate(intervention = gsub("_total_cost", "", intervention)) |> 
  mutate(total_cost = as.numeric(total_cost)) |> 
  mutate(title = case_when(intervention == "itn_campaign" ~ "ITN Campaign", 
                           intervention == "itn_routine" ~ "ITN Routine Distribution", 
                           intervention == "irs" ~ "IRS",
                           intervention == "lsm" ~ "LSM", 
                           intervention == "smc" ~ "SMC", 
                           intervention == "pmc" ~ "PMC", 
                           intervention == "iptp" ~ "IPTp", 
                           intervention == "vacc" ~ "Malaria Vaccine", 
                           intervention == "cm_public" ~ "Public Sector CM", 
                           intervention == "cm_private" ~ "Private Sector CM", 
                           intervention == "ss_sbc" ~ "Social Behaviour Change", 
                           intervention == "me" ~ "Monitoring & Evaluation", 
                           intervention == "cb" ~ "Capacity Building", 
                           intervention == "gc" ~ "Governance & Coordination", 
                           intervention == "rm" ~ "Resource Mobilisation", 
                           intervention == "ento_surveillance" ~ "Entomological surveillance")) |> 
  crossing(currency = c("Naira", "USD")) |> 
  mutate(total_cost = case_when(currency == "USD" ~ total_cost /1600, 
                                TRUE ~ total_cost)) |> 
  bind_rows(gf_data) |> 
  mutate(total_cost = round(total_cost, 0)) |> 
  mutate(scenario = "Scenario 1") 

# READ IN OLD SCENARIO DATA AND KEEP THE COSTS THAT AREN'T CHANGING AND PULL 
# ANY MODIFIED COSTS FROM ABOVE 
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

# these cost data have changed so need to be removed from scenarios 
to_change <- c("Public Sector CM", 
               "Private Sector CM",
               "IPTp", 
                "SMC", 
               "Entomological surveillance")

scenario_cost_data <- 
  scenario_cost_data |> 
 # remove values that need to be changed 
  filter(!title %in% 
           to_change) 

#-new sceario 2-----------------------------------------------------------------
# Targeted mass campaigns,
#  reduced IRS and LSM,
#  22 state vaccine rollout,
#  targeted PMC,
#   medium private sector CM private sector * 0.2
scenario2 <- 
  scenario_cost_data |> 
  filter(scenario == "scenario 2") |> 
  # add in case management  
  bind_rows(scenario1b |> filter(title == "Public Sector CM") |> select(-scenario)) |> 
  bind_rows(scenario1b |> filter(title == "Private Sector CM") |> select(-scenario)) |>
  # add in IPTp 
  bind_rows(scenario1b |> filter(title == "IPTp")|> select(-scenario)) |> 
  # add in SMC 
  bind_rows(scenario1b |> filter(title == "SMC")|> select(-scenario)) |> 
  # add in ento data 
  bind_rows(data.frame(title = "Entomological surveillance", 
                       scenario = "scenario 2", 
                       state_count = 11, 
                       lga_count = NA, 
                       total_cost = c(11 * unit_cost_data$ngn_cost[which(unit_cost_data$resource == "Entomological Surveillance-Cost per state")], 
                                      11 * unit_cost_data$usd_cost[which(unit_cost_data$resource == "Entomological Surveillance-Cost per state")]),
                       currency = c("Naira", "USD"),  
                       intervention_type = "Support Services") )  |> 
  # alter private sector costs as per previous examples
  mutate(total_cost = case_when(title == "Private Sector CM" ~ total_cost * 0.2, 
                                TRUE ~ total_cost)) |> 
  bind_rows(gf_data) |> 
  mutate(total_cost = round(total_cost, 0))


# to calculate PMC and ITN values 

#-new scenario 3----------------------------------------------------------------
# Targeted mass campaigns, 
# reduced IRS and LSM, 
# 2 state vaccine rollout, 
# 2 state PMC, 
# low private sector CM  private sector * 0.1
scenario3 <- 
  scenario_cost_data |> 
  filter(scenario == "scenario 3") |> 
  # add in case management  
  bind_rows(scenario1b |> filter(title == "Public Sector CM") |> select(-scenario)) |> 
  bind_rows(scenario1b |> filter(title == "Private Sector CM") |> select(-scenario)) |>
  # add in IPTp 
  bind_rows(scenario1b |> filter(title == "IPTp")|> select(-scenario)) |> 
  # add in SMC 
  bind_rows(scenario1b |> filter(title == "SMC")|> select(-scenario)) |> 
  # add in ento data 
  bind_rows(data.frame(title = "Entomological surveillance", 
                       scenario = "scenario 3", 
                       state_count = 11, 
                       lga_count = NA, 
                       total_cost = c(11 * unit_cost_data$ngn_cost[which(unit_cost_data$resource == "Entomological Surveillance-Cost per state")], 
                                      11 * unit_cost_data$usd_cost[which(unit_cost_data$resource == "Entomological Surveillance-Cost per state")]),
                       currency = c("Naira", "USD"),  
                       intervention_type = "Support Services") )  |> 
  # add in private sector case management costs at reduced subsidy as per previous meeting code 
  mutate(total_cost = case_when(title == "Private Sector CM" ~ total_cost * 0.1, 
                                TRUE ~ total_cost)) |> 
  bind_rows(gf_data) |> 
  mutate(total_cost = round(total_cost, 0))


#-new scenario 3----------------------------------------------------------------
# Targeted mass campaign, 
# reduced LSM, 
# no IRS, 
# 2 state vaccine rollout, 
# 2 state PMC, 
# no private sector CM
scenario4 <- 
  scenario_cost_data |> 
  filter(scenario == "scenario 4") |> 
  # add in case management  
  bind_rows(scenario1b |> filter(title == "Public Sector CM") |> select(-scenario)) |> 
  bind_rows(scenario1b |> filter(title == "Private Sector CM") |> select(-scenario)) |>
  # add in IPTp 
  bind_rows(scenario1b |> filter(title == "IPTp") |> select(-scenario)) |> 
  # add in SMC 
  bind_rows(scenario1b |> filter(title == "SMC") |> select(-scenario)) |> 
  # add in ento data 
  bind_rows(data.frame(title = "Entomological surveillance", 
                       scenario = "scenario 4", 
                       state_count = 26, 
                       lga_count = NA, 
                       total_cost = c(26 * unit_cost_data$ngn_cost[which(unit_cost_data$resource == "Entomological Surveillance-Cost per state")], 
                                      26 * unit_cost_data$usd_cost[which(unit_cost_data$resource == "Entomological Surveillance-Cost per state")]),
                       currency = c("Naira", "USD"),  
                       intervention_type = "Support Services") )  |> 
  # remove Private sector for this scenario
  mutate(total_cost = case_when(title == "Private Sector CM" ~ NA, 
                                TRUE ~ total_cost)) |> 
  # remove IRS 
  mutate(total_cost = case_when(title == "IRS" ~ NA, 
                                TRUE ~ total_cost)) |> 
  bind_rows(gf_data) |> 
  mutate(total_cost = round(total_cost, 0))


#-DATA TO COST THE REMAINING NEW INTERVENTIONS WITH COST CHANGES----------------
# INTERVENTIONS TO COST 
# ITN CAMPAIGN 
# ITN ROUTINE 
# PMC 

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

# read in intervention mix dataframes and filter to these interventions  
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
  select(state, lga, plan, intervention) |> 
  filter(intervention %in% c("itn_campaign", "itn_routine", "pmc"))

# extract quantification data for these interventions 
# Creating a total quantification and cost spreadsheet to be able to manipulate for 
# different costing scenarios 
state_data_extract    <- read.csv("exploratory-steps/data/working-data/codable-state-data-october.csv")
lga_data_extract      <- read.csv("exploratory-steps/data/working-data/codable-lga-data-october.csv")|> 
  # add state names to LGA level data 
  left_join(
    state_data_extract |>  select(spatial_level_1, spatial_level_2, state), 
    by=c("spatial_level_1", "spatial_level_2")
  ) |> 
  relocate(state, .before = lga) 

# format LGA names to match with shapefile 
lga_data_extract$lga <- gsub("-", " ", lga_data_extract$lga) #remove all "-"
lga_data_extract$lga <- gsub('[[:digit:]]+', '', lga_data_extract$lga) #remove numeric values from names
lga_data_extract$lga <- stringr::str_to_title(lga_data_extract$lga)    #make all names sentence case

sum(lga_data_extract$itn_campaign_net_quantity, na.rm = TRUE)

#-CAMPAIGN NET QUANTIFICATION
# Nets calculated as target population / 1.8
# Net bales calculated as nets / 50  
itn_quant <- 
  lga_data_extract |> 
  select(state, lga, pop_2025_projected, 
         itn_campaign_net_quantity, 
         itn_campaign_bale_quantity) |> 
  filter(!is.na(itn_campaign_net_quantity))|> 
  rename(itn_campaign_target_pop = pop_2025_projected)
# the reamining scenarios all reduce the number of LGAs with campaign slightly so lets not impute missing values 

# sum(itn_quant$itn_campaign_net_quantity)
# nrow(itn_quant) # 494 LGAs being targeted here with Nets - so this is causing the values to be off - as this is what is being summed in the spreadsheet for the calculation at the national level
# 
# itn_checks = read.csv("budget-viz-tool/working-data/2024-11-21-final-scen-1-v1.csv")
# sum(itn_checks$code_itn_campaign)
# # this has the right number of lgas 

# fully quantified not costed  
fully_quantified <- 
  lga_data_extract |> 
  select(state, lga, 
         pmc_target_pop = pop_number_children_0_2_yrs, 
         itn_routine_net_quantity) |> 
  # add routine net bale quantity 
  mutate(itn_routine_bale_quantity = itn_routine_net_quantity / 50) |> 
  left_join(itn_quant)  
  
# cost the quantification 
fully_quantified_costed <- 
  fully_quantified |> 
  mutate(itn_campaign_net_procurement_cost = itn_campaign_net_quantity * unit_cost_data$ngn_cost[which(unit_cost_data$resource == "ITN Campaign-Procurement per ITN (Dual AI)")],
         itn_campaign_net_distribution_procurement_cost = itn_campaign_bale_quantity * unit_cost_data$ngn_cost[which(unit_cost_data$resource == "ITN Campaign-Cost of distribution from State to LGA and from LGA to DHs")], 
         itn_campaign_campaign_cost = itn_campaign_net_quantity * unit_cost_data$ngn_cost[which(unit_cost_data$resource == "ITN Campaign-Operational cost per ITN")], 
         itn_campaign_total_cost = itn_campaign_net_procurement_cost + itn_campaign_net_distribution_procurement_cost + itn_campaign_campaign_cost,
         itn_routine_net_procurement_cost = itn_routine_net_quantity * unit_cost_data$ngn_cost[which(unit_cost_data$resource == "ITN Routine Distribution-Pocurement cost per ITN (Dual AI)")], 
         itn_routine_net_operational_cost = itn_routine_bale_quantity * unit_cost_data$ngn_cost[which(unit_cost_data$resource == "ITN Routine Distribution-Operational cost per bale")], 
         itn_routine_total_cost = itn_routine_net_procurement_cost + itn_routine_net_operational_cost, 
         pmc_total_cost = pmc_target_pop * unit_cost_data$ngn_cost[which(unit_cost_data$resource == "PMC-Cost per child per annum")] 
        ) |> 
  # reduce to total costs 
  select(state, lga, contains("total_cost"), contains("target_pop"), 
         itn_campaign_target_pop = itn_campaign_net_quantity, 
         itn_routine_target_pop = itn_routine_net_quantity, 
         -itn_campaign_target_pop) |> 
  # Pivot longer on total_cost and target_pop with matching pairs
  pivot_longer(
    cols = matches("^(.*)_total_cost|(.*)_target_pop$"), 
    names_to = c("intervention", ".value"), 
    names_pattern = "(.*)_(total_cost|target_pop)"
  )


# join with plan data
plan_comparison_costs <- 
  plan_comparison_mixes |> 
  left_join(fully_quantified_costed, by=c("state", "lga", "intervention")) |> 
  # summarise up to national level 
  group_by(plan, intervention) |> 
  summarise(total_cost = sum(total_cost, na.rm=TRUE), 
            target_pop = sum(target_pop, na.rm=TRUE)) |> 
  # add on additional national level costs for nets and the missing values from missing net values in Disputed State in Taraba which get dropped in this method
  mutate(total_cost = 
           case_when(
             intervention == "itn_campaign" ~ total_cost + 
               (unit_cost_data$ngn_cost[which(unit_cost_data$resource == "ITN Campaign-Storage of Hardwares")]) + 
               (19127 * unit_cost_data$ngn_cost[which(unit_cost_data$resource == "ITN Campaign-Procurement per ITN (Dual AI)")]) +
               (383 * unit_cost_data$ngn_cost[which(unit_cost_data$resource == "ITN Campaign-Cost of distribution from State to LGA and from LGA to DHs")]) +
               (19127 * unit_cost_data$ngn_cost[which(unit_cost_data$resource == "ITN Campaign-Operational cost per ITN")]),  
             intervention == "itn_routine" ~ total_cost + 
               (3093 * unit_cost_data$ngn_cost[which(unit_cost_data$resource == "ITN Routine Distribution-Pocurement cost per ITN (Dual AI)")]) +  
               (3093/50 * unit_cost_data$ngn_cost[which(unit_cost_data$resource == "ITN Routine Distribution-Operational cost per bale")]), 
         TRUE ~ total_cost)
         ) |> 
  crossing(currency = c("Naira", "USD")) |> 
  mutate(total_cost = case_when(currency == "USD" ~ total_cost /1600, 
                                TRUE ~ total_cost)) |> 
  mutate(total_cost = round(total_cost, 0)) |> 
  select(-target_pop) |> 
  mutate(title = case_when(intervention == "itn_campaign" ~ "ITN Campaign", 
                           intervention == "itn_routine" ~ "ITN Routine Distribution", 
                           intervention == "pmc" ~ "PMC"))

# check for differences in scenario 1 and the spreadsheet values 

difference <- 
  plan_comparison_costs |> 
  filter(plan == "Scenario 1") |> 
  left_join(scenario1b, by=c("intervention", "currency")) |> 
  mutate(difference_excel_to_us = total_cost.y - total_cost.x, 
         prop = difference_excel_to_us / total_cost.y * 100)


#-join these back to the scenario dataframes------------------------------------ 
intervention_counts <- 
  readxl::read_xlsx("budget-viz-tool/working-data/cost_data_HS_inc1b.xlsx") |> 
  janitor::clean_names() |>  
  rename(number_of_lgas = number_of_lg_as) |> 
  select(title = intervention,
         scenario, 
         state_count = number_of_states, 
         lga_count = number_of_lgas) 

scenario1b_new <- 
  scenario1b |> 
  left_join(plan_comparison_costs |> filter(plan == "Scenario 1"),
            by=c("scenario"= "plan", "intervention", "currency"), 
            suffix = c("", ".new")) |> 
  mutate(total_cost = ifelse(!is.na(total_cost.new), total_cost.new, total_cost)) %>%
  select(-total_cost.new) |> 
  mutate(plan = "Scenario 1", 
         scenario = "scenario 1") |> 
  left_join(intervention_counts) |> 
  select(-intervention, -title.new) 


scenario2_new <- 
  scenario2 |> 
  mutate(scenario = str_to_title(scenario)) |> 
  left_join(plan_comparison_costs |> filter(plan == "Scenario 2"),
            by=c("scenario"= "plan", "title", "currency"), 
            suffix = c("", ".new")) |> 
  mutate(total_cost = ifelse(!is.na(total_cost.new), total_cost.new, total_cost)) %>%
  select(-total_cost.new)|> 
  mutate(plan = "Scenario 2", 
         scenario = "scenario 2") |> 
  left_join(intervention_counts)|> 
  select(-intervention) 

scenario3_new <- 
  scenario3 |> 
  mutate(scenario = str_to_title(scenario)) |> 
  left_join(plan_comparison_costs |> filter(plan == "Scenario 3"),
            by=c("scenario"= "plan", "title", "currency"), 
            suffix = c("", ".new")) |> 
  mutate(total_cost = ifelse(!is.na(total_cost.new), total_cost.new, total_cost)) %>%
  select(-total_cost.new)|> 
  mutate(plan = "Scenario 3", 
         scenario = "scenario 3")|> 
  left_join(intervention_counts)|> 
  select(-intervention) 

scenario4_new <- 
  scenario4|> 
  mutate(scenario = str_to_title(scenario)) |> 
  left_join(plan_comparison_costs |> filter(plan == "Scenario 4"),
            by=c("scenario"= "plan", "title", "currency"), 
            suffix = c("", ".new")) |> 
  mutate(total_cost = ifelse(!is.na(total_cost.new), total_cost.new, total_cost)) %>%
  select(-total_cost.new)|> 
  mutate(plan = "Scenario 4", 
         scenario = "scenario 4")|> 
  left_join(intervention_counts) |> 
  select(-intervention) 


amen_scenarios <- 
  bind_rows(scenario1b_new, 
            scenario2_new, 
            scenario3_new, 
            scenario4_new) |> 
  mutate(intervention_type = case_when(
    title %in% c("Capacity Building",
                 "Entomological surveillance",
                 "Governance & Coordination", 
                 "Monitoring & Evaluation", 
                 "Resource Mobilisation",
                 "Social Behaviour Change") ~ "Support Services", 
    TRUE ~ "Malaria Interventions"
  )) |> 
  select(scenario, plan, title, state_count, lga_count, total_cost, currency, intervention_type)

write.csv(amen_scenarios, "budget-viz-tool/working-data/amen-new-scenarios-cost-data.csv")
