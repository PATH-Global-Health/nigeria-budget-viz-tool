# packages 
library("tidyverse")

#---------------------------------------------------------------------------------------
# Formatting data extracted from the costing spreadsheet to be used in the dashboard 
#   Create: 
#         National, State and LGA Total Cost tables  
#         National, State breakdown of interventions targeted and total costs 
#  This is the data that is presented in the National, State and LGA Summary Tabs 
#  but this data will change now with the same methods as I use for the plan comparison 
#  page but keeping for reference.  
# ----------------------------------------------------------------------------------------
        
#-read in all codable data----------------------------------------------------------------------------------------------------------------
national_data_extract <- read.csv("exploratory-steps/data/working-data/codable-national-data-october.csv")
state_data_extract    <- read.csv("exploratory-steps/data/working-data/codable-state-data-october.csv")
lga_data_extract      <- read.csv("exploratory-steps/data/working-data/codable-lga-data-october.csv")|> 
  # add state names to LGA level data 
  left_join(
    state_data_extract |>  select(spatial_level_1, spatial_level_2, state), 
    by=c("spatial_level_1", "spatial_level_2")
  ) |> 
  relocate(state, .before = lga) |> 
  filter(lga != "Disputed Area in Taraba State")

# format LGA names to match with shapefile 
lga_data_extract$lga <- gsub("-", " ", lga_data_extract$lga) #remove all "-"
lga_data_extract$lga <- gsub('[[:digit:]]+', '', lga_data_extract$lga) #remove numeric values from names
lga_data_extract$lga <- stringr::str_to_title(lga_data_extract$lga)    #make all names sentence case


#-First extract total total costs in USD and Naira for each spatial level-----------------------------------------------------------------

# National 
national_data_total_cost <- 
  national_data_extract |> 
  select(pop_2025_projected, cost_total_ngn, cost_total_usd) |> 
  pivot_longer(cols=starts_with("cost"), 
               names_to = "currency", 
               values_to = "full_cost") |> 
  mutate(currency = case_when(currency == "cost_total_ngn" ~ "Naira", 
                              TRUE ~ "USD")) |> 
  mutate(cost_per_person = full_cost/pop_2025_projected) 

write.csv(national_data_total_cost, "budget-viz-tool/working-data/national_data_total_cost.csv", row.names = FALSE)

# State 
state_data_total_cost <- 
  state_data_extract |> 
  select(state, pop_2025_projected, cost_total_ngn, cost_total_usd) |> 
  pivot_longer(cols=starts_with("cost"), 
               names_to = "currency", 
               values_to = "full_cost") |> 
  mutate(currency = case_when(currency == "cost_total_ngn" ~ "Naira", 
                              TRUE ~ "USD")) |> 
  mutate(cost_per_person = full_cost/pop_2025_projected)

write.csv(state_data_total_cost, "budget-viz-tool/working-data/state_data_total_cost.csv", row.names = FALSE)

# LGA 
lga_data_total_cost <- 
  lga_data_extract |> 
  select(state, lga, pop_2025_projected, cost_total_ngn, cost_total_usd) |> 
  pivot_longer(cols=starts_with("cost"), 
               names_to = "currency", 
               values_to = "full_cost") |> 
  mutate(currency = case_when(currency == "cost_total_ngn" ~ "Naira", 
                              TRUE ~ "USD")) |> 
  mutate(cost_per_person = full_cost/pop_2025_projected)

write.csv(lga_data_total_cost, "budget-viz-tool/working-data/lga_data_total_cost.csv", row.names = FALSE)


#-Intervention Mix data------------------------------------------------------------------------------------------------------------------
# The intervention mix column hasn't been changing with the changes in 
# the discussion on intervention mixes 
# so far: IRS has been removed 
# SMC has been amended to included Adamawa and Jigawa 
# PMC has remained
# Vaccine included in all areas? or just the 27 LGAs that were initially targeted

# lsm also isn't costed at the LGA level and only at the state level so 
# need to mutate the data to add the state level ID of LSM to the LGA level 
state_data_extract |> 
  select(state, contains("lsm")) |> 
  mutate(code_lsm = 1) |> # there's cost values for all states !! 
  select(state, code_lsm) 

intervention_mix <- 
  lga_data_extract |> 
  select(spatial_level_1:lga, 
         starts_with("code"),
         contains("eligable") , 
         -itn_routine_net_quantity_eligable,
         itn_campaign_net_quantity) |> 
  # recode intervention codes to reflect changes and create new mix col
  mutate(code_case_management = "CM", #everywhere case management
         code_smc = case_when(smc_eligable == 1 ~ "SMC", TRUE ~ NA),  #smc_eligable is updated 
         code_pmc = case_when(pmc_eligable == 1 ~ "PMC", TRUE ~ NA),  #include for the potential that it is 
         code_iptp = case_when(code_iptp == "X" ~ "IPTp", TRUE ~ NA), 
         code_vaccine_all = "Vaccine", 
         code_vaccine_limited = case_when(grepl("vaccine", code_intervention_mix) ~ "Vaccine", TRUE ~ NA), #setting this code for the 27 expected 
         code_irs = NA, # been removed from interest  ??
         code_lsm = "LSM", #there is a cost for all states not at the LGA level but in all states??
         code_itn_type = case_when(!is.na(itn_campaign_net_quantity) ~ "ITN Campaign", TRUE ~ "ITN Routine"),  
         code_itn_urban = case_when(code_urban_nets == "X" ~ "Urban", TRUE ~ NA)
  ) |> 
  # select correct formulation 
  select(state, lga, 
         code_case_management, code_smc, 
         code_pmc, code_iptp, 
         code_vaccine_all, code_vaccine_limited, code_irs, 
         code_lsm, code_itn_type, code_itn_urban)  |> 
  rowwise() %>%
  mutate(
    # Combine ITN type and ITN urban with "-" if both are present
    itn_combined = ifelse(
      !is.na(code_itn_type) & !is.na(code_itn_urban),
      paste(code_itn_type, code_itn_urban, sep = "-"),
      coalesce(code_itn_type, code_itn_urban)  # Use non-NA value if only one is present
    ),
    # Create the intervention summary - Including PMC and all Vaccine
    intervention_summary = paste(
      na.omit(c(
        code_case_management, 
        code_iptp,
        itn_combined,  # Include the combined ITN column
        code_smc, 
        code_pmc, 
        code_vaccine_all, 
        code_irs, 
        code_lsm
      )),
      collapse = " + "
    )) |> 
  ungroup() 

write.csv(intervention_mix, "budget-viz-tool/working-data/intervention_mix.csv", row.names = FALSE)


#-Prevalence data at the state level mapped to the LGA level for prevalence mapping--------------------------------------------------------------

# 2018 data from costing spreadsheet malaria prev according to RDT 
prevalence_data <- 
  state_data_extract |> 
  select(state, prev_u5_state) |> 
  mutate(year = 2018)

# 2021 data from Monique via MIS and selecting prev according to RDt too  
mis_2021_prev_data <- 
  read.csv("data/state-prev-results-2021.csv") |> 
  select(state = region, prev_u5_state = rdt_rate) |> 
  mutate(year = 2021, 
         state = str_to_title(state), 
         prev_u5_state = prev_u5_state*100, 
         state = case_when(state == "Fct" ~ "Federal Capital Territory", 
                           state == "Croriver" ~ "Cross River",
                           TRUE ~ state)) 

# join together 
prevalence_data <- bind_rows(prevalence_data, mis_2021_prev_data)

write.csv(prevalence_data, "budget-viz-tool/working-data/prevalence_data.csv", row.names = FALSE)


# Ribbon Summary data----------------------------------------------------------------------------------------------------------------
# dataset for the icons on the ribbon levels  
national_ribbon_data <- 
  national_data_extract |> 
  select(pop_2025_projected, pop_number_children_u5, pop_number_pw, 
         admin_number_states, admin_number_lgas, admin_number_wards) |> 
  crossing(national_data_total_cost |> select(-pop_2025_projected))

write.csv(national_ribbon_data, "budget-viz-tool/working-data/national_ribbon_data.csv", row.names = FALSE)


state_ribbon_data <- 
  state_data_extract |> 
  select(state, 
         pop_2025_projected, pop_number_children_u5, pop_number_pw, 
         admin_number_states, admin_number_lgas, admin_number_wards) |> 
  left_join(state_data_total_cost |> select(-pop_2025_projected), multiple = "all")

write.csv(state_ribbon_data, "budget-viz-tool/working-data/state_ribbon_data.csv", row.names = FALSE)


lga_ribbon_data <- 
  lga_data_extract |> 
  select(state, lga,
         pop_2025_projected, pop_number_children_u5, pop_number_pw, 
         admin_number_states, admin_number_lgas, admin_number_wards) |> 
  left_join(lga_data_total_cost |> select(-pop_2025_projected),
            by=c("state", "lga") ,multiple = "all")

write.csv(lga_ribbon_data, "budget-viz-tool/working-data/lga_ribbon_data.csv", row.names = FALSE)



#-Total cost summary table data---------------------------------------------------------------------------------------------------------------------

# summaries of interventions delivered to number of states and LGAs 
# LGA 
intervention_counts_lga <- 
  lga_data_extract |> 
  select(state, lga, 
         itn_campaign_total_cost,
         itn_routine_total_cost, 
         smc_total_cost, 
         code_lsm, 
         pmc_total_cost, 
         iptp_total_cost, 
         vacc_total_cost, 
         cm_public_total_cost, 
         cm_private_total_cost) |>
  mutate(code_lsm = 1) |> 
  pivot_longer(
    cols=c(-state, -lga),
    names_to = "intervention",
    values_to = "total_cost"
  ) |> 
  mutate(intervention = str_remove(intervention, "_total_cost")) |> 
  #change 0 to NA 
  mutate(total_cost = case_when(total_cost == 0 ~ NA, TRUE ~ total_cost)) |> 
  group_by(intervention) |> 
  summarise(lga_count = sum(!is.na(total_cost) & total_cost > 0)) |> 
  mutate(title = case_when(intervention == "itn_campaign" ~ "ITN Campaign", 
                           intervention == "itn_routine" ~ "ITN Routine Distribution", 
                           intervention == "irs" ~ "IRS",
                           intervention == "code_lsm" ~ "LSM", 
                           intervention == "smc" ~ "SMC", 
                           intervention == "pmc" ~ "PMC", 
                           intervention == "iptp" ~ "IPTp", 
                           intervention == "vacc" ~ "Malaria Vaccine", 
                           intervention == "cm_public" ~ "Public Sector Case Management", 
                           intervention == "cm_private" ~ "Private Sector Case Management" )) 

# State level
intervention_counts_state <- 
  lga_data_extract |> 
  select(state,  
         itn_campaign_total_cost,
         itn_routine_total_cost, 
         smc_total_cost, 
         lsm_total_cost, 
         pmc_total_cost, 
         iptp_total_cost, 
         vacc_total_cost, 
         cm_public_total_cost, 
         cm_private_total_cost) |>
  pivot_longer(
    cols=c(-state),
    names_to = "intervention",
    values_to = "total_cost"
  ) |> 
  group_by(state, intervention) |> 
  summarise(total_cost = sum(total_cost, na.rm=TRUE)) |> 
  mutate(intervention = str_remove(intervention, "_total_cost")) |> 
  group_by(intervention) |> 
  summarise(state_count = sum(!is.na(total_cost) & total_cost > 0)) |> 
  mutate(state_count = case_when(intervention == "lsm" ~ 37, TRUE ~ state_count))|> 
  mutate(title = case_when(intervention == "itn_campaign" ~ "ITN Campaign", 
                           intervention == "itn_routine" ~ "ITN Routine Distribution", 
                           intervention == "irs" ~ "IRS",
                           intervention == "lsm" ~ "LSM", 
                           intervention == "smc" ~ "SMC", 
                           intervention == "pmc" ~ "PMC", 
                           intervention == "iptp" ~ "IPTp", 
                           intervention == "vacc" ~ "Malaria Vaccine", 
                           intervention == "cm_public" ~ "Public Sector Case Management", 
                           intervention == "cm_private" ~ "Private Sector Case Management")) 


# this is intervention and support services total costs 
exchange_rate <- 1600 #keeping as in the spreadsheet

# extracting from the spreadsheet 
national_total_cost_summary <-  
  national_data_extract |> 
  select(ends_with("total_cost")) |> 
  select(-cm_rdt_kits_total_cost, -cm_al_total_cost, -cm_iv_artesunate_total_cost, -cm_ras_total_cost) |>  
  pivot_longer(
    cols=everything(),
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
                           intervention == "cm_public" ~ "Public Sector Case Management", 
                           intervention == "cm_private" ~ "Private Sector Case Management", 
                           intervention == "ss_sbc" ~ "Social Behaviour Change", 
                           intervention == "me" ~ "Monitoring & Evaluation", 
                           intervention == "cb" ~ "Capacity Building", 
                           intervention == "gc" ~ "Governance & Coordination", 
                           intervention == "rm" ~ "Resource Mobilisation")) |> 
  crossing(currency = c("Naira", "USD")) |> 
  mutate(total_cost = case_when(currency == "USD" ~ total_cost /1600, 
                                TRUE ~ total_cost)) |> 
  # add summary counts
  left_join(intervention_counts_lga) |> 
  left_join(intervention_counts_state) |> 
  select(intervention, title, state_count, lga_count, currency, total_cost) |> 
  mutate(intervention_type = case_when(
    intervention %in% c("ss_sbc","me","cb", "gc", "rm") ~ "Support Services", 
    TRUE ~ "Malaria Interventions"
  )) 


# adding target population data 
# ITN campaign - pop_2025_projected 
# ITN routine - pop_number_pw_children_u5
# IRS - NA
# LSM - admin_landmass_2perc_coverage_hectares
# SMC - pop_number_children_u5 * 0.95 
# PMC - pop_number_children_0_2_yrs * 0.85
# IPTp - pop_number_pw 
# Malaria Vaccine - pop_number_children_5_36_mons
# Case Management - target population is total pop at risk 
national_target_population <- 
  national_data_extract |> 
  select(itn_campaign_target_pop = pop_2025_projected, 
         itn_routine_target_pop = pop_number_pw_children_u5, 
         lsm_target_area = admin_landmass_2perc_coverage_hectares, 
         smc_target_pop = pop_number_children_u5,  
         pmc_target_pop = pop_number_children_0_2_yrs, 
         iptp_target_pop = pop_number_pw,
         vacc_target_pop = pop_number_children_5_36_mons, 
         cm_public_target_pop = pop_2025_projected, 
         cm_private_target_pop = pop_2025_projected) |> 
  mutate(smc_target_pop = smc_target_pop * 0.95, 
         pmc_target_pop = pmc_target_pop * 0.85) |> 
  pivot_longer(
    cols = everything(),  # Pivot all columns
    names_to = "intervention",     # New column for intervention names
    values_to = "target_value",  # New column for target population values
    names_pattern = "(.*)_target.*"  # Extract the part before "_target"
  ) |> 
  mutate(target = case_when(intervention == "lsm" ~ "Area (Hectares)", 
                            TRUE ~ "Population")) |> 
  mutate(title = case_when(intervention == "itn_campaign" ~ "ITN Campaign", 
                           intervention == "itn_routine" ~ "ITN Routine Distribution", 
                           intervention == "irs" ~ "IRS",
                           intervention == "lsm" ~ "LSM", 
                           intervention == "smc" ~ "SMC", 
                           intervention == "pmc" ~ "PMC", 
                           intervention == "iptp" ~ "IPTp", 
                           intervention == "vacc" ~ "Malaria Vaccine", 
                           intervention == "cm_public" ~ "Public Sector Case Management", 
                           intervention == "cm_private" ~ "Private Sector Case Management", 
                           intervention == "ss_sbc" ~ "Social Behaviour Change", 
                           intervention == "me" ~ "Monitoring & Evaluation", 
                           intervention == "cb" ~ "Capacity Building", 
                           intervention == "gc" ~ "Governance & Coordination", 
                           intervention == "rm" ~ "Resource Mobilisation")) 


national_total_cost_summary <- 
  national_total_cost_summary |> 
  left_join(national_target_population) |> 
  mutate(cost_per_target = total_cost / target_value)

write.csv(national_total_cost_summary, "budget-viz-tool/working-data/national_total_cost_summary.csv", row.names = FALSE)

# State level total cost data for table 
state_total_cost_summary <- 
  state_data_extract |> 
  select(state, ends_with("total_cost")) |> 
  select(-cm_rdt_kits_total_cost, -cm_al_total_cost, -cm_iv_artesunate_total_cost, -cm_ras_total_cost) |>  
  pivot_longer(
    cols=-state,
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
                           intervention == "cm_public" ~ "Public Sector Case Management", 
                           intervention == "cm_private" ~ "Private Sector Case Management", 
                           intervention == "ss_sbc" ~ "Social Behaviour Change", 
                           intervention == "me" ~ "Monitoring & Evaluation", 
                           intervention == "cb" ~ "Capacity Building", 
                           intervention == "gc" ~ "Governance & Coordination", 
                           intervention == "rm" ~ "Resource Mobilisation")) |> 
  crossing(currency = c("Naira", "USD")) |> 
  mutate(total_cost = case_when(currency == "USD" ~ total_cost /1600, 
                                TRUE ~ total_cost)) |> 
  left_join(  lga_data_extract |> 
                select(state, lga, 
                       itn_campaign_total_cost,
                       itn_routine_total_cost, 
                       smc_total_cost, 
                       lsm_total_cost, 
                       pmc_total_cost, 
                       iptp_total_cost, 
                       vacc_total_cost, 
                       cm_public_total_cost, 
                       cm_private_total_cost) |>
                pivot_longer(
                  cols=c(-state, -lga),
                  names_to = "intervention",
                  values_to = "total_cost"
                ) |> 
                group_by(state, lga, intervention) |> 
                summarise(total_cost = sum(total_cost, na.rm=TRUE)) |> 
                mutate(intervention = str_remove(intervention, "_total_cost")) |> 
                group_by(state, intervention) |> 
                summarise(lga_count = sum(!is.na(total_cost) & total_cost > 0)) |> 
                mutate(title = case_when(intervention == "itn_campaign" ~ "ITN Campaign", 
                                         intervention == "itn_routine" ~ "ITN Routine Distribution", 
                                         intervention == "irs" ~ "IRS",
                                         intervention == "lsm" ~ "LSM", 
                                         intervention == "smc" ~ "SMC", 
                                         intervention == "pmc" ~ "PMC", 
                                         intervention == "iptp" ~ "IPTp", 
                                         intervention == "vacc" ~ "Malaria Vaccine", 
                                         intervention == "cm_public" ~ "Public Sector Case Management", 
                                         intervention == "cm_private" ~ "Private Sector Case Management")) )|> 
  select(state, intervention, title, lga_count, currency, total_cost) |> 
  mutate(intervention_type = case_when(
    intervention %in% c("ss_sbc","me","cb", "gc", "rm") ~ "Support Services", 
    TRUE ~ "Malaria Interventions"
  )) 

# adding target population data 
# ITN campaign - pop_2025_projected 
# ITN routine - pop_number_pw_children_u5
# IRS - NA
# LSM - admin_landmass_2perc_coverage_hectares
# SMC - pop_number_children_u5 * 0.95 
# PMC - pop_number_children_0_2_yrs * 0.85
# IPTp - pop_number_pw 
# Malaria Vaccine - pop_number_children_5_36_mons
# Case Management - target population is total pop at risk 
state_target_population <- 
  state_data_extract |> 
  select(state, 
         itn_campaign_target_pop = pop_2025_projected, 
         itn_routine_target_pop = pop_number_pw_children_u5, 
         lsm_target_area = admin_landmass_2perc_coverage_hectares, 
         smc_target_pop = pop_number_children_u5,  
         pmc_target_pop = pop_number_children_0_2_yrs, 
         iptp_target_pop = pop_number_pw,
         vacc_target_pop = pop_number_children_5_36_mons, 
         cm_public_target_pop = pop_2025_projected, 
         cm_private_target_pop = pop_2025_projected) |> 
  mutate(smc_target_pop = smc_target_pop * 0.95, 
         pmc_target_pop = pmc_target_pop * 0.85) |> 
  pivot_longer(
    cols = -state,  # Pivot all columns
    names_to = "intervention",     # New column for intervention names
    values_to = "target_value",  # New column for target population values
    names_pattern = "(.*)_target.*"  # Extract the part before "_target"
  ) |> 
  mutate(target = case_when(intervention == "lsm" ~ "Area (Hectares)", 
                            TRUE ~ "Population")) |> 
  mutate(title = case_when(intervention == "itn_campaign" ~ "ITN Campaign", 
                           intervention == "itn_routine" ~ "ITN Routine Distribution", 
                           intervention == "irs" ~ "IRS",
                           intervention == "lsm" ~ "LSM", 
                           intervention == "smc" ~ "SMC", 
                           intervention == "pmc" ~ "PMC", 
                           intervention == "iptp" ~ "IPTp", 
                           intervention == "vacc" ~ "Malaria Vaccine", 
                           intervention == "cm_public" ~ "Public Sector Case Management", 
                           intervention == "cm_private" ~ "Private Sector Case Management", 
                           intervention == "ss_sbc" ~ "Social Behaviour Change", 
                           intervention == "me" ~ "Monitoring & Evaluation", 
                           intervention == "cb" ~ "Capacity Building", 
                           intervention == "gc" ~ "Governance & Coordination", 
                           intervention == "rm" ~ "Resource Mobilisation")) 

state_total_cost_summary <- 
  state_total_cost_summary |> 
  left_join(state_target_population) |> 
  mutate(cost_per_target = total_cost / target_value)

write.csv(state_total_cost_summary, "budget-viz-tool/working-data/state_total_cost_summary.csv", row.names = FALSE)


# LGA level total costs for table
lga_total_cost_summary <- 
  lga_data_extract |> 
  select(state, lga, ends_with("total_cost")) |> 
  select(-cm_rdt_kits_total_cost, -cm_al_total_cost, -cm_iv_artesunate_total_cost, -cm_ras_total_cost) |>  
  pivot_longer(
    cols=c(-state, -lga),
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
                           intervention == "cm_public" ~ "Public Sector Case Management", 
                           intervention == "cm_private" ~ "Private Sector Case Management", 
                           intervention == "ss_sbc" ~ "Social Behaviour Change", 
                           intervention == "me" ~ "Monitoring & Evaluation", 
                           intervention == "cb" ~ "Capacity Building", 
                           intervention == "gc" ~ "Governance & Coordination", 
                           intervention == "rm" ~ "Resource Mobilisation")) |> 
  crossing(currency = c("Naira", "USD")) |> 
  mutate(total_cost = case_when(currency == "USD" ~ total_cost /1600, 
                                TRUE ~ total_cost)) |> 
  mutate(intervention_type = case_when(
    intervention %in% c("ss_sbc","me","cb", "gc", "rm") ~ "Support Services", 
    TRUE ~ "Malaria Interventions"
  )) 

# adding target population data 
# ITN campaign - pop_2025_projected 
# ITN routine - pop_number_pw_children_u5
# IRS - NA
# LSM - admin_landmass_2perc_coverage_hectares
# SMC - pop_number_children_u5 * 0.95 
# PMC - pop_number_children_0_2_yrs * 0.85
# IPTp - pop_number_pw 
# Malaria Vaccine - pop_number_children_5_36_mons
# Case Management - target population is total pop at risk 
lga_target_population <- 
  lga_data_extract |> 
  select(state, lga,
         itn_campaign_target_pop = pop_2025_projected, 
         itn_routine_target_pop = pop_number_pw_children_u5, 
         lsm_target_area = admin_landmass_2perc_coverage_hectares, 
         smc_target_pop = pop_number_children_u5,  
         pmc_target_pop = pop_number_children_0_2_yrs, 
         iptp_target_pop = pop_number_pw,
         vacc_target_pop = pop_number_children_5_36_mons, 
         cm_public_target_pop = pop_2025_projected, 
         cm_private_target_pop = pop_2025_projected) |> 
  mutate(smc_target_pop = smc_target_pop * 0.95, 
         pmc_target_pop = pmc_target_pop * 0.85) |> 
  pivot_longer(
    cols = c(-state,-lga),  # Pivot all columns
    names_to = "intervention",     # New column for intervention names
    values_to = "target_value",  # New column for target population values
    names_pattern = "(.*)_target.*"  # Extract the part before "_target"
  ) |> 
  mutate(target = case_when(intervention == "lsm" ~ "Area (Hectares)", 
                            TRUE ~ "Population")) |> 
  mutate(title = case_when(intervention == "itn_campaign" ~ "ITN Campaign", 
                           intervention == "itn_routine" ~ "ITN Routine Distribution", 
                           intervention == "irs" ~ "IRS",
                           intervention == "lsm" ~ "LSM", 
                           intervention == "smc" ~ "SMC", 
                           intervention == "pmc" ~ "PMC", 
                           intervention == "iptp" ~ "IPTp", 
                           intervention == "vacc" ~ "Malaria Vaccine", 
                           intervention == "cm_public" ~ "Public Sector Case Management", 
                           intervention == "cm_private" ~ "Private Sector Case Management", 
                           intervention == "ss_sbc" ~ "Social Behaviour Change", 
                           intervention == "me" ~ "Monitoring & Evaluation", 
                           intervention == "cb" ~ "Capacity Building", 
                           intervention == "gc" ~ "Governance & Coordination", 
                           intervention == "rm" ~ "Resource Mobilisation")) 

lga_total_cost_summary <- 
  lga_total_cost_summary |> 
  left_join(lga_target_population) |> 
  mutate(cost_per_target = total_cost / target_value)

write.csv(lga_total_cost_summary, "budget-viz-tool/working-data/lga_total_cost_summary.csv", row.names = FALSE)

#-Donut chart takes these values as does the tree map-------------------------------------------------------

#-Data for intervention plots-------------------------------------------------------------------------------

# National level data
# want to add in the urban / non-urban nets into this distinction  
net_summary_costs_national <- 
  intervention_mix |> 
  select(state, lga, intervention = itn_combined) |> 
  left_join(lga_data_extract |> 
              select(state, lga,
                     itn_campaign_net_procurement_cost, 
                     itn_campaign_net_distribution_procurement_cost, 
                     itn_campaign_campaign_cost, 
                     itn_routine_net_procurement_cost,
                     itn_routine_net_operational_cost)) |> 
  group_by(intervention) |> 
  summarise_if(is.numeric, sum, na.rm = TRUE) |> 
  ungroup() |> 
  pivot_longer(
    cols = -intervention,
    names_to = c("full_name"),
    values_to = "value"
  ) |> 
  filter(value != 0)

# select out all the intervention specific costs of procurement and implemenation 
national_intervention_chart_data <- 
  national_data_extract |> 
  select(country,contains("cost")) |> 
  select(-starts_with("cost_total"), 
         -irs_total_cost, 
         -contains("total_cost"), 
         -smc_spaq_3_11_months_procurement_cost, 
         -smc_spaq_12_59_months_procurement_cost, 
         -starts_with("itn")
  ) |> 
  bind_cols(national_data_extract |> select(cm_private_total_cost, itn_campaign_storage_hardware_cost)) |> 
  pivot_longer(
    cols = -country,
    names_to = c("full_name"),
    values_to = "value"
  ) |> 
  bind_rows(net_summary_costs_national) |> 
  mutate(intervention = case_when(grepl("irs", full_name) ~ "IRS",
                                  grepl("lsm", full_name) ~ "LSM", 
                                  grepl("smc", full_name) ~ "SMC", 
                                  grepl("pmc", full_name) ~ "PMC", 
                                  grepl("iptp", full_name) ~ "IPTp", 
                                  grepl("vacc", full_name) ~ "Malaria Vaccine", 
                                  full_name == "cm_private_total_cost" ~ "Private Sector Case Management",
                                  grepl("cm", full_name) ~ "Public Sector Case Management", 
                                  grepl("ss_sbc", full_name) ~ "Social Behaviour Change", 
                                  grepl("me_", full_name) ~ "Monitoring & Evaluation", 
                                  grepl("cb", full_name) ~ "Capacity Building", 
                                  grepl("gc", full_name) ~ "Governance & Coordination", 
                                  grepl("rm", full_name) ~ "Resource Mobilisation", 
                                  full_name == "itn_campaign_storage_hardware_cost" ~ "ITN Campaign Net Storage",
                                  TRUE ~ intervention), 
         full_name = str_replace_all(full_name, "_", " "), 
         full_name = str_to_title(full_name),
         full_name = str_replace(full_name, "Itn", "ITN"), 
         full_name = str_replace(full_name, "Lsm", "LSM"), 
         full_name = str_replace(full_name, "Smc", "SMC"), 
         full_name = str_replace(full_name, "Spaq", "SPAQ"),
         full_name = str_replace(full_name, "Pmc", "PMC"), 
         full_name = str_replace(full_name, "Sp", "SP"), 
         full_name = str_replace(full_name, "Iptp", "IPTp"), 
         full_name = str_replace(full_name, "Vacc", "Malaria Vaccine"), 
         full_name = str_replace(full_name, "Cm", "CM"), 
         full_name = str_replace(full_name, "Rdt", "RDT"), 
         full_name = str_replace(full_name, "Al", "AL"), 
         full_name = str_replace(full_name, "Iv", "IV"), 
         full_name = str_replace(full_name, "Ras", "RAS"), 
         full_name = str_replace(full_name, "Eqa", "EQA"), 
         full_name = str_replace(full_name, "Me", "M&E"), 
         full_name = str_replace(full_name, "Cb", "CB"), 
         full_name = str_replace(full_name, "Shf", "Secondary Health Facility"), 
         full_name = str_replace(full_name, "Thf", "Tertiary Health Facility"), 
         full_name = str_replace(full_name, "Phc", "Primary Health Center"), 
         full_name = str_replace(full_name, "Lga", "LGA"), 
         full_name = str_replace(full_name, "Gc", "GC"), 
         full_name = str_remove(full_name, " Cost"), 
         full_name = str_remove(full_name, "Total")) |> 
  crossing(currency = c("Naira", "USD")) |> 
  mutate(value = case_when(currency == "USD" ~ value /exchange_rate, 
                           TRUE ~ value))

write.csv(national_intervention_chart_data, "budget-viz-tool/working-data/national_intervention_chart_data.csv", row.names = FALSE)

# State level summary 
net_summary_costs_state <- 
  intervention_mix |> 
  select(state, lga, intervention = itn_combined) |> 
  left_join(lga_data_extract |> 
              select(state, lga,
                     itn_campaign_net_procurement_cost, 
                     itn_campaign_net_distribution_procurement_cost, 
                     itn_campaign_campaign_cost, 
                     itn_routine_net_procurement_cost,
                     itn_routine_net_operational_cost)) |> 
  group_by(state, intervention) |> 
  summarise_if(is.numeric, sum, na.rm = TRUE) |> 
  ungroup() |> 
  pivot_longer(
    cols = c(-intervention, -state),
    names_to = c("full_name"),
    values_to = "value"
  ) |> 
  filter(value != 0)

# select out all the intervention specific costs of procurement and implemenation 
state_intervention_chart_data <- 
  state_data_extract |> 
  select(state,contains("cost")) |> 
  select(-starts_with("cost_total"), 
         -irs_total_cost, 
         -contains("total_cost"), 
         -smc_spaq_3_11_months_procurement_cost, 
         -smc_spaq_12_59_months_procurement_cost, 
         -starts_with("itn")
  ) |> 
  left_join(state_data_extract |> select(state, cm_private_total_cost)) |> 
  pivot_longer(
    cols = -state,
    names_to = c("full_name"),
    values_to = "value"
  ) |> 
  bind_rows(net_summary_costs_state) |> 
  mutate(intervention = case_when(grepl("irs", full_name) ~ "IRS",
                                  grepl("lsm", full_name) ~ "LSM", 
                                  grepl("smc", full_name) ~ "SMC", 
                                  grepl("pmc", full_name) ~ "PMC", 
                                  grepl("iptp", full_name) ~ "IPTp", 
                                  grepl("vacc", full_name) ~ "Malaria Vaccine", 
                                  full_name == "cm_private_total_cost" ~ "Private Sector Case Management",
                                  grepl("cm", full_name) ~ "Public Sector Case Management", 
                                  grepl("ss_sbc", full_name) ~ "Social Behaviour Change", 
                                  grepl("me_", full_name) ~ "Monitoring & Evaluation", 
                                  grepl("cb", full_name) ~ "Capacity Building", 
                                  grepl("gc", full_name) ~ "Governance & Coordination", 
                                  grepl("rm", full_name) ~ "Resource Mobilisation", 
                                  full_name == "itn_campaign_storage_hardware_cost" ~ "ITN Campaign Net Storage",
                                  TRUE ~ intervention), 
         full_name = str_replace_all(full_name, "_", " "), 
         full_name = str_to_title(full_name),
         full_name = str_replace(full_name, "Itn", "ITN"), 
         full_name = str_replace(full_name, "Lsm", "LSM"), 
         full_name = str_replace(full_name, "Smc", "SMC"), 
         full_name = str_replace(full_name, "Spaq", "SPAQ"),
         full_name = str_replace(full_name, "Pmc", "PMC"), 
         full_name = str_replace(full_name, "Sp", "SP"), 
         full_name = str_replace(full_name, "Iptp", "IPTp"), 
         full_name = str_replace(full_name, "Vacc", "Malaria Vaccine"), 
         full_name = str_replace(full_name, "Cm", "CM"), 
         full_name = str_replace(full_name, "Rdt", "RDT"), 
         full_name = str_replace(full_name, "Al", "AL"), 
         full_name = str_replace(full_name, "Iv", "IV"), 
         full_name = str_replace(full_name, "Ras", "RAS"), 
         full_name = str_replace(full_name, "Eqa", "EQA"), 
         full_name = str_replace(full_name, "Me", "M&E"), 
         full_name = str_replace(full_name, "Cb", "CB"), 
         full_name = str_replace(full_name, "Shf", "Secondary Health Facility"), 
         full_name = str_replace(full_name, "Thf", "Tertiary Health Facility"), 
         full_name = str_replace(full_name, "Phc", "Primary Health Center"), 
         full_name = str_replace(full_name, "Lga", "LGA"), 
         full_name = str_replace(full_name, "Gc", "GC"), 
         full_name = str_remove(full_name, " Cost"), 
         full_name = str_remove(full_name, "Total")) |> 
  crossing(currency = c("Naira", "USD")) |> 
  mutate(value = case_when(currency == "USD" ~ value /exchange_rate, 
                           TRUE ~ value)) |> 
  filter(!is.na(value))

write.csv(state_intervention_chart_data, "budget-viz-tool/working-data/state_intervention_chart_data.csv", row.names = FALSE)


# LGA level data 
net_summary_costs_lga <- 
  intervention_mix |> 
  select(state, lga, intervention = itn_combined) |> 
  left_join(lga_data_extract |> 
              select(state, lga,
                     itn_campaign_net_procurement_cost, 
                     itn_campaign_net_distribution_procurement_cost, 
                     itn_campaign_campaign_cost, 
                     itn_routine_net_procurement_cost,
                     itn_routine_net_operational_cost)) |> 
  group_by(state, lga, intervention) |> 
  summarise_if(is.numeric, sum, na.rm = TRUE) |> 
  ungroup() |> 
  pivot_longer(
    cols = c(-intervention, -state, -lga),
    names_to = c("full_name"),
    values_to = "value"
  ) |> 
  filter(value != 0)


lga_intervention_chart_data <- 
  lga_data_extract |> 
  select(state, lga, contains("cost")) |> 
  select(-starts_with("cost_total"), 
         -irs_total_cost, 
         -contains("total_cost"), 
         -smc_spaq_3_11_months_procurement_cost, 
         -smc_spaq_12_59_months_procurement_cost, 
         -starts_with("itn")
  ) |> 
  left_join(lga_data_extract |> select(state, lga, cm_private_total_cost)) |> 
  pivot_longer(
    cols = c(-state,-lga),
    names_to = c("full_name"),
    values_to = "value"
  ) |> 
  bind_rows(net_summary_costs_lga) |> 
  mutate(intervention = case_when(grepl("irs", full_name) ~ "IRS",
                                  grepl("lsm", full_name) ~ "LSM", 
                                  grepl("smc", full_name) ~ "SMC", 
                                  grepl("pmc", full_name) ~ "PMC", 
                                  grepl("iptp", full_name) ~ "IPTp", 
                                  grepl("vacc", full_name) ~ "Malaria Vaccine", 
                                  full_name == "cm_private_total_cost" ~ "Private Sector Case Management",
                                  grepl("cm", full_name) ~ "Public Sector Case Management", 
                                  grepl("ss_sbc", full_name) ~ "Social Behaviour Change", 
                                  grepl("me_", full_name) ~ "Monitoring & Evaluation", 
                                  grepl("cb", full_name) ~ "Capacity Building", 
                                  grepl("gc", full_name) ~ "Governance & Coordination", 
                                  grepl("rm", full_name) ~ "Resource Mobilisation", 
                                  full_name == "itn_campaign_storage_hardware_cost" ~ "ITN Campaign Net Storage",
                                  TRUE ~ intervention), 
         full_name = str_replace_all(full_name, "_", " "), 
         full_name = str_to_title(full_name),
         full_name = str_replace(full_name, "Itn", "ITN"), 
         full_name = str_replace(full_name, "Lsm", "LSM"), 
         full_name = str_replace(full_name, "Smc", "SMC"), 
         full_name = str_replace(full_name, "Spaq", "SPAQ"),
         full_name = str_replace(full_name, "Pmc", "PMC"), 
         full_name = str_replace(full_name, "Sp", "SP"), 
         full_name = str_replace(full_name, "Iptp", "IPTp"), 
         full_name = str_replace(full_name, "Vacc", "Malaria Vaccine"), 
         full_name = str_replace(full_name, "Cm", "CM"), 
         full_name = str_replace(full_name, "Rdt", "RDT"), 
         full_name = str_replace(full_name, "Al", "AL"), 
         full_name = str_replace(full_name, "Iv", "IV"), 
         full_name = str_replace(full_name, "Ras", "RAS"), 
         full_name = str_replace(full_name, "Eqa", "EQA"), 
         full_name = str_replace(full_name, "Me", "M&E"), 
         full_name = str_replace(full_name, "Cb", "CB"), 
         full_name = str_replace(full_name, "Shf", "Secondary Health Facility"), 
         full_name = str_replace(full_name, "Thf", "Tertiary Health Facility"), 
         full_name = str_replace(full_name, "Phc", "Primary Health Center"), 
         full_name = str_replace(full_name, "Lga", "LGA"), 
         full_name = str_replace(full_name, "Gc", "GC"), 
         full_name = str_remove(full_name, " Cost"), 
         full_name = str_remove(full_name, "Total")) |> 
  crossing(currency = c("Naira", "USD")) |> 
  mutate(value = case_when(currency == "USD" ~ value /exchange_rate, 
                           TRUE ~ value)) |> 
  filter(!is.na(value))

write.csv(lga_intervention_chart_data, "budget-viz-tool/working-data/lga_intervention_chart_data.csv", row.names = FALSE)
