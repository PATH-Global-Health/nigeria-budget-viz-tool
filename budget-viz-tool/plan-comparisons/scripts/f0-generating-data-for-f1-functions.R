
library(tidyverse)

# Creating a total quantification and cost spreadsheet to be able to manipulate for 
# different costing scenarios 
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

lga_data_extract$pop_2025_projected / lga_data_extract$itn_campaign_net_quantity 

sum(lga_data_extract$itn_campaign_net_quantity, na.rm=TRUE)

#-CAMPAIGN NET QUANTIFICATION---------------------------------------------------------------------------------------------------------------------------
# Nets calculated as target population / 1.8
# Net bales calculated as nets / 50  
itn_quant <- 
  lga_data_extract |> 
  select(state, lga, pop_2025_projected, 
         itn_campaign_net_quantity, 
         itn_campaign_bale_quantity) |> 
  # where data is missing quantify nets for campaigns as above 
  mutate(
    itn_campaign_net_quantity = 
      case_when(
        is.na(itn_campaign_net_quantity) ~ pop_2025_projected / 1.8, 
        TRUE ~ itn_campaign_net_quantity
        ),
    itn_campaign_bale_quantity = 
      case_when(
        is.na(itn_campaign_bale_quantity) ~ itn_campaign_net_quantity/50,
        TRUE ~ itn_campaign_bale_quantity
        )
    ) |> 
  rename(itn_campaign_target_pop = pop_2025_projected)
       
#-SMC DRUG QUANTIFICATION-------------------------------------------------------------------------------------------------------------------------------
#  SP+AQ can be procured in co-blistered packets containing one full course of SMC drugs for each cycle (1 tablet of SP and 3 tablets of AQ).   
#  Since SMC is delivered in two dosage groups: 3 to <12 months and >12 to 59 months, the number of packets for each age group needs to be quantified and 
#  then multiplied by 4, to cover every cycle.  In addition, a buffer stock between 10 -20% should be included to accommodate for loss, 
#  re-dosing and treatment of children from neighbouring locations. 

# calculation: 
# A. Total number of children under 5. 
# B. Number of children 3 to <12 months = (18% of A) 
# C. 10% buffer stock for children 3 to <12 months = (10% of B) 
# D. Total number of packets for children 3 to <12 months needed for one cycle of SMC = (B + C) 
# E. Total number of packets for children 3 to <12 months needed for one round of SMC = (4 x D) 
# F. Number of children >12 to 59 months = (77% of A) 
# G. 10% buffer stock for children >12 to 59 months = (10% of F)
# H. Total number of packets for children >12 to 59 months needed for one cycle of SMC = (F + G) 
# I. Total number of packets for children >12 to 59 months needed for one round of SMC = (4 x H) 
smc_monthly_rounds <- 4    #smc given over 4 months  
smc_pop_prop_3_11 <- 0.18  # 18% of the under 5 population is 3-11 months
smc_pop_prop_12_59 <- 0.77 # 77% of the under 5 population is 12-59 months
buffer = 1.1 # includes 10% buffer 

# not fully quantified not fully costed 
smc_quantified <-  
  lga_data_extract |> 
  select(state, lga, 
         pop_number_children_u5) |> 
  mutate(smc_spaq_quantity_3_11_months = pop_number_children_u5 *  smc_pop_prop_3_11 * smc_monthly_rounds * buffer, 
         smc_sqaq_quantity_12_59_months = pop_number_children_u5 *  smc_pop_prop_12_59 * smc_monthly_rounds * buffer, 
         smc_sqaq_quantity = smc_spaq_quantity_3_11_months + smc_sqaq_quantity_12_59_months,
         smc_target_pop = pop_number_children_u5 * (smc_pop_prop_3_11 + smc_pop_prop_12_59))

# fully quantified not costed  
fully_quantified <- 
  lga_data_extract |> 
  select(state, lga, 
         pmc_sp_total_quantity, 
         pmc_target_pop = pop_number_children_0_2_yrs, 
         itn_routine_net_quantity, 
         itn_routine_target_pop = pop_number_pw_children_u5)

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
  data.frame(resource = "Entomological Surveillance-Cost per state	per state", 
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


to_cost_interventions <-  
  smc_quantified |> 
  left_join(fully_quantified) |> 
  left_join(itn_quant) |> 
  mutate(itn_campaign_net_procurement_cost = itn_campaign_net_quantity * unit_cost_data$ngn_cost[which(unit_cost_data$resource == "ITN Campaign-Procurement per ITN (Dual AI)")],
         itn_campaign_net_distribution_procurement_cost = itn_campaign_bale_quantity * unit_cost_data$ngn_cost[which(unit_cost_data$resource == "ITN Campaign-Cost of distribution from State to LGA and from LGA to DHs")], 
         itn_campaign_campaign_cost = itn_campaign_net_quantity * unit_cost_data$ngn_cost[which(unit_cost_data$resource == "ITN Campaign-Operational cost per ITN")], 
         itn_campaign_total_cost = itn_campaign_net_procurement_cost + itn_campaign_net_distribution_procurement_cost + itn_campaign_campaign_cost,
         itn_routine_net_procurement_cost = itn_routine_net_quantity * unit_cost_data$ngn_cost[which(unit_cost_data$resource == "ITN Routine Distribution-Pocurement cost per ITN (Dual AI)")], 
         itn_routine_net_operational_cost = itn_routine_net_quantity * unit_cost_data$ngn_cost[which(unit_cost_data$resource == "ITN Routine Distribution-Operational cost per ITN")], 
         itn_routine_total_cost = itn_routine_net_procurement_cost + itn_routine_net_operational_cost, 
         smc_spaq_3_11_months_procurement_cost =  smc_spaq_quantity_3_11_months * unit_cost_data$ngn_cost[which(unit_cost_data$resource == "SMC-SPAQ-3-11 months-Procurement cost per SPAQ")],  
         smc_spaq_12_59_months_procurement_cost = smc_sqaq_quantity_12_59_months * unit_cost_data$ngn_cost[which(unit_cost_data$resource == "SMC-SPAQ-12-59 months-Procurement cost per SPAQ")], 
         smc_spaq_total_procurement_cost = smc_spaq_3_11_months_procurement_cost + smc_spaq_12_59_months_procurement_cost, 
         smc_campaign_cost = smc_target_pop * unit_cost_data$ngn_cost[which(unit_cost_data$resource == "SMC-Campaign cost per child")], 
         smc_total_cost = smc_spaq_total_procurement_cost + smc_campaign_cost, 
         pmc_sp_procurement_cost = pmc_sp_total_quantity * unit_cost_data$ngn_cost[which(unit_cost_data$resource == "PMC-SP-Procurement cost")], 
         pmc_distribution_cost = pmc_target_pop * unit_cost_data$ngn_cost[which(unit_cost_data$resource == "PMC-SP-Routine Distribution cost")], 
         pmc_total_cost = pmc_sp_procurement_cost + pmc_distribution_cost)

# now draw out the quantification of other variables then 
fully_quantified_and_costed_interventions <- 
  lga_data_extract |> 
  select(state, lga, 
         vacc_total_dose_quantity, 
         vacc_procurement_cost, 
         vacc_operational_cost, 
         vacc_total_cost, 
         vacc_target_pop = pop_number_children_5_36_mons, 
         cm_rdt_kit_quantity, 
         cm_act_packs_quantity, 
         cm_iv_artesunate_quantity, 
         cm_ras_quantity, 
         cm_rdt_kits_procurement_cost, 
         cm_al_procurement_cost, 
         cm_iv_artesunate_procurement_cost, 
         cm_ras_prcurement_cost, 
         cm_rdt_kits_distribution_cost, 
         cm_al_distribution_cost, 
         cm_iv_artesunate_distribution_cost, 
         cm_ras_distribution_cost, 
         cm_public_total_cost, 
         cm_private_total_cost, 
         cm_public_target_pop = pop_2025_projected, 
         cm_private_target_pop = pop_2025_projected, 
         iptp_sp_total_quantity, 
         iptp_sp_procurement_cost, 
         iptp_sp_distribution_cost, 
         iptp_total_cost, 
         iptp_target_pop = pop_number_pw) 

all_delivery_all_cost <- 
  left_join(fully_quantified_and_costed_interventions, 
            to_cost_interventions)

full_interventions <- 
  all_delivery_all_cost |> 
  select(state, lga, contains("total_cost"), contains("target_pop"), 
         itn_campaign_net_quantity, 
         itn_campaign_bale_quantity, 
         itn_routine_net_quantity) |> 
  # Pivot longer on total_cost and target_pop with matching pairs
  pivot_longer(
    cols = matches("^(.*)_total_cost|(.*)_target_pop$"), 
    names_to = c("intervention", ".value"), 
    names_pattern = "(.*)_(total_cost|target_pop)"
  ) |> 
  rename(target_value = target_pop) |> 
  mutate(target = "Population")

write.csv(full_interventions, 
          row.names = FALSE, 
          "budget-viz-tool/plan-comparisons/full-cost-inputs.csv")

lsm_fully_quantified_and_costed <- 
  state_data_extract |> 
  select(state, starts_with("lsm"), 
         admin_landmass_2perc_coverage_hectares)

write.csv(lsm_fully_quantified_and_costed, 
          row.names = FALSE, 
          "budget-viz-tool/plan-comparisons/lsm-full-cost-inputs.csv")

# additional national level data 
additional_national_level_costs <- 
  read.csv("exploratory-steps/data/working-data/codable-national-data-october.csv") |> 
  select(itn_campaign_storage_hardware_cost, 
         cm_eqa_national_cost, 
         irs_total_cost, 
         ento_surveillance_total_cost) 
  
write.csv(additional_national_level_costs, 
          row.names = FALSE, 
          "budget-viz-tool/plan-comparisons/additional-national-level-costs.csv")

# national support services
national_support_services_costs <- 
  read.csv("budget-viz-tool/working-data/national_total_cost_summary.csv") |> 
  filter(intervention_type == "Support Services")

write.csv(national_support_services_costs, 
          row.names = FALSE, 
          "budget-viz-tool/plan-comparisons/national-support-services-costs.csv")

# state support services 
state_support_services_costs <- 
  read.csv("budget-viz-tool/working-data/state_total_cost_summary.csv") |> 
  filter(intervention_type == "Support Services")

write.csv(state_support_services_costs, 
          row.names = FALSE, 
          "budget-viz-tool/plan-comparisons/state-support-services-costs.csv")

# lga support services 
lga_support_services_costs <- 
  read.csv("budget-viz-tool/working-data/lga_total_cost_summary.csv") |> 
  filter(intervention_type == "Support Services")

write.csv(lga_support_services_costs, 
          row.names = FALSE, 
          "budget-viz-tool/plan-comparisons/lga-support-services-costs.csv")


# test <-
#   full_interventions |>
#   group_by(intervention) |>
#   summarise(total_cost = sum(total_cost, na.rm = TRUE),
#             target_value = sum(target_value, na.rm = TRUE),
#             states_targeted = n_distinct(state),
#             lgas_targeted = n_distinct(paste(state, lga, sep = "_"))
#             )



# data to be read in by function 
## total cost data for each intervention 
## additional nation costs for interventions 
## national support service cost  
## and lsm specific input dataframe and cost summary

# Including a full intervention breakdown excluding total cost for the 
# itnerventional element dataframes needed  
full_elements <- 
  all_delivery_all_cost |> 
  select(state, lga,contains("cost")) |> 
  select(-contains("total_cost"), 
         -smc_spaq_3_11_months_procurement_cost, 
         -smc_spaq_12_59_months_procurement_cost) |> 
  left_join(all_delivery_all_cost |> select(state, lga, cm_private_total_cost)) |> 
  pivot_longer(
    cols = c(-state,-lga),
    names_to = c("full_name"),
    values_to = "value"
  ) |> 
  mutate(intervention = case_when(grepl("irs", full_name) ~ "irs",
                                  grepl("lsm", full_name) ~ "lsm", 
                                  grepl("smc", full_name) ~ "smc", 
                                  grepl("pmc", full_name) ~ "pmc", 
                                  grepl("iptp", full_name) ~ "iptp", 
                                  grepl("vacc", full_name) ~ "vacc", 
                                  full_name == "cm_private_total_cost" ~ "cm_private",
                                  grepl("cm", full_name) ~ "cm_public", 
                                  grepl("routine", full_name) ~ "itn_routine", 
                                  grepl("campaign", full_name) ~ "itn_campaign")) 

write.csv(full_elements, "budget-viz-tool/working-data/elemental-cost-data.csv", 
          row.names = FALSE)
  
  

