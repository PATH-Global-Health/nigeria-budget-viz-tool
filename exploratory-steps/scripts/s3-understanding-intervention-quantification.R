#-----------------------------------------------------------------------------------------------------------------------------------------
# Script to replicate costs in the budget spreadsheet 
# Replicate each intervention step by step at the LGA, State and National level
#-----------------------------------------------------------------------------------------------------------------------------------------


#-packages--------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)

#-read in all codable data----------------------------------------------------------------------------------------------------------------
unit_cost_data        <- read.csv("exploratory-steps/data/working-data/codable-unit-costs-october.csv")
national_data_extract <- read.csv("exploratory-steps/data/working-data/codable-national-data-october.csv")
state_data_extract    <- read.csv("exploratory-steps/data/working-data/codable-state-data-october.csv")
lga_data_extract      <- read.csv("exploratory-steps/data/working-data/codable-lga-data-october.csv")

working_data <- 
  lga_data_extract |> 
  # add state names to LGA level data 
  left_join(
    state_data_extract |>  select(spatial_level_1, spatial_level_2, state), 
    by=c("spatial_level_1", "spatial_level_2")
  ) |> 
  relocate(state, .before = lga) 

#-POPULATION DATA CHECKS-------------------------------------------------------------------------------------------------------------------
pop_calculations_check <- 
  working_data |> 
  select(state, lga, starts_with("pop_"))

# check estimation of 2025 population size 
# exponential growth rates?
pop_calculations_check <- 
  pop_calculations_check |> 
  group_by(lga) |> 
  mutate(pop_2025_projected_path_checks = 
           PATHtoolsZambia::scale_pop_growth_annual(
             initial_pop = pop_2006_census, 
             new_year = 2025, 
             initial_year = 2006, 
             growth_rate = 1+(pop_annual_growth_percent/100) #changing format from % to rate
             )
         )

pop_calculations_check$differences <- 
  round(pop_calculations_check$pop_2025_projected,0) - 
  round(pop_calculations_check$pop_2025_projected_path_checks,0)

# ✅ This is good all values are 0 so exponential growth accounted for 

# ❓Interestingly c("Akwanga", "Awe", "Doma", "Karu", "Keana", "Keffi", "Kokona", "Lafia", "Nasarawa2", "Nasarawa-Eggon", "Obi2", "Toto", "Wamba")
# ❓lgas in Nasarawa state have values in the admin_number_sfhs and admin_number_thfs columns where no other LGAs have this information and it is only
# ❓manually entered at the State level 

#-CREATE NEW INTERVENTION MIX COLUMNS--------------------------------------------------------------------------------------------------------
intervention_quant_checks <- 
  working_data |> 
  select(spatial_level_1:lga, 
         starts_with("code"),
         contains("eligable"), 
         -itn_routine_net_quantity_eligable)  
  
# recode the intervention delivery cols including campaign or routine net 
# delivery and urbnaicity and recode an intervention column for each of these 
# interventions 
intervention_quant_checks <- 
  intervention_quant_checks |> 
  mutate(
    code_intervention_mix_to_match = code_intervention_mix
  ) |> 
  separate_rows(
    code_intervention_mix_to_match, sep = "\\+"
  ) |> 
  mutate(
    code_intervention_mix_to_match = str_to_lower(code_intervention_mix_to_match)
  ) |> 
  mutate(
    code_intervention_mix_to_match = case_when(
      grepl("llin", code_intervention_mix_to_match) & itn_campaign_eligable == 1 ~ "itn campaign", 
      grepl("llin", code_intervention_mix_to_match) & itn_campaign_eligable == 0 ~ "itn routine", 
      TRUE ~ code_intervention_mix_to_match
    )
  ) |> 
  mutate(code_intervention_mix_to_match = case_when(
    !is.na(code_urban_nets) & grepl("itn", code_intervention_mix_to_match) ~ paste0(code_intervention_mix_to_match, " urban"),
    TRUE ~ code_intervention_mix_to_match
    )
  ) |> 
  # remove code columns  
  select(starts_with("spatial_level"), 
         state, lga, 
         code_intervention_mix, 
         code_intervention_mix_to_match)


#-CAMPAIGN NET QUANTIFICATION---------------------------------------------------------------------------------------------------------------------------

# Consultant noted that nets needed for campaign were provided by the Global Fund 
# and are not calculated - I checked the values and the number of nets needed
# is just the 2025 population total / 1.8 

net_pp <- 1.8       #assumption of 1.8 nets per person 
nets_per_bale <- 50 #number of nets per bale 

itn_campaign_net_quantification <- 
  intervention_quant_checks |> 
  # select campaign data
  filter(grepl("campaign", code_intervention_mix_to_match)) |> 
  # join with 2025 population data 
  left_join(working_data |> select(state, lga, target_pop = pop_2025_projected), 
            by=c("state", "lga")) |> 
  # quantify 
  mutate(itn_campaign_net_quantity = target_pop / net_pp, 
         itn_campaign_bale_quantity = itn_campaign_net_quantity/ nets_per_bale)

# check for differences 
campain_nets_differences <- 
  itn_campaign_net_quantification |> 
  left_join(working_data |> select(state, lga, 
                                   itn_campaign_net_quantity_excel = itn_campaign_net_quantity,
                                   itn_campaign_bale_quantity_excel = itn_campaign_bale_quantity), 
            by=c("state", "lga")) |> 
  mutate(difference_net_quant = round(itn_campaign_net_quantity - itn_campaign_net_quantity_excel, 0), 
         difference_bale_quant = round(itn_campaign_bale_quantity - itn_campaign_bale_quantity_excel ))

issues <- filter(campain_nets_differences, difference_net_quant >0)

# All from Kaduna State - range from 16 - 63 difference in raw nets (ours is higher) and +1 extra Bale 

# ❓ no buffer included??  - **Calculation of net: population divided by 1.8 plus buffer of 10% (rounded to bales)

#-ROUTINE NET QUNATIFICATION---------------------------------------------------------------------------------------------------------------------------- 
# Consultant noted that nets needed for campaign were provided by the Global Fund 
# these values are hard coded in the spreadsheet also so not clear how values are calculated  
# ❓ quantification?? 

# assumption of some coveage value of the Pregnant Women and U5 population? 
routine_net_quant <- 
  lga_data_extract |> 
  filter(itn_routine_eligable == 1) |> 
  select(lga, starts_with("pop"), itn_routine_net_quantity) |> 
  mutate(level = "lga") |> 
  bind_rows(state_data_extract |> 
              select(state, starts_with("pop"), itn_routine_net_quantity) |> 
              mutate(level = "state")) |> 
  bind_rows(national_data_extract |> 
              select(country, starts_with("pop"), itn_routine_net_quantity) |> 
              mutate(level = "national")) |> 
  mutate(target_pop = pop_number_pw_child) |> 
  mutate(cov_rate = itn_routine_net_quantity / target_pop   ) |> 
  group_by(level) |> 
  summarise(min_rate = min(cov_rate), 
            max_rate = max(cov_rate), 
            mean_rate = mean(cov_rate))


# Use given data for now - likely could be a 30% of the u5 and pregnant women population target?

#-SMC DRUG QUANTIFICATION-------------------------------------------------------------------------------------------------------------------------------
#  SP+AQ can be procured in co-blistered packets containing one full course of SMC drugs for each cycle (1 tablet of SP and 3 tablets of AQ).   
#  Since SMC is delivered in two dosage groups: 3 to <12 months and >12 to 59 months, the number of packets for each age group needs to be quantified and 
#  then multiplied by 4, to cover every cycle.  In addition, a buffer stock between 10 -20% should be included to accommodate for loss, 
#  re-dosing and treatment of children from neighbouring locations. 
# ❓buffer stock??

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


# https://www.malariaconsortium.org/media-download-file/201512090203/-/smc-start-up-guide_final-for-smc-dissemination-meeting_4nov2015.pdf

# calculate as methods above
smc_monthly_rounds <- 4    #smc given over 4 months  
# smc_number_doses <- 4    #4 doses of SP+AQ needed for each month of SMC - can ignoring this as I think this is calculating based on commodity of packets as above - 
smc_pop_prop_3_11 <- 0.18  # 18% of the under 5 population is 3-11 months
smc_pop_prop_12_59 <- 0.77 # 77% of the under 5 population is 12-59 months
buffer <- 1.1 # 10% included in spreadsheet

smc_quantification <- 
  intervention_quant_checks |> 
  # select smc campaign data
  filter(grepl("smc", code_intervention_mix_to_match)) |> 
  # join with 2025 population data 
  left_join(working_data |> select(state, lga, 
                                   target_pop = pop_number_children_u5 ), 
            by=c("state", "lga")) |> 
  # quantify 
  mutate(smc_spaq_quantity_3_11_months = target_pop *  smc_pop_prop_3_11 * smc_monthly_rounds * buffer, 
         smc_sqaq_quantity_12_59_months = target_pop *  smc_pop_prop_12_59 * smc_monthly_rounds * buffer, 
         smc_sqaq_quantity = smc_spaq_quantity_3_11_months + smc_sqaq_quantity_12_59_months, 
         smc_target_pop = target_pop * (smc_pop_prop_3_11 + smc_pop_prop_12_59))


# check for differences 
differences <- 
  smc_quantification |> 
  left_join(working_data |> select(state, lga, 
                                   smc_spaq_quantity_3_11_months_excel = smc_spaq_quantity_3_11_months,
                                   smc_sqaq_quantity_12_59_months_excel = smc_sqaq_quantity_12_59_months, 
                                   smc_sqaq_quantity_excel = smc_sqaq_quantity), 
            by=c("state", "lga")) |> 
  mutate(difference_1 = round(smc_spaq_quantity_3_11_months - smc_spaq_quantity_3_11_months_excel, 0), 
         difference_2 = round(smc_sqaq_quantity_12_59_months - smc_sqaq_quantity_12_59_months_excel, 0), 
         difference_3 = round(smc_sqaq_quantity - smc_sqaq_quantity_excel, 0))  


differences$difference_1
differences$difference_2
campain_nets_differences$difference_3

differences |> filter(difference_1 >0)

# ❓  Oyo   Irepo has SMC listed as an intervention mix column (and the SMC code column) but it's not listed as 1 in the smc eligible column - instead it has PMC eligable 
# which is correct? 

#-PMC DRUG QUANTIFICATION--------------------------------------------------------------------------------------------------------------------------
# From the costing consultant: 
# 1.	Children <1 year constitute 4% of the total population.
# 2.	Children 1-2 years constitute 4% of the total population.
# 3.	Antigen coverage rate = 85% (since immunization is being used as the contact point).
# 4.	Since one in four children/infants in Nigeria is underweight), 25% of children <1 year will take half instead of one tablet, while 25% of children 1-2 years will take one 
#      instead of 2 tablets. A factor of 0.75% was therefore used to quantify the required SP for each age group.
# 5.	There will be 12 touch points within a calendar year, hence 12 doses were planned for each child/infant in a year. 
# ❓❓❓

# My understanding is that PMC is delivered through the EPI system - assuming 4 doses of SP 6, 9, 12, and 15 months 
# Can still factor in a coverage rate here of 85% of the target population and 
# the need for different dosing rates 

pmc_doses <- 4 # number of contacts per child per year 
pmc_coverage <- 0.85 # assuming 85% of children attend EPI ❓❓
pmc_scaling <- 0.75  # assuming 1/4 children in each age category are underweight then 
pmc_sp_dose_u1_year_old <- 1 # 1 dose of SP for chilkdren under 1 years old
pmc_sp_dose_1_2_year_old <- 2 # 2 doses of SP for children 1-2 years old

# I think this should be 4 instead of 12 for the number of doses - unless it's 4*3 as in you need 3 doses of SP for 
# each PMC round- ah that's what it is it must be - is it a single dose - but then why do we multiply by 2 for 
# older children? 
# 



#-IPTp Quantification-----------------------------------------------------------

iptp_quantification <- 
  state_data_extract |> 
  # select smc campaign data
  select(state, starts_with("pop_number_pw"), starts_with("iptp")) |> 
  # join with 2025 population data 
  mutate(iptp_quant_q = iptp_sp_total_quantity / pop_number_pw )
  
































