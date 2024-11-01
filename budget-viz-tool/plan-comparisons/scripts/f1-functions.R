#-------------------------------------------------------------------------------
# Functions for the workshop to create the summary costs associated with 
# a new intervention mix plan
# 
# The following functions work in a two step process 
#   The first - takes an input data frame of 1's and 0's and creates a new 
#   summary dataframe with the written intervention mix column defined for 
#   use in creating the intervention mix maps on the comparison page 
#   
#   The second takes this intervention mix dataframe and creates a new 
#   output of the total costs associated with that intervention mix and the 
#   total cost breakdown at an intervention level that will populate the tables
#   and plots on the plan comparison page 
#   
#   A note on what we can and can't do at this stage: I've set this up 
#   just so that we can alter whether an LGA or for LSM a State (all LGAs in a 
#   state need to be targeted for this otherwise it won't work) is targeted 
#   with an intervention or not (effectively on and off switches) 
#   I haven't added in functionality at this stage to alter the target population 
#   or coverage of an intervention within an LGA (we wouldn't be able to quantify
#   intervention commodities for Case Management, IPTp, Routine Nets but if pushed 
#   we could potentially do this for vaccine, smc or pmc and potentially LSM  
#   but if that's asked for it's probably easier to do manually my end)
#   
#   The function effectively takes in the intervention mix specified from the 
#   first function, joins this to a dataframe where for each lga every 
#   intervention has been costed so will only join the costs where the intervention
#   is targeted. 
#   It then joins in some additional fixed costs and the national level 
#   support services costs (I haven't set these to be altered either, just 
#   keeping it at intervention mix currently) and will then summarise 
#   the plan up to the national level for each intervention delivered the output 
#   dataframe will be in the format  
#   Intervention Type | Item | States Targeted | LGAs Targeted | Target Denom | Target Pop | Total Cost 
#   
#--------------------------------------------------------------------------------

# Step 1 create a modifiable intervention mix - save this for use in data folder
# intervention_mix <- read.csv("budget-viz-tool/working-data/intervention_mix.csv")
# 
# modifiable_intervention_data_frame <-
#   intervention_mix |>
#   select(state, lga,
#         code_cm_public = code_case_management,
#         code_iptp,
#         code_smc,
#         code_pmc,
#         code_vacc = code_vaccine_all,
#         code_irs,
#         code_lsm,
#         code_itn_campaign = code_itn_type,
#         code_itn_routine = code_itn_type,
#         code_itn_urban = code_itn_urban) |>
#   # First transformation for NA values
#   mutate(across(
#     c(code_cm_public, code_smc, code_pmc, code_iptp, code_vacc, code_irs, code_lsm, code_itn_urban),
#     ~ ifelse(is.na(.), 0, 1)
#   )) |>
#   # Separate transformation for text columns
#   mutate(
#     code_itn_campaign = if_else(code_itn_campaign == "ITN Campaign", 1, 0),
#     code_itn_routine = if_else(code_itn_routine == "ITN Routine", 1, 0)
#   ) |>
#   # add cm_private in
#   mutate(code_cm_private = 1)
# 
# write.csv(modifiable_intervention_data_frame,
#           row.names = FALSE,
#           "budget-viz-tool/plan-comparisons/modifiable-intervention-mix.csv")

# modifiable_intervention_data_frame <- read.csv("budget-viz-tool/plan-comparisons/modifiable-intervention-mix.csv")

# Function to convert intervention dataframe with 1's and 0's into intervention mix dataframe 
create_intervention_mix_for_map <- function(data, plan_name = NULL, 
                                            plan_description = NULL){
  
  # Check if each state has a consistent value of 1 or 0 for 'code_lsm'
  inconsistent_states <- 
    data %>%
    group_by(state) %>%
    summarize(unique_values = n_distinct(code_lsm)) %>%
    filter(unique_values > 1) %>%
    pull(state)
  
  if (length(inconsistent_states) > 0) {
    stop(paste(
      "The following states have a mix of values for 'code_lsm':",
      paste(inconsistent_states, collapse = ", "), 
      "LSM can only be targeted at the State Level so all LGAs need a consistent value"
    ))
  }
  
  # Create Mix Map dataframe 
  mix_map <- 
    data |> 
    mutate(
      # Combine itn campain/routine with urban if there
      code_itn_campaign_urban = case_when(code_itn_campaign == 1 & code_itn_urban == 1 ~ 1, TRUE ~ 0), 
      code_itn_campaign = case_when(code_itn_campaign == 1 & code_itn_urban == 0 ~ 1, TRUE ~ 0), 
      code_itn_routine_urban = case_when(code_itn_routine == 1 & code_itn_urban == 1 ~ 1, TRUE ~ 0), 
      code_itn_routine = case_when(code_itn_routine == 1 & code_itn_urban == 0 ~ 1, TRUE ~ 0)) |> 
    select(-code_itn_urban) |> 
    # Pivot the data from wide to long format
    pivot_longer(cols = starts_with("code_"), 
                 names_to = "intervention", 
                 values_to = "value") %>%
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
        intervention == "itn_routine_urban" ~ "ITN Routine Urban", 
        intervention == "itn_campaign" ~ "ITN Campaign", 
        intervention == "itn_campaign_urban" ~ "ITN Campaign Urban", 
        TRUE ~ intervention_mix_to_show)
    ) |> 
    group_by(state, lga)  |> 
    # Concatenate interventions with "+" separator
    mutate(intervention_summary = paste(intervention_mix_to_show, collapse = " + ")) |> 
    # remove case management private from the mix  
    mutate(intervention_summary =str_remove_all(intervention_summary, "\\s*\\+ CM_PRIVATE$")) |> 
    mutate(intervention_summary =str_remove_all(intervention_summary, "CM_PRIVATE\\s*\\+\\s*")) |> 
    # add plan code 
    mutate(plan = plan_name, 
           plan_description = plan_description) |> 
    select(-value)
  
  return(mix_map)
  
}

# # test - works :) 
# plan_poop <- create_intervention_mix_for_map(modifiable_intervention_data_frame, plan_name = "baseline", 
#                                              plan_description = "Costed Operational Plan - Baseline")

# Function to convert new intervention_mix_for_map dataframe into the national 
# level summary cost data for comparison tab  

create_plan_cost_summary <- function(data){ 
  
  #-SUMMARY------------------------------------------------------------------------------
  # Print a summary of the interventions and number of states/LGAs being targeted
  cat("Costing scenario being generated for the following mix of interventions:")
  print(  
    data |> 
      group_by(intervention) |> 
      summarise(states_targeted = n_distinct(state),
                lgas_targeted = n_distinct(paste(state, lga, sep = "_"))
                )
    )

  # Ask the user if they want to proceed
  response <- readline(prompt = "Do you want to proceed with the costing generation? (yes/no): ")
  
  # Check the response and stop if no and they want to fix something
  if (tolower(response) != "yes") {
    stop("Costing generation has been stopped by the user.")
  }
  
  # Continue with the costing generation if the user chooses to proceed
  cat("Proceeding with the costing generation...\n")
  
  #-COSTING-------------------------------------------------------------------------------
  
  #-Add intervention costs-----------------------------------------------
  # read in the full cost dataframe generated for every intervention 
  full_interventions <- read.csv("budget-viz-tool/plan-comparisons/full-cost-inputs.csv")
  
  # join full cost data to the input intervention mix data - this will 
  # mean costs are only added for interventions that are present in each lga  
  plan_data <- 
    data |> 
    mutate(standardised_intervention = str_remove(intervention, "_urban")) |> 
    left_join(full_interventions, by=c("state", "lga", "standardised_intervention" = "intervention")) |> 
    # remove LSM as costs calculated as state level 
    filter(!is.na(total_cost))
  
  #-Cost out LSM at state level------------------------------------------- 
  # read in lsm cost data 
  lsm_cost_data <- read.csv("budget-viz-tool/plan-comparisons/lsm-full-cost-inputs.csv")
  
  # select costs for states targeted
  lsm_data <- 
    data |> 
    ungroup() |> 
    filter(intervention == "lsm") |> 
    select(state, intervention, plan) |> 
    distinct() |> 
    left_join(lsm_cost_data, by="state") |> 
    select(state, intervention, 
           total_cost = lsm_total_cost, 
           target_value = admin_landmass_2perc_coverage_hectares) |> 
    mutate(target = "Area (Hectares)")
  
    #-join these dataframes together and summarise to the national level---
    plan_data_cost <- 
      plan_data |> 
      bind_rows(lsm_data) |> 
      ungroup() |> 
      group_by(intervention, target) |> 
      summarise(total_cost = sum(total_cost, na.rm = TRUE),
                target_value = sum(target_value, na.rm = TRUE),
                state_count = n_distinct(state),
                lga_count = n_distinct(paste(state, lga, sep = "_"))
                  )
    
    #-read in other national cost data to add 
    additional_national_costs <-
      read.csv("budget-viz-tool/plan-comparisons/additional-national-level-costs.csv")
    
    # need to add in the cost of the missing nets from the disputed area in taraba state 
    # that were costed for in the national plan so that values align with baseline scenario 
    # Only adding in if the campaign values and routine values are >0 aka weren't 
    # removed entirely 
    if(plan_data_cost$total_cost[which(plan_data_cost$intervention == "itn_campaign")] > 0){
      itn_net_campain_additional_cost <- (19127 * 5585) + (383*10000) + (19127*584)
      
      plan_data_cost$total_cost[which(plan_data_cost$intervention == "itn_campaign")] <- 
        plan_data_cost$total_cost[which(plan_data_cost$intervention == "itn_campaign")] +
        itn_net_campain_additional_cost 
      
      # add the net storage of hardware cost 
      plan_data_cost$total_cost[which(plan_data_cost$intervention == "itn_campaign")] <- 
        plan_data_cost$total_cost[which(plan_data_cost$intervention == "itn_campaign")] +
        additional_national_costs$itn_campaign_storage_hardware_cost 
    }
    
    if(plan_data_cost$total_cost[which(plan_data_cost$intervention == "itn_routine")] > 0){
      itn_routine_additional_cost <- (3093 * 5585) +  (3093*1396)
      
      plan_data_cost$total_cost[which(plan_data_cost$intervention == "itn_routine")] <- 
        plan_data_cost$total_cost[which(plan_data_cost$intervention == "itn_routine")] +
        itn_routine_additional_cost 
    }
    
    # add the case management 
    plan_data_cost$total_cost[which(plan_data_cost$intervention == "cm_public")] <- 
      plan_data_cost$total_cost[which(plan_data_cost$intervention == "cm_public")] + 
      additional_national_costs$cm_eqa_national_cost
    
    # format this data and add in USD cost  
    exchange_rate <- 1600 #keeping as in the spreadsheet

    plan_data_cost <- 
      plan_data_cost |> 
      mutate(intervention_type = "Malaria Interventions") |> 
      mutate(title = case_when(intervention == "itn_campaign" ~ "ITN Campaign", 
                               intervention == "itn_campaign_urban" ~ "ITN Campaign Urban", 
                               intervention == "itn_routine" ~ "ITN Routine Distribution", 
                               intervention == "itn_routine_urban" ~ "ITN Routine Distribution Urban",
                               intervention == "irs" ~ "IRS",
                               intervention == "lsm" ~ "LSM", 
                               intervention == "smc" ~ "SMC", 
                               intervention == "pmc" ~ "PMC", 
                               intervention == "iptp" ~ "IPTp", 
                               intervention == "vacc" ~ "Malaria Vaccine", 
                               intervention == "cm_public" ~ "Public Sector Case Management", 
                               intervention == "cm_private" ~ "Private Sector Case Management" )) |> 
      crossing(currency = c("Naira", "USD")) |> 
      mutate(total_cost = case_when(currency == "USD" ~ total_cost /1600, 
                                    TRUE ~ total_cost))  
    
    # add in the support services cost 
    national_support_services <- 
      read.csv("budget-viz-tool/plan-comparisons/national-support-services-costs.csv")
    
    # combine rows for output 
    output_data <- 
      plan_data_cost |> 
      bind_rows(national_support_services) |> 
      mutate(plan = data$plan[1], 
             plan_description = data$plan_description[1])
    
    return(output_data)
    
}

# #test - works :) 
# baseline <- create_plan_cost_summary(plan_poop)
