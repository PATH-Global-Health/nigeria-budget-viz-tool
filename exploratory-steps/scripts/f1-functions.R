
# creating the total cost summaries
create_plan_cost_summary_grouped <- function(data, grouping = "national"){ 
  
  #-Adjust grouping based on the specified level----------------------------------------
  if (grouping == "national") {
    grouping_vars <- c("intervention")
  } else if (grouping == "state") {
    grouping_vars <- c("intervention", "state")
  } else if (grouping == "lga") {
    grouping_vars <- c("intervention", "state", "lga")
  } else {
    stop("Invalid grouping option. Choose 'national', 'state', or 'lga'.")
  }
  
  #-SUMMARY------------------------------------------------------------------------------
  # Print a summary of the interventions and number of states/LGAs being targeted
  cat("Costing scenario being generated for the following mix of interventions:")
  summary_data <- 
    data |> 
    group_by(intervention) |> 
    summarise(
      states_targeted = n_distinct(state),
      lgas_targeted = n_distinct(paste(state, lga, sep = "_"))
    )
  
  print(summary_data)
  cat(paste0("Costing scenario being generated for the following level:"), grouping)

  #-COSTING-------------------------------------------------------------------------------
  
  # Add intervention costs
  full_interventions <- read.csv("budget-viz-tool/plan-comparisons/full-cost-inputs.csv")
  
  # Join full cost data to the input intervention mix data
  plan_data <- 
    data |> 
    mutate(standardised_intervention = str_remove(intervention, "_urban")) |> 
    left_join(full_interventions, by = c("state", "lga", "standardised_intervention" = "intervention")) |> 
    filter(!is.na(total_cost))  # Remove LSM as costs calculated at state level
  
  #-Cost out LSM at state level------------------------------------------- 
  lsm_cost_data <- read.csv("budget-viz-tool/plan-comparisons/lsm-full-cost-inputs.csv")
  lsm_data <- 
    data |> 
    ungroup() |> 
    filter(intervention == "lsm") |> 
    select(state, intervention) |> 
    distinct() |> 
    left_join(lsm_cost_data, by = "state") |> 
    select(state, intervention, total_cost = lsm_total_cost, target_value = admin_landmass_2perc_coverage_hectares) |> 
    mutate(target = "Area (Hectares)")
  
  #-Join and summarise data at the specified level---------------------------
  plan_data_cost <- 
    plan_data |> 
    bind_rows(lsm_data) |> 
    ungroup() |> 
    group_by(across(all_of(grouping_vars))) |> 
    summarise(
      total_cost = sum(total_cost, na.rm = TRUE),
      target_value = sum(target_value, na.rm = TRUE),
      state_count = n_distinct(state),
      lga_count = n_distinct(paste(state, lga, sep = "_"))
    )
  
  # Apply national-level adjustments only if grouping is "national"
  if (grouping == "national") {
    additional_national_costs <- read.csv("budget-viz-tool/plan-comparisons/additional-national-level-costs.csv")
    
    # Adjust costs for ITN campaign
    if (plan_data_cost$total_cost[which(plan_data_cost$intervention == "itn_campaign")] > 0) {
      itn_net_campain_additional_cost <- (19127 * 5585) + (383 * 10000) + (19127 * 584)
      plan_data_cost$total_cost[which(plan_data_cost$intervention == "itn_campaign")] <- 
        plan_data_cost$total_cost[which(plan_data_cost$intervention == "itn_campaign")] +
        itn_net_campain_additional_cost 
      
      # Add the net storage hardware cost
      plan_data_cost$total_cost[which(plan_data_cost$intervention == "itn_campaign")] <- 
        plan_data_cost$total_cost[which(plan_data_cost$intervention == "itn_campaign")] +
        additional_national_costs$itn_campaign_storage_hardware_cost 
    }
    
    # Adjust costs for ITN routine
    if (plan_data_cost$total_cost[which(plan_data_cost$intervention == "itn_routine")] > 0) {
      itn_routine_additional_cost <- (3093 * 5585) + (3093 * 1396)
      plan_data_cost$total_cost[which(plan_data_cost$intervention == "itn_routine")] <- 
        plan_data_cost$total_cost[which(plan_data_cost$intervention == "itn_routine")] +
        itn_routine_additional_cost 
    }
    
    # Add case management national cost
    plan_data_cost$total_cost[which(plan_data_cost$intervention == "cm_public")] <- 
      plan_data_cost$total_cost[which(plan_data_cost$intervention == "cm_public")] + 
      additional_national_costs$cm_eqa_national_cost
  }
  
  
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
  
  
  # Apply support services costs based on the grouping level
  if (grouping == "national") {
    national_support_services <- read.csv("budget-viz-tool/plan-comparisons/national-support-services-costs.csv")
    support_services_data <- national_support_services
  } else if (grouping == "state") {
    state_support_services <- read.csv("budget-viz-tool/plan-comparisons/state-support-services-costs.csv")
    support_services_data <- state_support_services
  } else if (grouping == "lga") {
    lga_support_services <- read.csv("budget-viz-tool/plan-comparisons/lga-support-services-costs.csv")
    support_services_data <- lga_support_services
  }
  
  # Combine rows for output, adding the appropriate support services data
  output_data <- 
    plan_data_cost |> 
    bind_rows(support_services_data) |> 
    mutate(target = case_when(intervention == "lsm" ~"Area (Hectares)", 
                              TRUE ~ "Population"), 
           cost_per_target = total_cost / target_value)
  
  return(output_data)
  
}

# creating the elemental summaries 
create_plan_cost_elements_grouped <- function(data, grouping = "national"){
  
  #-Adjust grouping based on the specified level----------------------------------------
  if (grouping == "national") {
    grouping_vars <- c("intervention","full_name")
  } else if (grouping == "state") {
    grouping_vars <- c("intervention", "full_name", "state")
  } else if (grouping == "lga") {
    grouping_vars <- c("intervention", "full_name", "state", "lga")
  } else {
    stop("Invalid grouping option. Choose 'national', 'state', or 'lga'.")
  }
  
  # Add intervention costs
  full_elements <- read.csv("budget-viz-tool/working-data/elemental-cost-data.csv")
  
  # Join full cost data to the input intervention mix data
  plan_data <- 
    data |> 
    select(state, lga, intervention) |> 
    mutate(standardised_intervention = str_remove(intervention, "_urban")) |> 
    left_join(full_elements, by = c("state", "lga", "standardised_intervention" = "intervention"), 
              multiple="all")|> 
    filter(!is.na(value))  # Remove LSM as costs calculated at state level

  # Add LSM costs in 
  lsm_cost_data <- read.csv("budget-viz-tool/plan-comparisons/lsm-full-cost-inputs.csv")
  lsm_data <- 
    data |> 
    ungroup() |> 
    filter(intervention == "lsm") |> 
    select(state, intervention) |> 
    distinct() |> 
    left_join(lsm_cost_data, by = "state") |> 
    select(state, intervention, lsm_bti_procurement_cost, lsm_operational_cost) |> 
    pivot_longer(cols=c(-state, -intervention), 
                 names_to = "full_name", 
                 values_to = "value")
  
  # Apply grouping and summarization based on the specified level
  if (grouping == "national") {
    
    # Group by full_name and summarize values
    plan_data_grouped <- 
      plan_data |> 
      bind_rows(lsm_data) |> 
      ungroup() |> 
      group_by(intervention, full_name) |> 
      summarise(value = sum(value, na.rm = TRUE))
    
    # add in net and casemanagement additional costs 
    additional_national_costs <-
      read.csv("budget-viz-tool/plan-comparisons/additional-national-level-costs.csv") |> 
      pivot_longer(everything(), 
                   names_to = "full_name", 
                   values_to = "value") |> 
      mutate(intervention = case_when(grepl("itn", full_name) ~ "itn_campaign", 
                                      TRUE ~ "cm_public"))
    
    plan_data_grouped <- 
      bind_rows(plan_data_grouped, 
                additional_national_costs)
    
    # add in additional procurement operation and campaign costs for missing nets  
    # Adjust costs for ITN campaign
    if (plan_data_grouped$value[which(plan_data_grouped$full_name == "itn_campaign_net_procurement_cost" &
                                plan_data_grouped$intervention == "itn_campaign")] > 0) {
      
      plan_data_grouped$value[which(
        plan_data_grouped$full_name == "itn_campaign_net_procurement_cost"&
          plan_data_grouped$intervention == "itn_campaign")] <- 
        plan_data_grouped$value[which(plan_data_grouped$full_name == "itn_campaign_net_procurement_cost" &
                                        plan_data_grouped$intervention == "itn_campaign")] + (19127 * 5585) 
       
      plan_data_grouped$value[which(plan_data_grouped$full_name == "itn_campaign_net_distribution_procurement_cost"&
                                      plan_data_grouped$intervention == "itn_campaign")] <- 
        plan_data_grouped$value[which(plan_data_grouped$full_name == "itn_campaign_net_distribution_procurement_cost"&
                                        plan_data_grouped$intervention == "itn_campaign")] + (383 * 10000)
      
      plan_data_grouped$value[which(plan_data_grouped$full_name == "itn_campaign_campaign_cost" &
                                      plan_data_grouped$intervention == "itn_campaign")] <- 
        plan_data_grouped$value[which(plan_data_grouped$full_name == "itn_campaign_campaign_cost"&
                                        plan_data_grouped$intervention == "itn_campaign")] + (19127 * 584)


      
  }
    # Adjust costs for ITN routine
    if (plan_data_grouped$value[which(plan_data_grouped$full_name == "itn_routine_net_procurement_cost"  &
        plan_data_grouped$intervention == "itn_routine")] > 0) {
      
        plan_data_grouped$value[which(
        plan_data_grouped$full_name == "itn_routine_net_procurement_cost"&
          plan_data_grouped$intervention == "itn_routine")] <- 
        plan_data_grouped$value[which(plan_data_grouped$full_name == "itn_routine_net_procurement_cost" &
                                        plan_data_grouped$intervention == "itn_routine")] + (3093 * 5585)
      
      plan_data_grouped$value[which(plan_data_grouped$full_name == "itn_routine_net_operational_cost" &
                                      plan_data_grouped$intervention == "itn_routine")] <- 
        plan_data_grouped$value[which(plan_data_grouped$full_name == "itn_routine_net_operational_cost"&
                                        plan_data_grouped$intervention == "itn_routine")] + (3093 * 1396)
    }
    
    
  } else if (grouping == "state") {
    # Group by full_name and state, then summarize values
    plan_data_grouped <- 
      plan_data |> 
      bind_rows(lsm_data) |> 
      group_by(intervention, full_name, state) |> 
      summarise(value = sum(value, na.rm = TRUE))
    
  } else if (grouping == "lga") {
    # No summarization needed for LGA, just group by full_name, state, and lga
    plan_data_grouped <- 
      plan_data
  }
  
  # format 
  exchange_rate <- 1600 #keeping as in the spreadsheet
  
  plan_data_grouped <- 
    plan_data_grouped |> 
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
                                    intervention == "itn_campaign" ~ "ITN Campaign",
                                    intervention == "itn_campaign_urban" ~ "ITN Campaign Urban", 
                                    intervention == "itn_routine" ~ "ITN Routine", 
                                    intervention == "itn_routine_urban" ~ "ITN Routine Urban",
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
  
  # add in support services data from before  
  support_services <- 
    read.csv(paste0("budget-viz-tool/working-data/", grouping, "_intervention_chart_data.csv")) |> 
    filter(intervention %in% c(
      "Capacity Building", 
      "Governance & Coordination", 
      "Monitoring & Evaluation",
      "Social Behaviour Change", 
      "Resource Mobilisation"))
  
  plan_data_grouped <- 
    bind_rows(plan_data_grouped, support_services)
  
  return(plan_data_grouped)
  
  
  
  
  
}
