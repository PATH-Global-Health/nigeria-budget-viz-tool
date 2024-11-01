#------------------------------------------------------------------------------
# Script to detail the use of the function to generate new plan comparisons
# on the fly for the workshop 
# 
# This used two functions I've written in the "f1-functions.R" script  
# 
# Details:  
# The following functions work in a two step process 
#   
#   1. The first - takes an input data frame of 1's and 0's and creates a new 
#   summary dataframe with the written intervention mix column defined for 
#   use in creating the intervention mix maps 
#   
#   2. The second takes this intervention mix dataframe and creates a new 
#   output of the total costs associated with that intervention mix at an 
#   intervention level that will populate the tables and plots on the plan 
#   comparison page 
#   
#   A note on what we can and can't do at this stage: 
#   
#     I've set this up just so that we can alter whether an LGA or
#     for LSM a State (all LGAs in a state need to be targeted for this 
#     otherwise it won't work) is targeted  with an intervention or not 
#     (effectively on and off switches). 
#     
#     I haven't added in functionality at this stage to alter the target
#      population or coverage of an intervention within an LGA
#     (we wouldn't be able to quantify  intervention commodities for 
#     Case Management, IPTp, Routine Nets but if pushed we could potentially
#     do this for vaccine, smc or pmc and potentially LSM  
#     but if that's asked for it's probably easier to do manually my end)
#   
#   The function effectively takes in the intervention mix specified from the 
#   first function, joins this to a dataframe where for each lga every 
#   intervention has been costed so will only join the costs where the intervention
#   is targeted. 
#   It then joins in some additional fixed costs and the national level 
#   support services costs (I haven't set these to be altered either, just 
#   keeping it at intervention mix currently) and will then summarise 
#   the plan up to the national level for each intervention delivered 
#   
#   I've used this script here to generate the data for the plan comparison 
#   tab examples (e.g. No PMC, reduced vaccine targeting, no LSM)
#   And it generates a dataframe for the use in that tab  
#   All you will have to do is add some extra lines of code for the new scenarios 
#   that come up - either change the excel of the intervention mix (it's populated
#   now with the baseline mix code) or do it in R whatever is esiest 
#   pass this data to the first function with some additional details on the plan 
#   number and a short description and then pass that resulting data frame to the 
#   second function and bind all the dataframes together save this new dataframe 
#   with the same name and this will then autogenerate the plan comparison page 
#   so long as the data is saved with the same name and in the same location! 
#   
#   Hope that's clear! 
#   
#   p.s the create_plan_cost_summary function will ask you if you want to continue
#   after it shows a summary of the intervention mix
# -----------------------------------------------------------------------------------


# load packages 
library(tidyverse)

# load functions 
source("budget-viz-tool/plan-comparisons/scripts/f1-functions.R")

# read in modifiable intervention mix dataframe (parameterised for 
# baseline scenario as default)
intervention_mix <- 
  read.csv("budget-viz-tool/plan-comparisons/modifiable-intervention-mix.csv")

# generate the baseline scenario
baseline_mix <- 
create_intervention_mix_for_map(
  intervention_mix, 
  plan="baseline", 
  plan_description = "Costed Operational Plan Baseline"
  )

# generate the baseline costs
baseline_cost <-  create_plan_cost_summary(baseline_mix)

#-Plan 1 - remove PMC from targeted areas-------------------------------------------
no_pmc <- 
  intervention_mix |> 
  mutate(code_pmc = 0) # set to 0 to remove from everywhere

plan_1_mix <-
  create_intervention_mix_for_map(
    no_pmc, 
    plan="plan 1", 
    plan_description = "Removal of PMC"
    )
  
plan_1_cost <- create_plan_cost_summary(plan_1_mix)

#-Plan 2 - targeted vaccine to only the 27 LGAs in Kebbi and Bayeslsa--------------
vaccine_targeted_lgas <- c("Argungu", "Augie", "Bagudo", "Birnin Kebbi", "Bunza",
                           "Dandi", "Fakai", "Gwandu", "Jega", "Kalgo", 
                           "Koko/Besse", "Maiyama", "Ngaski", "Sakaba", 
                           "Shanga", "Suru", "Wasagu/Danko", "Yauri", "Zuru", 
                           "Brass", "Ekeremor", "Kolokuma/Opokuma", "Nembe", 
                           "Ogbia", "Sagbama", "Southern Ijaw", "Yenegoa")

vaccine_targted_lgas_and_states <- 
  intervention_mix |> 
  select(state, lga) |> 
  filter(lga %in% c(vaccine_targeted_lgas)) 
# no duplicates of LGAs in different states than expected 

less_vaccine <- 
  intervention_mix |> 
  mutate(
    code_vacc = 
      case_when(
        lga %in% c(vaccine_targeted_lgas) ~ 1, # only keeping 1 in targeted LGAs
        TRUE ~ 0
        )
    ) 

plan_2_mix <- 
  create_intervention_mix_for_map(
    less_vaccine, 
    plan="plan 2", 
    plan_description = "Limited Vaccine Targeted in Kebbi and Bayesla"
    )


plan_2_cost <- create_plan_cost_summary(plan_2_mix)

#-Plan 3 - No LSM-------------------------------------------------------------------
no_lsm <- 
  intervention_mix |> 
  mutate(code_lsm = 0) # set to 0 remove from everywhere

plan_3_mix <- 
  create_intervention_mix_for_map(
    no_lsm, 
    plan = "plan 3", 
    plan_description = "Removal of LSM"
    )

plan_3_cost <- create_plan_cost_summary(plan_3_mix)

#-Plan 4 No Urban Net Campaigns------------------------------------------------------
no_urban_campaigns <- 
  intervention_mix |> 
  # need to change itn_campaign to 0 where code_itn_urban == 1
   mutate(
     code_itn_campaign = 
       case_when(
         code_itn_urban == 1 & code_itn_campaign == 1 ~ 0, 
         TRUE ~ code_itn_campaign
         )
     )

plan_4_mix <- 
  create_intervention_mix_for_map(
    no_urban_campaigns, 
    plan="plan 4", 
    plan_description = "Removal of Urban ITN Campaigns"
    )

plan_4_cost <- create_plan_cost_summary(plan_4_mix)

#-Plan 5  ---------------------------------------------------------------------------
#-Plan 6 etc to be added here ------------------------------------------------------- 

#-Combine all comparisons together into a Mix and Cost Dataframe--------------------
plan_comparison_mixes <- 
  bind_rows(baseline_mix, 
            plan_1_mix, 
            plan_2_mix, 
            plan_3_mix, 
            plan_4_mix)

plan_comparison_costs <- 
  bind_rows(baseline_cost, 
            plan_1_cost, 
            plan_2_cost, 
            plan_3_cost, 
            plan_4_cost)

#-save in same place with the same name----------------------------------------------
write.csv(plan_comparison_mixes, 
          "budget-viz-tool/plan-comparisons/viz-data/plan-comparison-mixes.csv", 
          row.names = FALSE)

write.csv(plan_comparison_costs, 
          "budget-viz-tool/plan-comparisons/viz-data/plan-comparison-costs.csv", 
          row.names = FALSE)






























































