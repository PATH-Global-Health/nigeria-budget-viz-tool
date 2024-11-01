#-------------------------------------------------------------------------------
# script to replicate and understand costing spreadsheet 
# Aims: read in the formatted excel file and extract data into 
#       usable format
#-------------------------------------------------------------------------------

# load packages  
library(tidyverse)
library(janitor)

#-states and LGAs tab-----------------------------------------------------------

# this is a tricky spreadsheet to work through and needs a lot of reformatting
raw_states_lga <- 
  readxl::read_xlsx("exploratory-steps/data/Pragmatic Plan Template-18102024.xlsx",
                    sheet = "States and LGAs", skip = 4) |> 
  janitor::clean_names() |> 
  # remove rows with all NA data - this will be NA in the state/lga name col
  filter(!is.na(x2))

# work through renaming column headings  
working_states_lga <- 
  raw_states_lga |> 
  select(spatial_level = x1, # code identified of spatial levels and aggregations: Country, Geopolitical Zones, States, LGAs
         spatial_name = x2,  # Names - Country, Geopolitical Zones, States, LGAs all combined
         
         # INTERVENTION CODINGS - at LGA level this is X if Yes to be delivered at State, Geopolitical Zone and Country this counts number of LGAS with that intervention
         code_case_management = x3, # heading was removed this identified if case management delivered or not 
         code_smc = smc, 
         code_pmc = pmc, 
         code_iptp = ip_tp, 
         code_vaccine = vaccines, 
         code_vaccine_priority = vaccines_pri, 
         code_vaccine_1 = vaccines_i, 
         code_vaccine_1_priority = vaccines_i_pri, 
         code_vaccine_2 = vaccines_ii, 
         code_vaccine_3 = vaccines_iii, 
         code_vaccine_4 = vaccines_iv, 
         code_vaccine_5 = vaccines_v, 
         code_urban_nets = urban_new_llin_itn, 
         code_nets = new_gen_llin_itn, 
         code_irs = irs_19, 
         code_lsm = lsm, 
         code_intervention_counts = x21, # total number of different interventions per row
         code_intervention_mix = x22,    # intervention mix written out 
         
         # DEMOGRAPHIC DATA 
         pop_2006_census, # 2006 census population counts
         pop_annual_growth_percent = annual_growth_percent, # assumed annual growth rate per year - varies at the state level 
         pop_2025_projected = projected_pop_2025, # estimated 2025 population accouting for 19 years of annual growth 
         pop_percent_pop_u5 = percent_of_u_5_children, # percentage of the population total who are under 5
         pop_number_children_u5 = number_of_u_5_children, # number of children under 5 calculated from percentage above - varies at the state level
         pop_u5_smc_population = u_5_for_smc_95_percent, #❓95% of the under 5 population - accounting for smc in 3months+? 
         pop_u5_smc_pop_coded = number_of_u_5_for_smc_in, # smc target population multiplied by whether LGA gets SMC or no so is 0 when no SMC
         pop_number_pw = number_of_pw_5_percent, #expected number of pregnant women assuming they make up 5% of the pop_2025_projected
         pop_number_pw_children_u5 = number_of_u_5_pw, #sum of number_children_u5 and number_pw 
         pop_number_children_u1 = number_of_1yr_children, #number of children under 1 year old - assumed to be blanket 4% of the pop_2025_projected
         pop_number_children_1_2_yrs = number_of_1_2yrs_children, #number of children 1-2 years old - assumed to be blanket 4% of the pop_2025_projected
         pop_number_children_0_2_yrs = number_of_0_2yrs_children, #sum of pop_number_children_u1 pop_number_children_1_2
         pop_number_children_5_36_mons = children_5_36_mons_3_8_percent_of_pop, #number of children 5-36 months assuming 3.8% of pop_2025_projected
         
         # SPATIAL ADMIN DATA 
         admin_number_states = number_of_states, #number of states in admin levels >=state and NA if LGA row
         admin_proportion_states = proportion_of_states, #proportion of all states in that admin level and NA if lga row
         admin_number_lgas = number_of_lg_as, #number of lgas in admin levels >=lga 
         admin_proportion_lgas = proportion_46, #proportion of lgas in admin levels  
         admin_number_wards = number_of_wards, #number of wards in admin levels 
         admin_proportion_wards = proportion_48, #proportion of wards in admine levels  
         admin_landmass_sqkm = land_mass_for_lsm_sqkm, #total land mass of area in SqKm for LSM planning - data at >= state level 
         admin_landmass_2perc_coverage_sqkm = coverage_area_0_2_percent_sqkm, #❓2% coverage of land mass - area for LSM in SqKm
         admin_landmass_2perc_coverage_hectares = coverage_area_in_hectares, #❓2% coverage of land mass - area for LSM in Hectares
         admin_number_phc = number_of_ph_cs, # number of primary health centres   
         admin_proportion_phc = proportion_of_phc, #proportion of total primary health centres in that spatial unit  
         admin_number_shf = number_of_sh_fs, #number of secondary health facilties in spatial unit >= State (NA at LGA)
         admin_number_thf = number_of_th_fs, #number of tertiary health facilities in spatial unit >= State (NA at LGA)
         admin_number_shf_thf = number_of_secondary_tertiary_h_fs, #summed number of tertiary and secondary health facilities >= State level (NA at LGA)
         admin_proportion_shf_thf = proportion_57, #proportion of all secondary and tertiary facilties in spatial unit >=State (NA at LGA)
         admin_number_total_hf = total_number_of_h_fs, #total sum of all health facilities levels in that spatial unit >=State level (NA at LGA)
         admin_proportion_total_hf = proportion_59, #proportion of all health facilities all levels in that spatial unit >=State level (NA at LGA)
         
         # PREVALENCE DATA national DHS survey data 2018  
         prev_u5_state = x60, #prevalence data for state level prevalence in u5's from the 2018 DHS survey 
         
         # INTERVENTION DATA 
         ## ITNS
         itn_campaign_eligable = eligible_61, # ID 1 for yes to campaign 0 for no - coded at the LGA level NA for other spaital levels 
         itn_campaign_net_quantity = number_of_it_ns_for_campaign, # ❓state level quantity assuming 2 nets per person covered (Not calculated - this is just a static value - doesn't come from population/2 - might have some buffer added to it)- at the LGA level this value is populated by taking the proportion of the state population in that LGA * net quantity for that state
         itn_campaign_net_proportion = proportion_63, #proportion of total nets at that spatial level  
         itn_campaign_bale_quantity = number_of_itn_bales_for_campaign, #number of bales needed itn_campaign_net_quantity / 50 - assuming 50 nets per bale?
         itn_campaign_bale_proportion = proportion_65, #proportion of total bales at that spatial level 
         itn_routine_eligable = eligbile, #ID 1 for yes to routine distribution 0 for no - coded at the LGA level 
         itn_routine_net_quantity = number_of_lli_ns_for_cont_distribution, #❓state level quantity is hard coded - not sure where this value comes from - the lga level value is calculated by taking proportion of the u5 and pw population at lga level * itn state quantity
         itn_routine_net_quantity_eligable = number_eligible_for_cont_distribution, # itn_routine_net_quantity * itn_routine_eligable
         itn_routine_net_proportion = proportion_69, # proption of all eligable nets at that spatial level 
         #❓ assuming that  those with campaign don't get routine distribuition also but there are state level values for routine for all states?
         
         ## LSM
         lsm_bti_kg_quantity = bti_kg, #state level calculation - admin_landmass_2perc_coverage_hectares * 0.5 * 24 (0.5 kg per hectare * 24 contacts) - state only value 
         lsm_bti_kg_proprotion = proportion_of_bti, #proportion of total bti at spatial level (state only)
         
         ## SMC 
         smc_eligable = eligible_72, #eligability for SMC 
         smc_spaq_quantity_3_11_months = number_of_spaq_for_smc_3_11_months, #calculated as: pop_number_children_u5 * smc_eligable * 0.18 * 4 - number 4 for 4 monthly rounds of smc? 0.18 as assuming 18% of the under 5 population are this age ❓this is not accounting for total doses for smc?         
         smc_spaq_quantity_12_59_months = spaq_for_smc_12_59_months, #calculated as: pop_number_children_u5 * smc_eligable * 0.77 * 4 - number 4 for 4 monthly rounds of smc? 0.18 as assuming 77% of the under 5 population are this age ❓this is not accounting for total doses for smc?     
         smc_sqaq_quantity = number_of_spaq_for_smc_total, #total needed summing the above two columns 
         # Seems fine to ignore first two and calculate for all 3-59 months (i.e. pop_u5_smc_population or column or pop_u5_smc_pop_coded * 4) and you'd get same value as this column which sums the two previous columns 
         # ❓ –this is the number of courses, not the number of tablets 
         
         ## PMC 
         pmc_eligable = eligible_78, #eligability for PMC  
         pmc_sp_quantity_u1 = number_of_sp_for_pmc_1yr, # pop_number_children_u1 * 0.85 * 0.75 * 12 
         pmc_sp_quantity_1_2_years = number_of_sp_for_pmc_1_2yrs, #pop_number_children_1_2_yrs * 0.85 * 0.75 * 2 * 12
         pmc_sp_total_quantity = number_of_sp_for_pmc_total, # sum above two values 
         # 1.	Children <1 year constitute 4% of the total population.
         # 2.	Children 1-2 years constitute 4% of the total population.
         # 3.	Antigen coverage rate = 85% (since immunization is being used as the contact point).
         # 4.	Since one in four children/infants in Nigeria is underweight), 25% of children <1 year will take half instead of one tablet, while 25% of children 1-2 years will take one instead of 2 tablets. 
         #    A factor of 0.75% was therefore used to quantify the required SP for each age group.
         # 5.	There will be 12 touch points within a calendar year, hence 12 doses were planned for each child/infant in a year. 
         # ❓❓❓
         
         ## IPTp 
         iptp_sp_total_quantity = number_of_sp_for_ip_tp, # ❓ total quantity per state is a static value (From GF?) the LGA value is state total SP needed  * prop PW in each LGA pop_number_pw / state preg women pop total
         # ❓  Unclear what the IPTp coverage assumption is (works out at 2-3 doses per PW)
         
         ## VACCINE 
         vacc_total_dose_quantity = number_of_vaccine_doses, # Proportion of population 5-36 months old  pop_number_children_5_36_mons * 4 doses 
         
         ## CASE MANAGEMENT 
         cm_rdt_kit_quantity = number_of_rdt_kits, # number of RDT kits needed: state level value is pre set by GF: pop_number_children_u5 / pop_number_children_u5 @ state * state level commodity
         cm_act_packs_quantity = number_of_act_packs, # number of ACTs packs needed: state level value is pre set by GF: proportional dist to lga level pop_number_children_u5 / pop_number_children_u5 @ state * state level commodity
         cm_iv_artesunate_quantity = number_of_iv_artesunate, # number of IV Artesunate needed: state level value is pre set by GF: proportional dist to lga level pop_number_children_u5 / pop_number_children_u5 @ state * state level commodity
         cm_ras_quantity = number_of_ras, # number of RAS needed: state level value is pre set by GF: proportional dist to lga level pop_number_children_u5 / pop_number_children_u5 @ state * state level commodity
         # The quantification of case management commodities was done by GF for each state without disaggregating 
         # to LGAs. We decided to disaggregate proportionately to the LGAs using the proportionate distribution
         # of the under-5 children among the LGAs with the assumption that the number of under-5 children would 
         # give the best estimate since transmission intensity is highest among them.
         # ❓Global fund methodology??
         
         ## COSTING DATA 
         ## The calculation of costs - for every item (procurement, operations etc) there is a corresponding cost link column - 
         ## this cost link column extracts the total national cost for that item that is calculated in the detailed budget 
         ## spreadsheet (where unit cost is * by total quantity needed)
         ## I'm going to leave the costlink columns out for now as we don't need them for doing the calculations manually and it 
         ## saves space in the dataframe  (utimatley this data is in the national row anyway)
         
         # ITNs
         itn_campaign_net_procurement_cost = procurement_96, # total cost of procuring nets for campaign calculated as: national cost (cost link) * proportion of total campaign nets in that LGA itn_campaign_net_proportion (number nets needed * unit cost)
         itn_campaign_net_distribution_procurement_cost = distribution_to_lg_as, # number of bales needed * unit cost, proportion of bales needed * costlink. The cost of distribution from the state’s store to the LGAs and from LGAs to the distribution hubs (costed per bale of ITNs) was separated from the operational (campaign) cost.
         itn_campaign_campaign_cost = campaign, # net campagin cost - costlink * proportion nets for campaign needed -	Calculated as total number of nets multiplied by cost of distribution per net (ITN Campaign-Operational cost per ITN)  
         itn_campaign_storage_hardware_cost = storage_of_hardwares, # national level value of storage of net hardware - per year set value at national level only 
         itn_campaign_total_cost = total_102, # sum of the above 
         itn_routine_net_procurement_cost = procuremnet, # total cost of procuring nets for routine dist: calculated as national cost (cost link - unit cost of net * total quantity) * proportion of total nets needed at spatial level
         itn_routine_net_operational_cost = operational_106, # total operational costs for routine dist: calcualted as national cost (cost link - unit cost per net * total quantity) * proportion of total nets needed at spatial level
         itn_routine_total_cost = total_107, # total cost for rouitine distribution - summed itn_routine_net_procurement_cost + itn_routine_net_operational_cost
         
         # IRS 
         # ❓ IRS total costs are not calculated anywhere -  the cost link for IRS is just the total for the proposed 17 LGAs target for IRS -  state level totals are just the proportion of total lgas to target in that state * national level total
         irs_total_cost = irs_111, 
         
         # LSM 
         lsm_bti_procurement_cost = bti, # total cost of procuring Bti chemicals for LSM: calculated as national cost (cost link - unit cost per kg of bit * total kg quantity) * proportion of bti needed at that spatial level
         lsm_operational_cost = operational_cost, # total cost of running LSM: calculated as national cost (cost link - unit cost per ward * number of wards) * proportion of LGAs at that spatial level
         lsm_total_cost = total_116, # sum cost of previous two 
         
         # SMC 
         smc_spaq_3_11_months_procurement_cost = procure_spaq_3_11_months, # total cost of procuring SP-AQ for SMC in children aged 3-11 months old: calculated as national cost (cost link - unit cost per spaq unit cost * total number of SPAQ needed) * proportion of doses needed at spatial level  
         smc_spaq_12_59_months_procurement_cost = procure_spaq_12_59_months, # total cost of procuring SP-AQ for SMC in children aged 12-59 months old: calculated as national cost (cost link - unit cost per sqaq unit cost * total number of SPAQ needed) * proportion of doses needed at spatial level
         smc_spaq_total_procurement_cost = procure_spaq_total, # sum of previous two values 
         smc_campaign_cost = smc_campaign, # total cost of SMC campaign: calculated as national cost (cost link - unit cost per child * total children targeted) * proportion of total u5 population targeted for SMC in that LGA 
         smc_total_cost = total_124, # total cost of SMC - summed totals from above  
         #❓ why are the costs of spaq packs different for each age category?  - drug sizes are different?? 
         
         # PMC  
         pmc_sp_procurement_cost = procurement_126, # total cost of procuring SP for PMC: calculated as national cost (cost link - unit cost per SP * total SP quantity) * proportion of total SP needed at spatial scale
         pmc_distribution_cost = distribution, # total cost of distributing SP for PMC to health facilities: calcualated as national cost (cost link - unit cost per child * total children targeted) * proportion of total children at spatial scale 
         pmc_total_cost = total_129, # total cost of PMC - summed totals from above 
         
         # IPTp 
         iptp_sp_procurement_cost = procurement_131, # total cost of procuring SP for IPTp: calculated as national cost (cost link - unit cost per SP * total sp needed) * proportion of total needed at spatial scale 
         iptp_sp_distribution_cost = distrbution, # total cost of distributing SP for IPTP: calcualted as national cost (cost link - unit cost per SP * total SP doses distributed) * proportion of total SP needed at spatial scale
         iptp_total_cost = total_134, # total cost of IPTp - summed totals from above 
         
         # VACCINE 
         vacc_procurement_cost = procurement_136, # total cost of procuring vaccine doses: calculated as national cost (cost link - unit cost per dose * total doses needed) * proportion of total doses needed at spatial scale 
         vacc_operational_cost = operational_138, # total cost of vaccine delivery: calcualted as national cost (cost link - unit cost per child * total children targeted) * proportion of total children targeted at spatial scale
         vacc_total_cost = total_139, # total cost - summed from above
         
         # CASE MANAGEMENT 
         cm_rdt_kits_procurement_cost = rdt_kits_procurement, # total cost of procuring rdt kits: calculated as national cost (cost link - unit cost per kit * total rdt kits needed) * proportion of total kits needed at spatial scale 
         cm_rdt_kits_distribution_cost = rdt_kits_distribution, # total cost of distributing rdt kits: calculated as national cost (cost link - unit cost per kit distribution* total rdt kits needed) * proportion of total kits needed at spatial scale 
         cm_rdt_kits_total_cost = total_for_rdt_kits, # total summed from above
         cm_al_procurement_cost = al_procurement, # total cost of procuring AL doses: calculated as national cost (cost link - unit cost per AL dose * total AL needed) * proportion of total AL needed at spatial scale 
         cm_al_distribution_cost = al_distribution, # total cost of distributing AL doses: calculated as national cost (cost link - unit cost per AL dose * total AL needed) * proportion of total AL needed at spatial scale 
         cm_al_total_cost = total_for_al, # total cost AL from above
         cm_iv_artesunate_procurement_cost = iv_artesunate_procurement, #  total cost of procuring IV artesunate: calculated as national cost (cost link - unit cost per per 60mg powder * total needed) * proportion of total needed at spatial scale  
         cm_iv_artesunate_distribution_cost = iv_artesunate_distribution, # total cost of distributing IV artesunate: calculated as national cost (cost link - unit cost per per 60mg powder * total needed) * proportion of total needed at spatial scale  
         cm_iv_artesunate_total_cost = total_for_iv_artesunate, # total summed from above 
         cm_ras_prcurement_cost = ras_procurement, #  total cost of procuring RAS: calculated as national cost (cost link - unit cost per RAS * total needed) * proportion of total needed at spatial scale  
         cm_ras_distribution_cost = ras_distribution, #  total cost of procuring RAS: calculated as national cost (cost link - unit cost per RAS * total needed) * proportion of total needed at spatial scale  
         cm_ras_total_cost = total_for_ras, #  total cost of procuring RAS: calculated as national cost (cost link - unit cost per per RAS * total needed) * proportion of total needed at spatial scale  
         cm_eqa_national_cost = national_level_eqa_for_states, # National level value for quality assessment not proportioned out to spatial scales
         cm_public_total_cost = total_for_cm, # total costs all summed
         cm_private_total_cost = private_sector_cm_estimate, # 57.7% of the public sector cost?
         
         # SUPPORT SERVICES 
         # SBC
         ss_sbc_total_cost = sbc, # total cost for Social behaviour change: calculated as national cost (cost link - summed total of unit costs of different activites * quantity - calculations here are slightly confusing)
         # There are multiple activites that make up the SBC line item not described in this spreadsheet apart from in the detailed budget  
         # Conduct 10 compound meetings per ward on malaria prevention and treatment quarterly
         #      Facilitation fee-LGA - per person per day- 2 people 4 times * number of wards 
         #      Transport to and fro between LGAs and state capital - per person - 2 people 4 times * number of wards
         # Conduct 1 community dialogue per ward on malaria prevention and treatment quarterly
         #      Facilitation fee-LGA  - per person per day - 2 people 4 times * number of PHCs 
         #      Transport to and fro between LGAs and state capital - per person - 2 people 4 times * number of wards
         #      Refreshments - per person per day - 40 people 4 times * number of wards 
         # proportionally split up at the spatial level by national cost * proportion wards @ spatial scale 
         
         # M&E 
         me_national_level_cost = national_level_m_e, # total cost for National level activites for M&E - overall costs of strengthening the NHMIS at the national level 
         # cost calculation 
         # Print and distribute NHMIS tools  to health facilities 
         #      NHMIS Tools Printing-Cost per health facility - per facility - unit cost * number of facilities * 1 time
         #      NHMIS Tools Distribution-Cost per LGA - per LGA - unit cost * number of LGAs * 1 time
         # Conduct iMSV to States
         #      NMEP iMSV to States-Cost per State - per state - unit cost * number of states * 1 time
         # summed 
         # this is distributed out at lower spatial levels just kept at the national level 
         me_state_level_cost = state_lga_level_m_e, # total cost for state level activites for M&E  - calculated as national cost (cost link) * proportion of LGAs at that spatial scale 
         # cost calculation 
         # Strengthen NHMIS at the state level
         # State and LGA M&E Activities-Cost per LGA - unit cost per LGA * number of LGAs 
         me_total_cost = overall_m_e, # sum of above two items  
         
         # CAPACITY BUILDING 
         cb_national_level_cost = national_level_cb, # capicity building at the national level - national only cost 
         # cost calculation 
         # Carry out human resouce capacity building at the national level
         # Cunduct a 5-day Residential National Training of Trainer (NToT)on Malaria Care Management (MCM)-60 participants 
         #      Residential Workshop in Abuja-Cost per person per day - 1 workshop * 60 people * 5 days 
         cb_state_level_cost = state_level_cb, # capicity building at the state level - calculated as national cost (cost link) * proportion of states at that spatial level  
         # cost calculation 
         # Carry out human resouce capacity building at the state level
         # Cunduct a 5-day Residential State Training of Trainer (SToT) on MCM (40 participants)
         #      Residential Workshop in State Capital-Cost per person per day - 1 workshop * 40 people * 5 days * number of states 
         cb_shf_thf_level_cost = seconday_tertiary_level_cb, # capacity building at the secondary and tertiary health facility level - calculated as national level * proportion of SFHs and THFs at spatial level 
         # Carry out human resouce capacity building at the secondary/tertiary health facility level
         # Cunduct a 3-day Residential Training on MCM for healtcare providers in secondary and tertiary health facilities (5 participants per facility)
         #      Residential Workshop in State Capital-Cost per person per day - 1 workshop * 5 people * number of facilities * 3 days 
         cb_lga_phc_level_cost = lga_phc_level_cb, # capacity building at the LGA/ Primary health care level - caclulated as national level * proportion of PHcs at spatial level 
         # cost calculation 
         # Carry out human resouce capacity building at the LGA/PHC level
         # Cunduct a 3-day Non-residential Training on MCM for healtcare providers at the primary health facilities (2 participants per facility)
         #      Non-residential Workshop at LGA level-Cost per person per day - 1 workshop * 2 people * Number of PHCs * 3 days 
         cb_total_cost = overall_capacity_building, # summed capacity building total of all items above  
         
         # GOVERNANCE & COORDINATION 
         gc_national_level_cost = national_level_coordination, # National level only GC costs 
         # cost calculation 
         # Coordinate malaria programme at the national level
         # Cunduct a 3-day Annual Malaria Programme Review-60 participants
         #      Residential Workshop in Abuja-Cost per person per day - 1 workshop * 60 people * 3 days 
         # Cunduct a 5-day Workshop on NMEP MOP Development-60 participants
         #      Residential Workshop in Abuja-Cost per person per day - 1 workshop * 60 people * 5 days 
         gc_state_level_cost = state_level_coordination, # State level GC costs only  - national cost (cost link) * proprotion of states at spatial level 
         # cost calculation - summed
         # Cunduct a 2-day Quarterly Malaria Programme Review-60 participants
         #      Residential Workshop in State Capital-Cost per person per day - 4 workshops * 60 people * 2 days * Number of states
         # Cunduct a 1-day Quarterly mTWG Meeting-60 participants
         #      Non-residential Workshop in State Capital-Cost per person per day - 4 workshop * 60 people * 1 day * Number of states 
         # Cunduct a 1-day Quarterly State-LGA Coordination Meeting-60 participants
         #      Residential Workshop in State Capital-Cost per person per day - 4 workshops * 60 people * 1 day * Number of states
         # Cunduct a 5-day Workshop on SMEP MOP Development-60 participants
         #      Residential Workshop in State Capital-Cost per person per day - 1 workshop * 60 people * 5 days * Number of states
         # Conduct Quarterly ISS from the state to secondary facilities and LGAs
         #      Stationery - per person cost - 50 people * number of states * 4 times per year 
         #      Refreshments - per person per day - 50 people * number of states * 4 times per year
         #      Transport to and fro between LGAs and state capital per person cost - 50 people * number of states * 4 times per year 
         gc_lga_level_cost = lga_level_coordination, # LGA level GC costs - national level total (cost link) * proportion of total LGAs at that spatial level 
         # cost calculation
         # Coordinate malaria programme at the LGA level
         # Conduct Quarterly ISS from the LGA to PHCs
         #      Stationery - per person cost - 30 people * number of LGAs * 4 times per year
         #      Refreshments - per person per day cost - 30 people * number of LGAs * 4 times per year
         #      Transport within LGA - per person - 30 people * number of LGAs * 4 times per year
         gc_total_cost = governance_coordination, # total cost for GC activities - above values summed  
         
         # RESOURCE MOBILIZATION 
         rm_total_cost = resource_mobilization, # total cost for resource mobilisation - national level only value  
         # cost calculation  
         # Advocate for resource mobilization
         # Pay advocacy visits to state governors for resource mobilization for malaria programme
         #      NMEP Advocacy Visit to State Governors-Cost per state - 37 states * unit cost 
         
         # TOTALS 
         cost_total_ngn = ngn, #summary of all costs at spatial scale in Naira 
         cost_total_usd = usd #summary of all costs at spatial scale in USD 
        )|> 
  separate(spatial_level, into = c("spatial_level_1", "spatial_level_2", "spatial_level_3"), 
           sep = "\\.", fill = "right") %>%
  mutate(across(starts_with("spatial_level"), ~ as.numeric(as.character(.)))) |> 
  mutate(spatial_level_2 = case_when(spatial_level_2 >=10 ~ round(spatial_level_2 / 1e15,0), 
                                     TRUE ~ spatial_level_2))

#-SAVE THIS DATA-----------------------------------------------------------------------------------------------
write.csv(working_states_lga, "exploratory-steps/data/working-data/codable-pragmatic-plan-template-october.csv", row.names = FALSE)


#-SPATIAL SCALE SUMMARIES---------------------------------------------------------------------------------------
national_data_extract <- 
  working_states_lga |> 
  filter(spatial_name == "Country") |> 
  rename(country = spatial_name)

geopolitical_zone_data_extract <- 
  working_states_lga |> 
  filter(is.na(spatial_level_2), is.na(spatial_level_3), !is.na(spatial_level_1))|> 
  rename(geopolitical_zone = spatial_name)

state_data_extract <- 
  working_states_lga |> 
  filter(is.na(spatial_level_3), !is.na(spatial_level_2), !is.na(spatial_level_1))|> 
  # remove disputed area in taraba state from state list 
  filter(spatial_name != "Disputed Area in Taraba State") |> 
  rename(state = spatial_name)

lga_data_extract <- 
  working_states_lga|> 
  filter(!is.na(spatial_level_3), !is.na(spatial_level_2), !is.na(spatial_level_1)) |> 
  rename(lga = spatial_name)

# save data 
write.csv(national_data_extract, "exploratory-steps/data/working-data/codable-national-data-october.csv", row.names = FALSE)
write.csv(geopolitical_zone_data_extract, "exploratory-steps/data/working-data/codable-geopolitical-zone-data-october.csv", row.names = FALSE)
write.csv(state_data_extract, "exploratory-steps/data/working-data/codable-state-data-october.csv", row.names = FALSE)
write.csv(lga_data_extract, "exploratory-steps/data/working-data/codable-lga-data-october.csv", row.names = FALSE)

# check the campaign number of nets needed is just pop / 1.8 
working_states_lga$net_scaler <- working_states_lga$pop_2025_projected / working_states_lga$itn_campaign_net_quantity 

working_states_lga$net_scaler[which(working_states_lga$itn_campaign_eligable == 1)]
# this is just the 1.8 nets per person assumption ✅

#-INTERVENTION MIX DATA FOR MONIQUE-----------------------------------------------------------------------------

# formatting data to the LGA level 
int_mix <- 
  working_states_lga |> 
  select(spatial_level, spatial_name, 
         starts_with("code"), itn_campaign_eligable, 
         itn_routine_eligable, smc_eligable, pmc_eligable
         ) |> 
  separate(spatial_level, into = c("spatial_level_1", "spatial_level_2", "spatial_level_3"), 
           sep = "\\.", fill = "right") %>%
  mutate(across(starts_with("spatial_level"), ~ as.numeric(as.character(.)))) 


int_mix$spatial_level_2 <- 
  ifelse(int_mix$spatial_level_2 >= 10, round(int_mix$spatial_level_2 / 1e15,0), int_mix$spatial_level_2)

# LGA intervention mix
int_mix_lga <- 
  int_mix |> 
  # remove anything that is NA in spatial level 3 i.e isn't an LGA 
  filter(!is.na(spatial_level_3 )) |> 
  rename(lga = spatial_name)

# state with joing code for LGAs 
int_mix_state <- 
  int_mix |> 
  filter(is.na(code_intervention_mix)) |> 
  filter(!is.na(spatial_level_1), !is.na(spatial_level_2)) |> 
  select(spatial_level_1, spatial_level_2, state = spatial_name) |> 
  # remove disputed area in taraba state from state list 
  filter(state != "Disputed Area in Taraba State")

# Joing data together based on spatial_level_2 (state) data 
int_mix_full <- 
  int_mix_state |> 
  left_join(int_mix_lga, by=c("spatial_level_1","spatial_level_2")) |> 
  select(-starts_with("spatial_level"))

write.csv(int_mix_full, "intervention-mix-for-monique-update.csv", row.names = FALSE)
