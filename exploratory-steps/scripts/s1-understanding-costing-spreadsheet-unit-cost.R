#-------------------------------------------------------------------------------
# script to replicate and understand costing spreadsheet 
# Aims: read in the formatted excel file and extract data into 
#       usable format
#-------------------------------------------------------------------------------

# load packages  
library(tidyverse)
library(janitor)

#-Unit Cost Data----------------------------------------------------------------

# Unit cost sheet 
# Extract the data from row 9 as this is the table formatted data
# Some of the column headings are split over two rows so I'm also cleaning 
# those column names up here  
# Data also has additional rows of formula copied columns with no reference 
# data so removing those empty rows from the data frame
raw_unit_cost <- 
  readxl::read_xlsx("exploratory-steps/data/Pragmatic Plan Template-09102024.xlsx", 
                    sheet = "Unit Cost", 
                    skip = 9 # skip the first 9 rows data to be 
                             # extracted separately 
                   ) |> 
  clean_names() |> 
  rename(reference_year = year, 
         reference_currency = currency, 
         reference_cost = cost, 
         reference_cost_lcu = lcu
         )

print(raw_unit_cost |> filter(is.na(resource)), n=500) # empty rows 

raw_unit_cost <- 
  filter(raw_unit_cost, !is.na(resource)) # remove empty rows

exchange_rate <- 1600 # 1USD = 1,666 Naira 

# duplicate row 
# IPTp-SP-Distribution cost per SP	per SP	2024	NGN	202	0.13	NMEP/GC7	NMEP
# IPTp-SP-Routine Distribution cost	per SP	2024	NGN	202	0.13	NMEP/GC7	NMEP
# Keep Routine Distribution as this is the row referenced in the excel 
raw_unit_cost <- 
  filter(raw_unit_cost, resource !="IPTp-SP-Distribution cost per SP")


# column descriptions  
# resource: procurement, operational and distribution activities 
#           assocaited with each intervention 
# unit: unit of cost associated with the activity 
# rederence_year: year of data for costing 
# reference_currency: currency cost data was calculated in - NGN currently 
# reference_cost: linked cost value for unit 
# reference_cost_lcu: lcu = local currency unit - cost in LCU linked value: NGN 
# usd_equivalence: unit cost in USD assuming conversaion rate of 
#                 1 USD = 1,600 NGN
# notes: where the costing data was sourced from 
# source: again where the costing data was sources from  

# some of this `reference_cost` data is static - in that it has been manually
# entered into the spread sheet cells and some of this isn't with values drawn
# in from the 'cost inputs for... 
# SMC Campaign, ITN Campaign, IRS, LSM CM commodities, EQA for CM, NHMIS Tools 
# Printing, NHMIS Tools Distribu, M&E activities, PMC commodities, PMC SP 
# Distribution, LLIN & SP, Workshops 

# 🚩 Right now use the raw values but then build in calculations of these 

# reformat the unit cost data spreadsheet 
# splitting the resrouce column into matchable distinct items 
working_unit_cost <- 
  raw_unit_cost |> 
  mutate(
    intervention = 
      case_when(
        grepl("ITN Campaign", resource) ~ "ITN Campaign",
        grepl("ITN Routine Distribution", resource) ~ "ITN Routine Distribution",
        grepl("IRS", resource) ~ "IRS",
        grepl("LSM", resource) ~ "LSM",
        grepl("Malaria Vaccine", resource) ~ "Vaccine",
        grepl("SMC", resource) ~ "SMC",
        grepl("PMC", resource) ~ "PMC",
        grepl("IPTp", resource) ~ "IPTp",
        grepl("Case Management", resource) ~ "Case Management",
        TRUE ~ "Support Services"
      ), 
    cost_category = 
      case_when(
        grepl("ITN Campaign-Procurement per ITN \\(Dual AI\\)", resource) ~ "Procurement",
        grepl("ITN Campaign-Cost of distribution from State to LGA and from LGA to DHs", resource) ~ "Procurement",
        grepl("ITN Campaign-Operational cost per ITN", resource) ~ "Campaign",
        grepl("ITN Campaign-Storage of Hardwares", resource) ~ "Storage", 
        grepl("ITN Routine Distribution-Pocurement cost per ITN \\(Dual AI\\)", resource) ~ "Procurement", 
        grepl("ITN Routine Distribution-Operational cost per ITN", resource) ~ "Operational", 
        grepl("IRS-Cost per LGA", resource) ~ "Combined",
        grepl("LSM-Bti-Procurement cost per kg", resource) ~ "Procurement",
        grepl("LSM-Operational cost per LGA", resource) ~ "Operational",
        grepl("Malaria Vaccine-Procurement cost", resource) ~ "Procurement",
        grepl("Malaria Vaccine-Operational cost", resource) ~ "Operational",
        grepl("Procurement cost per SP", resource) ~ "Procurement",
        grepl("SMC-Campaign cost per child", resource) ~ "Campaign",
        grepl("SP-Procurement", resource) ~ "Procurement",
        grepl("SP-Routine Distribution cost", resource) ~ "Distribution", 
        grepl("SP-Routine Distribution cost", resource) ~ "Distribution", 
        grepl("Case Management", resource) & grepl("Procurement", resource) ~ "Procurement",
        grepl("Case Management", resource) & grepl("Distribution", resource) ~ "Distribution",
        grepl("EQA", resource) ~ "National Level EQA",
        
        grepl("NHMIS Tools Printing-Cost per health facility", resource) ~ "M&E",
        grepl("NHMIS Tools Distribution-Cost per LGA", resource) ~ "M&E",
        grepl("NMEP iMSV to States-Cost per State", resource) ~ "M&E",
        grepl("State and LGA M&E Activities-Cost per LGA", resource) ~ "M&E",
        grepl("NMEP Advocacy Visit to State Governors-Cost per state", resource) ~ "Resource Mobilization",
        grepl("Venue", resource) ~ NA,
        grepl("Training", resource) ~ NA,
        grepl("Photocopy", resource) ~ NA,
        grepl("Printing", resource) ~ NA,
        grepl("Refreshments", resource) ~ "Governance and Coordination", # this is also in SBC costs so need to replicate this row for SBC too 
        grepl("Tea break", resource) ~NA,
        grepl("DSA", resource) ~ NA,
        grepl("Stationery", resource) ~ "Governance and Coordination",
        grepl("Workshop", resource) ~ "Governance and Coordination",
        grepl("Transport within LGA", resource) ~ "Governance and Coordination",
        grepl("Transport to and fro between LGAs and state capital", resource) ~ "Governance and Coordination", # this is also in SBC costs so need to replicate this row for SBC too
        grepl("Transport", resource) ~ "Transport",
        grepl("Facilitation fee", resource) ~ "SBC", # this is also in IRS costs so need to replicate this row for IRS too 
        TRUE ~ NA
      ), 
    intervention_subtype_cat = 
      case_when(
        grepl("ITN", resource) ~ "Dual-AI",
        grepl("Bti", resource) ~ "Bti", 
        grepl("SPAQ-3-11 months", resource) ~ "SPAQ-3-11 months", 
        grepl("SPAQ-12-59 months", resource) ~ "SPAQ-12-59 months", 
        grepl("SP", resource) ~ "SP", 
        grepl("AL", resource) ~ "AL", 
        grepl("Artesunate injections", resource) ~ "Artesunate injections", 
        grepl("RAS", resource) ~ "Rectal Artesunate Suppositories (RAS)", 
        grepl("RDT kits", resource) ~ "RDT kits & consumables", 
        grepl("EQA", resource) ~ "EQA", 
        grepl("NHMIS Tools Printing", resource) ~ "NHMIS Tools Printing",
        grepl("NHMIS Tools Distribution", resource) ~ "NHMIS Tools Distribution",
        grepl("NMEP iMSV", resource) ~ "iMSV to States",
        grepl("State and LGA M&E Activities", resource) ~ "M&E Activities",
        grepl("NMEP Advocacy Visit", resource) ~ "NMEP Advocacy Visit to State Governors",
        grepl("Small hall-20 capacity", resource) ~ "Venue-Small hall-20 capacity",
        grepl("Medium size hall-50 capacity", resource) ~ "Venue-Medium hall-50 capacity",
        grepl("Big hall-70 capacity", resource) ~ "Venue-Big hall-70 capacity",
        grepl("Auditorium", resource) ~ "Venue-Auditorium",
        grepl("Training/workshop documents/materials", resource) ~ "Training/workshop documents/materials",
        grepl("Training on malaria microscopy", resource) ~ "Training on malaria microscopy",
        grepl("Printing of document", resource) ~ "Printing of document",
        grepl("Photocopy of document", resource) ~ "Photocopy of document",
        grepl("Printing of booklets", resource) ~ "Printing of booklets",
        grepl("Refreshments", resource) ~ "Refreshments",
        grepl("Tea break", resource) ~ "Tea break",
        grepl("Lunch", resource) ~ "Lunch",
        grepl("DSA", resource) ~ "DSA",
        grepl("Stationery", resource) ~ "Stationery",
        grepl("Residential Workshop", resource) ~ "Residential Workshop",
        grepl("Non-residential Workshop", resource) ~ "Non-residential Workshop",
        grepl("Flight ticket", resource) ~ "Flight ticket-Return within Nigeria",
        grepl("Airport Taxi", resource) ~ "Airport Taxi (x4)",
        grepl("Travel-Ground", resource) ~ "Travel-Ground",
        grepl("Advocacy kits", resource) ~ "Advocacy kits",
        grepl("Facilitation fee", resource) ~ "Facilitation fee",
        grepl("LSM", resource) ~ NA, 
        grepl("IRS", resource) ~ NA,
        grepl("Vaccine", resource) ~ "R21",
        grepl("SMC-Campaign", resource) ~ NA,
        TRUE ~ resource  # Default value for rows that don't match any condition is just the resource value where no removal needed
      ),
    cost_spatial_level = 
      case_when(
        grepl("storage", resource) ~ "National",
        grepl("Abuja", resource) ~ "Abuja",
        grepl("State capital", resource) ~ "State capital",
        grepl("State Capital", resource) ~ "State capital",
        grepl("state capital", resource) ~ "State capital",
        grepl("state", resource) ~ "State",
        grepl("State", resource) ~ "State",
        grepl("LGA", resource) ~ "LGA",
        grepl("National", resource) ~ "National", 
        grepl("ITN Campaign-Cost of distribution from State to LGA and from LGA to DHs", resource) ~ NA, 
        TRUE ~ NA
      )
    ) |> 
  # reorder and add in duplicate rows where needed (where cost relates to multiple items)
  select(resource, intervention, cost_category, intervention_subtype_cat, cost_spatial_level, unit,
         ngn_cost=reference_cost, usd_cost = usd_equivalence, cost_year = reference_year, 
         source) 

# add additional lines for items that cross two intervention cats 
# Filter the rows to replicate
rows_to_replicate <- 
  working_unit_cost %>%
  filter(grepl("Facilitation fee-LGA", resource) | 
           grepl("Refreshments", resource) | 
           grepl("Transport to and fro between LGAs and state capital", resource))

# 2. Modify the column 'cost_type' for these replicated rows
# For example, change 'cost_type' to "New Value" for the replicated rows
modified_rows <- 
  rows_to_replicate %>%
  mutate(cost_category = case_when(cost_category == "Governance and Coordination" ~  "SBC", 
                                   cost_category == "SBC" ~  "IRS"))

# 3. Append the modified rows back to the original dataset
# Combine the original dataset with the modified rows
working_unit_cost <- bind_rows(working_unit_cost, modified_rows)  

# ❓ ROUTINE NET DISTRIBUTION COST IS JUST ASSMED AS 0.25 OF ITN Routine Distribution-Pocurement cost per ITN (Dual AI)
# NET COST ??

#-SAVE UNIT COST DATA---------------------------------------------------------------------
write.csv(working_unit_cost, "exploratory-steps/data/working-data/codable-unit-costs-october.csv", row.names = FALSE)


## I will just use these unit costs for now and then I want to add in 
## calculations of unit costs from line list items 







#-Cost Line List data per intervention------------------------------------------

# test <- tidyxl::xlsx_cells("data/Pragmatic Plan Template-23092024.xlsx", sheets = "Unit Cost")

# SMC cost input data 
# This is a line list of costs associated with Kwara States 2023 SMC 
# operational budget  
# the assumption here is that costs are equivalent across all states and lgas 
# Work on estimating these costs in R next 

