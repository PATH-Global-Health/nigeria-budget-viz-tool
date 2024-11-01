#-------------------------------------------------------------------------------
# Cleaning shapefiles to work with programmatic data 
# Aim: alter name values in the shape file to match those in costing data 
#-------------------------------------------------------------------------------

# Shape files
country_outline <- sf::st_read("exploratory-steps/data/shapefiles/country/country_shapefile.shp")
state_outline   <- sf::st_read("exploratory-steps/data/shapefiles/state/state_shapefile.shp")
lga_outline     <- sf::st_read("exploratory-steps/data/shapefiles/lga/lga_shapefile.shp")

name_data <- read.csv("intervention-mix-for-monique-update.csv")

# function to check discordant names between data and shape file and reformatting shapefile to match data
discordant_names <- function(names1, names2){
  
  different_names <- setdiff(names1, names2)
  
  return(print(different_names))
  
}

# check names state
discordant_names(lga_outline$state, name_data$state) 
lga_outline$state[which(lga_outline$state == "Akwa-Ibom")] <- "Akwa Ibom"
discordant_names(lga_outline$state, name_data$state) 
discordant_names(name_data$state, lga_outline$state) 

# check names LGA
discordant_names(lga_outline$lga, name_data$lga)
lga_outline$lga <- gsub("-", " ", lga_outline$lga) #remove all '-'
name_data$lga <- gsub("-", " ", name_data$lga) #remove all "-"
name_data$lga <- gsub('[[:digit:]]+', '', name_data$lga) #remove numeric values from names
name_data$lga <- stringr::str_to_title(name_data$lga)    #make all names sentence case
lga_outline$lga <- stringr::str_to_title(lga_outline$lga) 
discordant_names(lga_outline$lga, name_data$lga)

lga_outline <- 
  lga_outline |>
  mutate(lga = case_when(lga == "Abuja Municipal Area Council" ~ "Abuja Municipal",
                         lga == "Ayedaade" ~ "Aiyedade",
                         lga == "Ayedire" ~ "Aiyedire",
                         lga == "Aiyekire (Gbonyin)" ~ "Aiyekire (Gboyin)",
                         lga == "Ajeromi/Ifelodun" ~ "Ajeromi Ifelodun",
                         lga == "Arewa" ~ "Arewa Dandi",
                         lga == "Atisbo" ~ "Atigbo",
                         lga == "Barkin Ladi" ~ "Barikin Ladi",
                         lga == "Bekwarra" ~ "Bekwara",
                         lga == "Birniwa" ~ "Biriniwa",
                         # lga == "Birnin Kudu" ~ "Birni Kudu",
                         lga == "Busari" ~ "Bursari", 
                         lga == "Dambam" ~ "Damban", 
                         lga == "Danbatta" ~ "Dambatta", 
                         # lga == "Dange Shuni" ~ "Dange Shnsi", 
                         lga == "Efon" ~ "Efon Alayee", 
                         # lga == "Yewa North" ~ "Egbado North", 
                         # lga == "Yewa South" ~ "Egbado South", 
                         lga == "Ezinihitte Mbaise" ~ "Ezinihitte", 
                         # lga == "Ganye" ~ "Ganaye", 
                         # lga == "Girei" ~ "Gireri", 
                         # lga == "Gwadabawa" ~ "Gawabawa", 
                         lga == "Ibadan North East" ~ "Ibadan Central (Ibadan North East)", 
                         lga == "Ido Osi" ~ "Idosi Osi", 
                         lga == "Ifako/Ijaye" ~ "Ifako Ijaye", 
                         lga == "Ile Oluji/Okeigbo" ~ "Ile Oluji Okeigbo", 
                         # lga == "Ilejemeje" ~ "Ilemeji", 
                         lga == "Ilesa East" ~ "Ilesha East", 
                         lga == "Ilesa West" ~ "Ilesha West", 
                         lga == "Isuikwuato" ~ "Isuikwato", 
                         lga == "Karim Lamido" ~ "Karin Lamido", 
                         # lga == "Mai'Adua" ~ "Mai'adua", 
                         lga == "Makarfi" ~ "Markafi", 
                         # lga == "Matazu" ~ "Matazuu", 
                         lga == "Nafada" ~ "Nafada (Bajoga)", 
                         lga == "Obi Nwga" ~ "Obi Nwa", 
                         lga == "Obio/Akpor" ~ "Obia/Akpor", 
                         lga == "Ogori/Magongo" ~ "Ogori/Mangongo", 
                         lga == "Olamaboro" ~ "Olamabolo", 
                         lga == "Oshodi/Isolo" ~ "Oshodi Isolo", 
                         lga == "Otukpo" ~ "Oturkpo", 
                         # lga == "Paikoro" ~ "Pailoro", 
                         lga == "Sagamu" ~ "Shagamu", 
                         lga == "Shongom" ~ "Shomgom", 
                         # lga == "Takai" ~ "Takali", 
                         lga == "Tarmuwa" ~ "Tarmua", 
                         lga == "Umunneochi" ~ "Umu Nneochi", 
                         lga == "Onuimo" ~ "Unuimo", 
                         lga == "Danko/Wasagu" ~ "Wasagu/Danko", 
                         lga == "Yenagoa" ~ "Yenegoa", 
                         lga == "Zangon Kataf" ~ "Zango Kataf", 
                         TRUE ~ lga))

# check again 
discordant_names(lga_outline$lga, name_data$lga)

# check the other direction 
discordant_names(name_data$lga, lga_outline$lga)

# save the new shapefiles to working data 
st_write(lga_outline, "exploratory-steps/data/working-data/lga_shapefile.shp")
st_write(state_outline, "exploratory-steps/data/working-data/state_shapefile.shp")
st_write(country_outline, "exploratory-steps/data/working-data/country_shapefile.shp")
