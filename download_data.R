# Author: Wazir Khan Ahmadzai

# ---------------- Download ETools Data ---------------------------- #

# set filter here, if multiple countries, seperate them by comma (,), if 
# all countries then just write "all"
# Please also check the spelling of country names, they should be written in 
# small letters, without spaces, space is _ (underscore)

country_name <- "south_sudan"

# status filter 
# if all then set to "all" else provide a string vector e.g. c("active", "signed")
status_filter <- "all"


# ---------------- Install and Load the Required Packages ----------- #
# function to install and include the required packages
install_pkgs <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Lists required packages
packages <- c("httr", "dplyr", "tidyr", "jsonlite", "stringr")


# Loads packages
invisible(install_pkgs(packages))

# Removes intermediary objects from the environment
rm(ipak)

# ----------------- end of installing and loading the packages ----- #

# ---------------- Load the functions files ------------------------ #
source("functions.R")


# downloading partners information -------------------------------------
# change -serializer to v2 from std if you want to download it for PBI without location information

interventions_url <- "interventions/?-serializer=std&page_size=1000"

if(tolower(country_name) != "all") {
  interventions_url <-  paste0(interventions_url, "&country_name=", country_name)
}

# call the fromEtools function
interventions <- fromEtools(interventions_url, "results")
interventions <- clean_colnames(interventions)

if(status_filter != "all") {
  
  interventions <- interventions %>% subset(status %in% status_filter)
  
}

# removing the list columns, to have a flat table for exporting
all_interventions <- interventions %>% select(!c(cp_outputs_data, offices_data,
                                                 partner_focal_points_data, 
                                                 sections_data, 
                                                 unicef_focal_points_data, 
                                                 locations_data))

all_interventions$amendment_types <- gsub("\\]", "", all_interventions$amendment_types)
all_interventions$amendment_types <- gsub("\\[", "", all_interventions$amendment_types)
all_interventions$amendment_types <- gsub("'", "", all_interventions$amendment_types)

# creating the interventions_locations data 
interventions_locations <- interventions %>% select(id, intervention_id, partner_id, 
                                                    reference_number, partner_vendor_number, 
                                                    locations_data)
interventions_locations <- unnest(interventions_locations, locations_data, names_repair = "unique", keep_empty = TRUE)

# saving the data as CSV files

# convert date-time into number to be the suffix of the file we export
file_suffix <- round(as.numeric(Sys.time()))

# export the all_interventions
write.csv(all_interventions, paste("exports/interventions_", file_suffix, ".csv", sep = ""), na="", row.names = FALSE)

# export the interventions_locations, you can connect both these files with different ids
write.csv(interventions_locations, paste("exports/interventions_locations_", file_suffix, ".csv", sep = ""), na="", row.names = FALSE)

# clean the memory if needed, just uncomment
# rm(list = ls())