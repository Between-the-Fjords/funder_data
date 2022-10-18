# Make data dictionaries

# load libraries
source(file = "R/load_packages.R")

# load clean data
#source("R/data_dic/download_clean_data.R")

# data dictionary function
source("R/data_dic/make_data_dictionary.R")

# read in data description table
description_table <- read_excel("R/data_dic/data_description.xlsx")

#************************************************************************
#************************************************************************
### 1 BIOMASS (EXAMPLE)

#biomass <- read_csv("seed_predation/data/Funder_clean_biomass_2022.csv")


# biomass_dic <- make_data_dictionary(data = biomass,
#                                       description_table = description_table,
#                                       table_ID = "biomass")


#************************************************************************

