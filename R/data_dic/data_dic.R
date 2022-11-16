# Make data dictionaries

# load libraries
source(file = "R/load_packages.R")

# load clean data
#source("R/data_dic/download_clean_data.R")

# data dictionary function
# if needed install the remotes package
#install.packages("remotes")

# then install the dataDocumentation package
#remotes::install_github("audhalbritter/dataDocumentation")

# and load it
library(dataDocumentation)

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

