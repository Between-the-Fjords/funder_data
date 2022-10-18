# Download clean data from OSF
# install.packages("devtools")
#devtools::install_github("Between-the-Fjords/dataDownloader")
library(dataDownloader)

node <- "tx9r2"

# 1 Biomass
get_file(node = node,
         file = "Funder_biomass_2015-2021.csv",
         path = "clean_data/vegetation",
         remote_path = "1) Vegetation")

