library(tidyverse)
library(tidylog)
library(lubridate)

PRS_probes<-read_csv2(file = "data/FUNDER_raw_PRS_probes_2021.csv", skip = 4) %>%
  filter(`WAL #` != "Method Detection Limits (mdl):") %>%
  rename(ID = `Sample ID`)

# detection limits for the elements
detection_limit <- read_csv2(file = "data/FUNDER_raw_PRS_probes_2021.csv", skip = 4) %>%
  slice(1) %>%
  select(`NO3-N`:Cd) %>%
  pivot_longer(cols = everything(), names_to = "elements", values_to = "detection_limit")

install.packages("remotes")

# then install the dataDocumentation package
remotes::install_github("audhalbritter/dataDocumentation")

# and load it
library(dataDocumentation)

#Metadata
meta_data<-create_funder_meta_data()

prs_data<-PRS_probes %>%
  filter(ID!="CONTROL") %>%
  #we deleted the probes control without any treatment that was added by the lab
  separate(col=ID,into=c("siteID","blockID","treatment"),sep="-")%>%
  mutate(treatment=recode(treatment,
                          'BF'='FB',
                          'BG'= 'GB',
                          'FG'='GF',
                          'BFG'='FGB'
  )) %>%
  mutate(siteID=str_to_title(tolower(siteID))) %>%
  mutate(plotID=paste0(siteID,blockID,treatment)) %>%
  select(-siteID,-blockID) %>%
  left_join(meta_data,by=c("treatment","plotID")) %>%
#Make real dates
rename(burial_date = `Burial Date`, retrieval_date = `Retrieval Date`) %>%
mutate(burial_date = ymd(burial_date),
       retrieval_date = ymd(retrieval_date),
       burial_length = retrieval_date - burial_date) %>%
  pivot_longer(cols = `NO3-N`:Cd, names_to = "elements", values_to = "value") %>%
  left_join(detection_limit, by = "elements") %>%

    # remove values below detection limit
    filter(value > detection_limit) %>%
    select(siteID, blockID, treatment, plotID, burial_length, elements, value, detection_limit, burial_date, retrieval_date, Notes)

  write_csv(prs_data, file = "clean_data/CNP_cycling/FUNDER_clean_available_nutrients_2021.csv")


