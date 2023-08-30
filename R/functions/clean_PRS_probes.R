# clean PRS probe data

clean_prs <- function(prs_raw, prs_detection_limit, funder_meta){

  prs_raw %>%
    # we deleted the probes control without any treatment that was added by the lab
    filter(ID != "CONTROL") %>%
    separate(col = ID, into = c("siteID", "blockID", "treatment"),sep = "-")%>%
    mutate(treatment = recode(treatment,
                            'BF' = 'FB',
                            'BG' = 'GB',
                            'FG' = 'GF',
                            'BFG' = 'FGB')) %>%
    mutate(siteID = str_to_title(tolower(siteID))) %>%
    mutate(plotID = paste0(siteID, blockID, treatment)) %>%
    select(-siteID, -blockID) %>%
    left_join(funder_meta, by = c("treatment","plotID")) %>%
    # Make real dates
    rename(burial_date = `Burial Date`, retrieval_date = `Retrieval Date`) %>%
    mutate(burial_date = ymd(burial_date),
           retrieval_date = ymd(retrieval_date),
           burial_length = retrieval_date - burial_date) %>%
    pivot_longer(cols = `NO3-N`:Cd, names_to = "elements", values_to = "value") %>%
    left_join(prs_detection_limit, by = "elements") %>%

    # remove values below detection limit
    filter(value > detection_limit) %>%
    select(siteID, blockID, treatment, plotID, burial_length, elements, value, detection_limit, burial_date, retrieval_date, notes = Notes)

}



