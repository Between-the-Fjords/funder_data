#' Create funder meta data
#' @description Function to make funder meta data.
#' @param path path for storing csv output. Default is NULL. The path is only needed i csv_output = TRUE.
#' @param csv_output logical; argument csv_output or not.
#' The default is csv_output = FALSE. If csv_output = TRUE a csv file is produced and saved under the path name.
#'
#' @return a tibble and optionally a csv file
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom tidyr crossing
#' @importFrom readr csv_output
#'
#' @examples
#' create_funder_meta_data()
#'
#' @export


create_funder_meta_data <- function(path = NULL, csv_output = FALSE){

  siteID = c("Arhelleren",
             "Alrust",
             "Fauske",
             "Gudmedalen",
             "Hogsete",
             "Lavisdalen",
             "Ovstedalen",
             "Rambera",
             "Skjelingahaugen",
             "Ulvehaugen",
             "Veskre",
             "Vikesland")

  blockID = c(1:4)

  treatment = c("C", "F", "G", "B", "FB", "GB", "GF", "FGB")

  funder_metadata <- crossing(siteID, blockID, treatment) %>%
    mutate(plotID = paste0(str_sub(siteID, 1, 3), blockID, treatment),
           blockID = paste0(str_sub(siteID, 1, 3), blockID))

  if(csv_output){
    write_csv(funder_metadata, path)
  }

  funder_metadata

}
