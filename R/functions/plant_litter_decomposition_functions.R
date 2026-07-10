#################################################################
############# plant litter decomposition functions ##############
#################################################################

clean_pre <- function(data) {

  # load weights before burying:
  readxl::read_xlsx(data) |>
    # readxl::read_xlsx(here("data/FUNDER_raw_beforeburrying_litter_biomass_2021.xlsx")) |>
    mutate(
      native_graminoids = if_else(plotID == "Ovs3B", "1.00000", native_graminoids),
      native_graminoids = if_else(plotID == "Skj3C", "1.00000", native_graminoids),
      graminoids_before_burying = if_else(plotID == "Ovs3B", "1.00000", graminoids_before_burying),
      graminoids_before_burying = if_else(plotID == "Skj3C", "1.00000", graminoids_before_burying),
      graminoids_before_burying = round(as.numeric(graminoids_before_burying), digits = 5)
    ) |>
    # replace "FG" in treatment column with the correct "GF":
    mutate(
      treatment = recode_values(
        treatment,
        "FG" ~ "GF",
        default = treatment
      ),
      plotID = paste0(site, block, treatment)
    ) |>
    select(
      site, block, treatment, plotID, forbs_before_burying, graminoids_before_burying,
      forb_comments_before, graminoid_comments_before
    )

}

clean_post <- function(data) {

  readxl::read_xlsx(data, col_types = c(rep("text", 4), "date", rep("numeric", 6), rep("text", 2))) |>
    # subtract falcon tube weight from dry weight to get true dry weight:
    mutate(
      forb_dry_weight_g = forb_dry_weight_g - forb_Falcon_weight_g,
      graminoid_dry_weight_g = graminoid_dry_weight_g - graminoid_Falcon_weight_g
    ) |>
    select(plotID, forb_dry_weight_g, graminoid_dry_weight_g, forb_comment, graminoid_comment)

}

get_com <- function(pre, post) {

  pre_com <- pre |>
    select(plotID, forb_comments_before, graminoid_comments_before) |>
    pivot_longer(
      cols = c(forb_comments_before, graminoid_comments_before),
      names_to = "litter_type", values_to = "comment_before"
    ) |>
    mutate(litter_type = recode_values(
      litter_type, "forb_comments_before" ~ "forb", "graminoid_comments_before" ~ "graminoid"
    ))

  post_com <- post |>
    select(plotID, forb_comment, graminoid_comment) |>
    rename(
      forb_comments_after = forb_comment,
      graminoid_comments_after = graminoid_comment
    ) |>
    pivot_longer(
      cols = c(forb_comments_after, graminoid_comments_after),
      names_to = "litter_type", values_to = "comment_after"
    ) |>
    mutate(litter_type = recode_values(
      litter_type, "forb_comments_after" ~ "forb", "graminoid_comments_after" ~ "graminoid"
    ))

  left_join(pre_com, post_com)

}

finish <- function(pre, post, comments) {
  post |>
    # join with pre_weights:
    left_join(pre) |>
    # calculate relative weight loss as net weight loss / weight pre burying:
    mutate(
      forb_rel_weight_loss = (forbs_before_burying - forb_dry_weight_g) / forbs_before_burying,
      graminoid_rel_weight_loss = (graminoids_before_burying - graminoid_dry_weight_g) / graminoids_before_burying
    ) |>
    # clean up the data:
    mutate(
      siteID = site,
      blockID = paste0(siteID, block)
    ) |>
    select(
      siteID, blockID, plotID, treatment,
      forb_rel_weight_loss, # forbs_before_burying, forb_dry_weight_g,
      graminoid_rel_weight_loss # , graminoids_before_burying, graminoid_dry_weight_g
    ) |>
    pivot_longer(
      cols = c(forb_rel_weight_loss, graminoid_rel_weight_loss),
      names_to = "litter_type",
      values_to = "relative_weight_loss"
    ) |>
    mutate(litter_type = recode_values(
      litter_type,
      "forb_rel_weight_loss" ~ "forb",
      "graminoid_rel_weight_loss" ~ "graminoid"
    )) |>
    # add burial date:
    mutate(
      burial_date = recode_values(
        siteID,
        "Alr" ~ "2021-08-05",
        "Vik" ~ "2021-08-09",
        "Hog" ~ "2021-08-11",
        "Fau" ~ "2021-08-04",
        "Ovs" ~ "2021-08-13",
        "Ves" ~ "2021-08-18",
        "Arh" ~ "2021-08-19",
        "Ram" ~ "2021-08-17",
        "Ulv" ~ "2021-08-06",
        "Skj" ~ "2021-08-16",
        "Gud" ~ "2021-08-10",
        "Lav" ~ "2021-08-12"
      ),
      # retrieval date:
      retrieval_date = siteID,
      retrieval_date = recode_values(
        retrieval_date,
        "Alr" ~ "2022-08-24",
        "Vik" ~ "2022-08-22",
        "Hog" ~ "2022-08-23",
        "Fau" ~ "2022-08-25",
        "Ovs" ~ "2022-08-29",
        "Ves" ~ "2022-08-30",
        "Arh" ~ "2022-08-31",
        "Ram" ~ "2022-09-01",
        "Ulv" ~ "2022-09-07",
        "Skj" ~ "2022-09-08",
        "Gud" ~ "2022-09-06",
        "Lav" ~ "2022-09-05"
      ),
      # fix site names:
      siteID = recode_values(
        siteID,
        "Alr" ~ "Alrust",
        "Vik" ~ "Vikesland",
        "Hog" ~ "Hogsete",
        "Fau" ~ "Fauske",
        "Ovs" ~ "Ovstedalen",
        "Ves" ~ "Veskre",
        "Arh" ~ "Arhelleren",
        "Ram" ~ "Rambera",
        "Ulv" ~ "Ulvehaugen",
        "Skj" ~ "Skjelingahaugen",
        "Gud" ~ "Gudmedalen",
        "Lav" ~ "Lavisdalen"
      )
    ) |>
    relocate(burial_date, retrieval_date, siteID, blockID, plotID, treatment, relative_weight_loss, litter_type) |>
    left_join(comments) |>
    dataDocumentation::funcabization(convert_to = "FunCaB")
}
