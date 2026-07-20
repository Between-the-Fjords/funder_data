#####################################################################
############# fungal_necromass decomposition functions ##############
#####################################################################

# get fungal necromass weights pre
clean_necro_pre <- function(data) {

  # Load melanised mycelium:
  before_mel <- read_xlsx(
    data,
    sheet = 1
  ) |>
    rename(
      mel_gram = "Weight (g)",
      mel_tag = "Tag no.",
      mel_comp = "Composition category, see separate sheet"
    ) |>
    mutate(
      mel_gram = sub(",", ".", mel_gram),
      mel_gram = as.numeric(mel_gram)
    )

  # Load non-melanised mycelium:
  before_nonmel <- read_xlsx(
    data,
    sheet = 2
  ) |>
    rename(
      non_gram = "Weight (g)",
      non_tag = "Tag no.",
      non_comp = "Composition category, see separate sheet"
    ) |>
    mutate(
      non_gram = sub(",", ".", non_gram),
      non_gram = as.numeric(non_gram)
    )

  # Combine to wide data frame for corrections:
  before <- cbind(before_mel, before_nonmel) |>
    mutate(

      # tag 0586 was misspelled as 586 - correct it:
      mel_tag = recode_values(mel_tag, "586" ~ "0586", default = mel_tag),

      # tag 1859 was misspelled as 1858 - correct it:
      mel_tag = sub("1858", "1859", mel_tag)
    ) |>
    # a few tags have been assigned twice with different starting weights. we
    # can assume what the tag numbers should have been based on missing tag
    # nubmers, e.g. by comparing with gaps in tag order, but we have no way of
    # knowing which start weight is correct. we therefore assign mean starting
    # weights for dupicated tags and correct the tag number (see below)
    mutate(mel_gram = mean(mel_gram), .by = mel_tag)

  # Shift the data to long format:
  before_long <- rbind(
    data.frame(
      tag = c(before$mel_tag, before$non_tag),
      weight_before = c(before$mel_gram, before$non_gram),
      comp = c(before$mel_comp, before$non_comp),
      mycelium = c(
        rep("melanised", times = nrow(before)),
        rep("non-melanised", times = nrow(before))
      )
    )
  ) |>
    # some tag numbers have been given twice, we need to correct them
    group_by(across(tag)) |>
    mutate(
      duplicate_id = row_number(),

      # based on
      tag = if_else(condition = duplicate_id == 2 & tag == "1977",
                    true = "1677",
                    false = tag),
      tag = if_else(condition = duplicate_id == 2 & tag == "1689",
                    true = "1684",
                    false = tag),
      tag = if_else(condition = duplicate_id == 2 & tag == "1881",
                    true = "1884",
                    false = tag)) |>
    select(-duplicate_id) |>
    ungroup()
  # before burying data is ready.

  return(before_long)
}

# get necromass post
clean_necro_post <- function(data) {
  read.csv(data, dec = ",") |>
    # remove hash tags from tag numbers:
    mutate(
      mel_chip_number = sub("#", "", mel_chip_number),
      nonmel_chip_number = sub("#", "", nonmel_chip_number),
      # the sample with tag 1964 was accidentally dried and weighed twice,
      # and dates of drying and the resulting dry weight was entered twice.
      # we use the data from the first drying and weighing:
      date_dried_mel = sub("25-04-12/25-05-01", "25-04-12", date_dried_mel),
      mel_dry_weight_g = sub("10,6531/10,7110", "10,6531", mel_dry_weight_g),
      mel_dry_weight_g = sub(",", ".", mel_dry_weight_g),
      mel_dry_weight_g = as.numeric(mel_dry_weight_g),
      # a few samples have had their tags mixed up in the post burying data
      # sheet, and we correct them here
      # ploID: Ulv1GB
      mel_chip_number = sub("1543", "1632", mel_chip_number),
      nonmel_chip_number = sub("1632", "1543", nonmel_chip_number),
      # plotID: Ves2GB
      mel_chip_number = sub("1573", "1891", mel_chip_number),
      nonmel_chip_number = sub("1891", "1573", nonmel_chip_number),
      # plotID: Ram2FGB
      nonmel_chip_number = sub("1863", "1763", nonmel_chip_number)
    )
}

# get comments from post weights (there are none in pre weights)
get_necromass_comments <- function(post) {

  post |>
    select(plotID, mel_comment, nonmel_comment) |>
    pivot_longer(
      cols = c(mel_comment, nonmel_comment),
      names_to = "mycelium", values_to = "comment_after"
    ) |>
    mutate(mycelium = recode_values(
      mycelium,
      "mel_comment" ~ "melanised",
      "nonmel_comment" ~ "non-melanised"
    )) |>
    mutate(comment_after = na_if(comment_after, ""))
}

# finish the dataset
finish_necromass <- function(pre, post, comments) {

  # shift the data to longer format:
  post_long <- rbind(
    data.frame(
      siteID = rep(post$site, 2),
      blockID = rep(post$block, 2),
      plotID = rep(post$plotID, 2),
      treatment = rep(post$treatment, 2),
      tag = c(
        post$mel_chip_number,
        post$nonmel_chip_number
      ),
      falcon = c(
        post$mel_Falcon_weight_g,
        post$nonmel_Falcon_weight_g
      ),
      dry_weight_with_falcon = c(
        post$mel_dry_weight_g,
        post$nonmel_dry_weight_g
      )
    )
  ) |>
    # subtract Falcon tube weight from dry weight:
    mutate(dry_weight_after = dry_weight_with_falcon - falcon) |>
    select(-falcon, -dry_weight_with_falcon)

  # join pre and post:
  left_join(post_long, pre#_long,
            , by = "tag") |>
    # remove missing samples:
    filter(tag != "") |>
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
      ),
      relative_weight_loss = (weight_before - dry_weight_after) / weight_before,
      blockID = paste0(substr(siteID, 1, 3), blockID),
      particle_size = recode_values(
        comp,
        1 ~ "small",
        2 ~ "mixed",
        3 ~ "large"
      )
    ) |>
    select(burial_date, retrieval_date, tag, siteID, blockID, plotID, treatment,
           relative_weight_loss, mycelium, particle_size) |>
    left_join(comments) |>
    dataDocumentation::funcabization(convert_to = "FunCaB")

}
