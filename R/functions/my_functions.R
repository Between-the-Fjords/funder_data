# my functions


# save data as csv
save_csv <- function(file, name) {

  filepath <- paste0("clean_data/", name)
  output <- write_csv(x = file, file = filepath)
  filepath
}
