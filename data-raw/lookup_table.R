## code to prepare `lookup_table` dataset goes here
lookup_table <- readr::read_csv("stopad_lookup_table.csv")
usethis::use_data(lookup_table, internal = TRUE, overwrite = TRUE)
