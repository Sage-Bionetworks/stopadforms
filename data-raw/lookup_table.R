## code to prepare `lookup_table` dataset goes here
lookup_table <- readr::read_csv("stopad_lookup_table.csv")
partial_betas <- readr::read_csv("stopad_beta_weights.csv")
usethis::use_data(lookup_table, partial_betas, internal = FALSE, overwrite = TRUE)
