## code to prepare `lookup_table` dataset goes here
lookup_table <- readr::read_csv("stopad_lookup_table.csv")
partial_betas <- readr::read_csv("stopad_beta_weights.csv")

if (!dir.exists("../data")) {
    dir.create("../data")
}

save(lookup_table, file = "../data/lookup_table.rda")
save(partial_betas, file = "../data/partial_betas.rda")
