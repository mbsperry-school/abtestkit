n <- 5000
raw_data <- read.csv("data-raw/marketing_AB.csv")
set.seed(42)

psa_group <- raw_data[raw_data$test.group == "psa", ]
historical_data <- psa_group[1:1000, ]
historical_data$converted <- as.numeric(historical_data$converted == "True")
historical_data <- historical_data[, c("test.group", "converted")]

remaining_psa <- psa_group[1001:nrow(psa_group), ]
ad_group <- raw_data[raw_data$test.group == "ad", ]

# Sample from each arm
sample_psa_indices <- sample(nrow(remaining_psa), size = n, replace = FALSE)
psa_sample <- remaining_psa[sample_psa_indices, ]
sample_ad_indices <- sample(nrow(ad_group), size = n, replace = FALSE)
ad_sample <- ad_group[sample_ad_indices, ]

sampled <- rbind(ad_sample, psa_sample)
sampled$converted <- as.numeric(sampled$converted == "True")
experiment_data <- sampled[, c("test.group", "converted")]

usethis::use_data(historical_data, overwrite = TRUE)
usethis::use_data(experiment_data, overwrite = TRUE)
