n <- 5000

raw_data <- read.csv("data-raw/marketing_AB.csv")

set.seed(42)

psa_group <- raw_data[raw_data$test.group == "psa", ]
ad_group <- raw_data[raw_data$test.group == "ad", ]

sample_psa_indices <- sample(nrow(psa_group), size = n, replace = FALSE)
psa_sample <- psa_group[sample_psa_indices, ]

sample_ad_indices <- sample(nrow(ad_group), size = n, replace = FALSE)
ad_sample <- ad_group[sample_ad_indices, ]

sampled <- rbind(ad_group, psa_group)
sampled$converted <- as.numeric(raw_data$converted)

sampled_clean <- sampled[, c("test.group", "converted")]

usethis::use_data(sampled_clean, overwrite = TRUE)
