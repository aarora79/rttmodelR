#include code from other modules
source("src/globals.R")
source("src/create_dataset_by_ping_size.R")
source("src/clean.R")
source("src/analyze.R")
source("src/lomax.R")

need_to_clean_data = TRUE

if(need_to_clean_data == TRUE) {
  # clean the data if needed
  clean_ping_resp_data()
}

analyze_all()


# lomax model the distribution
lomax_model_with_flat_prior(808)
