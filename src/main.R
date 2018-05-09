#include code from other modules
source("src/globals.R")
source("src/create_dataset_by_ping_size.R")
source("src/clean.R")
source("src/analyze.R")
source("src/pareto.R")

action_clean_data   = F
action_analyze_data = F

if(action_clean_data == TRUE) {
  # clean the data if needed
  clean_ping_resp_data("train", 3)  # month = March (so 3)
  clean_ping_resp_data("test",  4)  # month = April (so 4)
}

if (action_analyze_data == TRUE) {
  analyze_all("train")
  analyze_all("test")
}



# lomax model the distribution
#model_with_paretoI(808)
model_with_paretoI(40, "train")
#model_with_paretoI(1480)
