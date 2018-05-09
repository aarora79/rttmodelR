source("src/globals.R")

create_dataset_by_ping_size <- function(bytes, df, pattern) {
  #create a directory to store the clean data
  dir.create("data", showWarnings = FALSE)
  fname = file.path("data", paste0(pattern, "_ping_", bytes, ".csv"))
  write.csv(df[df$bytes==bytes,], fname, row.names = F)
}