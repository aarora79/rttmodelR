source("src/globals.R")
source("src/analyze.R")

bytes = c(40, 108, 808, 1480)
map(bytes, analyze_ping_resp_data)



files <- file.path("data", dir("data", pattern = "*_high_ping_response_times.csv"))
data <- files %>%
  map(read.csv, skip=0, sep=",", stringsAsFactors = F) %>%    # read in all the files individually, using
  # the function read_csv() from the readr package
  
  reduce(rbind) %>%        # reduce with rbind into one dataframe
  select(gap)
df = data[data$bytes == 108,]
df = data
plot(density(df$gap))
