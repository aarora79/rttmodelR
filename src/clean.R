#include code from other modules
source("src/globals.R")
source("src/create_dataset_by_ping_size.R")

clean_ping_resp_data <- function(pattern, mon) {
  ########## Get the data ###################
  
  # we use the map reduce paradigm instead of writing a for loop
  # take each file, apply a map function (in this case read.table)
  # and then reduce (combine) the output from each operation
  # using a row bind i.e. concatenate all the dataframes
  files <- file.path("raw_data", dir(path = "raw_data", pattern = paste0("^", pattern)))
  data <- files %>%
    map(read.table, header=F, sep = "", skip=1, stringsAsFactors=F, fill=T) %>%    # read in all the files individually, using
    # the function read.table() from the utils package
    reduce(rbind)        # reduce with rbind into one dataframe
  
  #lets look at the data, 10 random rows
  sample_n(data, 10)
  
  #another perspective on the same data
  glimpse(data)
  
  #we know we are looking at ping data, exclude out everything
  #that is not the correct number of bytes (most likely due to
  #parsing failures because of error messages in the logs)
  
  #1. convert to tibble
  #2. only include rows parsed correctly
  #3. combine fields to create a date string and then convert to date
  #4. parse out rtt, bytes, sequence number
  #ping_sizes_of_interest = c("40", "108", "808", "1480")
  ping_sizes_of_interest = c("40")
  df = data %>%
    as.tibble() %>%   
    filter(V7 %in% ping_sizes_of_interest) %>% 
    mutate(timestamp = ymd_hms(paste(str_sub(V6, 1, -2), V2, V3, V4)),
           rtt = as.numeric(str_split(V13, "=", simplify = T)[,2]),
           bytes = as.numeric(V7),
           seq = as.numeric(str_split(V11, "=", simplify = T)[,2])) %>%
    filter(month(timestamp) == mon) %>%
    dplyr::select(timestamp, rtt, bytes, seq) %>%
    filter(rtt < RTT_ERROR_THRESHOLD)
  
  #lets take a look at the data again
  sample_n(df, 10)
  
  #create a directory to store the clean data
  dir.create("data", showWarnings = FALSE)
  fname = file.path("data", "ping.csv")
  write.csv(df, fname, row.names = F)
  
  map(unique(df$bytes), create_dataset_by_ping_size, df, pattern)
}