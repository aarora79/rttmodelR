source("src/utils.R")

analyze_ping_resp_data <- function(bytes) {
  #bytes = 808
  flog.info("going to analyze ping dataset for bytes=%d", bytes)
  start_time <- Sys.time()
  
  fname = file.path("data", paste0("ping_", bytes, ".csv"))
  df = read.csv(fname, stringsAsFactors = F) 
  
  #convert timestamp from string to POSIXct
  df$timestamp = ymd_hms(df$timestamp)
  #descriptive stats for fields we are interested in
  describe(df[, c("rtt")])
  
  
  #lets make a timeseries to begin with,
  #we start with the 40 byte ping timeseries..
  #1. Filter the dataframe to only include the 40 byte pings
  #2. Convert the timeseries to a convenient timeseries *tibble*
  #3. Rollup the tibble into hourly average
  #4. Plot with timestamp on x-axis and rtt on y axis
  p = df %>%
    ggplot(aes(timestamp, rtt)) + geom_line(col="lightblue") +
    ggtitle(paste0("Ping response time for ", bytes, " byte pings")) +
    xlab("Time") + ylab("round trip time (ms)") + 
    theme_tq() + 
    theme(text = element_text(size=10),
          axis.text.y = element_text(size=10), legend.position = "none") 
  p
  save_plot(p, file_name=paste0(bytes, "_byte_ping_rtt_timeseries.png"))
  
  #hourly average timeseries
  p = df %>%
    as_tbl_time(index=timestamp) %>%
    as_period("hourly") %>%
    ggplot(aes(timestamp, rtt)) + geom_line(col="lightblue") +
    ggtitle(paste0("Ping response time for ", bytes, " byte pings"), subtitle="hourly average") +
    xlab("Time") + ylab("round trip time (ms)") +
    theme_tq() + 
    theme(text = element_text(size=10),
          axis.text.y = element_text(size=10), legend.position = "none")
  save_plot(p, file_name=paste0(bytes, "_byte_ping_rtt_hourly_average_timeseries.png"))
  
  #it is also useful to see the distribution of rtt values
  #we want to color each ping bytes category with a different color
  #to see if there is a difference in distribution for each ping size
  p <- df %>%
    ggplot(aes(x = rtt, colour = as.factor(bytes))) +
    geom_density(position="identity", fill = NA, size = 1, show.legend = F) +
    scale_x_continuous(name = "RTT (ms)") +
    scale_y_continuous(name = "Density") +
    ggtitle("Density plot for Round trip times") +
    theme_tq() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          text = element_text(size = 12),
          legend.position = "bottom") +
    scale_colour_brewer(palette="Accent") +
    labs(colour = "Ping Size") 
  save_plot(p, file_name=paste0(bytes, "_byte_ping_rtt_density_plot.png"))
  
  
  #extract out temporal fields from the timestamp
  #to prepare a heatmap of hourly ping rtt average
  #grouped by ping size
  df2 = df %>%
    mutate(day = day(timestamp), hour = hour(timestamp), 
           five_minute_intv = round(minute(timestamp)/5)) %>%
    group_by(bytes, day, hour, five_minute_intv) %>%
    summarize(rtt = mean(rtt))
  
  #it is also useful to see the distribution of rtt values
  #we want to color each ping bytes category with a different color
  #to see if there is a difference in distribution for each ping size
  p <- df2 %>%
    ggplot(aes(x = rtt, colour = as.factor(bytes))) +
    geom_density(position="identity", fill = NA, size = 1, show.legend = F) +
    scale_x_continuous(name = "RTT (ms)") +
    scale_y_continuous(name = "Density") +
    ggtitle("Density plot for 5 Minute Average of Round trip times") +
    theme_tq() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          text = element_text(size = 12),
          legend.position = "bottom") +
    scale_colour_brewer(palette="Accent") +
    labs(colour = "Ping Size") 
  save_plot(p, file_name=paste0(bytes, "_byte_ping_5_minute_average_rtt_density_plot.png"))
  
  sum(df$rtt > RTT_CONG_HIGH_WATERMARK)
  mean(df$rtt > RTT_CONG_HIGH_WATERMARK)
  
  #find out streaks of high ping times
  df_high_ping_times = df %>%
    filter(rtt >= RTT_CONG_HIGH_WATERMARK) %>%
    mutate(gap = as.numeric(timestamp - lag(timestamp))) %>%
    filter(gap > LAG_THRESHOLD) %>%
    select(timestamp, bytes, gap)
  save_csv(df_high_ping_times, file_name=paste0(bytes, "_byte_high_ping_response_times.csv"))
  
  p <- df_high_ping_times %>%
    ggplot(aes(x = gap, colour = as.factor(bytes))) +
    geom_density(position="identity", fill = NA, size = 1, show.legend = F) +
    scale_x_continuous(name = "Time duration (in seconds)", labels = comma) +
    scale_y_continuous(name = "Density") +
    ggtitle("Density plot for time between incidents of high ping response times") +
    theme_tq() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          text = element_text(size = 12),
          legend.position = "bottom") +
    scale_colour_brewer(palette="Accent") +
    labs(colour = "Ping Size")
  save_plot(p, file_name=paste0(bytes, "_byte_ping_time_duration_between_high_ping_response_times_density_plot.png"))

  end_time <- Sys.time()
  flog.info("time taken for analyzing ping data for bytes=%d is %.2f minutes", bytes, end_time-start_time)
}
#probability
#find out streaks of high ping times
#df_high_ping_times_prob = df %>%
#filter(rtt >= RTT_CONG_HIGH_WATERMARK) %>%
#  group_by(day(timestamp)) %>%
#  summarise(prob_high_watermark = mean(rtt >= RTT_CONG_HIGH_WATERMARK))


analyze_all <- function() {
  bytes = c(40, 108, 808, 1480)
  map(bytes, analyze_ping_resp_data)
  
  ##files <- file.path("data", dir("data", pattern = "*_high_ping_response_times.csv"))
  ##data <- files %>%
  ##  map(read.csv, skip=0, sep=",", stringsAsFactors = F) %>%    # read in all the files individually, using
    # the function read_csv() from the readr package
    
  ##  reduce(rbind) %>%        # reduce with rbind into one dataframe
  ##  select(gap)
  ##df = data[data$bytes == 808,]
  ##df = data
  ##plot(density(df$gap))
}
