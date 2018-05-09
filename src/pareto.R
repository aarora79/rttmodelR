#include code from other modules
source("src/globals.R")

eval_model <- function(B, scale, shape, r, q, threshold=2000, cutoff=50000) {
  set.seed(20092013)
  #shape <- c(1.645694)
  #s = rparetoI(B, scale, shape)
  get_T <- function(x) {
    #flog.info("q=%f\n", q)
    y = rparetoI(B, scale, x)
    y = y[y<=cutoff]
    #mean(y > threshold)
    if(q==0.95) {
      mean(y)
    } else {
      quantile(y, q)
    }
  }
  test_stat = unlist(map(shape, get_T))
  
  plot(density(test_stat))
  cat(sprintf("posterior predictive p-value of observed test statistic %.4f\n", mean(quantile(r, q) > test_stat)))
  if(q==0.95){
    v <- mean(r)
  } else {
    v <- quantile(r, q)
  }
  
  abline(v=v, col='blue')
  
  ci = quantile(test_stat, c(0.025, 0.975))
  cat(sprintf("95%% confidence interval for test statistic is [%.2f, %.2f]\n", ci[1],ci[2]))
  if(v >= ci[1] & v <= ci[2]) {
    cat(sprintf("%f quantile for observed data is %.2f which is within the credible interval [%.2f, %.2f]\n", q, v, ci[1],ci[2]))
  } else {
    cat(sprintf("%f quantile for observed data is %.2f which is NOT within the credible interval [%.2f, %.2f]\n", q, v, ci[1],ci[2]))
    
  }
}

draw_posterior_predictive_paretoI <- function(B, scale, shape, bytes,
                                              threshold=2000, cutoff=50000) {
  set.seed(20092013)
  samples = rparetoI(B, scale, shape)
  samples = samples[samples <= cutoff]
  
  #credible interval
  cat(sprintf("[scale=%.4f, shape=%.4f] credible interval is\n", scale, shape))
  print(quantile(samples, c(0.025, 0.975)))
  
  #just for plotting limit the x axis to 1000
  title = paste0("Posterior predictive distribution, scale=", scale, ", shape=", shape)
  plot(density(samples[samples < 30000]), main=title)
  abline(v=threshold, col=2)
  
  prob_of_val_above_threshold_from_cdf = 1 - pparetoI(threshold, scale, shape)
  cat(sprintf("[scale=%.4f, shape=%.4f] Probability FROM CDF of seeing a value > %d is %.4f\n", scale, shape, threshold, prob_of_val_above_threshold_from_cdf))
  
  prob_of_val_above_threshold = mean(samples > threshold)
  cat(sprintf("[scale=%.4f, shape=%.4f] Probability FROM  Empiricial distribution of seeing a value > %d is %.4f\n", scale, shape, threshold, prob_of_val_above_threshold))
  
  # plot the posterior predictive distribution
  df_samples = data.frame(x=samples, bytes=bytes)
  p <- df_samples %>%
    ggplot(aes(x, col=as.factor(bytes))) +
    geom_density(position="identity", fill = NA, size = 1, show.legend = F) +
    scale_x_continuous(name = "RTT (ms)") +
    scale_y_continuous(name = "Density") +
    ggtitle("Density plot for Round trip times") + #, "Posterior Predictive Distribution") +
    theme_tq() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          text = element_text(size = 12),
          legend.position = "bottom") +
    scale_colour_brewer(palette="Accent")
  save_plot(p, file_name=paste0(bytes, "_byte_ping_rtt_density_plot_posterior_predictive.png"))
  print(p)
  samples
}

model_with_paretoI <- function(bytes) {
  set.seed(20092013)
  #bytes = 40
  flog.info("going to use ParetoII model ping dataset for bytes=%d", bytes)
  start_time <- Sys.time()
  
  fname = file.path("data", paste0("ping_", bytes, ".csv"))
  df = read.csv(fname, stringsAsFactors = F) 
  
  end_time <- Sys.time()
  flog.info("time taken for reading ping data for bytes=%d is %.2f seconds", bytes, difftime(end_time, start_time, units="secs"))
  
  
  n = nrow(df)
  #r = df$rtt[df$rtt>0 & df$rtt <= 15000]
  r = df$rtt[df$rtt>0]

  
  a = n
  mu = min(r)
  lambda0 = max(r-mu)
  #b  = sum(log(1+((r-mu)/lambda0)))
  #b = sum(log(r/min(r)))
  #min_r = min(r)+30
  min_r = 520
  b = sum(log((r/min_r)))
  B = 20000
  shape = rgamma(B, a, b)
  
  
  plot(cumsum(shape)/(1:B), type = 'l', ylab = 'Running Mean', xlab = 'B')
  shape = shape[-(1:(length(shape)/2))]
  acf(shape)
  plot(density(shape))
  
  scale = min_r
  threshold = 2000
  cutoff <-1.1*max(r)
  rtt_samples = draw_posterior_predictive_paretoI(B, scale, median(shape), bytes, threshold, cutoff)
  summary(rtt_samples)
  summary(r)
  save_csv (rtt_samples, "paretoI_rtt_samples.csv", dir_name="data")
  
  eval_model(B, scale, shape, r, 0.25, threshold, cutoff)
  eval_model(B, scale, shape, r, 0.50, threshold, cutoff)
  eval_model(B, scale, shape, r, 0.75, threshold, cutoff)
  eval_model(B, scale, shape, r, 0.95, threshold, cutoff)
}