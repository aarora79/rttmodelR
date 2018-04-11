#include code from other modules
source("src/globals.R")

#lomax model the distribution

draw_samples_from_norm_approx <- function(n, r, lambda) {
  set.seed(20092013)
  mu = n/(sum(log(1+r/lambda)))
  sigma2 = n/((sum(log(1+r/lambda)))^2)
  B  = 20000
  samples = rnorm(B, mu, sqrt(sigma2))
  
  cat(sprintf("=== summary statistics for alpha given lambda as %d ===\n", lambda))
  print(summary(samples))
  
  cat(sprintf("\n95%% credible interval for alpha given lambda as %d\n", lambda))
  print(quantile(samples, c(0.025, 0.975)))
  
  plot(density(samples), main=paste0("pdf for alpha with lambda=", lambda))
  samples
}


draw_posterior_predictive_lomax <- function(B, lambda, alpha, threshold=2000) {
  set.seed(20092013)
  samples = rlomax(B, scale=lambda, shape3.q = alpha)
  
  #credible interval
  cat(sprintf("the credible interval for lambda=%d, alpha=%.4f\n", lambda, alpha))
  print(quantile(samples, c(0.025, 0.975)))
  
  #just for plotting limit the x axis to 1000
  title = paste0("Posterior predictive distribution, lambda=", lambda, ", alpha=", alpha)
  plot(density(samples[samples < 50000]), main=title)
  abline(v=threshold, col=2)
  
  prob_of_val_above_threshold_from_cdf = (1 + threshold/lambda)^(-alpha)  
  cat(sprintf("[lambda=%d, alpha=%.4f] Probability FROM CDF of seeing a value > %d is %.4f\n", lambda, alpha, threshold, prob_of_val_above_threshold_from_cdf))
  
  prob_of_val_above_threshold = mean(samples > threshold)
  cat(sprintf("[lambda=%d, alpha=%.4f] Probability FROM Empiricial distribution of seeing a value > %d is %.4f\n", lambda, alpha, threshold, prob_of_val_above_threshold))
  samples
}

draw_posterior_predictive_paretoIV <- function(B, location, scale, 
                                               inequality, shape, threshold=2000, 
                                               cutoff=50000) {
  set.seed(20092013)
  samples = rparetoIV(B, location, scale, inequality, shape)
  samples = samples[samples <= cutoff]
  
  #credible interval
  cat(sprintf("[lambda=%d, scale=%.4f, inequality=%.4f, shape=%.4f] credible interval is\n", location, scale, inequality, shape))
  print(quantile(samples, c(0.025, 0.975)))
  
  #just for plotting limit the x axis to 1000
  title = paste0("Posterior predictive distribution, location=", location, ", scale=", scale, "inequality=", inequality, ", shape=", shape)
  plot(density(samples[samples < 30000]), main=title)
  abline(v=threshold, col=2)
  
  prob_of_val_above_threshold_from_cdf = 1-pparetoIV(threshold, location, scale, inequality, shape)
  cat(sprintf("[lambda=%d, scale=%.4f, inequality=%.4f, shape=%.4f] Probability FROM CDF of seeing a value > %d is %.4f\n", location, scale, inequality, shape, threshold, prob_of_val_above_threshold_from_cdf))
  
  prob_of_val_above_threshold = mean(samples > threshold)
  cat(sprintf("[lambda=%d, scale=%.4f, inequality=%.4f, shape=%.4f] Probability FROM  Empiricial distribution of seeing a value > %d is %.4f\n", location, scale, inequality, shape, threshold, prob_of_val_above_threshold))
  
  samples
}

draw_posterior_predictive_paretoI <- function(B, scale, shape,
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
  
  prob_of_val_above_threshold_from_cdf = pparetoI(threshold, scale, shape)
  cat(sprintf("[scale=%.4f, shape=%.4f] Probability FROM CDF of seeing a value > %d is %.4f\n", scale, shape, threshold, prob_of_val_above_threshold_from_cdf))
  
  prob_of_val_above_threshold = mean(samples > threshold)
  cat(sprintf("[scale=%.4f, shape=%.4f] Probability FROM  Empiricial distribution of seeing a value > %d is %.4f\n", scale, shape, threshold, prob_of_val_above_threshold))
  
  samples
}

draw_posterior_predictive_lnorm <- function(B, mu, sigma2, threshold=2000) {
  set.seed(20092013)
  samples = rlnorm(B, mu, sqrt(sigma2))
  
  #credible interval
  cat(sprintf("[mu=%.4f, sigma2=%.4f] credible interval is\n", mu, sigma2))
  print(quantile(samples, c(0.025, 0.975)))
  
  #just for plotting limit the x axis to 1000
  title = paste0("Posterior predictive distribution for mean=", mu, ", sigma^2=", sigma2)
  cat(sprintf("[mu=%.4f, sigma^2=%.4f] Posterior predictive distribution\n", mu, sigma2))
  plot(density(samples[samples < 30000]), main=title)
  abline(v=threshold, col=2)
  
  prob_of_val_above_threshold_from_cdf = plnorm(threshold, mu, sqrt(sigma2))
  cat(sprintf("[mu=%.4f, sigma^2=%.4f] Probability FROM CDF of seeing a value > %d is %.4f\n", mu, sigma2, threshold, prob_of_val_above_threshold_from_cdf))
  
  prob_of_val_above_threshold = mean(samples > threshold)
  cat(sprintf("[mu=%.4f, sigma^2=%.4f] Probability FROM  Empiricial distribution of seeing a value > %d is %.4f\n", mu, sigma2, threshold, prob_of_val_above_threshold))
  
  samples
}

model_with_lomax <- function(bytes) {
  set.seed(20092013)
  bytes = 808
  flog.info("going to  use lomax model ping dataset for bytes=%d", bytes)
  start_time <- Sys.time()
  
  fname = file.path("data", paste0("ping_", bytes, ".csv"))
  df = read.csv(fname, stringsAsFactors = F) 
  
  end_time <- Sys.time()
  flog.info("time taken for reading ping data for bytes=%d is %.2f seconds", bytes, difftime(end_time, start_time, units="secs"))
  
  
  n = nrow(df)
  r = df$rtt
  r = r[r>0]
  lambda0 = 4
  alpha = draw_samples_from_norm_approx(n, r, lambda0)
  rtt_samples = draw_posterior_predictive(B, lambda0, median(alpha))
  
  a = n + 1
  mu = 0
  b  = sum(log(1+((r-mu)/lambda0)))
  B = 10000
  shape = rgamma(B, a, b)
  plot(density(shape))
  
  scale = 0.01
  threshold = 2000
  rtt_samples = draw_posterior_predictive_paretoI(B, mu, scale, median(shape), threshold)
  save_csv (rtt_samples, "lomax_rtt_samples.csv", dir_name="data")
}

model_with_lnorm <- function(bytes) {
  set.seed(20092013)
  bytes = 808
  flog.info("going to use log-normal model ping dataset for bytes=%d", bytes)
  start_time <- Sys.time()
  
  fname = file.path("data", paste0("ping_", bytes, ".csv"))
  df = read.csv(fname, stringsAsFactors = F) 
  
  end_time <- Sys.time()
  flog.info("time taken for reading ping data for bytes=%d is %.2f seconds", bytes, difftime(end_time, start_time, units="secs"))
  
  
  n = nrow(df)
  r = df$rtt
  r = r[r>0]
  
  
  x_l = mean(log(r))
  sd_mean = var(r)/n 
  mu = rnorm(B, mean(log(r)), sqrt(var(r)/n))
  #samples = rlnorm(B, median(mu), var(r))
  cond_var = rinvgamma(B, (n-1)/2, ((n-1)/2)*(sum((log(r)-x_l)^2))/(n-1))
  samples = rlnorm(B*10, mean(mu), mean(sqrt(cond_var)))
  samples2 = samples[samples>520]
  plot(density(samples2))
  rtt_samples = draw_posterior_predictive_lnorm(B, median(mu), var(r), threshold)
  save_csv (rtt_samples, "lnorm_rtt_samples.csv", dir_name="data")
}

model_with_paretoI <- function(bytes) {
  set.seed(20092013)
  bytes = 808
  flog.info("going to use ParetoII model ping dataset for bytes=%d", bytes)
  start_time <- Sys.time()
  
  fname = file.path("data", paste0("ping_", bytes, ".csv"))
  df = read.csv(fname, stringsAsFactors = F) 
  
  end_time <- Sys.time()
  flog.info("time taken for reading ping data for bytes=%d is %.2f seconds", bytes, difftime(end_time, start_time, units="secs"))
  
  
  n = nrow(df)
  r = df$rtt[df$rtt>0]
  
  a = n + 1
  mu = min(r)
  lambda0 = max(r-mu)
  #b  = sum(log(1+((r-mu)/lambda0)))
  b = sum(log(r/min(r)))
  B = 10000
  shape = rgamma(B, a, b)
  plot(density(shape))
  
  scale = min(r)
  threshold = 2000
  rtt_samples = draw_posterior_predictive_paretoI(B, scale, median(shape), threshold, cutoff <-1.1*max(r))
  summary(rtt_samples)
  summary(r)
  save_csv (rtt_samples, "paretoII_rtt_samples.csv", dir_name="data")
}

model_with_paretoIV <- function(bytes) {
  set.seed(20092013)
  bytes = 808
  flog.info("going to use ParetoIV model ping dataset for bytes=%d", bytes)
  start_time <- Sys.time()
  
  fname = file.path("data", paste0("ping_", bytes, ".csv"))
  df = read.csv(fname, stringsAsFactors = F) 
  
  end_time <- Sys.time()
  flog.info("time taken for reading ping data for bytes=%d is %.2f seconds", bytes, difftime(end_time, start_time, units="secs"))
  
  
  n = nrow(df)
  r = df$rtt[df$rtt>0]
  
  a = n + 1
  location = min(r)
  b  = sum(log(1+((r-location)/lambda0)))
  B = 10000
  shape = rgamma(B, a, b)
  plot(density(shape))
  
  scale = 1
  inequality = 1
  threshold = 2000
  rtt_samples = draw_posterior_predictive_paretoIV(B*10, location, scale, inequality, median(shape), threshold, cutoff <- 1.1*max(r))
  #rtt_samples = rtt_samples[rtt_samples < 30000]
  save_csv (rtt_samples, "paretoIV_rtt_samples.csv", dir_name="data")
}
