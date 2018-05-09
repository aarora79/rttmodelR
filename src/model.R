#include code from other modules
source("src/globals.R")

set.seed(20092013)

draw_density_plot_with_ci <- function(x, v, q) {
  d <- as.data.frame(x=x)
  
  p <- ggplot(data=d) +
    theme_tq() + 
    geom_density(aes(x=x, y = ..density..), color = '#619CFF') +
    labs(x = paste0("T", as.integer(100*q)),
         title = "Test statistic density from replicates",
         caption="(vertical black line corresponds to test statistic from observed data)")
  
  # new code is below
  q2.3  <- quantile(x, .023) # 2 Std 95.4%
  q97.7 <- quantile(x, .977)
  
  x.dens  <- density(x)
  df.dens <- data.frame(x=x.dens$x, y=x.dens$y)
  
  p + 
    geom_area(data = subset(df.dens, x >= q2.3 & x <= q97.7), # 2 Std 95.4%
              aes(x=x,y=y), fill='#619CFF', alpha=0.6) +
    geom_vline(xintercept=v, show_guide=T)
}

eval_model <- function(q, B, scale, shape, r, pattern, bytes, dataset, model) {
  set.seed(20092013)

  flog.info("going to run eval_model for scale %.2f, shape %.2f, q %.2f\n", scale, shape, q)
  start_time <- Sys.time()
  
  get_T <- function(x) {
    y = rparetoI(B, scale, x)
    quantile(y, q)
  }
  test_stat = unlist(map(shape, get_T))
  v <- r[as.integer(100*q)]
  
  p = draw_density_plot_with_ci(test_stat, v, q)
  p
  ggsave(file.path("plots", paste0(pattern, "_", bytes, "_byte_T",as.integer(100*q), "density.png")))
  
  plot(density(test_stat))
  p_val <- mean(test_stat > v)
  
  cat(sprintf("posterior predictive p-value of observed test statistic %.4f\n", p_val))
  
  abline(v=v, col='blue')
  
  ci = quantile(test_stat, c(0.025, 0.975))
  ci <- round(ci)
  
  cat(sprintf("95%% confidence interval for test statistic is [%.2f, %.2f]\n", ci[1],ci[2]))
  if(v >= ci[1] & v <= ci[2]) {
    in_ci = "Yes"
    cat(sprintf("%f quantile for observed data is %.2f which is within the credible interval [%.2f, %.2f]\n", q, v, ci[1],ci[2]))
  } else {
    in_ci = "No"
    cat(sprintf("%f quantile for observed data is %.2f which is NOT within the credible interval [%.2f, %.2f]\n", q, v, ci[1],ci[2]))
  }
  end_time <- Sys.time()
  flog.info("eval_model took %.2f seconds", difftime(end_time, start_time, units="secs"))
  
  data.frame(q=q, v=v, median_test_stat=median(test_stat), 
             mean_test_stat=mean(test_stat),
             CI_L=ci[1], CI_U=ci[2],
             CI=paste0("[", ci[1], ", ", ci[2], "]"), CI_size=as.integer(ci[2]-ci[1]),
             p_val=p_val, in_ci = in_ci, dataset=dataset, model=model)
}

run_gibbs_sampler <- function(r, 
                              ts = 1,
                              #alpha = 10,
                              #bs = 5,
                              alpha = 3,
                              bs = 1,
                              a = 1,
                              b = 0.001, 
                              B = 50000,
                              burn_in = 0.5, 
                              thin = 5) {
  flog.info("params used ->\n")
  flog.info("ts %.2f, alpha %.2f, bs %.2f, a %.2f, b %.2f, B %d, burn_in %.2f, thin %d\n",
            ts, alpha, bs, a, b, B, burn_in, thin)
  n <- length(r)
  flog.info(" n %d, min value in data %.2f\n", n, min(r))
  
  theta	<- vector(length = B)
  beta	<- vector(length = B)
  
  theta[1] <- ts
  beta[1]  <- bs
  
  s_l_r_rm <- sum(log(r/min(r)))
  for(t in 2:B){
    ## update beta ##
    beta[t]  <- rgamma(1, alpha+a, theta[t-1]+b)
    
    ## update theta ##
    theta[t] <- rgamma(1, n+alpha, s_l_r_rm+beta[t-1])
  }
  
  # discard 50% burn-in
  burn_in <- 0.5
  beta = beta[(length(beta)*burn_in):length(beta)]
  i = seq(1: length(beta))
  beta = beta[i[i %% thin == 0]]
  beta_mat           = matrix(beta, ncol = 1)
  colnames(beta_mat) = 'beta'
  beta_list				<- mcmc.list(list(mcmc(beta_mat)))
  suppressWarnings(mcmcplot1(beta_list, greek=T))
  
  # discard 50% burn-in
  theta = theta[(length(theta)*burn_in):length(theta)]
  i = seq(1: length(theta))
  theta = theta[i[i %% thin == 0]]
  theta_mat           = matrix(theta, ncol = 1)
  colnames(theta_mat) = 'theta'
  theta_list				<- mcmc.list(list(mcmc(theta_mat)))
  suppressWarnings(mcmcplot1(theta_list, greek=T))
  
  list(theta=theta, beta=beta)
}

#######################
# read data
#######################
bytes = 40
flog.info("going to use ParetoI model ping dataset for bytes=%d", bytes)
start_time <- Sys.time()

pattern <- "train"
dataset <- "March 2018"
#pattern <- "test"
#dataset <- "April 2018"
fname = file.path("data", paste0(pattern, "_ping_", bytes, ".csv"))
df = read.csv(fname, stringsAsFactors = F) 
end_time <- Sys.time()
flog.info("time taken for reading ping data for bytes=%d is %.2f seconds", bytes, difftime(end_time, start_time, units="secs"))

# print some random rows from the dataset
df_sample <- sample_n(df, 5)
col_names <- c("timestamp", "RTT (in milliseconds)", "packet size", "sequence number")
colnames(df_sample) <- col_names
fname = file.path("output", paste0(pattern, "_ping_", bytes, "_sample.csv"))
write.csv(df_sample, fname, row.names = F)
kable(df_sample, col.names = col_names)


# make a density plot
par(mfrow=c(1,2))

plot(density(df$rtt), main="Density plot of RTT measurements")


#######################
# wrangle it
#######################
r <- df$rtt[df$rtt>0]
r <- as.numeric(quantile(r, seq(0.01, 1, 0.01)))
# make a density plot
plot(density(r), main="Density plot of low-resolution version of data", col='blue')


dev.copy(png, file.path("plots", paste0(pattern, "_density_rtt.png")))
dev.off()
par(mfrow=c(1,1))


#######################
# plot it
#######################
n <- length(r)
plot(density(r))

#######################
# is pareto a good fit? Try with a flat prior
#######################
flog.info("########################################\n")
flog.info("Adequacy of fit with flat prior")
flog.info("########################################\n")
model <- "Flat prior"
B <- 20000
theta = rgamma(B, n+1, sum(log(r/min(r))))
# save theta for drawing inferences on new data
theta_df = data.frame(theta=theta)
fname = file.path("output", "posterior", paste0(pattern, "_", bytes, "b_theta_from_flat_prior.csv"))
write.csv(theta_df, fname, row.names = F)

q  = c(seq(0.1, 1, 0.1), 0.75, 0.95)
eval_results = map(q, eval_model, B, min(r), theta, r, pattern, bytes, dataset, model) %>%
  reduce(rbind)
fname <- file.path("output", "test_stat", paste0(pattern , "_", bytes, "b_model_fit_flat_prior.csv"))
write.csv(eval_results, fname, row.names = F)




#######################
# run Gibbs sampler
#######################
run1 <- run_gibbs_sampler(r, 1)
run2 <- run_gibbs_sampler(r, 1.5)
run3 <- run_gibbs_sampler(r, 2.5)
run4 <- run_gibbs_sampler(r, 3.0)

allChains <- mcmc.list(list(mcmc(run1$theta),
                            mcmc(run2$theta),
                            mcmc(run3$theta),
                            mcmc(run4$theta))) 
gelman.diag(allChains)

########################
# combine samples for posterior summaries and inference
########################
theta <- c(run1$theta, run2$theta, run3$theta, run4$theta)
theta_mat           = matrix(theta, ncol = 1)
colnames(theta_mat) = 'theta'
theta_list				<- mcmc.list(list(mcmc(theta_mat)))
suppressWarnings(mcmcplot1(theta_list, greek=T))

beta <- c(run1$beta, run2$beta, run3$beta, run4$beta)
beta_mat           = matrix(beta, ncol = 1)
colnames(beta_mat) = 'beta'
beta_list				<- mcmc.list(list(mcmc(beta_mat)))
suppressWarnings(mcmcplot1(beta_list, greek=T))

set.seed(20092013)
par(mfrow=c(1,1))
title <- paste0("Density plot of RTT samples")
plot(density(r), main=title, ylim=c(0, 0.0020))
lines(density(rpareto(10^4, min(r), median(theta))), col='blue')
# Add a legend
legend("topright", legend=c("Actual data", "Model (used median of theta)"),
       col=c("black", "blue"), lty=1:1, cex=0.8)
dev.copy(png, file.path("plots", paste0(pattern, "_density_rtt_actual_and_model.png")))
dev.off()


########################
# adequacy of fit
########################
flog.info("########################################\n")
flog.info("Adequacy of fit after Gibbs sampling")
flog.info("########################################\n")

B <- 20000
model = "Gibbs sampler"
eval_results = map(q, eval_model, B, min(r), theta, r, pattern, bytes, dataset, model) %>%
  reduce(rbind)
fname <- file.path("output", "test_stat", paste0(pattern, "_", bytes, "b_model_fit_gibbs_sampler.csv"))
write.csv(eval_results, fname, row.names = F)

# save theta for drawing inferences on new data
theta_df = data.frame(theta=theta)
fname = file.path("output", "posterior", paste0(pattern, "_", bytes, "b_theta_from_gibbs_sampler.csv"))
write.csv(theta_df, fname, row.names = F)

# save beta for drawing inferences on new data
beta_df = data.frame(beta=beta)
fname = file.path("output", "posterior", paste0(pattern, "_", bytes, "b_beta_from_gibbs_sampler.csv"))
write.csv(beta_df, fname, row.names = F)

#######################
# compare theta from flat prior and Gibbs sampler
#######################
fname = file.path("output", "posterior", paste0(pattern, "_", bytes, "b_theta_from_gibbs_sampler.csv"))
theta_gibbs = read.csv(fname)
theta_gibbs$model = "Gibbs sampler"
theta_gibbs$dataset = dataset

fname = file.path("output", "posterior", paste0(pattern, "_", bytes, "b_theta_from_flat_prior.csv"))
theta_flat_prior = read.csv(fname)
theta_flat_prior$model = "Flat prior"
theta_flat_prior$dataset = dataset

theta = rbind(theta_gibbs, theta_flat_prior)
p <- ggplot(data=theta) +
  theme_tq() + 
  geom_density(aes(x=theta, y = ..density.., col=model)) +
  labs(x = "theta",
       title = "Comparing theta from two models",
       subtitle = " Gibbs sampler Vs Flat prior",
       caption=paste0("(dataset: ", dataset, ")"))
fname = file.path("plots", paste0(pattern, "_", bytes, "b_model_comparison.png"))
ggsave(fname)

# create summary as a table
theta_summary = theta %>%
  group_by(model) %>%
  summarize(q_025 = quantile(theta, c(0.025)),
            q_500 = quantile(theta, c(0.5)),
            q_975 = quantile(theta, c(0.975))) %>%
  mutate(point_estimate = round(q_500, 4), CI = paste0("[", round(q_025, 4), ", ", round(q_975, 4), "]"))
fname = file.path("output", "posterior", paste0(pattern, "_", bytes, "b_theta_sensitivity.csv"))
write.csv(theta_summary, fname, row.names = F)
kable(theta_summary[, c("model", "point_estimate", "CI")], col.names = c("Model", "Median", "95% credible interval"))

#######################
# Compare results
#######################
path <- file.path("output", "test_stat")
files <- file.path(path, dir(path=path, pattern = "^(test|train)"))
results <- files %>%
  map(read.csv, stringsAsFactors = F) %>%    # read in all the files individually
  reduce(rbind)        # reduce with rbind into one dataframe

# sensitivity analysis
dataset <-"March 2018"
results_train = results[results$dataset == dataset, c("q", "model", "CI", "in_ci")]
## this works only because in_ci value happens to be same for both models!!!
results_spread = spread(results_train, model, CI) %>%
  mutate(q = paste0("T", as.integer(100*q)))
fname = file.path("output", "sensitivity", paste0(dataset, "_", bytes, "b_flat_vs_Gibbs.csv"))
write.csv(results_spread, fname)
x = kable(results_spread, col.names = c("Statistic", "In 95% credible interval?", 
      "Flat Prior", "Gibbs Sampler"), format = "latex", booktabs = T) %>%
  column_spec(2:3, width = "2.5cm")
fname = file.path("plots", paste0(pattern, "_", bytes, "b_model_comparison_credible_interval"))
kable_as_image(x, filename = fname)

m <- "Gibbs sampler"
title <- "Comparing RTT in March and April 2018"

p = results %>%
  filter(model == m) %>%
  filter(q >= 0.5 & q != 1.0) %>%
  ggplot(aes(x = 100*q, y = mean_test_stat, col=dataset)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymax = CI_U, ymin = CI_L)) + 
  labs(x="Percentile", y="RTT (milliseconds)",
       title=title,
       subtitle="95% credible intervals and median value for key test statistics", 
       caption=paste0("(Model: ", m, ")"),
       col="") +
  theme_bw() + 
  theme(legend.position = "bottom")
p
fname = file.path("plots", paste0("rtt_comparison_two_datasets_", m, ".png"))
ggsave(fname)

###################################
# make inferences about new data using theta from training set
###################################
pattern <- "train"
fname = file.path("output", "posterior", paste0(pattern, "_", bytes, "b_theta_from_gibbs_sampler.csv"))
theta_gibbs = read.csv(fname)
theta = theta_gibbs$theta

pattern <- "test"
dataset <- "April 2018"
fname = file.path("data", paste0(pattern, "_ping_", bytes, ".csv"))
df_test = read.csv(fname, stringsAsFactors = F) 

# lets choose April 13th for testing
r = df_test %>%
  filter(rtt != 0) %>%
  mutate(timestamp = ymd_hms(timestamp)) %>%
  filter(month(timestamp) == 4 & day(timestamp) == 13) %>%
  dplyr::select(rtt)

r <- as.numeric(quantile(r$rtt, seq(0.01, 1, 0.01)))

B       <- 20000
model   <- "Gibbs sampler"
pattern <- "new_data"
dataset <- "april13th"
q = c(0.75, 0.5, 0.6, 0.7, 0.8, 0.9)
eval_results = map(q, eval_model, B, min(r), theta, r, pattern, bytes, dataset, model) %>%
  reduce(rbind)
fname <- file.path("output", "test_stat", paste0(pattern, "_", bytes, "b_model_fit_gibbs_sampler.csv"))
write.csv(eval_results, fname, row.names = F)



