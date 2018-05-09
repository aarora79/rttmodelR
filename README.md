# rttmodelR
Bayesian modelling of ping response time in a telecommunication network.

Latency is a key metric for network performance especially during periods of congestion. One of the measures of network latency is round trip time (RTT) which is defined as the length of time it takes for a data packet to be sent from a sender on one side of the link to a receiver on the other side and its acknowledgement to be received back by the sender. A larger RTT generally implies that the network might be congested or there may be other anomalies. 

We collected RTT measurements from a communication network for a two month period (March-April 2018) and studied their distribution. Data is split into two datasets, a training dataset corresponding to data from March 2018 and a test dataset corresponding to data from April 2018. All RTT meansurments are in milliseconds and correspond to a 40 byte packet. 

There were two main objectives of this study as listed below.

1. Determine if the RTT measurements could be modeled as a well known distribution (see Figure 1) and if so use the draws from the posterior of the model parameters to make inferences on key test statistics (such as the 75th percentile of the RTT which would represent the upper bound on the RTT for 75% of the traffic).

2. Use the draws from the posterior of the model parameters to make inferences about new data collected in the future without having to collect large amount of data. Answer questions such as "is the network more congested now than it was last month?", "has the 75th percentile of the RTT increased?". Any change in the inferences about new data as compared to the original data could then be viewed in context of either endogenous factors (such as upgrade of a key network component) or exogenous factors (such as huge subscriber growth) or a combination thereof.

The data was modelled using the Pareto Type I distribution first using a flat prior for the scale parameter and then using a (Gamma) conjugate prior. Gibbs sampler was used to sample from the full conditional distribution for the scale parameter and the hyperparameter used for the conjugate prior.

![](https://github.com/aarora79/rttmodelR/raw/master/plots/figure4.png)


