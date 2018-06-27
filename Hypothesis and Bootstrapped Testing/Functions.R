#Bootstrapping mean
boot_mean <- function(sample0) {
  resample <- sample(sample0, length(sample0), replace=TRUE)
  return(mean(resample))
}

#Bootstrapping mean difference
boot_mean_diffs <- function(sample0, mean_hyp) {
  resample <- sample(sample0, length(sample0), replace=TRUE)
  return( mean(resample) - mean_hyp )
}

#Estimate the difference between means of both bootstrapped samples.
bootstrap_diff_median <- function(sample0,sample1){
  resample_0 <- sample(sample0,size = length(sample0),replace = TRUE)
  resample_1 <- sample(sample1,size = length(sample1),replace = TRUE)
  diff_median <- median(resample_0)-median(resample_1)
  return(diff_median)
}

#T-statistics
#Bootstrapping T-statistics
boot_t_stat <- function(sample0, mean_hyp) {
  resample <- sample(sample0, length(sample0), replace=TRUE)
  diff <- mean(resample) - mean_hyp
  resample_se <- sd(resample)/sqrt(length(resample))
  return( diff/resample_se )
}

#Bootstrapped alternative values of t
boot_alt_t <- function(sample0,sample1){
  resample_0 <- sample(sample0,size = length(sample0),replace = TRUE)
  resample_1 <- sample(sample1,size = length(sample1),replace = TRUE)
  alt_t <- t.test(resample_0,resample_1,var.equal = FALSE)
  return(alt_t$statistic)}

#Bootstrapped null values of t
bootstrap_null_t <- function(sample0,com){
  resample_0 <- sample(sample0, size = length(sample0), replace = TRUE)
  null_t <- t.test(resample_0, com, var.equal = FALSE)
  return(null_t$statistic)}

#Median
#Bootstrap the difference between the two sample's median
#(compare the median of two bootstrapped samples)
bootstrap_diff_median <- function(sample0,sample1){
  resample_0 <- sample(sample0,size = length(sample0),replace = TRUE)
  resample_1 <- sample(sample1,size = length(sample1),replace = TRUE)
  diff_median <- median(resample_0)-median(resample_1)
  return(diff_median)
}

#Bootstrap the ‘null’ difference
#(compare the median of bootstrapped samples against the median of the original sample)
bootstrap_null_median <- function(sample0,com){
  resample_0 <- sample(sample0, size = length(sample0), replace = TRUE)
  null_median <- median(resample_0)-median(com)
  return(null_median)}

#F-Value (To compare variance)
#Bootstrap alternative f-stat
bootstrap_alt_f <- function(larger_sd_sample, smaller_sd_sample){
  resample_larger_sd <- sample(larger_sd_sample, length(larger_sd_sample), replace=TRUE)
  resample_smaller_sd <- sample(smaller_sd_sample, length(smaller_sd_sample), replace=TRUE)
  alt_f <- var(resample_larger_sd) / var(resample_smaller_sd)
  return(alt_f)}

#Bootstrap null f-stat
bootstrap_null_f <- function(larger_sd_sample){
  resample_larger_sd <- sample(larger_sd_sample, length(larger_sd_sample), replace=TRUE)
  null_f <- var(resample_larger_sd) / var(larger_sd_sample)
  return(null_f)}

