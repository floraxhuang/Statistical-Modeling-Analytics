#Traditional ANOVA
#May be achieved by oneway.test or aov

#Bootstrapped ANOVA
boot_anova <- function(t1, t2, t3, t4, treat_nums) {
  size1 = length(t1)
  size2 = length(t2)
  size3 = length(t3)
  size4 = length(t4)
  
  null_grp1 = sample(t1 - mean(t1), size1, replace=TRUE)
  null_grp2 = sample(t2 - mean(t2), size2, replace=TRUE)
  null_grp3 = sample(t3 - mean(t3), size3, replace=TRUE)
  null_grp4 = sample(t4 - mean(t4), size4, replace=TRUE)
  null_values = c(null_grp1, null_grp2, null_grp3,null_grp4)
  
  alt_grp1 = sample(t1, size1, replace=TRUE)
  alt_grp2 = sample(t2, size2, replace=TRUE)
  alt_grp3 = sample(t3, size3, replace=TRUE)
  alt_grp4 = sample(t4, size4, replace=TRUE)
  alt_values = c(alt_grp1, alt_grp2, alt_grp3, alt_grp4)
  
  return(c(oneway.test(null_values ~ treat_nums, var.equal=TRUE)$statistic,
           oneway.test(alt_values ~ treat_nums, var.equal=TRUE)$statistic))
}