population <- tibble(
  climate_change_affects = c(rep("Yes", 62000), rep("No", 38000))
)
ggplot(population, aes(x = climate_change_affects)) + 
  geom_bar() +
  labs(
    x = "", y = "",
    title = "Do you think climate change is affecting your local community?"
  )

population %>% 
  count(climate_change_affects) %>% 
  mutate(p = n/sum(n))

n <- 120
samp <- population %>%
  sample_n(size = n)

samp %>% count(climate_change_affects) %>% mutate(p = n/sum(n))

#specific numbers taken from previous question
n*0.55 #success condition check
n*0.45 #fail    condition check

##confidence level finding

#99% confidence interval
a <- 0.01
percentile <- (1-a/2)
qnorm(percentile, 0, 1)

###creating a 90% confidence interval for samp

#Retrieve Point Estimate
point_estimate <- samp %>%
  count(climate_change_affects) %>% 
  mutate(p = n/sum(n))
point_estimate <- point_estimate$p[2]

#Calculate Critical Value
a <- 0.1
percentile <- (1-a/2)
crit_val <- qnorm(percentile, 0, 1)

#Calculate Standard Error
std_err <- sqrt((point_estimate * (1-point_estimate))/n)

#Calculate Confidence Interval
lower_bound <- point_estimate - crit_val * std_err
upper_bound <- point_estimate + crit_val * std_err

lower_bound
upper_bound

#Creating the data frame for the sampling distribution
critical_value <- qnorm(0.95, 0, 1)
sample_size <- 120

sample_props120 <- population %>%
  rep_sample_n(size = sample_size, reps = 10000, replace = TRUE) %>%
  count(climate_change_affects) %>%
  filter(climate_change_affects == "Yes") %>%
  mutate(p_hat = n / sample_size) %>%
  mutate(lower = p_hat - (critical_value * sqrt((p_hat*(1-p_hat))/sample_size)), 
         upper = p_hat + (critical_value * sqrt((p_hat*(1-p_hat))/sample_size))) %>%
  mutate(capture_parameter = (lower <= 0.62 & upper >= 0.62))

mean(sample_props120$capture_parameter)


#Mean Confidence Intervals
microbrews <- read_csv(file.choose())
samp_mean <- mean(microbrews$abv)

#nrow() gets the number of rows in the dataframe. which is the sample size
samp_size <- nrow(microbrews)
mean_stderr <- sd(microbrews$abv) / sqrt(samp_size)
mean_a <- 0.01 #Alpha value for a 90% confidence interval
mean_percentile <- (1-a/2)

#qt() is used to find the critical value of a t-distribution
#   the degrees of freedom is the sample size - 1
mean_crit_val <- qt(mean_percentile, samp_size-1)

mean_lower_bound <- samp_mean - mean_crit_val * mean_stderr
mean_upper_bound <- samp_mean + mean_crit_val * mean_stderr
