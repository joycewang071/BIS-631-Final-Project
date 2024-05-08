# devtools::install_github("synth-inference/synthdid")

library(synthdid)
library(ggplot2)

library(purrr)

library(xtable)

# Create 3-D array control covariats ----
# First, make sure your data frame is complete with all weeks and IP_May combinations
all_weeks <- min(weibo_week$week):max(weibo_week$week)
all_IP_May <- unique(weibo_week$IP_May)
covariate_names <- c("female", "verified")

# Create an empty list to store data for each IP_May
weibo_list <- vector("list", length(all_IP_May))
names(weibo_list) <- all_IP_May

# Fill the list with complete data frames for each IP_May
for (ip in all_IP_May) {
  temp_data <- weibo_week %>%
    filter(IP_May == ip) %>%
    complete(week = all_weeks, fill = list(female = NA, verified = NA)) %>%
    arrange(week)
  weibo_list[[ip]] <- temp_data
}

# Number of weeks and covariates
n_weeks <- length(all_weeks)
n_covariates <- length(covariate_names)

# Initialize the 3D array
weibo_array <- array(NA, dim = c(length(all_IP_May), n_weeks, n_covariates))
dimnames(weibo_array) <- list(all_IP_May, all_weeks, covariate_names)

# Populate the array
for (i in seq_along(all_IP_May)) {
  for (j in seq_along(covariate_names)) {
    weibo_array[i, , j] <- weibo_list[[i]][[covariate_names[j]]]
  }
}


# setup ----


setup = panel.matrices(as.data.frame(as_tibble(weibo_week)),  unit = 1,
                       time = 2,
                       outcome = 3,
                       treatment = 7,
                       treated.last = TRUE)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)

estimates = list(tau.hat)

print(unlist(estimates))

sqrt(vcov(tau.hat, method='placebo'))

synthdid_plot(estimates, se.method='placebo')

attributes(tau.hat)

unit.weights = synthdid_controls(estimates, weight.type='omega', mass=1)
time.weights = synthdid_controls(estimates, weight.type='lambda', mass=1)


unit.table <- xtable(data.frame(Weight = unit.weights))
time.table = xtable(data.frame(Weight = time.weights))
print(unit.table)

data.frame(Weight = unit.weights)
data.frame(Weight = time.weights)
