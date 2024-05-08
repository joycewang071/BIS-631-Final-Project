# 2x2 DiD estimand ----

# Filter data to get means for relevant groups and periods
treatment_pre <- filter(weibo_week, IP_May == "Shanghai", week < 13)
treatment_post <- filter(weibo_week, IP_May == "Shanghai", week >= 13)
control_pre <- filter(weibo_week, IP_May != "Shanghai", week < 13)
control_post <- filter(weibo_week, IP_May != "Shanghai", week >= 13)

# Calculate means for each group
mean_treatment_pre <- mean(treatment_pre$proportion, na.rm = TRUE)
mean_treatment_post <- mean(treatment_post$proportion, na.rm = TRUE)
mean_control_pre <- mean(control_pre$proportion, na.rm = TRUE)
mean_control_post <- mean(control_post$proportion, na.rm = TRUE)

# Calculate DID estimand
tau_hat_DID <- (mean_treatment_post - mean_treatment_pre) - (mean_control_post - mean_control_pre)
tau_hat_DID

# Robust Variance of the 2x2 DID Estimator ----

# Sample sizes (you'll need to replace N with actual counts from your data)
N1_t1 <- nrow(treatment_post)
N1_t <- nrow(treatment_pre)
N0_t1 <- nrow(control_post)
N0_t <- nrow(control_pre)

# Variance estimates (dummy values, replace with actual calculations)
sigma2_1_t1 <- var(treatment_post$proportion, na.rm = TRUE)
sigma2_1_t <- var(treatment_pre$proportion, na.rm = TRUE)
sigma2_0_t1 <- var(control_post$proportion, na.rm = TRUE)
sigma2_0_t <- var(control_pre$proportion, na.rm = TRUE)

# Calculate robust variance of DID estimator
var_tau_hat_DID <- (sigma2_1_t1 / N1_t1) + (sigma2_1_t / N1_t) + (sigma2_0_t1 / N0_t1) + (sigma2_0_t / N0_t)
var_tau_hat_DID

