library(pwr)
require(ggplot2)
library(readr)
library(effsize)

mother_dfa <- read_csv(paste0("/Users/ali/Documents/Experiment/dfa-test.csv"))

dfa<-mother_dfa
dfa <- subset(dfa, task=='n-back')
#Subset the data for the two conditions
dfa_condition_1 <- dfa[dfa$n_back == 1, c("sub", "effort")]
dfa_condition_3 <- dfa[dfa$n_back == 3, c("sub", "effort")]

# Merge the two data frames by subject_id to ensure alignment
merged_dfa <- merge(dfa_condition_1, dfa_condition_3, by = "sub", suffixes = c("_1", "_3"))

# Rename the columns for better clarity
colnames(merged_dfa) <- c("sub", "baseline", "treatment")
# Now, calculate the difference for alpha between conditions
dfa_diff <- merged_dfa$baseline - merged_dfa$treatment

mean_dfa_diff <- mean(dfa_diff, na.rm = TRUE)
sd_dfa_diff <- sd(dfa_diff, na.rm = TRUE)

cohen_d <- mean_dfa_diff / sd_dfa_diff   # Cohen's d formula

pwr.t.test(d = cohen_d, n = nrow(merged_dfa), sig.level = 0.01, type = "paired", alternative = "two.sided")

pwr.t.test(d = cohen_d, power = 0.80, sig.level = 0.01, type = "paired", alternative = "two.sided")

power_analysis <- pwr.t.test(d = cohen_d, power = 0.80, sig.level = 0.01, type = "paired", alternative = "two.sided")


# Rename the columns for better clarity
colnames(merged_dfa) <- c("sub", "baseline", "treatment")

# Remove rows with missing or non-finite values
merged_dfa <- na.omit(merged_dfa) # Remove rows with NA
merged_dfa <- merged_dfa[is.finite(merged_dfa$baseline) & is.finite(merged_dfa$treatment), ] # Remove non-finite values

# Calculate the difference between the two conditions
dfa_diff <- merged_dfa$baseline - merged_dfa$treatment

# Descriptive statistics
mean_dfa_diff <- mean(dfa_diff, na.rm = TRUE)
sd_dfa_diff <- sd(dfa_diff, na.rm = TRUE)

# Check for valid standard deviation (prevent division by zero)
if(sd_dfa_diff > 0) {
  cohen_d <- mean_dfa_diff / sd_dfa_diff   # Cohen's d calculation
} else {
  cohen_d <- NA  # If SD is 0 or not valid, set Cohen's d to NA
}

# Perform paired t-test
t_test <- t.test(merged_dfa$baseline, merged_dfa$treatment, paired = TRUE)

# Create a data frame for plotting with color aesthetic
plot_data <- data.frame(
  value = c(merged_dfa$baseline, merged_dfa$treatment),
  group = rep(c("Baseline", "Treatment"), each = nrow(merged_dfa))
)

# Plotting the density and adding the statistical results
ggplot(plot_data, aes(x = value, color = group)) +
  # Density plot for both groups
  geom_density(size = 1) +
  # Add means and confidence intervals for baseline and treatment
  geom_errorbarh(aes(xmin = mean(merged_dfa$baseline, na.rm = TRUE) - sd(merged_dfa$baseline, na.rm = TRUE),
                     xmax = mean(merged_dfa$baseline, na.rm = TRUE) + sd(merged_dfa$baseline, na.rm = TRUE),
                     y = 0.0), height = 0.00, color = "blue") +
  geom_errorbarh(aes(xmin = mean(merged_dfa$treatment, na.rm = TRUE) - sd(merged_dfa$treatment, na.rm = TRUE),
                     xmax = mean(merged_dfa$treatment, na.rm = TRUE) + sd(merged_dfa$treatment, na.rm = TRUE),
                     y = 0.0), height = 0.00, color = "red", linetype = "dashed") +
  scale_color_manual(values = c("Baseline" = "blue", "Treatment" = "red")) +  # Match colors with lines
  theme_minimal() + 
  labs(title = "Density Plot of Baseline vs Treatment",
       x = "Values", y = "Probability", color = "Group") #+ annotate("text", x = mean(merged_dfa$baseline, na.rm = TRUE), y = 0.01, label = paste0("t: ", round(t_test$statistic, 2), ", df: ", round(t_test$parameter, 1), ", p-value: ", round(t_test$p.value, 4),if(!is.na(cohen_d)) paste0(", Cohen's d: ", round(cohen_d, 2)) else ""),size = 4, color = "black")



print(power_analysis$n/22)
print(cohen_d)
print(pwr.t.test(d = cohen_d, n = nrow(merged_dfa), sig.level = 0.01, type = "paired", alternative = "two.sided"))

