library(readr)
library("readxl")
library(ggrepel)
require(ggplot2)
library(broom)
library(investr)
library(dplyr)
library(tidyr)
library(pracma)
library(lme4)
library(R.matlab)
library(gridExtra)


full_path_rolling <-"/Volumes/Effort_data/brainstorm_db/ADOEFFORT_protocol/data/Group_analysis/@intra/matrix_concat_240309_0120_band.mat"
full_path_static <-"/Volumes/Effort_data/brainstorm_db/ADOEFFORT_protocol/data/Group_analysis/@intra/matrix_concat_240309_0122_band.mat"
full_path_proactive <- "/Volumes/Effort_data/brainstorm_db/ADOEFFORT_protocol/data/Group_analysis/@intra/matrix_concat_240309_0125_band.mat"
full_path_reactive <-"/Volumes/Effort_data/brainstorm_db/ADOEFFORT_protocol/data/Group_analysis/@intra/matrix_concat_240309_0126_band.mat"

mat_data <- readMat(full_path_reactive)


# Assuming your data is stored in mat_data
tTime <- t(mat_data[["Time"]])
t10 <- t(mat_data[["Value"]][1,])
t20 <- t(mat_data[["Value"]][2,])
t30 <- t(mat_data[["Value"]][3,])
# Convert mat_data to a data frame
mat_df <- data.frame(Time = t(mat_data[["Time"]]),
                     Value1 = t(t10),
                     Value2 = t(t20),
                     Value3 = t(t30))

# Convert data to long format for ggplot
mat_df_long <- pivot_longer(mat_df, cols = starts_with("Value"), names_to = "Variable", values_to = "Value")

# Plot the smoothed lines with weaker smoothing
p <- ggplot(mat_df_long, aes(x = Time, y = Value, color = Variable)) +
  geom_smooth(method = "loess", span = 0.1) + # Use loess method with weaker smoothing
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Seconds", y = "Amplitude", title = "Evoke Response Potential (Reactive)") +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.2))+
  guides(color = guide_legend(title = "Difficulty"))

p
# Save the plot in high resolution
ggsave("/Users/ali/Desktop/effort_p3.png", plot = p, width = 10, height = 6, dpi = 300)



back_up <- dfa
dfa <- back_up

dfa <- na.omit(dfa)
# Filter the data frame based on strategy
static_data <- dfa %>% filter(strategy == "static")
rolling_data <- dfa %>% filter(strategy == "rolling")

# Fit polynomial regressions and extract p-values for "static" strategy
static_model <- lm(p3_norm ~ poly(effort, 1), data = static_data)
static_p_value <- summary(static_model)$coefficients[2, 4]

# Fit polynomial regressions and extract p-values for "rolling" strategy
rolling_model <- lm(p3_norm ~ poly(effort, 1), data = rolling_data)
rolling_p_value <- summary(rolling_model)$coefficients[2, 4]

# Create the ggplot with smooth line and constant standard error interval
p <- ggplot(dfa, aes(x = pupil_norm, y = score, color = factor(m_correct))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 1), se = FALSE) +
  #labs(x = "Self-reported Effort", y ="Normlized P300", color = "Strategy") +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.9))

p
# Annotate the plot with p-values
p + labs(caption = paste("p-value rolling: ", round(rolling_p_value, 3), " p-value static: ", round(static_p_value, 3)))

# Save the plot in high resolution
ggsave("/Users/ali/Desktop/effort_p3.png", plot = p, width = 10, height = 6, dpi = 300)




# Fit polynomial regressions for "static" strategy
static_model <- lm(score ~ poly(pupil_norm, 2) * n_back, data = static_data)

# Extract p-value for interaction term for "static" strategy
static_p_value <- summary(static_model)$coefficients["poly(pupil_norm, 2):n_back", "Pr(>|t|)"]

# Fit polynomial regressions for "rolling" strategy
rolling_model <- lm(score ~ poly(pupil_norm, 2) * n_back, data = rolling_data)

# Extract p-value for interaction term for "rolling" strategy
rolling_p_value <- summary(rolling_model)$coefficients["poly(pupil_norm, 2):n_back", "Pr(>|t|)"]

# Create the ggplot with smooth line and constant standard error interval
p <- ggplot(dfa, aes(x = pupil_norm, y = alpha, color = strategy, shape = factor(m_correct))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 1), se = FALSE) +
  labs(x = "Normalized Pupil Size", y = "dprime", color = "Strategy", shape = "Difficulty") +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.8))
p
# Annotate the plot with p-values
p + labs(caption = paste("p-value rolling: ", round(rolling_p_value, 3), " p-value static: ", round(static_p_value, 3)))

