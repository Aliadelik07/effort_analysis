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
library(eegkit)
library(gridExtra)
library(corrplot)

dfa <- read_csv(paste0("/Users/ali/Desktop/Experiment/allsubs-nback-test.csv"))

dfa <- subset(dfa, sub != "sub01") # sub01 saw the feedback then indicated his judgments...

# Convert "rolling" to 1 and "static" to 0
dfa$strategy <- ifelse(dfa$strategy == "rolling", 1, ifelse(dfa$strategy == "static", 0, dfa$strategy))
dfa$strategy <- as.factor(dfa$strategy)

# Fit logistic regression model
model <- glm(strategy ~ pac_lang , data = dfa, family = binomial)
summary(model)

# Predict probabilities using the model
predicted_probabilities <- predict(model, type = "response")
# Create a dataframe with predicted probabilities
plot_data <- data.frame(theta = dfa$pac_lang, 
                        predicted_probability = predicted_probabilities)

# Plot the model
ggplot(plot_data, aes(x = theta, y = predicted_probability)) +
  geom_line(color = "blue") +
  geom_point(data = dfa, aes(y = as.numeric(strategy)), color = "red") +
  labs(x = "theta_norm", y = "Predicted Probability", 
       title = "Logistic Regression Model") +
  ylim(0, 1) +
  theme_minimal()

# OLS
lm_model <- lm( effort ~ pac_norm * alpha_norm + theta_norm , data = dfa)
summary(lm_model)

# Capture summary as text
summary_text <- capture.output(summary(lm_model))

# Save the summary as a .png image
png("/Users/ali/Desktop/effort_summary.png", width = 800, height = 600)
cat(summary_text, sep = "\n")  # Print summary text
dev.off()


# mixed models
slider_effort.response
model <- lmer( dprime ~ alpha_norm +  (1|sub), data = dfa)
summary(model)

ggplot(dfa, aes(x = theta, y = alpha)) +
  stat_smooth(method = "lm", se = FALSE) +
  geom_point() + 
  facet_wrap(~ sub, scales = "free")


# Create 3D scatterplot with grouping by dfa$n_back
plot_ly(data = subset(dfa, n_back > 0), 
        x = ~ pac_norm, y = ~ alpha_norm, z = ~ dprime, color = ~factor(strategy),
        type = "scatter3d", mode = "markers") %>%
  add_trace(x = ~dfa$alpha_norm, y = ~dfa$pac_norm, z = predict(lm_model), 
            type = "scatter3d", mode = "lines", line = list(color = "blue")) %>%
  layout(scene = list(xaxis = list(title = "theta"),
                      yaxis = list(title = "alpha"),
                      zaxis = list(title = "tpac"),
                      aspectmode = "cube"))

ggplot(subset(dfa,n_back>2), aes(x = pac_lang, y = dprime)) +
  stat_smooth(method = "lm", formula =  y ~  poly(x,1), se = FALSE) +
  geom_point() + 
  stat_poly_eq(
    aes(label = paste(formatC(..p.value.., digits = 2, format = "f"))), 
    formula = y ~ poly(x, 1), 
    parse = TRUE, 
    geom = "text", 
    size = 5,
    label.x = "left",  # Adjust the x position of the label
    label.y = "top",
    color = "blue" # Adjust the y position of the label
  ) +
  theme_minimal()





# Fit the linear model
lm_model <- lm(pupil_norm ~ pac_norm:alpha_norm , data = dfa)
summary(lm_model)

# Extract coefficients and p-values
coefficients <- coef(summary(lm_model))[, "Estimate"]
p_values <- coef(summary(lm_model))[, "Pr(>|t|)"]

# Calculate adjusted R-squared
adjusted_r_squared <- summary(lm_model)$adj.r.squared

# Preprocess predictor names
predictors <- gsub("_norm", "", names(coefficients))

# Create a data frame for visualization
visualization_data <- data.frame(
  predictor = predictors,
  coefficient = coefficients,
  p_value = p_values
)

# Calculate correlation matrix
cor_matrix <- cor(dfa[, c("pupil_norm", "pac_norm", "alpha_norm", "theta_norm")])

# Coefficient plot with p-values and adjusted R-squared in caption
coefficient_plot <- ggplot(visualization_data, aes(x = predictor, y = coefficient)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_text(aes(label = sprintf("%.3f", p_value), y = coefficient + 0.05), 
            vjust = -0.5, color = "black", size = 3) +
  labs(title = "Coefficient Plot of Pupil Size",
       x = "Predictors",
       caption = paste("\nAdjusted R-squared:", round(adjusted_r_squared,3))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

coefficient_plot
# Save the plot
png("/Users/ali/Desktop/pupil_coefficient_plot.png", width = 1400, height = 1220, res = 300)
print(coefficient_plot)
dev.off()
