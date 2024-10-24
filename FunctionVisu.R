visu_lm <- function(lm_model) {
  # Extract coefficients and p-values
  coefficients <- coef(summary(lm_model))[, "Estimate"]
  p_values <- coef(summary(lm_model))[, "Pr(>|t|)"]
  
  title_ml <- capture.output(summary(lm_model))[3]
  
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
  
  # Coefficient plot with p-values and adjusted R-squared in caption
  coefficient_plot <- ggplot(visualization_data, aes(x = predictor, y = coefficient)) +
    geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7) +
    geom_text(aes(label = sprintf("%.3f", p_value), y = 0), 
              vjust = -0.5, color = "black", size = 3) +
    labs(title = title_ml,
         x = "Predictors",
         caption = paste("\nAdjusted R-squared:", round(adjusted_r_squared, 3))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(coefficient_plot)
  ggsave(paste0("/Volumes/x9/results/allsubs/figure/",title_ml,".png"), plot = coefficient_plot, width = 10, height = 10, dpi = 300)
  
}

visu_lmr <- function(model) {
  # Extract coefficients and p-values
  coefficients <- coef(summary(model))[, "Estimate"]
  std_errors <- coef(summary(model))[, "Std. Error"]
  p_values <- coef(summary(model))[, "Pr(>|t|)"]
  
  title_ml <- capture.output(summary(model))[2]
  
  # Calculate adjusted R-squared
  adjusted_r_squared <- summary(lm_model)$adj.r.squared
  
  # Preprocess predictor names
  predictors <- gsub("_norm", "", names(coefficients))
  
  # Create a data frame for visualization
  visualization_data <- data.frame(
    predictor = predictors,
    coefficient = coefficients,
    std_error = std_errors,
    p_value = p_values
  )
  
  # Coefficient plot with p-values and adjusted R-squared in caption
  coefficient_plot <- ggplot(visualization_data, aes(x = predictor, y = coefficient)) +
    geom_point() +
    geom_errorbar(aes(ymin = coefficient - std_error, ymax = coefficient + std_error), width = 0.2) +
    #geom_text(aes(label = sprintf("%.3f", p_value), y = 0), vjust = -0.5, color = "black", size = 3) +
    labs(title = title_ml,
         x = "Predictors",
         caption = paste("\nAdjusted R-squared:", round(adjusted_r_squared, 3))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme_minimal()
  
  print(coefficient_plot)
  ggsave(paste0("/Volumes/x9/results/allsubs/figure/",title_ml,".png"), plot = coefficient_plot, width = 10, height = 10, dpi = 300)
}



visu_lmr_combined <- function(models, model_names) {
  # Initialize an empty data frame to hold combined data
  combined_data <- data.frame()
  
  for (i in seq_along(models)) {
    # Extract model and its summary
    model <- models[[i]]
    summary_model <- summary(model)
    
    # Extract coefficients, standard errors, and p-values
    coefficients <- coef(summary_model)[, "Estimate"]
    std_errors <- coef(summary_model)[, "Std. Error"]
    p_values <- coef(summary_model)[, "Pr(>|t|)"]
    
    # Preprocess predictor names
    predictors <- gsub("_norm", "", names(coefficients))
    
    # Create a data frame for this model
    model_data <- data.frame(
      predictor = predictors,
      coefficient = coefficients,
      std_error = std_errors,
      p_value = p_values,
      model = model_names[i]
    )
    
    # Combine this model's data with the overall combined data
    combined_data <- rbind(combined_data, model_data)
  }
  
  # Coefficient plot with error bars, p-values, and adjusted R-squared in caption
  coefficient_plot <- ggplot(combined_data, aes(x = predictor, y = coefficient, fill = model, color = model)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7), alpha = 0.7) +
    geom_errorbar(aes(ymin = coefficient - std_error, ymax = coefficient + std_error), 
                  width = 0.2, position = position_dodge(width = 0.7)) +
    #geom_text(aes(label = sprintf("%.3f", p_value), y = 0), position = position_dodge(width = 0.7), vjust = -0.5, color = "black", size = 3) +
    labs(title = "Coefficient Comparison Across Models",
         x = "Predictors") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(coefficient_plot)
  sentence <- paste(unlist(model_names), collapse = "_")
  ggsave(paste0("/Volumes/x9/results/allsubs/figure/combined_coefficient_plot_",sentence,"_bar.png"), plot = coefficient_plot, width = 10, height = 10, dpi = 300)
  
  # line plot
  # Create the time column with NA as default
  combined_data$time <- NA
  combined_data$time[grepl("t1$", combined_data$model)] <- "t1"
  combined_data$time[grepl("t2$", combined_data$model)] <- "t2"
  
  # Create the strategy column
  #combined_data$predictor <- sub("^task", "", combined_data$predictor)
  #names(combined_data)[names(combined_data) == "predictor"] <- "task"
  
  # Create the effort column with NA as default
  combined_data$category <- NA
  combined_data$category[grepl("^1", combined_data$model)] <- "1"
  combined_data$category[grepl("^2", combined_data$model)] <- "2"
  combined_data$category[grepl("^3", combined_data$model)] <- "3"
  
  #combined_data$task <- NA
  #combined_data$task <- ifelse(combined_data$strategy %in% c("rolling", "static"), "n-back", "ax-cpt")
  
  # Create the plot
  coefficient_plot <- ggplot(combined_data, aes(x = time, y = coefficient, group = interaction( category), color = interaction(category))) +
    geom_line() +
    geom_point() +
    facet_wrap(~predictor) +
    labs(title = "Coefficient over Time by Effort and Strategy",
         x = "Time",
         y = "Coefficient") +
    theme_minimal()
  
  print(coefficient_plot)
  sentence <- paste(unlist(model_names), collapse = "_")
  ggsave(paste0("/Volumes/x9/results/allsubs/figure/combined_coefficient_plot_",sentence,"_line_strategy.png"), plot = coefficient_plot, width = 10, height = 10, dpi = 300)
  
}












# Create the time column with NA as default
combined_data$time <- NA

# Update the time column based on the condition
combined_data$time[grepl("t1$", combined_data$model)] <- "t1"
combined_data$time[grepl("t2$", combined_data$model)] <- "t2"

# Create the effort column with NA as default
combined_data$effort <- NA

# Update the effort column based on the condition
combined_data$effort[grepl("^h", combined_data$model)] <- "high"
combined_data$effort[grepl("^l", combined_data$model)] <- "low"

combined_data$task <- NA
combined_data$task <- ifelse(combined_data$predictor %in% c("strategyrolling", "strategystatic"), "n-back", "ax-cpt")

# Create the plot
coefficient_plot <- ggplot(combined_data, aes(x = time, y = coefficient, group = interaction(effort, predictor), color = interaction(effort, predictor))) +
  geom_line() +
  geom_point() +
  facet_wrap(~task) +
  labs(title = "Coefficient over Time by Effort and Strategy",
       x = "Time",
       y = "Coefficient") +
  theme_minimal()

coefficient_plot
