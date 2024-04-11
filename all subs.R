#importing the reposonses cleaned by python code with the same name
library(readr)
library("readxl")
library(ggrepel)
require(ggplot2)
library(investr)
library(dplyr)
library(ggpubr)
library(lmerTest)
library(ggpmisc)
library(kableExtra)
library(webshot)
library(xtable)
library(broom.mixed)
library(knitr)
library(plotly)
library(gridExtra)

write.csv(dfa, paste0("/Users/ali/Desktop/Experiment/allsubs-nback-test.csv"))
# if you already combined and just want to load agian
dfa <- read_csv(paste0("/Users/ali/Desktop/Experiment/allsubs-nback-test.csv"))
#------------------------------------------------------------
# Prepare a vector of subject numbers
subjects <- sprintf("%02d", 1:4)  # This will create "01", "02", "03", "04", "05"
#subjects <- c("01", "03", "04", "05","07","05")

subjects <- subjects[subjects != "05"] # doesn't have p3

#task <- "n_back"
#task <- "ax_cpt"

# Map each subject to a strategy
strategies <- c("rolling", "rolling", "rolling", "static", "static", "rolling", "static")
#strategies <- c("proactive", "proactive", "proactive", "reactive","reactive","proactive","reactive")

# Initialize an empty list to store data frames
df_list <- list()

# Loop through each subject and strategy
for(n in subjects) {

  file_path <- paste0("/Volumes/Effort_data/subs/sub", n, "/sub", n, "-", strategies[as.integer(n)], "-test.csv")
  df <- read_csv(file_path)
  
  # pac of specific channel to pac alone
  #columns_to_rename <- grep("^pac_", names(df))
  #names(df)[columns_to_rename] <- "pac"
  
  # Normalizing and adding 'sub' and 'strategy' columns
  df <- df %>% mutate(ps_norm = scale(ps),
                      theta_norm = scale(theta),
                      beta_norm = scale(beta),
                      gamma_norm = scale(gamma),
                      alpha_norm = scale(alpha),
                      pac_norm = scale(pac_lang),
                      p3_norm = scale(p3),
                      effort_category = ifelse(effort < median(effort), "low effort", "high effort"),
                      sub = paste0("sub", n),
                      strategy = strategies[as.integer(n)])
  

  
  # Add the processed data frame to the list
  df_list[[n]] <- df
}

# Combine all data frames into one
dfa <- bind_rows(lapply(df_list, select,'gamma','gamma_norm','pac_lang','pac_norm','beta','beta_norm','m_correct','theta','theta_norm','alpha','alpha_norm','p3','p3_norm','psd','tpac','slider_valenc.response','effort_category','slider_arous.response','slider_effort.response','slider_time.response','n_block','ps','dps','dprime','g','sub', 'ps_norm','strategy', 'effort','n_back', 'arousal','valence','rt_h','rt_f','time','score','DifficultyLevel'))
#dfa <- bind_rows(lapply(df_list, select,'DifficultyLevel','p3','dps','m_correct','slider_valenc.response','effort_category','slider_arous.response','slider_effort.response','slider_time.response','n_block','ps','dps','dprime','g','sub', 'pupil_norm','strategy', 'effort','n_back', 'arousal','valence','rt_h','rt_f','time','score'))

# Remove rows where m_correct > score
dfa <- dfa[dfa$m_correct <= dfa$score, ]

# weighted performance
dfa$wg <- dfa$g * dfa$n_back


# Iterate through each column of dfa
for (col in names(dfa)) {
  # Check if the column is numeric
  if (is.numeric(dfa[[col]])) {
    # Replace outliers with NA for the current column
    dfa[[col]][abs(scale(dfa[[col]])) >= 3] <- NA
  }
}

dfa$pac_lang[(dfa$pac_lang > 0.3) & (dfa$sub == "sub02" | dfa$sub == "sub04")] <- NA

#removing sub01 because he see the feedback and then rated effort
#dfa <- subset(dfa, sub != "sub01")
# ----------------------------------------------------------
# in some models lmer asks to scale these variables..
#dfa$tpac <- scale(dfa$tpac)
#dfa$theta <- scale(dfa$theta)
#dfa$alpha <- scale(dfa$alpha)
#------------------------------------ modeling!
model1 <- lmer(slider_effort.response ~ poly(n_back:strategy,2) + (1|sub), dfa, REML = FALSE)
model2 <- lmer(g ~ tpac + (1|sub), dfa, REML = FALSE)
summary(model2)

anova(model1, model2)


# impact of score on valence
#detrending valence
dfa$valence_dt <- dfa$slider_valenc.response - lm(slider_valenc.response ~ n_block, data = dfa)$fitted.values
# mixed model to see the impact of score
model_valence <- lmer(lead(valence_dt, 0) ~ score * n_back + (1|sub), data = subset(dfa,n_back <4))
summary(model_valence)

# let's visulize
# Obtain residuals
residuals <- resid(model_valence)

# Create a residual plot
residual_plot <- ggplot(data.frame(residuals), aes(x = fitted(model), y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  xlab("Fitted values") +
  ylab("Residuals") +
  ggtitle("Residual Plot")

# Create a QQ plot
qq_plot <- qqnorm(residuals, main = "QQ Plot of Residuals")
qq_line <- qqline(residuals, col = "red")


summary(lm(effort ~ ps_norm  , dfa))

# model for effort and performance
summary(lmer(dprime ~ slider_effort.response + strategy + n_back + (1|sub), dfa, REML= TRUE))
#------------------------------------------------------------
#------------------------------------ Kernel density of parameters

# --------------- don't forget to change the strategy!!!

dfa_long <- tidyr::gather(dfa, key = "variable", value = "value", 
                          slider_valenc.response, slider_arous.response, 
                          slider_effort.response, slider_time.response)

s <- "proactive"
gg <- ggplot(subset(dfa_long,strategy == s), aes(x = value, fill = variable))
gg <- gg + geom_density(alpha = 0.5)
gg <- gg + xlim(0, 100)
# Facet by both sub and n_back
gg <- gg + facet_wrap(~sub + n_back, scales = "free_y")
gg <- gg + labs(title = paste0("Kernel Density Estimates ",s),
                x = "Value",
                y = "Density")
print(gg)
ggsave(paste0("/Volumes/Effort_data/subs/figure/density_",task,"_",s,".png"), plot = gg, width = 10, height = 6, dpi = 300)

# Kernel density of visual scale analogs after normalization
dfa_long <- tidyr::gather(dfa, key = "variable", value = "value", 
                          valence, arousal, 
                          effort, time)

s <- "reactive"
gg <- ggplot(subset(dfa_long,strategy == s), aes(x = value, fill = variable))
gg <- gg + geom_density(alpha = 0.5)
gg <- gg + xlim(0, 1)
# Facet by both sub and n_back
gg <- gg + facet_wrap(~ sub + n_back, scales = "free_y")
gg <- gg + labs(title = paste0("Kernel Density Estimates ",s),
                x = "Value",
                y = "Density")
print(gg)
ggsave(paste0("/Volumes/Effort_data/subs/figure/density_",task,"_",s,"_minmaxed.png"), plot = gg, width = 10, height = 6, dpi = 300)

#------------------------------------------------------------
# --------------------------- n_back vs y by sub

y_values <- c("effort", "dprime", "score", "pupil_norm","tpac","alpha","theta")
y_values <- c("theta","alpha","tpac")

# Loop over each y value
for (y in y_values) {
  # Create the plot
  plot <- ggplot(dfa, aes(x = n_back, y = .data[[y]])) +
    geom_point(alpha = 0.8) +
    stat_smooth(method = "lm", se = TRUE, formula = y ~ poly(x, 2)) +
    geom_violin(aes(group = n_back, fill = sub), alpha = 0.5) +
    facet_wrap(~sub, scales = "free") +
    labs(
      title = paste("Difficulty and", y),
      x = "Qualitative Difficulty Level",
      y = y
    )
  # Fit linear model
  lm_model <- lm(data = dfa, formula = as.formula(paste(y, "~ poly(n_back, 2)")))
  
  # Extract p-value
  p_value <- summary(lm_model)$coefficients["poly(n_back, 2)1", "Pr(>|t|)"]
  
  # Add caption
  plot <- plot + labs(caption = paste("p-value total: ", round(p_value,3)))
  print(plot)
  
  file_path <- file.path("/Volumes/Effort_data/subs/figure/", paste(task,"_n_back_", y, "by sub.png", sep = ""))
  ggsave(filename = file_path, plot = plot, width = 10, height = 6, dpi = 300)
}

# --------------------------- n_back vs y by strategy
y_values <- c("theta_norm","alpha_norm","tpac_norm")
# Loop over each y value
for (y in y_values) {
  # Create the plot
  plot <- ggplot(dfa, aes(x = n_back, y = .data[[y]])) +
    geom_point(alpha = 0.8) +
    stat_smooth(method = "lm", se = TRUE, formula = y ~ poly(x, 1)) +
    geom_violin(aes(group = n_back, fill = strategy), alpha = 0.5) +
    facet_wrap(~strategy) +
    labs(
      title = paste("Difficulty and", y),
      x = "Qualitative Difficulty Level",
      y = y
    )
  
  # Fit linear model for rolling strategy
  lm_model_rolling <- lm(data = subset(dfa, strategy == 'rolling'), formula = as.formula(paste(y, "~ poly(n_back, 1)")))
  p_value_rolling <- summary(lm_model_rolling)$coefficients["poly(n_back, 1)", "Pr(>|t|)"]
  
  # Fit linear model for static strategy
  lm_model_static <- lm(data = subset(dfa, strategy == 'static'), formula = as.formula(paste(y, "~ poly(n_back, 1)")))
  p_value_static <- summary(lm_model_static)$coefficients["poly(n_back, 1)", "Pr(>|t|)"]
  
  # Add caption
  plot <- plot + labs(caption = paste("p-value rolling: ", round(p_value_rolling, 3), " p-value static: ", round(p_value_static, 3)))
  
  print(plot)
  
  file_path <- file.path("/Volumes/Effort_data/subs/figure/", paste(task, "_n_back_", y, "by_strategy.png", sep = ""))
  ggsave(filename = file_path, plot = plot, width = 10, height = 6, dpi = 300)
}


#------------------------------------------------------------
# ------------------------------ phenomenology
# Define the x values
x_values <- c("effort","n_block", "tpac", "p3", "psd",'theta','alpha',"pupil_norm","pupil_max_norm","time","dps")
x_values <- c("theta_norm","alpha_norm","tpac_norm")


for (x in x_values) {
  # Create the plot
  phenom <- ggplot(dfa) + 
    geom_point(aes_string(x = x, y = "arousal", color = "'Arousal'")) + 
    geom_smooth(aes_string(x = x, y = "arousal", color = "'Arousal'"), method = "lm", se = TRUE) +
    geom_point(aes_string(x = x, y = "valence", color = "'Valence'")) +
    geom_smooth(aes_string(x = x, y = "valence", color = "'Valence'"), method = "lm", se = TRUE) +
    labs(
      title = paste("Phenomenal", x),
      x = x,
      y = "Arousal and valence"
    ) + 
    scale_color_manual(
      name = "Measures",
      values = c("Arousal" = "orange", "Valence" = "purple")
    ) +
    theme(
      legend.position = c(.25, .95),
      legend.justification = c("right", "top"),
      legend.box.just = "right"
    ) #+ facet_wrap(~ sub,scale="free")
  
  # Extract p-values
  p_value_arousal <- summary(lm(arousal ~ ., data = dfa[, c(x, "arousal")]))$coefficients[2, 4]
  p_value_valence <- summary(lm(valence ~ ., data = dfa[, c(x, "valence")]))$coefficients[2, 4]
  
  # Annotate p-values onto the plot
  phenom <- phenom +
    geom_text(x = Inf, y = Inf, label = paste("p-value:", round(p_value_arousal, 3)), 
              hjust = 1, vjust = 1, color = "orange") +
    geom_text(x = Inf, y = Inf, label = paste("p-value:", round(p_value_valence, 3)), 
              hjust = 1, vjust = 2.5, color = "purple")
  
  # Print the plot
  print(phenom)
  
  # Construct the file path
  file_path <- file.path("/Volumes/Effort_data/subs/figure/", paste(task,"_phenom_", x, ".png", sep = ""))
  
  # Save the plot
  ggsave(filename = file_path, plot = phenom, width = 10, height = 6, dpi = 300)
}

#------------------------------------------------------------
# ----------------------------- x vs y by n_back

# Define x_values and y_values
x_values <- c("effort", "tpac", "p3", "psd",'theta','alpha',"pupil_norm","pupil_max_norm")
y_values <- c("dprime", "effort","pupil_norm","pupil_max_norm", "time", "rt_h",'theta','alpha',"valence_dt") # score

x_values <- c("theta_norm","alpha_norm","tpac_norm")
y_values <- c("dprime", "effort","pupil_norm", "time", "rt_h") # score


# Loop over each y_value
for (y_val in y_values) {
  
  # Loop over each x_value
  for (x_val in x_values) {
    
    # Skip if the dependent and independent variables are the same
    if (y_val == x_val) {
      next
    }
    
    # Initialize empty data frame to store p-values
    p_values <- data.frame(n_back = character(), p_value = numeric())
    
    # Loop over each level of n_back
    for (n_level in unique(dfa$n_back)) {
      # Subset the data for the current level of n_back
      subset_data <- dfa[dfa$n_back == n_level, ]
      
      # Perform linear regression using dynamic y and x variables
      lm_formula <- as.formula(paste(y_val, "~", x_val))
      lm_model <- lm(lm_formula, data = subset_data)
      
      # Extract the p-value
      p_value <- summary(lm_model)$coefficients[2, 4]
      
      # Add the p-value to the data frame
      p_values <- rbind(p_values, data.frame(n_back = n_level, p_value = p_value))
    }
    
    # Perform linear regression for the total dataset
    lm_model_total <- lm(lm_formula, data = dfa)
    # Extract the p-value for the total dataset
    p_value_total <- summary(lm_model_total)$coefficients[2, 4]
    
    # Create the plot
    perform <- ggplot(dfa, aes_string(x = x_val, y = y_val, col = "factor(n_back)")) +
      stat_smooth(aes_string(group = "factor(n_back)"), method = "lm", formula = "y ~ poly(x, 1)", se = FALSE) +
      geom_smooth(method = "lm", se = TRUE, color = "black") + # Add regression line for all data
      geom_point() +
      theme(
        legend.position = c(1, 0),  # Place legend in top-right corner
        legend.justification = c(1, 0),  # Justify legend to the right
        legend.box.just = "right",  # Justify the legend box to the right
        plot.caption = element_text(color = "black", face = "italic")
      ) +
      labs(
        caption = paste("p-value 1:", round(p_values[,2][1], 3),
                        "p-value 2:", round(p_values[,2][2], 3),
                        "p-value 3:", round(p_values[,2][3], 3),
                        "p-value total:", round(p_value_total, 3))
      ) #+ facet_wrap(~ sub, scale="free")
        
    
    # Print the plot
    print(perform)
    
    # Construct the file path to include both y_val and x_val
    file_path <- file.path("/Volumes/Effort_data/subs/figure/", paste(task,"_", y_val, "_vs_", x_val, "by_n.png", sep = ""))
    
    # Save the plot
    ggsave(filename = file_path, plot = perform, width = 10, height = 6, dpi = 300)
  }
}

#------------------------------------------------------------
# ----------------------------- interactions of sub.strategy

ggplot(dfa, aes(x = strategy, y = slider_valenc.response, col = factor(n_back))) +
  stat_smooth(aes(group = factor(n_back)), method = "lm",formula =  y ~ poly(x,1), se = FALSE) +
  geom_point() #+ facet_wrap(~ sub)

ggplot(dfa, aes(x = strategy, y = valence)) +
  stat_smooth(method = "lm", formula =  y ~ 1 + poly(x,1), se = FALSE) +
  geom_point() + 
  #facet_wrap(~ sub, scales = "free") +
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


#------------------------------------------------------------
# ----------------------------- x vs y by strategy

# remember to change the strategy in the code !!!!!

x_values <- c("effort", "tpac", "p3", "psd",'theta','alpha', 'pupil_norm',"pupil_max_norm","n_block")
y_values <- c("dprime", "effort", "pupil_norm","pupil_max_norm", "time", "rt_h","n_block")

x_values <- c("theta_norm","alpha_norm","tpac_norm")
y_values <- c("dprime", "effort","pupil_norm", "time", "rt_h","pupil_max_norm","n_block") # score



# Loop through combinations of x and y values
for (x_val in x_values) {
  for (y_val in y_values) {
    # Check if x_val and y_val are the same
    if (x_val == y_val) {
      next  # Skip to the next iteration if x_val and y_val are the same
    }
    # Perform the regression analysis
    lm1 <- lm(as.formula(paste(y_val,"~", x_val)), data = subset(dfa, strategy == "rolling"))
    p_values1 <- summary(lm1)$coefficients[2, "Pr(>|t|)"][1]
    
    lm2 <- lm(as.formula(paste(y_val,"~", x_val)), data = subset(dfa, strategy == "static"))
    p_values2 <- summary(lm2)$coefficients[2, "Pr(>|t|)"][1]
    
    # Create the plot
    plot <- ggplot(dfa, aes_string(x = x_val, y = y_val, col = "factor(strategy)")) +
      stat_smooth(aes_string(group = "factor(strategy)"), method = "lm", formula = "y ~ poly(x, 1)", se = TRUE) +
      geom_point() +
      labs(
        caption = paste("p-values static:", round(p_values2, 3), "p-values rolling:", round(p_values1, 3))
      ) +
      theme(legend.position = c(0.9, 0.9))
    
    # Print or save the plot as per your requirement
    print(plot)
    
    # Construct the file path to include both y_val and x_val
    file_path <- file.path("/Volumes/Effort_data/subs/figure/", paste(task,"_", y_val, "_vs_", x_val, "by_strategy.png", sep = ""))
    
    # Save the plot
    ggsave(filename = file_path, plot = plot, width = 10, height = 6, dpi = 300)
  }
}

#------------------------------------------------------------
# ---------------------------- wilcoxon test for the impact of strategy

# List of y_values
y_values <- c("tpac", "psd", "p3",'theta','alpha',"effort","score", "dprime", "rt_h", "time", "valence", "arousal", "pupil_norm","slider_arous.response","slider_valenc.response","slider_effort.response")
x_values <- c("strategy", "effort_category")  # Adding x_values

y_values <- c("theta_norm","alpha_norm","tpac_norm")
x_values <- c("strategy", "effort_category")  # Adding x_values

# Iterate over each y_value
for (y_value in y_values) {
  for (x_value in x_values) {  # Loop over x_values
    # Run Wilcoxon test
    test_result <- wilcox.test(get(y_value) ~ get(x_value), data = dfa)
    
    # Create a box plot and add a significance annotation
    wilcox_plot <- ggplot(dfa, aes_string(x = x_value, y = y_value)) +
      geom_boxplot(fill = "#69b3a2", color = "#2e4053", alpha = 0.8) +
      geom_point(position = position_jitter(width = 0.2), alpha = 0.5, color = "#2e4053") +
      geom_hline(yintercept = median(dfa[[y_value]]), color = "#2e4053", linetype = "dashed", size = 1) +
      labs(title = paste(toupper(y_value), "by", toupper(x_value)),
           x = toupper(x_value),
           y = toupper(y_value),
           caption = paste("Wilcoxon p-value:", round(test_result$p.value, 3))) +
      theme_minimal() +
      theme(plot.title = element_text(size = 20, hjust = 0.5),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            legend.position = "none")
    
    print(wilcox_plot)
    # Define file path
    file_path <- file.path("/Volumes/Effort_data/subs/figure/", paste(task,"_test_", y_value, "_", x_value, ".png", sep = ""))
    
    # Save the plot
    ggsave(filename = file_path, plot = wilcox_plot, width = 10, height = 6, dpi = 300)
  }
}


#-----------------------------------------------------------------
# -------------------------------- violin_plot by sub and strategy

# Vector of y values
y_values <- c("effort", "dprime", "tpac", "p3", "psd",'theta','alpha', "pupil_norm","pupil_max_norm", "time", "valence", "rt_h")

y_values <- c("theta_norm","alpha_norm","tpac_norm")


# Loop through y values
for (y in y_values) {
  dfa <- dfa %>%
    mutate(strategy_sub = interaction(strategy, sub))
  
  # Create the violin plot
  violin_plot <- ggplot(dfa, aes(x = strategy_sub, y = !!sym(y), fill = strategy)) +
    geom_violin(trim = FALSE) +
    scale_fill_brewer(palette = "Set3") + 
    geom_boxplot(width = 0.1, fill = "white") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
    labs(
      title = paste("Parameters by Strategy-Subject Interaction (Y =", y, ")"),
      x = "Strategy-Subject Interaction",
      y = y
    )
  
  print(violin_plot)
  
  file_path <- file.path("/Volumes/Effort_data/subs/figure/", paste(task,"_violin_strategy_sub_", y, ".png", sep = ""))
  # Save the plot
  ggsave(filename = file_path, plot = violin_plot, width = 10, height = 6, dpi = 300)
}

#------------------------------------------------------------
# ------------------------------------------ elasticity
# Create a function to compute elasticity
compute_elasticity <- function(data, x_value) {
  pct_change_g <- (data$g - lag(data$g)) / lag(data$g)
  pct_change_x <- (data[[x_value]] - lag(data[[x_value]])) / lag(data[[x_value]])
  elasticity <- mean(pct_change_g / pct_change_x, na.rm = TRUE)
  return(elasticity)
}

# Create an empty list to store elasticity results
elasticity_results <- list()

# Loop through all combinations of strategy, sub, and n_back
for (strategy_val in unique(dfa$strategy)) {
  for (sub_val in unique(dfa$sub)) {
    for (n_back_val in unique(dfa$n_back)) {
      # Subset the dataframe
      subset_df <- subset(dfa, strategy == strategy_val & sub == sub_val & n_back == n_back_val)
      
      # Compute elasticity for the subset
      elasticity <- compute_elasticity(subset_df, "p3_norm")
      
      # Store the elasticity result along with strategy, sub, and n_back
      result_entry <- data.frame(strategy = strategy_val,
                                 sub = sub_val,
                                 n_back = n_back_val,
                                 elasticity = elasticity)
      
      # Append the result to the elasticity_results list
      elasticity_results[[length(elasticity_results) + 1]] <- result_entry
    }
  }
}

# Combine the dataframes in elasticity_results into a single dataframe
elasticity_df <- do.call(rbind, elasticity_results)

# Plot with smoothed trend line
ggplot(subset(elasticity_df,-20<elasticity & elasticity<20), aes(x = factor(n_back), y = elasticity, group = strategy, color = strategy)) +
  #geom_line() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add smoothed trend line
  labs(x = "Difficulty Level", y = "Elasticity", title = "Pupil elasticity of performance") +
  theme_minimal()

# Plot with violin plot
ggplot(subset(elasticity_df,-1000<elasticity & elasticity<1000), aes(x = factor(n_back), y = elasticity, fill = strategy)) +
  geom_violin() +
  labs(x = "Difficulty Level", y = "Elasticity", title = "Pupil elasticity of performance") +
  theme_minimal()


# ------------------------------------showing the leontief relation between strategy effort and performance
# Remove rows with missing values
df3d <- na.omit(dfa)
df3d <- (dfa)
# Impute missing values with mean
# Replace "column_name" with the actual column containing missing values
df3d$effort[is.na(df3d$effort)] <- mean(df3d$effort, na.rm = TRUE)
df3d$dprime[is.na(df3d$dprime)] <- mean(df3d$dprime, na.rm = TRUE)
df3d$pupil_norm[is.na(df3d$pupil_norm)] <- mean(df3d$pupil_norm, na.rm = TRUE)



# Create 3D scatterplot with grouping by dfa$n_back
plot_ly(data = subset(dfa,n_back>0), x = ~n_back, y = ~strategy, z = ~ dprime, color = ~factor(round(effort,1)), type = "scatter3d", mode = "markers") %>%
  layout(scene = list(xaxis = list(title = "n back"),
                      yaxis = list(title = "Strategy"),
                      zaxis = list(title = "Dprime"),
                      aspectmode = "cube"))

# Fit a linear model
lm_model <- lm(dprime ~ effort + strategy, data = subset(dfa, n_back > 0))

# Create 3D scatterplot with grouping by dfa$n_back
plot_ly(data = subset(dfa, n_back > 0), x = ~effort, y = ~strategy, z = ~dprime, color = ~factor(round(n_back, 1)), type = "scatter3d", mode = "markers") %>%
  add_trace(x = ~dfa$n_back, y = ~dfa$strategy, z = predict(lm_model), 
            type = "scatter3d", mode = "lines", line = list(color = "blue")) %>%
  layout(scene = list(xaxis = list(title = "Effort"),
                      yaxis = list(title = "Strategy"),
                      zaxis = list(title = "Dprime"),
                      aspectmode = "cube"))
