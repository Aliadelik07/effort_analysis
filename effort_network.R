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
library(corrplot)
library(sjPlot)
library(coda)
library(coefplot2)
library(ggtext)
library(mediation)
library(eegkit)
library(multiwave)

# save the final dfa
write.csv(df_pac, paste0("/Users/ali/Desktop/Experiment/dfpac-test.csv"))

# pac of high vs low effort
df_pac <- read_csv(paste0("/Users/ali/Desktop/Experiment/dfpac-test.csv"))

# behavioral
dfa <- read_csv(paste0("/Users/ali/Desktop/Experiment/dfa-test.csv"))

dfa <- dfa[dfa$strategy %in% c("rolling", "static"), ]
dfa <- dfa[dfa$strategy %in% c("proactive", "reactive"), ]

# Convert "rolling" to 1 and "static" to 0
dfa$strategy <- ifelse(dfa$strategy == "rolling", 1, ifelse(dfa$strategy == "static", 0, dfa$strategy))
dfa$strategy <- ifelse(dfa$strategy == "proactive", 1, ifelse(dfa$strategy == "reactive", 0, dfa$strategy))

dfa$strategy <- as.factor(dfa$strategy)


for (col in names(df_pac)) {
  # Check if the column is numeric
  if (is.numeric(dfa[[col]])) {
    # Replace outliers with NA for the current column
    df_pac[[col]][abs(scale(df_pac[[col]])) >= 3] <- NA
  }
}

#-------------------- Fit a binomial mixed model for strategy and PAC
model <- glmer(strategy ~ 0 + g + n_back + (1 | sub), data = dfa, family = binomial)
summary(model)

# Fit logistic regression model
model <- glm(strategy ~ pac_lang, data = dfa, family = binomial)


# OLS
lm_model <- lm( effort ~ pac_lang* theta_norm + alpha_norm , data = dfa)
summary(lm_model)

# Capture summary as text
summary_text <- capture.output(summary(lm_model))

# Save the summary as a .png image
png("/Users/ali/Desktop/effort_summary.png", width = 800, height = 600)
cat(summary_text, sep = "\n")  # Print summary text
dev.off()


# mixed models -----------------
model3 <- lmer( time ~  0 + strategy +  (1|sub), data = dfa)
summary(model3)
visu_lmr(model)
coefplot2(model)
visu_lmr_combined(list(model1, model2,model3), c("Low Effort", "High Effort","Time"))



# Create 3D scatterplot with grouping by dfa$n_back
plot_ly(data = subset(dfa, n_back > 0), 
        x = ~ pac_norm, y = ~ alpha_norm, z = ~ dprime, color = ~factor(strategy),
        type = "scatter3d", mode = "markers") %>%
  add_trace(x = ~dfa$alpha_norm, y = ~dfa$pac_norm, z = predict(lm_model), 
            type = "scatter3d", mode = "lines", line = list(color = "blue")) %>%
  layout(scene = list(xaxis = list(title = "theta"),
                      yaxis = list(title = "alpha"),
                      zaxis = list(title = "pac"),
                      aspectmode = "cube"))

ggplot(subset(dfa,n_back>0), aes(x = pac_lang, y = dprime)) +
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




#-------------------- effort network
#which subset to regress?
df_task <- dfa[dfa$strategy %in% c("rolling", "static"), ]
df_task  <- dfa[dfa$strategy %in% c("proactive", "reactive"), ]

# meditation regression
# Fit the mediator model
mediator_model <- lm(pac_lang ~ effort, data = df_task)

# Fit the outcome model
outcome_model <- lm(g ~ effort + pac_lang, data = df_task)

# Conduct the mediation analysis
mediation_result <- mediate(mediator_model, outcome_model, treat = "effort", mediator = "pac_lang", boot = TRUE, sims = 1000)

# Summarize the results
summary(mediation_result)







# Fit the linear model
model <- lmer(slider_effort.response ~ 0 + strategy +  (1 | sub), data = subset(dfa, n_back>0) )
summary(model)
visu_lmr(model)

model <- lm(I(theta/beta) ~ 0 + strategy, data = dfa)

model <- lm(effort ~ pac_norm : alpha_norm, data = dfa)

model <- lm(g ~ 0 + strategy, data = subset(dfa,n_back>1))

model <- lm(I(pac_norm) ~ ps_bs +n_back, data = subset(dfa,n_back>0))

model <- lm(I(alpha_norm ) ~  0 + strategy, data = dfa)

summary(model)

visu_lm(model)
visu_lmr(model)
coefplot2(model)

# pac of high vs low effort -------------------

indir <- "/Volumes/Effort_data/subs/"

sub_list <- read_csv("/Users/ali/Desktop/Experiment/check_list.csv")
sub_list <- sub_list[!is.na(sub_list$sub), ]

rows_to_remove <- c(1, 2, 3, 4, 10, 19, 20, 32, 34) #bad files
sub_list <- sub_list[-rows_to_remove, ]


df_pac <- data.frame()
# Loop through the data
for (isub in 1:nrow(sub_list)) {
  
  sub <- sub_list$sub[isub]
  strategy <- sub_list$strategy[isub]
  
  # Attempt to change the working directory
  setwd_result <- try(setwd(paste0("/Volumes/Effort_data/brainstorm_db/ADOEFFORT_protocol/data/",sub)), silent = TRUE)
  
  # Check if the directory change was successful
  if (inherits(setwd_result, "try-error")) { message(sub,"_",strategy,". Skipping this iteration.")
    next
  }

#finding ch indices
full_path_ch <- file.path(getwd(), list.files(recursive = TRUE, full.names = TRUE)[grepl("channel\\.mat$", list.files(recursive = TRUE))])
mat_data_ch <- readMat(full_path_ch[1])
channel <- mat_data_ch[["Channel"]][1,1,]

channel <- as.data.frame(lapply(channel, unlist))
channel <- t(channel)
rownames(channel) <- NULL


ch_names_pac <- c('F3','F4','Fz')


ch_idx_pac <- c()
for (i in 1:length(ch_names_pac)) {
  # Find the column index where the current row name exists
  currentIndex <- which(channel == ch_names_pac[i])
  
  if (length(currentIndex) > 0) {
    # Add current index to the list of column indices
    ch_idx_pac <- c(ch_idx_pac, currentIndex)
  } else {
    cat(ch_names_pac[i], "' not found.\n")
  }
}


# importing the pac .mat

# high effort
files_in_directory <- list.files(paste0("high-effort_",strategy), full.names = TRUE)
target_file <- grep("^timefreq_pac_", basename(files_in_directory), value = TRUE)

mat_data_high <- readMat(paste0("high-effort_",strategy, "/", target_file))

# low effort
files_in_directory <- list.files(paste0("low-effort_",strategy), full.names = TRUE)
target_file <- grep("^timefreq_pac_", basename(files_in_directory), value = TRUE)

mat_data_low <- readMat(paste0("low-effort_",strategy, "/", target_file))

#  combining them
mat_data_high_tf <- as.numeric(mat_data_high[["TF"]][ch_idx_pac,])
mat_data_low_tf <- as.numeric(mat_data_low[["TF"]][ch_idx_pac,])

# Convert to data frames
df_high <- data.frame(TF = mat_data_high_tf, Group = 'High', sub = sub, strategy = strategy)
df_low <- data.frame(TF = mat_data_low_tf, Group = 'Low', sub = sub, strategy = strategy)

# Combine the data frames
df_combined <- rbind(df_high, df_low)

# Accumulate in df_pac
df_pac <- rbind(df_pac, df_combined)

# Create the kernel density plot
fig <- ggplot(df_combined, aes(x = TF, fill = Group)) +
  geom_density(alpha = 0.5) +
  labs(title = paste0("Kernel Density Estimation of high vs low effort of ",sub," ",strategy),
       x = "Value",
       y = "Density") +
  theme_minimal() +
  scale_fill_manual(values = c("High" = "blue", "Low" = "red")) +
  theme(legend.title = element_blank())

#print(fig)
}

df_pac$task <- ifelse(df_pac$strategy %in% c("rolling", "static"), "N-Back", "AX-CPT")
df_pac$ch <- rep(ch_names_pac, length.out = nrow(df_pac))


#density plot
ggplot(df_pac, aes(x = TF, fill = Group)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ strategy) +
  labs(title = "Midfrontal Phase Amplitude Coupling of high vs low effort",
       x = "TF",
       y = "Density") +
  theme_minimal() +
  scale_fill_manual(values = c("High" = "blue", "Low" = "red")) +
  theme(legend.title = element_blank())


df_pac <- df_pac %>%
  mutate(effort = ifelse(Group == "Low", 0, 1))

ggplot(subset(df_pac,ch_names_pac=="F4"), aes(x = Group, y = TF)) +
  stat_smooth(method = "lm", se = TRUE, aes(group = sub), color = "blue", alpha = 0.1, linetype = "dashed") +
  stat_smooth(method = "lm", se = TRUE, color = "red")  
  #facet_wrap(~ strategy, scales = "fixed")





model <- lmer(TF ~ 0 + Group + (1 | sub) + (1|strategy), data = subset(df_pac,ch_names_pac=="F4") )
summary(model)
visu_lmr(model)


# Function to extract p-value from linear regression model
get_p_value <- function(data) {
  lm_model <- lm(TF ~ 0 + effort, data = data)
  return(summary(lm_model)$coefficients[1, "Pr(>|t|)"])
}

# Calculate p-value for each strategy
p_values <- df_pac %>%
  group_by(strategy) %>%
  summarise(p_value = get_p_value(.))

# Plot
ggplot(df_pac, aes(x = effort, y = TF, colour = ch)) +
  stat_smooth(method = "lm", se = FALSE, aes(group = ch)) +
  stat_smooth(method = "lm", se = TRUE, color = "black", alpha = 0.2, linetype = "dashed") +
  facet_wrap(~ strategy, scales = "fixed")
  #geom_text(data = p_values, aes(label = paste("p =", round(p_value, 4)), x = Inf, y = Inf),hjust = 1, vjust = 1, size = 4, color = "black")

# Fit the mixed-effects model
mixed_model <- lmer(TF ~ 0 + Group + (1|strategy) + (1|sub), data = df_pac)
summary(mixed_model)

# Perform ANOVA on the mixed-effects model
anova(mixed_model)

wilcox.test(TF ~ Group, data = subset(df_pac,task=="AX-CPT"))



# Fit the GLMM --------------

model <- glmer(TF ~ 0 + Group:strategy + (1|sub), data = df_pac, family = binomial)

# Summarize the model
summary(model)
# Get the predicted effects for Group
predicted_effects <- ggpredict(model, terms = "Group")

# Convert 'x' to a factor to ensure proper categorical handling
predicted_effects$x <- as.factor(predicted_effects$x)

# Plot the predicted effects with points and error bars
ggplot(predicted_effects, aes(x = x, y = predicted)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  labs(x = "Group", y = "Predicted TF", title = "Predicted Effects of Group on TF") +
  theme_minimal()


# violin plot -----------

y_values <- c("TF")
# Loop through y values
for (y in y_values) {
  df_pac<- df_pac %>%
    mutate(strategy_sub = interaction(strategy,sub))
  
  # Create the violin plot
  violin_plot <- ggplot(subset(df_pac), aes(x = Group, y = !!sym(y), fill = ch)) +
    geom_violin(trim = FALSE) +
    scale_fill_brewer(palette = "Set3") + 
    #geom_boxplot(width = 0.1, fill = "white") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
    labs(
      title = paste("Parameters by Strategy (Y =", y, ")"),
      x = "Strategy",
      y = y
    ) + theme_minimal() + facet_wrap(strategy)
  
  print(violin_plot)
  
  file_path <- file.path("/Volumes/Effort_data/subs/figure/", paste(task,"_violin_strategy_sub_", y, ".png", sep = ""))
  # Save the plot
  #ggsave(filename = file_path, plot = violin_plot, width = 10, height = 6, dpi = 300)
}
