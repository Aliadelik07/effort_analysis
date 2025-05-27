#importing modules =====================
library(readr)
library("readxl")
library(ggrepel)
require(ggplot2)
library(investr)
library(dplyr)
library(vroom)
library(vars)
library(tseries)
library(sjPlot)
library(patchwork) 
library(lmerTest)
library(mgcv)


#block_analysis ---------------------------------
# this function compute the Evoke Pupillary Response of 
# pupil size, microsaccade, blink, magnitude of each block 
# for all participants

process_block_data <- function(df,bf,event,num,pre,post,sub_name,strategy_name) {
  block_indices <- which(df$text == "block")
  post_position = post
  pre_position = pre
  step_size = post_position - pre_position
  
  if (length(block_indices) != length(bf$n_block)) {
    print(paste0("The number of blocks in pupil is less than the behaviour. Skipping : ",sub_name,' ',strategy_name))
    return(NULL)  # Skips the function and returns NULL
  }
  
  all_data <- list()
  
  for (i in seq_along(block_indices)) {
    
    block_index <- block_indices[i]
    
    # Define the upper limit: next block index if available, otherwise include all remaining indices
    upper_limit <- if (i < length(block_indices)) block_indices[i + 1] else length(df$text) + 1
    
    # Find trial indices occurring after the current block index
    trial_indices <- which(df$text == event & seq_along(df$text) > block_index & seq_along(df$text) < upper_limit)
    
    #if needed, populate
    trial_indices <- trial_indices + step_size * seq(0, num - 1)
    
    #initilizing the vectors
    pupil_sizes <- vector("list", step_size + 1)
    microsacc <- vector("list", step_size + 1)

    #pupil
    for (j in 0:step_size) {
      temp <- df$ps[trial_indices + j] # storing the jth position of the trial of this block
      pupil_sizes[[j+1]] <- mean(temp, na.rm = TRUE) #averaging the jth position
    }
    
    #microsaccade
    for (j in 0:step_size) {
      temp <- df$ms[trial_indices + j] # storing the jth position of the trial of this block
      microsacc[[j + 1]] <- mean(temp, na.rm = TRUE) #averaging the jth position
    }
    

    block_data <- data.frame(
      sub = sub_name,
      strategy = strategy_name,
      n_block = bf$n_block[i],
      position = pre_position:post_position,
      pupil_size = unlist(pupil_sizes),
      microsacc = unlist(microsacc),
      effort = round(bf$slider_effort.response[i],2),
      pac = bf$pac3[i],
      pac_low = bf$pac3_low[i],
      pac_high = bf$pac3_high[i],
      performance = bf$g[i],
      rt = bf$rt_h[i],
      n_back = bf$n_back[i]
    )
    all_data[[i]] <- block_data
  }
  return(do.call(rbind, all_data))
}

# Min-Max normalization function
minmax_normalize <- function(x,a) {(x - min(a, na.rm = TRUE)) / (max(a, na.rm = TRUE) - min(a, na.rm = TRUE))}


# ----------------------- import after running the preprocessing pupil.R

sub_list <- read_csv("/Users/ali/Documents/Experiment/check_list.csv")
rows_to_remove <- c(1, 2, 3, 4, 5, 6, 10, 19, 20, 32, 33, 34, 39, 42, 43, 45, 46, 53, 54) #bad files
sub_list <- sub_list[-rows_to_remove, ] # recordings to remove
sub_list <- sub_list[!is.na(sub_list$sub), ] # removing the NaN

dfplot_list <- list()

# Loop through the participants ----------
for (isub in 1:nrow(sub_list)) {
  
  sub <- sub_list$sub[isub]
  strategy <- sub_list$strategy[isub]
  
  df <- read.csv(paste0("/Volumes/x9/INITIAL_DATABASE/",sub,"/",sub,"-",strategy,"-test-pupil.csv"), encoding = "UTF-8")
  bf <- read.csv(paste0("/Volumes/x9/INITIAL_DATABASE/",sub,"/",sub,"-",strategy,"-test.csv"), encoding = "UTF-8")
  
  # Update "10", "20", and "30" messages to "stim"
  df$text[df$text == "10"] <- "stim"
  df$text[df$text == "20"] <- "stim"
  df$text[df$text == "30"] <- "stim"
  df$text[df$text == "2"] <- "rest"
  df$text[df$text == "8"] <- "resp"
  
  # EPR function of on stim
  plot_data_stim <- process_block_data(df, bf, 'stim', 1, 0, 1000, sub, strategy)
  
  # EPR function of on rest
  plot_data_rest <- process_block_data(df, bf, 'rest', 1, 0, 1000, sub,strategy)
  
  # removing outliers
  na1 <- sum(is.na(plot_data_stim))
  repeat {
    # Initialize a variable to track if NAs were added
    previous_na_count <- sum(is.na(plot_data_stim))
    
    # Iterate through each column of plot_data_stim
    for (col in names(plot_data_stim)) {
      # Check if the column is numeric
      if (is.numeric(plot_data_stim[[col]])) {
        # Replace outliers with NA for the current column
        plot_data_stim[['pupil_size']][abs(scale(plot_data_stim[['pupil_size']])) >= 3] <- NA
        plot_data_stim[['microsacc']][abs(scale(plot_data_stim[['microsacc']])) >= 3] <- NA
        
        plot_data_rest[['pupil_size']][abs(scale(plot_data_rest[['pupil_size']])) >= 3] <- NA
        plot_data_rest[['microsacc']][abs(scale(plot_data_rest[['microsacc']])) >= 3] <- NA
      }
    }
    
    # Check if no more NAs were added
    current_na_count <- sum(is.na(plot_data_stim))
    
    # Exit the loop if no new NAs were added
    if (current_na_count == previous_na_count) {
      break
    }
  }
  
  na2 <- sum(is.na(plot_data_stim))
  
  print(paste("Percentage of the outlier is:", (na2 - na1) / (nrow(plot_data_stim) * ncol(plot_data_stim))))

  
  # renaming 
  # List of columns to modify
  columns_to_modify <- c("pupil_size", "microsacc")
  
  # Rename the specified columns
  colnames(plot_data_stim)[colnames(plot_data_stim) %in% columns_to_modify] <- 
    paste0(columns_to_modify, "_stim")
  
  # Rename the specified columns
  colnames(plot_data_rest)[colnames(plot_data_rest) %in% columns_to_modify] <- 
    paste0(columns_to_modify, "_rest")
  
  #combining
  plot_data <- cbind(plot_data_rest, plot_data_stim)
  plot_data <- plot_data[, !duplicated(colnames(plot_data))]
  
  #normlizing 
  plot_data$pupil_size_stim_norm = minmax_normalize(plot_data$pupil_size_stim,c(plot_data$pupil_size_rest, plot_data$pupil_size_stim))
  plot_data$pupil_size_rest_norm = minmax_normalize(plot_data$pupil_size_rest,c(plot_data$pupil_size_rest, plot_data$pupil_size_stim))
  
  #plot_data$microsacc_stim_norm = minmax_normalize(plot_data$microsacc_stim,c(plot_data$microsacc_rest, plot_data$microsacc_stim))
  #plot_data$microsacc_rest_norm = minmax_normalize(plot_data$microsacc_rest,c(plot_data$microsacc_rest, plot_data$microsacc_stim))
  
  #average pupil per block
  bs_avg <- plot_data %>%
    group_by(n_block) %>%
    summarise(ps_norm = mean(pupil_size_stim_norm, na.rm = TRUE)) %>%
    ungroup()
  
  bf$ps <- bs_avg$ps_norm
  write.csv(bf, paste0("/Volumes/x9/INITIAL_DATABASE/",sub,"/",sub,"-",strategy,"-test.csv"))
  
  
  # Add the processed data frame to the list
  dfplot_list[[isub]] <- plot_data

  
  cat('|',rep("#", isub), rep("-", nrow(sub_list) - isub),'|\n', sep = "")
  }


# Combine all data frames into one
dfplot <- bind_rows(dfplot_list)

dfplot <- dfplot %>%
  mutate(task = ifelse(strategy %in% c('static', 'rolling'), 'n-back', 'ax-cpt'))


# save the final dfplot
write.csv(dfplot, paste0("/Volumes/x9/INITIAL_DATABASE/dfplot_stim_lambda_6-mai2025.csv"))

#============================================= plotting =================
dfplot <- read_csv(paste0('/Volumes/x9/INITIAL_DATABASE/dfplot_stim_lambda_6-mai2025.csv'))


#reorgnising rest and stim
df_stim <- dfplot[, c("pupil_size_stim_norm", "microsacc_stim", "position", "strategy",'task', "sub", "n_block", "n_back", "effort")]
colnames(df_stim) <- gsub("_stim", "", colnames(df_stim))

df_stim$effort_category <- ifelse(df_stim$effort > median(df_stim$effort, na.rm = TRUE),
                                  "High effort", "Low effort")

df_rest <- dfplot[, c("pupil_size_rest_norm", "microsacc_rest", 
                    "position", "strategy",'task', "sub", 
                   "n_block", "n_back", "effort")]
colnames(df_rest) <- gsub("_rest", "", colnames(df_rest))

df_rest$effort_category <- "Rest"
df_rest$effort <- -1

df_eye <- rbind(df_stim, df_rest)

# converting to time and hz
df_eye$time <- round(df_eye$position/500,1)

df_agg <- df_eye %>%
  group_by(time, task, strategy, sub, n_back, n_block, effort_category) %>%
  summarize(ps = mean(pupil_size_norm, na.rm = TRUE),
            ms = mean(microsacc, na.rm = TRUE)*100)

df_agg <- df_agg[df_agg$effort_category != 'Rest', ]


df_agg <- dfplot %>%
  group_by(position, sub, strategy, effort_category) %>%
  summarize(ps = mean(pupil_size_stim_norm, na.rm = TRUE))

# Generate the plot
fig <- ggplot(df_agg, aes(x = position, y = ps, 
                          group = interaction(effort_category, strategy),  # Group by both effort category and strategy
                          color = interaction(effort_category, strategy))) +  # Color by both effort category and strategy
  geom_smooth(method = "loess", span = 0.3, se = TRUE) +  # Smoothing
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Trial onset line
  geom_vline(xintercept = 500, linetype = "dashed", color = "black") +  # Trial offset line
  annotate("text", x = 0, y = 0.67, label = "Onset", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +  # Label for onset
  annotate("text", x = 500, y = 0.67, label = "Offset", vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +  # Label for offset
  theme_minimal(base_size = 16) +  # Theme customization
  scale_color_manual(values = c(
    "high effort.reactive" = "#8B0000", "low effort.reactive" = "#FFC0CB", 
    "high effort.static" = "#FFB84D", "low effort.static" = "#FFD700", 
    "high effort.rolling" = "#32CD32", "low effort.rolling" = "#98FB98", 
    "high effort.proactive" = "#0000FF", "low effort.proactive" = "#87CEFA")) +  # Custom colors
  scale_x_continuous(
    name = "Time (sec)",  # Custom x-axis label
    breaks = c(0, 250, 500, 750, 1000),  # Define custom tick marks
    labels = c("0", "0.5","1","1.5", "2")  # Custom tick labels
  ) +
  labs(title = "Stim-locked EPR of Pupil Size",
       y = "Pupil Size (%)",
       color = "Effort & Strategy") # + theme(legend.position = c(0.8, 0.2),legend.key = element_rect(fill = NA))

# Print the figure
print(fig)



ggsave(filename = "/Volumes/x9/results/allsubs/figure/EPR_ps_6_all.png",  # The name of the output file
       plot = fig,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300)                     # Resolution in dots per inch

# Generate the plot
fig <- ggplot(df_agg, aes(x = time, y = ms, 
                          group = effort_category,
                          color = effort_category)) +
  geom_smooth(method = "loess", span = 0.2, se = TRUE) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Trial onset line
  geom_vline(xintercept = 1, linetype = "dashed", color = "black") +  # Trial offset line
  annotate("text", x = 0, y = 0.4, label = "Onset",  vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +  # Label for the line
  annotate("text", x = 1, y = 0.4, label = "Offset",  vjust = -0.5, hjust = 0, size = 4, fontface = "bold") +  # Label for the line
  theme_minimal(base_size = 16) +  # Increase font size
  scale_color_manual(values = c("Rest" = "#00008B", "High effort" = "#8B0000", "Low effort" = "#FFC0CB")) +
  labs(title = "Stim-locked EPR of Microsaccade",
       x = "Seconds",
       y = "Microsaccade (Hz)",
       color = "") +
  theme(legend.position = c(0.8, 0.2),
        legend.key = element_rect(fill = NA))+
  scale_x_continuous(breaks = seq(0, 2, by = 0.2))#+ facet_wrap(~task)

# Print the plot
print(fig)

ggsave(filename = "/Volumes/x9/results/allsubs/figure/EPR_ms_6_all.png",  # The name of the output file
       plot = fig,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300)                     # Resolution in dots per inch

# statistics ========
dfplot_loaded <- read_csv("/Volumes/x9/INITIAL_DATABASE/dfplot_stim_lambda_6-mai2025.csv", locale = locale(encoding = "UTF-8"))
dfplot <- dfplot_loaded

#dfplot <- dfplot %>%left_join(dfa %>% dplyr::select(sub, strategy, block, m_correct), by = c("sub", "strategy", "block"))

# removing inf
#dfplot <- dfplot %>% filter_all(all_vars(!is.infinite(.)))
# removing NA
dfplot_clean <- na.omit(dfplot_loaded)


# pupil gam general additive model --------
dfplot_clean$sub <- as.factor(dfplot_clean$sub)

# pac ~ ps
gam_model <- gam( pac ~  pupil_size_stim*strategy + n_back + n_block + s(position)  + s(sub, bs = "re"), data = subset(dfplot_clean,task=='ax-cpt'))
summary(gam_model)
gam_model <- gam( pac  ~  pupil_size_stim*strategy + n_back + n_block + s(position)  + s(sub, bs = "re"), data = subset(dfplot_clean,task=='n-back'))
summary(gam_model)

# effort ~ ps
gam_model <- gam(pupil_size_stim ~ effort*strategy + n_back + n_block + s(position) + s(sub, bs = "re"), data = subset(dfplot_clean,task=='ax-cpt'))
summary(gam_model)
gam_model <- gam(pupil_size_stim ~ effort*strategy + n_back + n_block + s(position) + s(sub, bs = "re"), data = subset(dfplot_clean,task=='n-back'))
summary(gam_model)


# slop of ms -------------
dfplot_ms <- dfplot_loaded %>%filter(position >= 50 & position <= 200) #50-200

dfplot_ms <- dfplot_ms %>%
  group_by(sub, task, strategy, n_block, n_back, effort, pac, pac_low, rt, performance) %>%
  filter(n_distinct(position, na.rm = TRUE) > 1) %>%  # Ensure multiple unique values
  summarise(
    ms_slop = if (sum(!is.na(position) & !is.na(microsacc_stim)) > 1) 
      coef(lm(microsacc_stim ~ position, data = cur_data()))[2] 
    else NA_real_,
    ms = mean(microsacc_stim, na.rm = TRUE),
    .groups = "drop"
  )

dfplot_ms <- subset(dfplot_ms, ms_slop >0)

# Fit the linear model
model <- lm(ms_slop ~ effort*strategy + n_back + n_block, data = subset(dfplot_ms,task=='ax-cpt'))
summary(model)
model <- lm(ms_slop ~ effort*strategy + n_back + n_block, data = subset(dfplot_ms,task=='n-back'))
summary(model)

# Plot
plot_model(model, type = "eff", terms = "n_back") +
  scale_x_continuous(breaks = c(1, 2, 3), labels = c("1", "2", "3")) +
  labs(x = "Difficulty Level", y = "Slope of Microsaccade Rate") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 16),  # Increase axis title size
    axis.text = element_text(size = 14),   # Increase axis labels size
    plot.title = element_text(size = 18, face = "bold") # Increase plot title size
  )

fig <- ggplot(dfplot_ms, aes(x = rt , y = ms_slop ,col= strategy)) +
  stat_smooth(aes(), method = "lm", formula = y ~ poly(x, 1), se = TRUE) +
  theme_minimal() +
  geom_point()+
  labs(x = "Reported effort", y = "Slop of Microsaccade rate", color = "Strategy") +
  theme(legend.position = c(0, 0), legend.justification = c(0, 0),
        axis.title = element_text(size = 18),    # Increase axis title font size
        axis.text = element_text(size = 18),      # Increase axis labels font size
        legend.title = element_text(size = 18),   # Increase legend title font size
        legend.text = element_text(size = 18),    # Increase legend text font size
        strip.text = element_text(size = 18)) + facet_wrap(~task, scales = 'fixed')
fig

# pupil Average -------------
dfplot_avg <- dfplot_loaded %>%
  group_by(sub,task, strategy, n_back, n_block,effort, pac, pac_low,rt,performance) %>%
  filter(n_distinct(position) > 1) %>%  # Ensure at least 2 unique values
  summarise(
            ps = mean(pupil_size_stim, na.rm = TRUE),
            ms = mean(microsacc_stim, na.rm = TRUE),
            .groups = "drop")


lmer_model <- lmer(ps ~ effort * strategy + n_back + n_block + (1 | sub), data = subset(dfplot_avg,task=='ax-cpt'))
summary(lmer_model)
lmer_model <- lmer(ps ~ effort * strategy + n_back + n_block + (1 | sub), data = subset(dfplot_avg,task=='n-back'))
summary(lmer_model)


lmer_model <- lmer(pac ~ ps * strategy + n_back + n_block + (1 | sub), data = subset(dfplot_avg,task=='ax-cpt'))
summary(lmer_model)
lmer_model <- lmer(pac ~ ps * strategy + n_back + n_block + (1 | sub), data = subset(dfplot_avg,task=='n-back'))
summary(lmer_model)
