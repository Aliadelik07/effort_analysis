

# Import libraries required for the vignette
require(eyelinker)
require(dplyr)
## Warning: package 'dplyr' was built under R version 3.6.2
require(tidyr)
## Warning: package 'tidyr' was built under R version 3.6.2
require(ggplot2)
## Warning: package 'ggplot2' was built under R version 3.6.2
require(intervals)
require(stringr)

n <- "03"
strategy <- "rolling"
#import
dat <- read.asc(paste0("/Users/ali/Desktop/Experiment/sub",n,"/sub",n,"-",strategy,"-test.asc"))

# Combine the lists into one data frame
dfp <- data.frame(time = dat$raw$time, pupil_size = dat$raw$ps)
dfb <- data.frame(time = dat$blinks$stime, dur = dat$blinks$dur)
merged_df <- merge(dat$msg, dfp, by = "time", all = TRUE)
merged_df <- merge(merged_df, dfb, by = "time", all = TRUE)
merged_df <- merged_df[order(merged_df$time), ]


# removing all blinks
for(i in 1:nrow(merged_df)) {
  # Check if 'dur' is not NA
  if(!is.na(merged_df$dur[i])) {
    # Calculate the time range
    start_time <- merged_df$time[i] - 100
    end_time <- merged_df$time[i] + merged_df$dur[i] + 100
    
    # Find indices of 'time' within this range
    indices <- which(merged_df$time >= start_time & merged_df$time <= end_time)
    
    # Set 'pupil_size' to NA for these indices
    merged_df$pupil_size[indices] <- NA
  }
}

# Identify rows where 'dur' is not NA
non_na_dur_rows <- which(!is.na(merged_df$dur))

# Create a vector to store the indices of rows to be removed
rows_to_remove <- integer(0)

# For each non-NA 'dur' row, add its index and the indices of the three rows
# before and after it to the vector
for (row in non_na_dur_rows) {
  start_index <- max(1, row - 3)
  end_index <- min(nrow(merged_df), row + 3)
  rows_to_remove <- c(rows_to_remove, start_index:end_index)
}

# Remove duplicate indices
rows_to_remove <- unique(rows_to_remove)

# Remove the rows
merged_df <- merged_df[-rows_to_remove, ]

merged_df$dur <- NULL


# Fill NA values with 0 But it should change to drop all with 0 values
#merged_df[is.na(merged_df)] <- 0
#sum(draft_df$text == 2)
#merged_df <- na.omit(merged_df)


#========================== mean of absolute and mean of first deravitive
# Calculate first derivative of pupil_size
merged_df$first_derivative <- c(NA, diff(merged_df$pupil_size))

# Find index positions where text is 1 and 3
idx_one <- which(merged_df$text == 1)
idx_three <- which(merged_df$text == 3)

# Initialize lists to store the mean values and their indices
mean_values_list <- list()
mean_derivative_list <- list()

# Loop to find mean and mean of first derivative for segments of interest
for (i in 1:length(idx_one)) {
  current_one_idx <- idx_one[i]
  threes_after_one <- idx_three[idx_three > current_one_idx]
  
  if (length(threes_after_one) >= 2) {
    start_idx <- threes_after_one[1] + 1
    end_idx <- threes_after_one[2] - 1
    
    # Calculate mean of pupil_size
    mean_value <- mean(merged_df$pupil_size[start_idx:end_idx], na.rm = TRUE)
    # Calculate mean of first derivative
    mean_derivative <- mean(merged_df$first_derivative[start_idx:end_idx], na.rm = TRUE)
    
    # Append the mean values to the lists
    mean_values_list[[paste("Segment", i)]] <- mean_value
    mean_derivative_list[[paste("Segment", i)]] <- mean_derivative
  }
}

# Convert the lists to a data frame for easier viewing
dfp <- data.frame(Segment = names(mean_values_list),
                        Mean_Value = unlist(mean_values_list),
                        Mean_Derivative = unlist(mean_derivative_list))

write.csv(dfp, paste0("/Users/ali/Desktop/Experiment/sub",n,"/sub",n,"-",strategy,"-test-pupil.csv" ))
# ============================================= export

df <- read_csv(paste0("/Users/ali/Desktop/Experiment/sub",n,"/sub",n,"-",strategy,"-test.csv"))
dfp <- read_csv(paste0("/Users/ali/Desktop/Experiment/sub",n,"/sub",n,"-",strategy,"-test-pupil.csv"))


df$ps <- dfp$Mean_Value
df$dps <- dfp$Mean_Derivative

write.csv(df, paste0("/Users/ali/Desktop/Experiment/sub",n,"/sub",n,"-",strategy,"-test.csv" ))

# ============================================= plot
library(readr)
library("readxl")
library(ggrepel)
require(ggplot2)
library(investr)

n <- "03"
strategy <- "rolling"

lm <- lm(effort ~ dps , data = df)
ps_dps <- df$effort - fitted(lm)
summary(lm(effort ~ dps:n_back, df))

df <- read_csv(paste0("/Users/ali/Desktop/Experiment/sub",n,"/sub",n,"-",strategy,"-test.csv"))

fig <- ggplot(df, aes(effort, ps)) +    
  geom_point() + stat_smooth(method = "lm",formula = y ~ x) + labs(title = "Effort and Pupil",x = "effort",y = "mean pupil size")

fig 

ggsave(filename = paste0("/Users/ali/Desktop/Experiment/sub",n,"/figures/sub",n,"-",strategy,"-Effort and mean Pupil.png" ),  # The name of the output file
       plot = fig,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300,                  # Resolution in dots per inch
       type = "cairo"              # This can ensure better quality on some systems
)


fig <- ggplot(df, aes(effort, dps)) +    
  geom_point() + stat_smooth(method = "lm",formula = y ~ x) + labs(title = "Effort and Pupil",x = "effort",y = "mean changes of pupil size")

fig 

ggsave(filename = paste0("/Users/ali/Desktop/Experiment/sub",n,"/figures/sub",n,"-",strategy,"-Effort and change Pupil.png" ),  # The name of the output file
       plot = fig,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300,                  # Resolution in dots per inch
       type = "cairo"              # This can ensure better quality on some systems
)

summary(lm (effort ~ ps,df))

perform <- ggplot(df, aes(x = ps, y = g, color = factor(n_back))) +
  stat_smooth(aes(group = factor(n_back)), method = "lm", formula = y ~ poly(x,2),  se = FALSE) +
  geom_point()+ labs(title = "Perfromance and Effort",x = "Pupil Size",y = "normlized dprime",color = "load")+
  theme(legend.position = c(.15, .35),legend.justification = c("right", "top"),legend.box.just = "right")
perform

ggplot(df, aes(n_block, ps)) +    
  geom_point() + stat_smooth(method = "lm",formula = y ~ x) + 
  labs(title = "pupil size and task progress",
       x = "block",
       y = "pupil size")

phenom <- ggplot(df) + 
  geom_point(aes(x = ps, y = arousal, color = "Arousal")) + 
  geom_smooth(aes(x = ps, y = arousal, color = "Arousal"), method = "lm", se = TRUE) +
  
  geom_point(aes(x = ps, y = valence, color = "Valence")) +
  geom_smooth(aes(x = ps, y = valence, color = "Valence"), method = "lm", se = TRUE) +
  
  labs(
    title = "Phenomenal Pupil ",
    x = "Mean pupil size",
    y = "Arousal and Valence"
  ) + 
  scale_color_manual(
    name = "Measures",
    values = c("Arousal" = "orange", "Valence" = "purple")
  ) +
  theme(
    legend.position = c(.25, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right"
  )

phenom

ggsave(filename = paste0("/Users/ali/Desktop/Experiment/sub",n,"/figures/sub",n,"-",strategy,"-mean Pupil with arousla and valence.png" ),  # The name of the output file
       plot = phenom,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300,                  # Resolution in dots per inch
       type = "cairo"              # This can ensure better quality on some systems
)

phenom <- ggplot(df) + 
  geom_point(aes(x = dps, y = arousal, color = "Arousal")) + 
  geom_smooth(aes(x = dps, y = arousal, color = "Arousal"), method = "lm", se = TRUE) +
  
  geom_point(aes(x = dps, y = valence, color = "Valence")) +
  geom_smooth(aes(x = dps, y = valence, color = "Valence"), method = "lm", se = TRUE) +
  
  labs(
    title = "Phenomenal Pupil ",
    x = "Mean changes in pupil size",
    y = "Arousal and Valence"
  ) + 
  scale_color_manual(
    name = "Measures",
    values = c("Arousal" = "orange", "Valence" = "purple")
  ) +
  theme(
    legend.position = c(.25, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right"
  )

phenom

ggsave(filename = paste0("/Users/ali/Desktop/Experiment/sub",n,"/figures/sub",n,"-",strategy,"-mean changes Pupil with arousla and valence.png" ),  # The name of the output file
       plot = phenom,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300,                  # Resolution in dots per inch
       type = "cairo"              # This can ensure better quality on some systems
)

perform <- ggplot(df, aes(ps, rt_h, col=factor(n_back))) +    
  geom_point() + stat_smooth(method = "lm",se = FALSE,formula = y ~ poly(x,1)) +
  labs(
    title = "Performance and Pupil size",
    x = "Mean Pupil size",
    y = "performance (normlized dprime)",
    color = "n back"
  ) + 
  theme(
    legend.position = c(.2, .4),
    legend.justification = c("right", "top"),
    legend.box.just = "right"
  )
perform

