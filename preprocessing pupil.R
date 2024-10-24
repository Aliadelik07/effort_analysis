require(eyelinker)
require(dplyr)
require(tidyr)
require(ggplot2)
require(intervals)
require(stringr)
library("readxl")
library(readr)

sub_list <- read_csv("/Users/ali/Desktop/Experiment/check_list.csv")
rows_to_remove <- c(1, 2, 3, 4, 5, 6, 10, 19, 20,  32, 33, 34, 43, 45, 46, 53) #bad files
sub_list <- sub_list[-rows_to_remove, ] # recordings to remove
sub_list <- sub_list[!is.na(sub_list$sub), ] # removing the NaN

peaks_list <- list()

# Loop through the data
for (isub in 1:nrow(sub_list)){
  
  sub <- sub_list$sub[isub]
  strategy <- sub_list$strategy[isub]

  dat <- read.asc(paste0("/Volumes/x9/INITIAL_DATABASE/",sub,"/",sub,"-",strategy,"-test.asc"))

  # Combine the lists of variable into one data frame
  dfp <- data.frame(time = dat$raw$time, pupil_size = dat$raw$ps, x = dat$raw$xp, y = dat$raw$yp)
  dfb <- data.frame(time = dat$blinks$stime, dur = dat$blinks$dur)
  merged_df <- merge(dat$msg, dfp, by = "time", all = TRUE)
  merged_df <- merge(merged_df, dfb, by = "time", all = TRUE)
  merged_df <- merged_df[order(merged_df$time), ]
  
  #removing all blinks ----------
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
  
  # Assign NA to specified rows in all columns except 'time'
  merged_df[rows_to_remove, "pupil_size"] <- NA
  
  merged_df$dur <- NULL
 
  
  df_grouped <- merged_df

  # block indices -------
  df_grouped$text <- gsub("200 1", "1", df_grouped$text)
  
  # Loop through the dataframe to update the text
  found_one <- FALSE
  changed_three <- FALSE
  
  for (i in 1:nrow(df_grouped)) {
    if (!is.na(df_grouped$text[i]) && df_grouped$text[i] == "1") {
      found_one <- TRUE
      changed_three <- FALSE  # Reset this flag after each "1"
    }
    
    if (found_one && !is.na(df_grouped$text[i]) && df_grouped$text[i] == "3" && !changed_three) {
      df_grouped$text[i] <- "block"
      changed_three <- TRUE  # Ensure only the first "3" is changed after a "1"
    }
  }
 
  
# microsaccade -----
  # Define the sampling rate and delta_t
  delta_t <- 0.002  # 2 ms in seconds
  
  # Compute horizontal velocity component using a centered difference method
  df_grouped <- df_grouped %>%
    mutate(vx = (lead(x, 2) + lead(x, 1) - lag(x, 1) - lag(x, 2)) / (6 * delta_t),
           vy = (lead(y, 2) + lead(y, 1) - lag(y, 1) - lag(y, 2)) / (6 * delta_t))
  
 
  sigma_x = (median((df_grouped$vx - median(df_grouped$vx, na.rm = TRUE))^2,na.rm = TRUE))^0.5
  sigma_y = (median((df_grouped$vx - median(df_grouped$vx, na.rm = TRUE))^2,na.rm = TRUE))^0.5

  # Calculate the velocity magnitude (Euclidean norm of vx and vy)
  df_grouped <- df_grouped %>%
    mutate(velocity = sqrt(vx^2 + vy^2))

  # Initialize a vector to store the sums of is_microsaccade
  sum_microsaccades <- numeric()
  
  # Loop through lambda values from 1 to 10
  for (lambda in 1:10) {
    
    # Calculate thresholds for microsaccade detection
    threshold_x <- lambda * sigma_x
    threshold_y <- lambda * sigma_y
    
    # Apply the microsaccade detection criterion
    df_grouped <- df_grouped %>%
      mutate(is_microsaccade = ifelse((vx / threshold_x)^2 + (vy / threshold_y)^2 > 1, 1, 0))

    # Sum is_microsaccade, ignoring NA values
    sum_microsaccades[lambda] <- sum(df_grouped$is_microsaccade, na.rm = TRUE)
    
  }
  
  # Create a data frame for plotting
  lambda_values <- 1:10
  microsaccade_data <- data.frame(lambda = lambda_values, sum_microsaccades = sum_microsaccades)
  
  # Add a new column with the difference of sum_microsaccades
  microsaccade_data$difference1 <- c(NA, diff(microsaccade_data$sum_microsaccades))
  # Add a new column with the difference of sum_microsaccades
  microsaccade_data$difference2 <- c(NA, diff(microsaccade_data$difference1))

  # Set lambda (threshold multiplier) 
  #  the lambda with the highest convexity: 
  #   where relatively most removal of microsaccade takes place
  max_diff_index <- which.max(microsaccade_data$difference2)
  lambda_at_max_diff <- microsaccade_data$lambda[max_diff_index]
  
  # Create a plot with sum_microsaccades and difference
  fig <- ggplot(microsaccade_data, aes(x = lambda)) +
    # Line and points for sum_microsaccades
    geom_line(aes(y = sum_microsaccades, color = "Sum of Microsaccades")) +
    geom_point(aes(y = sum_microsaccades, color = "Sum of Microsaccades")) +
    # Labels and title
    labs(title = paste0(sub,'_',strategy,'(',lambda_at_max_diff,')'),
         x = "Lambda (Threshold Multiplier)",
         y = "Sum of Microsaccades / Difference") +
    
    # Color legend for both lines
    scale_color_manual(values = c("Sum of Microsaccades" = "blue", "Difference" = "red")) +
    
    # Use minimal theme
    theme_minimal()
  
  print(fig)
  
  lambda <- lambda_at_max_diff
  
  # Calculate thresholds for microsaccade detection
  threshold_x <- lambda * sigma_x
  threshold_y <- lambda * sigma_y
  
  # Apply the microsaccade detection criterion
  df_grouped <- df_grouped %>%
    mutate(is_microsaccade = ifelse((vx / threshold_x)^2 + (vy / threshold_y)^2 > 1, 1, 0))
  
  # Compute magnitude (Euclidean distance between consecutive points)
  df_grouped <- df_grouped %>%
    mutate(magnitude = sqrt((lead(x, 1) - x)^2 + (lead(y, 1) - y)^2))
  
  # Find peaks (local maxima of velocity)
  df_grouped <- df_grouped %>%
    mutate(is_peak = (velocity > lag(velocity)) & (velocity > lead(velocity)))
  
  # Filter to get only peak velocities during micro-saccade
  peaks <- df_grouped %>%
    filter(is_peak == TRUE, is_microsaccade == 1) %>%
    select(time, velocity, magnitude)
  
  # Add the processed data frame to the list
  peaks_list[[isub]] <- peaks
  # ------------------------saving the pre-processed data
  write.csv(df_grouped, paste0("/Volumes/x9/INITIAL_DATABASE/",sub,"/",sub,"-",strategy,"-test-pupil.csv"))
  cat('|',rep("#", isub), rep("-", nrow(sub_list) - isub),'|\n', sep = "")
}

# Combine all data frames into one
peaks_combined <- bind_rows(peaks_list)
# Plot peak velocity vs magnitude
fig <- ggplot(peaks, aes(x = magnitude, y = velocity)) +
  geom_point(color = 'blue') +
  geom_smooth(method = 'lm', color = 'red', se = FALSE) +  # Optional: Add a trend line
  labs(title = "All Sub", x = "Magnitude (degrees)", y = "Peak Velocity (degrees per second)") +
  theme_minimal()

print(fig)
ggsave(paste0("/Volumes/x9/results/allsubs/figure/Magnitude_PeakVelocity.png"), plot = fig, width = 10, height = 6, dpi = 300)
