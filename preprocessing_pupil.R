# importing modules -----

require(eyelinker)
require(dplyr)
require(tidyr)
require(ggplot2)
require(intervals)
require(stringr)
library("readxl")
library(readr)


# adding the pariticipants info -------
sub_list <- read_csv("/Users/ali/Documents/Experiment/check_list.csv")
rows_to_remove <- c(1, 2, 3, 4, 5, 6, 10, 19, 20,  32, 33, 34, 43, 45, 46, 53) #bad files
sub_list <- sub_list[-rows_to_remove, ] # recordings to remove
sub_list <- sub_list[!is.na(sub_list$sub), ] # removing the NaN

peaks_list <- list()

# Loop through the data
for (isub in 1:nrow(sub_list)){
  
  sub <- sub_list$sub[isub]
  strategy <- sub_list$strategy[isub]
  
  dat <- read.asc(paste0("/Volumes/x9/INITIAL_DATABASE/",sub,"/",sub,"-",strategy,"-test.asc"))
  
  viewing_distance <- 80 #cm to monitor
  
  # importing time, pupil, coordinates ------
  dfp <- data.frame(time = dat$raw$time, ps = dat$raw$ps, x_deg = dat$raw$xp/viewing_distance, y_deg = dat$raw$yp/viewing_distance)
  
  # Events -------
  dfp <- merge(dat$msg, dfp, by = "time", all = TRUE)
  
  #blinks -------
  dfp$blink <- 0
  
  # Loop through each blink period and mark the blink column
  for (i in 1:length(dat[["blinks"]][["stime"]])) {
    # Define the blink window for this blink
    start_time <- dat[["blinks"]][["stime"]][i] - 100
    end_time <- dat[["blinks"]][["etime"]][i] + 100
    
    # Mark time points within the blink window as 1
    dfp$blink[dfp$time >= start_time & dfp$time <= end_time] <- 1
  }
  
  #removeing blinks
  dfp[dfp$blink == 1, c("ps", "x_deg", "y_deg")] <- NA
  
  #adding fixation ----
  dfp$fix <- 0
  # Loop through each fix period and mark the fix column
  for (i in 1:length(dat[["fix"]][["stime"]])) {
    # Define the fixation window for this fixation
    start_time <- dat[["fix"]][["stime"]][i]
    end_time <- dat[["fix"]][["etime"]][i]
    
    # Mark time points within the fix window as 1
    dfp$fix[dfp$time >= start_time & dfp$time <= end_time] <- 1
  }
  

  # Create the plot with custom color gradient
  hfig <- ggplot(dfp, aes(x = x_deg, y = y_deg)) +
    geom_bin2d() +
    scale_fill_gradientn(
      colors = c("white", "green", "red"),
      name = "Count"
    ) +
    labs(
      title = paste("Heatmap of Eye Positions |", sub, "-", strategy),
      x = "Horizontal", 
      y = "Vertical"
    ) +
    theme_minimal()
  
  hfig

  # Save the plot
  ggsave(
    filename = paste0("/Volumes/x9/results/allsubs/figure/heatmap_", sub, "_", strategy, ".png"),
    plot = hfig,
    width = 8,
    height = 6,
    dpi = 300
  )
  
# block indices ------
  dfp$text <- gsub("200 1", "1", dfp$text)
  
  # Loop through the dataframe to update the text
  found_one <- FALSE
  changed_three <- FALSE
  
  for (i in 1:nrow(dfp)) {
    if (!is.na(dfp$text[i]) && dfp$text[i] == "1") {
      found_one <- TRUE
      changed_three <- FALSE  # Reset this flag after each "1"
    }
    
    if (found_one && !is.na(dfp$text[i]) && dfp$text[i] == "3" && !changed_three) {
      dfp$text[i] <- "block"
      changed_three <- TRUE  # Ensure only the first "3" is changed after a "1"
    }
  }
 
  
# microsaccade -----
  # Define the sampling rate and delta_t
  delta_t <- 0.002  # 2 ms
  
  # Compute horizontal velocity component using a centered difference method
  dfp <- dfp %>%
    mutate(vx = (lead(x_deg, 2) + lead(x_deg, 1) - lag(x_deg, 1) - lag(x_deg, 2)) / (6 * delta_t),
           vy = (lead(y_deg, 2) + lead(y_deg, 1) - lag(y_deg, 1) - lag(y_deg, 2)) / (6 * delta_t))
  
  sigma_x = (median((dfp$vx)^2,na.rm = TRUE) - median(dfp$vx, na.rm = TRUE)^2)^0.5
  sigma_y = (median((dfp$vy)^2,na.rm = TRUE) - median(dfp$vy, na.rm = TRUE)^2)^0.5

  # Calculate the velocity magnitude (Euclidean norm of vx and vy)
  dfp <- dfp %>%
    mutate(velocity = sqrt(vx^2 + vy^2))
  
  lambda <- 6 # threshold
  
  # Calculate thresholds for microsaccade detection
  threshold_x <- lambda * sigma_x
  threshold_y <- lambda * sigma_y
  
  # Apply the microsaccade detection criterion
  dfp <- dfp %>%
    mutate(ms = ifelse(fix == 1 & ((vx / threshold_x)^2 + (vy / threshold_y)^2 > 1), 1, 0))
  
  
  # Compute magnitude (Euclidean distance between consecutive points)
  dfp <- dfp %>%
    mutate(magnitude = sqrt((lead(x_deg, 1) - x_deg)^2 + (lead(y_deg, 1) - y_deg)^2))
  
  # Find peaks (local maxima of velocity)
  dfp <- dfp %>%
    mutate(v_peak = (velocity > lag(velocity)) & (velocity > lead(velocity)))
  
  # Filter to get only peak velocities during micro-saccade
  peaks <- dfp %>%
    filter(v_peak == TRUE, ms == 1) %>%
    select(time, velocity, magnitude)
  
  # Add the processed data frame to the list
  peaks_list[[isub]] <- peaks
  # ------------------------saving the pre-processed data
  write.csv(dfp, paste0("/Volumes/x9/INITIAL_DATABASE/",sub,"/",sub,"-",strategy,"-test-pupil.csv"))
  cat('|',rep("#", isub), rep("-", nrow(sub_list) - isub),'|\n', sep = "")
}


# Plot peak velocity vs magnitude of all -----

# Combine all data frames into one
peaks_combined <- bind_rows(peaks_list)

fig <- ggplot(peaks, aes(x = magnitude, y = velocity)) +
  geom_point(color = 'blue') +
  geom_smooth(method = 'lm', color = 'red', se = FALSE) +  # Optional: Add a trend line
  labs(title = "All Sub", x = "Magnitude (degrees)", y = "Peak Velocity (degrees per second)") +
  theme_minimal()

print(fig)
ggsave(paste0("/Volumes/x9/results/allsubs/figure/Magnitude_PeakVelocity.png"), plot = fig, width = 10, height = 6, dpi = 300)
