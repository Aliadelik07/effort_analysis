require(eyelinker)
require(dplyr)
require(tidyr)
require(ggplot2)
require(intervals)
require(stringr)

n <- "03"
strategy <- "proactive"

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

# Assign NA to specified rows in all columns except 'time'
merged_df[rows_to_remove, "pupil_size"] <- NA

merged_df$dur <- NULL


# changing the names of the start of each trial
merged_df <- merged_df %>%
  mutate(text = case_when(
    text %in% c("10", "20", "30") ~ "trial",
    TRUE ~ text
  ))
# changing the names of the start of each block
for (i in 1:(nrow(merged_df) - 1)) {
  # Check if the current row's text is "1" and not NA
  if (!is.na(merged_df$text[i]) && merged_df$text[i] == "1") {
    # Look for the next "trial" after the current "1"
    for (j in (i + 1):nrow(merged_df)) {
      if (!is.na(merged_df$text[j]) && merged_df$text[j] == "trial") {
        # Change the text of the row before the first "trial" to "block"
        if (j > 1) { # to avoid indexing at 0
          merged_df$text[j - 1] <- "block"
        }
        break # Stop the inner loop after finding the first "trial"
      }
    }
  }
}

# Change 'block' to 'block_start'
merged_df$text[merged_df$text == "block"] <- "block_start"

# Mark the first 3 rows after each 'block_start' as 'block_end'
for (i in which(merged_df$text == "block_start")) {
  if (i <= nrow(merged_df) - 3) {
    merged_df$text[(i + 1):(i + 3)] <- "block_end"
  }
}

# Change '2' to 'rest_start'
merged_df$text[merged_df$text == "2"] <- "rest_start"

# Mark the first 3 rows after each 'rest_start' as 'rest_end'
for (i in which(merged_df$text == "rest_start")) {
  if (i <= nrow(merged_df) - 3) {
    merged_df$text[(i + 1):(i + 3)] <- "rest_end"
  }
}



backup_df2 <- merged_df

merged_df <- backup_df2
#================= Replace 'pupil_size' values with NA when they are > 7000 or < 5000
# Find indices where 'pupil_size' is < 5000 or > 7000
outlier_indices <- which(merged_df$pupil_size < 5000 | merged_df$pupil_size > 7000)

# Create a vector to store the indices of rows to NA
rows_to_na <- integer(0)

# For each outlier row, add indices of the row and the 10 rows before and after it to the vector
for (index in outlier_indices) {
  start_index <- max(1, index )
  end_index <- min(nrow(merged_df), index)
  rows_to_na <- c(rows_to_na, start_index:end_index)
}

# Remove duplicate indices and sort
rows_to_na <- sort(unique(rows_to_na))

# Assign NA to specified rows in 'pupil_size'
merged_df[rows_to_remove, "pupil_size"] <- NA


# Find indices of 'block'
block_indices <- which(merged_df$text == "block_start")

# Ensure there are at least two 'block' occurrences
if(length(block_indices) >= 2) {
  # Extract the part of the dataframe between the first and second 'block'
  plot_data <- merged_df[block_indices[3]:block_indices[5], ]
  
  # Create the plot
  ggplot(plot_data, aes(x = time, y = pupil_size)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Pupil Size Over Time (First to Second Block)", x = "Time", y = "Pupil Size")
} else {
  print("Less than two 'block' occurrences found in the data")
}


# ============== ploy curve
# Find indices of 'trial'
trial_indices <- which(merged_df$text == "trial")

# Initialize a data frame to store the average pupil sizes for each trial
average_pupil_sizes <- data.frame(trial_number = integer(), average_pupil_size = numeric())

# Process each trial
for (i in 1:length(trial_indices)) {
  # Assuming each trial ends at the next 'trial' text or at the end of the dataframe
  if (i < length(trial_indices)) {
    trial_end_index <- trial_indices[i + 1] - 1
  } else {
    trial_end_index <- nrow(merged_df)
  }
  
  # Extract the trial data
  trial_data <- merged_df[trial_indices[i]:trial_end_index, ]
  
  # Calculate the average pupil size for the trial
  avg_pupil_size <- mean(trial_data$pupil_size, na.rm = TRUE)
  
  # Append to the data frame
  average_pupil_sizes <- rbind(average_pupil_sizes, data.frame(trial_number = i, average_pupil_size = avg_pupil_size))
}

# Find indices of 'block_start' and 'trial'
block_start_indices <- which(merged_df$text == "block_start")
trial_indices <- which(merged_df$text == "trial")

# Identify the first trial after each 'block_start'
first_trials_after_block_starts <- sapply(block_start_indices, function(x) {
  next_trial_index <- which(trial_indices > x)[1]
  return(next_trial_index)
})

# Create a color vector for plotting, default color is "blue"
average_pupil_sizes$color <- "blue"
  
# Highlight the first trial after each 'block_start' in red
average_pupil_sizes$color[first_trials_after_block_starts] <- "red"
  
# Plotting
ggplot(average_pupil_sizes, aes(x = trial_number, y = average_pupil_size, fill = color)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Average Pupil Size for Each Trial with First Trials After Block Start in Red",
       x = "Trial Number",
       y = "Average Pupil Size") +
  theme_minimal()

# ---------------down sampling by 5 (frame rate is 500) to have every 0.001 seconds or 10 milisecond per row.

# Assuming your dataframe is named df
df <- merged_df

# Round the time to the nearest multiple of 10
df$time <- round(df$time / 5) * 5

# Group by the rounded time
df_grouped <- df %>% 
  group_by(time) %>%
  summarize(
    pupil_size = mean(pupil_size, na.rm = TRUE),
    # Change text column to a single value for each group
    text = paste(na.omit(text), collapse = " ")
  )

df_grouped$text <- gsub("200 1", "1", df_grouped$text)


# ----------------------- arousal for action.
df <- df_grouped 

# Find indices where text == "1"
indices <- which(df$text == "1")

x = 500
# Initialize a list to store pupil_size values for each relative position
relative_pupil_sizes <- vector("list", 2*x + 1)

# Loop over each relative position

for (i in -x:x) {
  temp <- numeric()
  
  # Extract pupil_size values for each occurrence of text == "1"
  for (index in indices) {
    # Ensure the index + i is within the dataframe bounds
    if ((index + i) > 0 && (index + i) <= nrow(df)) {
      temp <- c(temp, df$pupil_size[index + i])
    }
  }
  
  # Store the average pupil_size for this relative position
  relative_pupil_sizes[[i + 2*x + 1]] <- mean(temp, na.rm = TRUE)
}


plot_data <- data.frame(
  position = -x:x,
  avg_pupil_size = unlist(relative_pupil_sizes)
)

# Determine y-values for the beginning and end of the geom line
beginning_y <- plot_data$avg_pupil_size[1]
end_y <- plot_data$avg_pupil_size[length(plot_data$avg_pupil_size)]

# Plot the data with a dashed trend line and horizontal lines
ggplot(plot_data, aes(x = position, y = avg_pupil_size)) +
  geom_line() +
  #geom_smooth(method = "loess", span = 0.2, se = FALSE, color = "blue", linetype = "dashed") +
  geom_hline(yintercept = beginning_y, linetype = "dotted", color = "red") +
  geom_hline(yintercept = end_y, linetype = "dotted", color = "red") +
  theme_minimal() +
  labs(title = "Mean pupil size before block start",
       x = "Position Relative to block start in 10 milisecond",
       y = "Pupil Size")
