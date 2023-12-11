require(eyelinker)
require(dplyr)
require(tidyr)
require(ggplot2)
require(intervals)
require(stringr)

n <- "01"
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

# Assign NA to specified rows in all columns except 'time'
merged_df[rows_to_remove, "pupil_size"] <- NA

merged_df$dur <- NULL

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

# ------------------------saveing the preprocessed data
write.csv(df_grouped, paste0("/Users/ali/Desktop/Experiment/sub",n,"/sub",n,"-",strategy,"-test-pupil.csv" ))


