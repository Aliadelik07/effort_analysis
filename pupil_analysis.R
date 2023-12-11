library(readr)
library("readxl")
library(ggrepel)
require(ggplot2)
library(investr)
# ----------------------- importing.
n <- "01"
strategy <- "rolling"
df <- read_csv(paste0("/Users/ali/Desktop/Experiment/sub",n,"/sub",n,"-",strategy,"-test-pupil.csv"))


# Loop through the dataframe to update the text
found_one <- FALSE
changed_three <- FALSE

for (i in 1:nrow(df)) {
  if (!is.na(df$text[i]) && df$text[i] == "1") {
    found_one <- TRUE
    changed_three <- FALSE  # Reset this flag after each "1"
  }
  
  if (found_one && !is.na(df$text[i]) && df$text[i] == "3" && !changed_three) {
    df$text[i] <- "block"
    changed_three <- TRUE  # Ensure only the first "3" is changed after a "1"
  }
}


# ----------------------- arousal for action
# Find indices where text == "block"
indices <- which(df$text == "block")

x = 200
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
  geom_smooth(method = "loess", span = 0.1, se = TRUE, color = "pink", linetype = "solid") +
  #geom_hline(yintercept = beginning_y, linetype = "dotted", color = "red") +
  #geom_hline(yintercept = end_y, linetype = "dotted", color = "red") +
  theme_minimal() +
  scale_x_continuous(breaks = c(-100,0, 100), labels = c("-0.5","0", "0.5")) +
  labs(title = paste0("Average pupil size before block start (Sub ",n," ",strategy,")"),
       x = "Second",
       y = "Pupil size")
# ----------------------- post-error arousal
# Find indices of false_alarm
indices <- which(df$text == "12")

x = 400
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
  geom_smooth(method = "loess", span = 0.1, se = TRUE, color = "pink", linetype = "solid") +
  #geom_hline(yintercept = beginning_y, linetype = "dotted", color = "red") +
  #geom_hline(yintercept = end_y, linetype = "dotted", color = "red") +
  theme_minimal() +
  scale_x_continuous(breaks = c(-200,0, 200), labels = c("-0.1","0", "0.1")) +
  labs(title = paste0("Average pupil size after false_alarm (Sub ",n," ",strategy,")"),
       x = "Second",
       y = "Pupil size")


#---------------------ERP by load
# Function to process dataframe for a given text value

x = 400

process_data <- function(df, text_value) {
  indices <- which(df$text == text_value)
  relative_pupil_sizes <- vector("list", 2*x + 1)
  
  for (i in -50:x) {
    temp <- numeric()
    for (index in indices) {
      if ((index + i) > 0 && (index + i) <= nrow(df)) {
        temp <- c(temp, df$pupil_size[index + i])
      }
    }
    relative_pupil_sizes[[i + x + 1]] <- mean(temp, na.rm = TRUE)
  }
  
  data.frame(
    position = -50:x,
    avg_pupil_size = unlist(relative_pupil_sizes),
    text_value = as.factor(text_value)
  )
}

# Process data for each text value
data_10 <- process_data(df, "10")
data_20 <- process_data(df, "20")
data_30 <- process_data(df, "30")

# Combine all data
all_data <- rbind(data_10, data_20, data_30)


# Plot the data with different lines for each text value
ggplot(all_data, aes(x = position, y = avg_pupil_size, color = text_value)) +
  geom_line() +
  geom_smooth(method = "loess", span = 0.1, se = TRUE, linetype = "dashed") +
  theme_minimal() +
  scale_x_continuous(breaks = c(0, 200,400), labels = c("0", "1","2")) +
  labs(title = paste0("Average pupil size relative to trial start (Sub ",n," ",strategy,")"),
       x = "Second",
       y = "Average Pupil Size",
       color = "Level")

# ----------------------- arousal for action by load level
update_text <- function(df) {
  last_one_index <- NULL  # Track the last index where text was "1"
  
  for (i in 1:nrow(df)) {
    # Check if df$text[i] is not NA before comparing
    if (!is.na(df$text[i])) {
      if (df$text[i] == "block") {
        last_one_index <- i  # Update the last seen "1"
      } else if (!is.null(last_one_index) && df$text[i] %in% c("10", "20", "30")) {
        # If we find "10", "20", or "30" after a "1"
        df$text[last_one_index] <- paste("block", df$text[i], sep = "_")
        last_one_index <- NULL  # Reset the tracker
      }
    }
  }
  return(df)
}

# Update the dataframe
df <- update_text(df)

x = 200

process_data <- function(df, text_value) {
  indices <- which(df$text == text_value)
  relative_pupil_sizes <- vector("list", 2*x + 1)
  
  for (i in -x:x) {
    temp <- numeric()
    for (index in indices) {
      if ((index + i) > 0 && (index + i) <= nrow(df)) {
        temp <- c(temp, df$pupil_size[index + i])
      }
    }
    relative_pupil_sizes[[i + x + 1]] <- mean(temp, na.rm = TRUE)
  }
  
  data.frame(
    position = -x:x,
    avg_pupil_size = unlist(relative_pupil_sizes),
    text_value = as.factor(text_value)
  )
}

# Process data for each text value
data_10 <- process_data(df, "block_10")
data_20 <- process_data(df, "block_20")
data_30 <- process_data(df, "block_30")

# Combine all data
all_data <- rbind(data_10, data_20, data_30)


# Plot the data with different lines for each text value
ggplot(all_data, aes(x = position, y = avg_pupil_size, color = text_value)) +
  geom_line() +
  geom_smooth(method = "loess", span = 0.1, se = TRUE, linetype = "dashed") +
  theme_minimal() +
  scale_x_continuous(breaks = c(-200,0, 200), labels = c("-1","0", "1")) +
  labs(title = paste0("Average pupil size relative to block start (Sub ",n," ",strategy,")"),
       x = "Seconds",
       y = "Average pupil size",
       color = "Level")

#--------------------------------by block analaysis

# Update "1_10", "1_20", and "1_30" to "block"
df$text <- ifelse(df$text %in% c("block_10", "block_20", "block_30"), "block", df$text)

# Update "10", "20", or "30" to "trial"
df$text <- ifelse(grepl("10|20|30", df$text), "trial", df$text)

# Function to process data for each block
process_block_data <- function(df) {
  block_indices <- which(df$text == "block")
  all_data <- list()
  
  for (block_index in block_indices) {
    trial_indices <- which(df$text == "trial" & seq_along(df$text) > block_index)
    relative_pupil_sizes <- vector("list", 201)
    
    for (i in -100:100) {
      temp <- numeric()
      
      for (trial_index in trial_indices) {
        if ((trial_index + i) > 0 && (trial_index + i) <= nrow(df)) {
          temp <- c(temp, df$pupil_size[trial_index + i])
        }
      }
      relative_pupil_sizes[[i + 101]] <- mean(temp, na.rm = TRUE)
    }
    
    block_data <- data.frame(
      position = -100:100,
      avg_pupil_size = unlist(relative_pupil_sizes),
      block_index = block_index
    )
    all_data[[block_index]] <- block_data
  }
  return(do.call(rbind, all_data))
}


rating <- read_csv(paste0("/Users/ali/Desktop/Experiment/sub",n,"/sub",n,"-",strategy,"-test.csv"))
effort <- rating$effort

x = 400
# Function to process data for each block
process_block_data <- function(df, efforts) {
  block_indices <- which(df$text == "block")
  if(length(block_indices) != length(efforts)) {
    stop("The number of blocks and effort ratings do not match.")
  }
  
  all_data <- list()
  
  for (i in seq_along(block_indices)) {
    block_index <- block_indices[i]
    trial_indices <- which(df$text == "trial" & seq_along(df$text) > block_index)
    relative_pupil_sizes <- vector("list", x+1)
    
    for (j in -20:x) {
      temp <- numeric()
      
      for (trial_index in trial_indices) {
        if ((trial_index + j) > 0 && (trial_index + j) <= nrow(df)) {
          temp <- c(temp, df$pupil_size[trial_index + j])
        }
      }
      relative_pupil_sizes[[j + x+1]] <- mean(temp, na.rm = TRUE)
    }
    
    block_data <- data.frame(
      position = -20:x,
      avg_pupil_size = unlist(relative_pupil_sizes),
      effort = efforts[i]
    )
    all_data[[i]] <- block_data
  }
  return(do.call(rbind, all_data))
}

# Process and plot the data
plot_data <- process_block_data(df, rating$effort)
ggplot(plot_data, aes(x = position, y = avg_pupil_size, group = effort, color = as.factor(round(effort,1)))) +
  #geom_line() +
  geom_smooth(method = "loess", span = 0.1, se =FALSE) +
  theme_minimal() +
  scale_x_continuous(breaks = c(0,200, 400), labels = c("0","1", "2")) +
  labs(title = paste0("Average pupil size relative to trial start (Sub ",n," ",strategy,")"),
       x = "Seconds",
       y = "Average Pupil Size",
       color = "Effort Rating")


# Calculate median effort
median_effort <- median(rating$effort)

# Categorize efforts as 'low effort' or 'high effort'
rating$effort_category <- ifelse(rating$effort < median_effort, "low effort", "high effort")

# Function to process data for each block
process_block_data <- function(df, effort_categories) {
  block_indices <- which(df$text == "block")
  if(length(block_indices) != length(effort_categories)) {
    stop("The number of blocks and effort categories do not match.")
  }
  
  all_data <- list()
  
  for (i in seq_along(block_indices)) {
    block_index <- block_indices[i]
    trial_indices <- which(df$text == "trial" & seq_along(df$text) > block_index)
    relative_pupil_sizes <- vector("list", x+1)
    
    for (j in -20:x) {
      temp <- numeric()
      
      for (trial_index in trial_indices) {
        if ((trial_index + j) > 0 && (trial_index + j) <= nrow(df)) {
          temp <- c(temp, df$pupil_size[trial_index + j])
        }
      }
      relative_pupil_sizes[[j + x+1]] <- mean(temp, na.rm = TRUE)
    }
    
    block_data <- data.frame(
      position = -20:x,
      avg_pupil_size = unlist(relative_pupil_sizes),
      effort_category = effort_categories[i]
    )
    all_data[[i]] <- block_data
  }
  return(do.call(rbind, all_data))
}

# Process and plot the data
plot_data <- process_block_data(df, rating$effort_category)
ggplot(plot_data, aes(x = position, y = avg_pupil_size, group = effort_category, color = as.factor(effort_category))) +
  #geom_line() +
  geom_smooth(method = "loess", span = 0.1, se = TRUE) +
  scale_x_continuous(breaks = c(0,200, 400), labels = c("0","1", "2")) +
  theme_minimal() +
  labs(title = paste0("Average pupil size relative to trial start (Sub ",n," ",strategy,")"),
       x = "Seconds",
       y = "Average Pupil Size",
       color = "Effort Rating")
