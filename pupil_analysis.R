library(readr)
library("readxl")
library(ggrepel)
require(ggplot2)
library(investr)
library(dplyr)
library(vroom)

# ----------------------- import after running the preprocessing pupil.R
n <- "15"
strategy <- "proactive"
df <- read.csv(paste0("/Volumes/Effort_data/subs/sub",n,"/sub",n,"-",strategy,"-test-pupil.csv"), encoding = "UTF-8")

#------------------------ mean and derivative of each block

# Calculate first derivative of pupil_size
df$first_derivative <- c(NA, diff(df$pupil_size))

# Find index positions where text is 1 and 3
idx_block <- which(df$text == "block")
idx_three <- which(df$text == 3)

# Initialize lists to store the mean values and their indices
mean_values_list <- list()
mean_derivative_list <- list()

# Loop to find mean and mean of first derivative for segments of interest
for (i in 1:length(idx_block)) {
  current_block_idx <- idx_block[i]
  threes_after_block <- idx_three[idx_three > current_block_idx]
  
  if (length(threes_after_block) >= 2) {
    start_idx <- current_block_idx
    end_idx <- threes_after_block[1] - 1
    
    # Calculate mean of pupil_size
    mean_value <- mean(df$pupil_size[start_idx:end_idx], na.rm = TRUE)
    # Calculate mean of first derivative
    mean_derivative <- mean(df$first_derivative[start_idx:end_idx], na.rm = TRUE)
    
    # Append the mean values to the lists
    mean_values_list[[paste("block", i)]] <- mean_value
    mean_derivative_list[[paste("block", i)]] <- mean_derivative
  }
}

# Convert the lists to a data frame for easier viewing
ddf <- data.frame(Segment = names(mean_values_list),
                  Mean_Value = unlist(mean_values_list),
                  Mean_Derivative = unlist(mean_derivative_list))

#---------------------- saving average and deravitive for response analysis
bdf <- read.csv(paste0("/Volumes/Effort_data/subs/sub",n,"/sub",n,"-",strategy,"-test.csv"))

bdf$ps <- ddf$Mean_Value
bdf$dps <- ddf$Mean_Derivative



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
fig <- ggplot(plot_data, aes(x = position, y = avg_pupil_size)) +
  geom_line() +
  geom_smooth(method = "loess", span = 0.1, se = TRUE, color = "pink", linetype = "solid") +
  #geom_hline(yintercept = beginning_y, linetype = "dotted", color = "red") +
  #geom_hline(yintercept = end_y, linetype = "dotted", color = "red") +
  theme_minimal() +
  scale_x_continuous(breaks = c(-100,0, 100), labels = c("-0.5","0", "0.5")) +
  labs(title = paste0("Average pupil size before block start (Sub ",n," ",strategy,")"),
       x = "Second",
       y = "Pupil size")

fig
ggsave(filename = paste0("/Volumes/Effort_data/subs/sub",n,"/figures/sub",n,"-",strategy,"- before block start pupil.png"),  # The name of the output file
       plot = fig,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300,                  # Resolution in dots per inch
       type = "cairo"              # This can ensure better quality on some systems
)
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
fig <- ggplot(plot_data, aes(x = position, y = avg_pupil_size)) +
  geom_line() +
  geom_smooth(method = "loess", span = 0.1, se = TRUE, color = "pink", linetype = "solid") +
  #geom_hline(yintercept = beginning_y, linetype = "dotted", color = "red") +
  #geom_hline(yintercept = end_y, linetype = "dotted", color = "red") +
  theme_minimal() +
  scale_x_continuous(breaks = c(-200,0, 200), labels = c("-0.1","0", "0.1")) +
  labs(title = paste0("Average pupil size after false_alarm (Sub ",n," ",strategy,")"),
       x = "Second",
       y = "Pupil size")

fig
ggsave(filename = paste0("/Volumes/Effort_data/subs/sub",n,"/figures/sub",n,"-",strategy,"-pupil after false alarm.png"),  # The name of the output file
       plot = fig,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300,                  # Resolution in dots per inch
       type = "cairo"              # This can ensure better quality on some systems
)

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
fig <- ggplot(all_data, aes(x = position, y = avg_pupil_size, color = text_value)) +
  #geom_line() +
  geom_smooth(method = "loess", span = 0.1, se = TRUE) +
  theme_minimal() +
  scale_x_continuous(breaks = c(0, 200,400), labels = c("0", "1","2")) +
  labs(title = paste0("Average pupil size relative to trial start (Sub ",n," ",strategy,")"),
       x = "Second",
       y = "Average Pupil Size",
       color = "Level")+
  theme(legend.position = c(0.8, 0.9)) 

fig
ggsave(filename = paste0("/Volumes/Effort_data/subs/sub",n,"/figures/sub",n,"-",strategy,"- ERP_n.png"),  # The name of the output file
       plot = fig,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300,                  # Resolution in dots per inch
       type = "cairo"              # This can ensure better quality on some systems
)


process_data <- function(df, text_value) {
  indices <- which(df$text == text_value)
  relative_pupil_sizes <- vector("list", 2*x + 1)
  
  for (i in -x:x) {
    temp <- numeric()
    for (index in indices) {
      if ((index + i) > 0 && (index + i) <= nrow(df)) {
        temp <- c(temp, df$first_derivative[index + i])
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
all_data <- all_data %>%
  mutate(avg_pupil_size = ifelse(avg_pupil_size > 10 | avg_pupil_size < -10, NA, avg_pupil_size))

# Plot the data with different lines for each text value
fig <- ggplot(all_data, aes(x = position, y = avg_pupil_size, color = text_value)) +
  geom_line() +
  geom_smooth(method = "loess", span = 0.1, se = TRUE, linetype = "dashed") +
  theme_minimal() +
  #scale_x_continuous(breaks = c(-200,0, 200), labels = c("-1","0", "1")) +
  labs(title = paste0("Derivative pupil size relative to block start (Sub ",n," ",strategy,")"),
       x = "Seconds",
       y = "Derivative pupil size",
       color = "Level")

fig
ggsave(filename = paste0("/Volumes/Effort_data/subs/sub",n,"/figures/sub",n,"-",strategy,"-block start derevative pupil.png"),  # The name of the output file
       plot = fig,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300,                  # Resolution in dots per inch
       type = "cairo"              # This can ensure better quality on some systems
)

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

#x=200
x = 20000

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
fig <- ggplot(all_data, aes(x = position, y = avg_pupil_size, color = text_value)) +
  #geom_line() +
  geom_smooth(method = "loess", span = 0.1, se = TRUE) +
  theme_minimal() +
  #scale_x_continuous(breaks = c(-200,0, 200), labels = c("-1","0", "1")) +
  labs(title = paste0("Average pupil size relative to block start (Sub ",n," ",strategy,")"),
       x = "Seconds",
       y = "Average pupil size",
       color = "Level")
fig
ggsave(filename = paste0("/Volumes/Effort_data/subs/sub",n,"/figures/sub",n,"-",strategy,"-block start pupil.png"),  # The name of the output file
       plot = fig,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300,                  # Resolution in dots per inch
       type = "cairo"              # This can ensure better quality on some systems
)

# ------------------------------- feedback's EVOKE PUPIL RESPONSE




#--------------------------------by block analysis

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
plot_data <- process_block_data(df, bdf$effort)
plot_data <- left_join(plot_data, bdf %>% select(effort, valence), by='effort')
plot_data <- left_join(plot_data, bdf %>% select(effort, effort_category), by='effort')
plot_data <- left_join(plot_data, bdf %>% select(effort, valence_category), by='effort')

plot_data$valence_category <- paste(plot_data$valence_category, "valence", sep=" ")
plot_data$effort_valence_category <- paste(plot_data$effort_category, plot_data$valence_category, sep=" ")


fig <- ggplot(plot_data, aes(x = position, y = avg_pupil_size, group = effort, color = as.factor(round(effort,1)))) +
  #geom_line() +
  geom_smooth(method = "loess", span = 0.1, se =FALSE) +
  theme_minimal() +
  scale_x_continuous(breaks = c(0,200, 400), labels = c("0","1", "2")) +
  labs(title = paste0("Average pupil size relative to trial start (Sub ",n," ",strategy,")"),
       x = "Seconds",
       y = "Average Pupil Size",
       color = "Effort Rating")

fig
ggsave(filename = paste0("/Volumes/Effort_data/subs/sub",n,"/figures/sub",n,"-",strategy,"-effortVSpupilPerBlock.png"),  # The name of the output file
       plot = fig,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300,                  # Resolution in dots per inch
       type = "cairo"              # This can ensure better quality on some systems
)

fig <- ggplot(plot_data, aes(x = position, y = avg_pupil_size, group = valence, color = as.factor(round(valence,1)))) +
  #geom_line() +
  geom_smooth(method = "loess", span = 0.1, se =FALSE) +
  theme_minimal() +
  scale_x_continuous(breaks = c(0,200, 400), labels = c("0","1", "2")) +
  labs(title = paste0("Average pupil size relative to trial start (Sub ",n," ",strategy,")"),
       x = "Seconds",
       y = "Average Pupil Size",
       color = "Valence Rating")

fig
ggsave(filename = paste0("/Volumes/Effort_data/subs/sub",n,"/figures/sub",n,"-",strategy,"-valenceVSpupilPerBlock.png"),  # The name of the output file
       plot = fig,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300,                  # Resolution in dots per inch
       type = "cairo"              # This can ensure better quality on some systems
)


# finding the max of each avg_pupil_size
peak_point <- process_block_data(df, bdf$n_block)

colnames(peak_point)[colnames(peak_point) == "effort"] <- "n_block"
# save it in the hard disk for future
write.csv(peak_point, paste0("/Volumes/Effort_data/subs/sub",n,"/sub",n,"-",strategy,"-test-pupil ERP per block.csv" ))


# Step 1: Find the position of the maximum avg_pupil_size
max_position <- peak_point %>%
  filter(avg_pupil_size == max(avg_pupil_size, na.rm = TRUE)) %>%
  pull(position) %>%
  unique()

# Step 2: Retrieve avg_pupil_size and effort_category for that position
result <- peak_point %>%
  filter(position %in% max_position) %>%
  select(avg_pupil_size, n_block)

bdf$max_ps <- result$avg_pupil_size



fig <- ggplot(plot_data, aes(x = position, y = avg_pupil_size, group = effort_category, color = as.factor(effort_category))) +
  #geom_line() +
  geom_smooth(method = "loess", span = 0.1, se = TRUE) +
  scale_x_continuous(breaks = c(0,200, 400), labels = c("0","1", "2")) +
  theme_minimal() +
  labs(title = paste0("Average pupil size relative to trial start (Sub ",n," ",strategy,")"),
       x = "Seconds",
       y = "Average Pupil Size",
       color = "Effort Rating")
fig
ggsave(filename = paste0("/Volumes/Effort_data/subs/sub",n,"/figures/sub",n,"-",strategy,"-effortVSpupil.png"),  # The name of the output file
       plot = fig,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300,                  # Resolution in dots per inch
       type = "cairo"              # This can ensure better quality on some systems
)

fig <- ggplot(plot_data, aes(x = position, y = avg_pupil_size, group = valence_category, color = as.factor(valence_category))) +
  #geom_line() +
  geom_smooth(method = "loess", span = 0.1, se = TRUE) +
  scale_x_continuous(breaks = c(0,200, 400), labels = c("0","1", "2")) +
  theme_minimal() +
  labs(title = paste0("Average pupil size relative to trial start (Sub ",n," ",strategy,")"),
       x = "Seconds",
       y = "Average Pupil Size",
       color = "Valence Rating")
fig
ggsave(filename = paste0("/Volumes/Effort_data/subs/sub",n,"/figures/sub",n,"-",strategy,"-valenceVSpupil.png"),  # The name of the output file
       plot = fig,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300,                  # Resolution in dots per inch
       type = "cairo"              # This can ensure better quality on some systems
)

fig <- ggplot(plot_data, aes(x = position, y = avg_pupil_size, group = effort_valence_category, color = as.factor(effort_valence_category))) +
  #geom_line() +
  geom_smooth(method = "loess", span = 0.1, se = TRUE) +
  scale_x_continuous(breaks = c(0,200, 400), labels = c("0","1", "2")) +
  theme_minimal() +
  labs(title = paste0("Average pupil size relative to trial start (Sub ",n," ",strategy,")"),
       x = "Seconds",
       y = "Average Pupil Size",
       color = "Effort and Valence Rating")
fig
ggsave(filename = paste0("/Volumes/Effort_data/subs/sub",n,"/figures/sub",n,"-",strategy,"-effort_valenceVSpupil.png"),  # The name of the output file
       plot = fig,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300,                  # Resolution in dots per inch
       type = "cairo"              # This can ensure better quality on some systems
)

# ------------------------ on feedback
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
    trial_indices <- which(df$text == "14" & seq_along(df$text) > block_index)
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
plot_data <- process_block_data(df, bdf$effort)
plot_data <- left_join(plot_data, bdf %>% select(effort, valence), by='effort')
plot_data <- left_join(plot_data, bdf %>% select(effort, effort_category), by='effort')
plot_data <- left_join(plot_data, bdf %>% select(effort, valence_category), by='effort')


plot_data$valence_category <- paste(plot_data$valence_category, "valence", sep=" ")
plot_data$effort_valence_category <- paste(plot_data$effort_category, plot_data$valence_category, sep=" ")

fig <- ggplot(plot_data, aes(x = position, y = avg_pupil_size, group = effort, color = as.factor(round(effort,1)))) +
  #geom_line() +
  geom_smooth(method = "loess", span = 0.1, se =FALSE) +
  theme_minimal() +
  scale_x_continuous(breaks = c(0,200, 400), labels = c("0","1", "2")) +
  labs(title = paste0("Average pupil size relative to trial start (Sub ",n," ",strategy,")"),
       x = "Seconds",
       y = "Average Pupil Size",
       color = "Effort Rating")

fig
ggsave(filename = paste0("/Volumes/Effort_data/subs/sub",n,"/figures/sub",n,"-",strategy,"-FDeffortVSpupilPerBlock.png"),  # The name of the output file
       plot = fig,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300,                  # Resolution in dots per inch
       type = "cairo"              # This can ensure better quality on some systems
)

fig <- ggplot(plot_data, aes(x = position, y = avg_pupil_size, group = valence, color = as.factor(round(valence,1)))) +
  #geom_line() +
  geom_smooth(method = "loess", span = 0.1, se =FALSE) +
  theme_minimal() +
  scale_x_continuous(breaks = c(0,200, 400), labels = c("0","1", "2")) +
  labs(title = paste0("Average pupil size relative to Score Feedback (Sub ",n," ",strategy,")"),
       x = "Seconds",
       y = "Average Pupil Size",
       color = "Valence Rating")

fig
ggsave(filename = paste0("/Volumes/Effort_data/subs/sub",n,"/figures/sub",n,"-",strategy,"-FDvalenceVSpupilPerBlock.png"),  # The name of the output file
       plot = fig,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300,                  # Resolution in dots per inch
       type = "cairo"              # This can ensure better quality on some systems
)



fig <- ggplot(plot_data, aes(x = position, y = avg_pupil_size, group = effort_category, color = as.factor(effort_category))) +
  #geom_line() +
  geom_smooth(method = "loess", span = 0.1, se = TRUE) +
  scale_x_continuous(breaks = c(0,200, 400), labels = c("0","1", "2")) +
  theme_minimal() +
  labs(title = paste0("Average pupil size relative to Score Feedback (Sub ",n," ",strategy,")"),
       x = "Seconds",
       y = "Average Pupil Size",
       color = "Effort Rating")
fig
ggsave(filename = paste0("/Volumes/Effort_data/subs/sub",n,"/figures/sub",n,"-",strategy,"-FDeffortVSpupil.png"),  # The name of the output file
       plot = fig,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300,                  # Resolution in dots per inch
       type = "cairo"              # This can ensure better quality on some systems
)

fig <- ggplot(plot_data, aes(x = position, y = avg_pupil_size, group = valence_category, color = as.factor(valence_category))) +
  #geom_line() +
  geom_smooth(method = "loess", span = 0.1, se = TRUE) +
  scale_x_continuous(breaks = c(0,200, 400), labels = c("0","1", "2")) +
  theme_minimal() +
  labs(title = paste0("Average pupil size relative to Score Feedback (Sub ",n," ",strategy,")"),
       x = "Seconds",
       y = "Average Pupil Size",
       color = "Valence Rating")
fig
ggsave(filename = paste0("/Volumes/Effort_data/subs/sub",n,"/figures/sub",n,"-",strategy,"-FDvalenceVSpupil.png"),  # The name of the output file
       plot = fig,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300,                  # Resolution in dots per inch
       type = "cairo"              # This can ensure better quality on some systems
)

fig <- ggplot(plot_data, aes(x = position, y = avg_pupil_size, group = effort_valence_category, color = as.factor(effort_valence_category))) +
  #geom_line() +
  geom_smooth(method = "loess", span = 0.1, se = TRUE) +
  scale_x_continuous(breaks = c(0,200, 400), labels = c("0","1", "2")) +
  theme_minimal() +
  labs(title = paste0("Average pupil size relative to Score Feedback (Sub ",n," ",strategy,")"),
       x = "Seconds",
       y = "Average Pupil Size",
       color = "Effort and Valence Rating")
fig
ggsave(filename = paste0("/Volumes/Effort_data/subs/sub",n,"/figures/sub",n,"-",strategy,"-FDeffort_valenceVSpupil.png"),  # The name of the output file
       plot = fig,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300,                  # Resolution in dots per inch
       type = "cairo"              # This can ensure better quality on some systems
)


write.csv(bdf, paste0("/Volumes/Effort_data/subs/sub",n,"/sub",n,"-",strategy,"-test.csv" ))
