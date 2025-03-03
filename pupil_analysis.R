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

# ----------------------- import after running the preprocessing pupil.R

sub_list <- read_csv("/Users/ali/Desktop/Experiment/check_list.csv")
rows_to_remove <- c(1, 2, 3, 4, 5, 6, 10, 19, 20, 32, 33, 34, 39, 42, 43, 45, 46, 53, 54) #bad files
sub_list <- sub_list[-rows_to_remove, ] # recordings to remove
sub_list <- sub_list[!is.na(sub_list$sub), ] # removing the NaN


# Loop through the data
for (isub in 1:nrow(sub_list)) {

  sub <- sub_list$sub[isub]
  strategy <- sub_list$strategy[isub]
  
  df <- read.csv(paste0("/Volumes/x9/INITIAL_DATABASE/",sub,"/",sub,"-",strategy,"-test-pupil.csv"), encoding = "UTF-8")
  
  #------------------------ mean and derivative of each block
  
  # Calculate first derivative of pupil_size
  df$first_derivative <- c(NA, diff(df$pupil_size))
  
  # Find index positions where text is 1 and 3
  idx_block <- which(df$text == "block")
  idx_three <- which(df$text == 3)
  idx_two <- which(df$text == 2)
  
  # Initialize lists to store the mean values and their indices
  mean_pupil_stim_list <- list()
  mean_derivative_list <- list()
  mean_pupil_rest_list <- list()
  mean_microsacc_stim_list <- list()
  mean_microsacc_rest_list <- list()
  mean_magnitude_stim_list <- list()
  mean_magnitude_rest_list <- list()
  mean_blink_stim_list <- list()
  mean_blink_rest_list <- list()
  
  # Loop to find mean and mean of first derivative for segments of interest: a block
  for (i in 1:length(idx_block)) {
    current_block_idx <- idx_block[i]
    threes_after_block <- idx_three[idx_three > current_block_idx]
    rest_idx <- idx_two[idx_two > current_block_idx]
    
    if (length(threes_after_block) >= 2) {
      start_idx <- current_block_idx
      end_idx <- threes_after_block[1] - 1
      
      # mean pupil_size
      mean_pupil_stim <- mean(df$pupil_size[start_idx:end_idx], na.rm = TRUE)
      mean_pupil_rest <- mean(df$pupil_size[rest_idx:rest_idx+1000], na.rm = TRUE)
      
      mean_derivative <- mean(df$first_derivative[start_idx:end_idx], na.rm = TRUE)
      
      # mean microsaccade
      mean_microsacc_stim <- mean(df$ms[start_idx:end_idx], na.rm = TRUE)
      mean_microsacc_rest <- mean(df$ms[rest_idx:rest_idx+1000], na.rm = TRUE)
      
      # mean magnitude
      mean_magnitude_stim <- mean(df$magnitude[start_idx:end_idx], na.rm = TRUE)
      mean_magnitude_rest <- mean(df$magnitude[rest_idx:rest_idx+1000], na.rm = TRUE)
      
      # mean blink
      mean_blink_stim <- mean(df$blink[start_idx:end_idx], na.rm = TRUE)
      mean_blink_rest <- mean(df$blink[rest_idx:rest_idx+10000], na.rm = TRUE)
      
      
      # Append the mean values to the lists
      mean_pupil_stim_list[[paste("block", i)]] <- mean_pupil_stim
      mean_pupil_rest_list[[paste("block", i)]] <- mean_pupil_rest
      
      mean_derivative_list[[paste("block", i)]] <- mean_derivative
      
      mean_microsacc_stim_list[[paste("block", i)]] <- mean_microsacc_stim
      mean_microsacc_rest_list[[paste("block", i)]] <- mean_microsacc_rest
      
      mean_magnitude_stim_list[[paste("block", i)]] <- mean_magnitude_stim
      mean_magnitude_rest_list[[paste("block", i)]] <- mean_magnitude_rest
      
      mean_blink_stim_list[[paste("block", i)]] <- mean_blink_stim
      mean_blink_rest_list[[paste("block", i)]] <- mean_blink_rest
    }
  }
  
  # Convert the lists to a data frame for easier viewing
  ddf <- data.frame(Segment = names(mean_pupil_stim_list),
                    mean_pupil_stim = unlist(mean_pupil_stim_list),
                    mean_pupil_rest = unlist(mean_pupil_rest_list),
                    mean_derivative = unlist(mean_derivative_list),
                    mean_microsacc_stim = unlist(mean_microsacc_stim_list),
                    mean_microsacc_rest = unlist(mean_microsacc_rest_list),
                    mean_magnitude_stim = unlist(mean_magnitude_stim_list),
                    mean_magnitude_rest = unlist(mean_magnitude_rest_list),
                    mean_blink_stim = unlist(mean_blink_stim_list),
                    mean_blink_rest = unlist(mean_blink_rest_list)
  )
  
  #---------------------- saving average and deravitive for response analysis
  bdf <- read.csv(paste0("/Volumes/x9/INITIAL_DATABASE/",sub,"/",sub,"-",strategy,"-test.csv"))
  
  
  bdf$pupil_stim <- ddf$mean_pupil_stim
  bdf$pupil_rest <- ddf$mean_pupil_rest
  bdf$microsacc_stim <- ddf$mean_microsacc_stim
  bdf$microsacc_rest <- ddf$mean_microsacc_rest
  bdf$magnitude_stim <- ddf$mean_magnitude_stim
  bdf$magnitude_rest <- ddf$mean_magnitude_rest
  bdf$blink_stim <- ddf$mean_blink_stim
  bdf$blink_rest <- ddf$mean_blink_rest
  
  bdf$ps_bs <- (bdf$pupil_stim - bdf$pupil_rest)/bdf$pupil_rest
  bdf$ps_bs[is.infinite(bdf$ps_bs)] <- NA
  bdf$ms_bs <- (bdf$microsacc_stim - bdf$microsacc_rest)/bdf$microsacc_rest
  bdf$ms_bs[is.infinite(bdf$ms_bs)] <- NA
  
  
  
  if(FALSE){
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
    labs(title = paste0("Average pupil size before block start (",sub," ",strategy,")"),
         x = "Second",
         y = "Pupil size")
  
  fig
  ggsave(filename = paste0("/Volumes/x9/results/",sub,"/figures/",sub,"-",strategy,"- before block start pupil.png"),  # The name of the output file
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
    labs(title = paste0("Average pupil size after false_alarm (",sub," ",strategy,")"),
         x = "Second",
         y = "Pupil size")
  
  fig
  ggsave(filename = paste0("/Volumes/x9/results/",sub,"/figures/",sub,"-",strategy,"-pupil after false alarm.png"),  # The name of the output file
         plot = fig,                    # The ggplot object you created
         width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
         dpi = 300,                  # Resolution in dots per inch
         type = "cairo"              # This can ensure better quality on some systems
  )
  
  #---------------------ERP by load
  # Function to process dataframe for a given text value
  x = 400
  
  process_data <- function(df, text_value,value) {
    indices <- which(df$text == text_value)
    relative_pupil_sizes <- vector("list", 2*x + 1)
    
    for (i in -50:x) {
      temp <- numeric()
      for (index in indices) {
        if ((index + i) > 0 && (index + i) <= nrow(df)) {
          temp <- c(temp, value[index + i])
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
  data_10 <- process_data(df, "10",df$pupil_size)
  data_20 <- process_data(df, "20",df$pupil_size)
  data_30 <- process_data(df, "30",df$pupil_size)
  
  # Combine all data
  all_data <- rbind(data_10, data_20, data_30)
  
  
  # Plot the data with different lines for each text value
  fig <- ggplot(all_data, aes(x = position, y = avg_pupil_size, color = text_value)) +
    #geom_line() +
    geom_smooth(method = "loess", span = 0.1, se = TRUE) +
    theme_minimal() +
    scale_x_continuous(breaks = c(0, 200,400), labels = c("0", "1","2")) +
    labs(title = paste0("Average pupil size relative to trial start (",sub," ",strategy,")"),
         x = "Second",
         y = "Average Pupil Size",
         color = "Level")+
    theme(legend.position = c(0.8, 0.9)) 
  
  
  fig
  ggsave(filename = paste0("/Volumes/x9/results/",sub,"/figures/",sub,"-",strategy,"--ERP_pupil_n.png"),  # The name of the output file
         plot = fig,                    # The ggplot object you created
         width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
         dpi = 300,                  # Resolution in dots per inch
         type = "cairo"              # This can ensure better quality on some systems
  )
  
  data_10 <- process_data(df, "10",df$ms)
  data_20 <- process_data(df, "20",df$ms)
  data_30 <- process_data(df, "30",df$ms)
  
  # Combine all data
  all_data <- rbind(data_10, data_20, data_30)
  
  
  # Plot the data with different lines for each text value
  fig <- ggplot(all_data, aes(x = position, y = avg_pupil_size, color = text_value)) +
    #geom_line() +
    geom_smooth(method = "loess", span = 0.1, se = TRUE) +
    theme_minimal() +
    scale_x_continuous(breaks = c(0, 200,400), labels = c("0", "1","2")) +
    labs(title = paste0("Average pupil size relative to trial start (",sub," ",strategy,")"),
         x = "Second",
         y = "Average Pupil Size",
         color = "Level")+
    theme(legend.position = c(0.8, 0.9)) 
  
  
  fig
  ggsave(filename = paste0("/Volumes/x9/results/",sub,"/figures/",sub,"-",strategy,"-ERP_microsacc_n.png"),  # The name of the output file
         plot = fig,                    # The ggplot object you created
         width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
         dpi = 300,                  # Resolution in dots per inch
         type = "cairo"              # This can ensure better quality on some systems
  )
  
  # computing the deravitive
  all_data$diff_avg_pupil_size <- c(NA, diff(all_data$avg_pupil_size))
  
  # removing the first row of each load
  all_data$diff_avg_pupil_size[all_data$position == -50] <- NA
  
  # Plot the data with different lines for each text value
  fig <- ggplot(all_data, aes(x = position, y = diff_avg_pupil_size, color = text_value)) +
    #geom_line() +
    geom_smooth(method = "loess", span = 0.1, se = TRUE) +
    theme_minimal() +
    #scale_x_continuous(breaks = c(-200,0, 200), labels = c("-1","0", "1")) +
    labs(title = paste0("Derivative pupil size relative to block start (",sub," ",strategy,")"),
         x = "Seconds",
         y = "Derivative pupil size",
         color = "Level")
  
  fig
  ggsave(filename = paste0("/Volumes/x9/results/",sub,"/figures/",sub,"-",strategy,"-block start derevative pupil.png"),  # The name of the output file
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
  
  # Process data for each text value
  data_10 <- process_data(df, "block_10",df$pupil_size)
  data_20 <- process_data(df, "block_20",df$pupil_size)
  data_30 <- process_data(df, "block_30",df$pupil_size)
  
  # Combine all data
  all_data <- rbind(data_10, data_20, data_30)
  
  
  # Plot the data with different lines for each text value
  fig <- ggplot(all_data, aes(x = position, y = avg_pupil_size, color = text_value)) +
    #geom_line() +
    geom_smooth(method = "loess", span = 0.1, se = TRUE) +
    theme_minimal() +
    #scale_x_continuous(breaks = c(-200,0, 200), labels = c("-1","0", "1")) +
    labs(title = paste0("Average pupil size relative to block start (",sub," ",strategy,")"),
         x = "Seconds",
         y = "Average pupil size",
         color = "Level")
  fig
  ggsave(filename = paste0("/Volumes/x9/results/",sub,"/figures/",sub,"-",strategy,"-block_start_pupil.png"),  # The name of the output file
         plot = fig,                    # The ggplot object you created
         width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
         dpi = 300,                  # Resolution in dots per inch
         type = "cairo"              # This can ensure better quality on some systems
  )
  
  
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
  process_block_data <- function(df, efforts, value) {
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
            temp <- c(temp, value[trial_index + j])
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
  plot_data <- process_block_data(df, bdf$effort,df$pupil_size)
  plot_data <- left_join(plot_data, bdf %>% select(effort, valence), by='effort')
  plot_data <- left_join(plot_data, bdf %>% select(effort, effort_category), by='effort')
  plot_data <- left_join(plot_data, bdf %>% select(effort, valence_category), by='effort')
  plot_data <- left_join(plot_data, bdf %>% select(effort, ps_pupil_rest), by='effort')

  plot_data$effort_valence_category <- paste(plot_data$effort_category, plot_data$valence_category, sep=" ")
  
  
  # normlizing by DC-offset
  pupil_rest_data <- plot_data %>%
    filter(position < 0) %>%
    group_by(effort) %>%
    summarize(pupil_rest = mean(avg_pupil_size, na.rm = TRUE))
  
  #plot_data <- left_join(plot_data, pupil_rest_data, by = "effort")
  
  plot_data$normalized_avg_pupil_size <- (plot_data$avg_pupil_size - plot_data$ps_pupil_rest)/plot_data$ps_pupil_rest
  
  
  fig <- ggplot(plot_data, aes(x = position, y = normalized_avg_pupil_size, group = effort, color = as.factor(round(effort,1)))) +
    #geom_line() +
    geom_smooth(method = "loess", span = 0.1, se =FALSE) +
    theme_minimal() +
    scale_x_continuous(breaks = c(0,200, 400), labels = c("0","1", "2")) +
    labs(title = paste0("Average pupil size relative to trial start (",sub," ",strategy,")"),
         x = "Seconds",
         y = "Average Pupil Size",
         color = "Effort Rating")
  
  fig
  ggsave(filename = paste0("/Volumes/x9/results/",sub,"/figures/",sub,"-",strategy,"-effortVSpupilPerBlock.png"),  # The name of the output file
         plot = fig,                    # The ggplot object you created
         width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
         dpi = 300,                  # Resolution in dots per inch
         type = "cairo"              # This can ensure better quality on some systems
  )
  
  fig <- ggplot(plot_data, aes(x = position, y = normalized_avg_pupil_size, group = valence, color = as.factor(round(valence,1)))) +
    #geom_line() +
    geom_smooth(method = "loess", span = 0.1, se =FALSE) +
    theme_minimal() +
    scale_x_continuous(breaks = c(0,200, 400), labels = c("0","1", "2")) +
    labs(title = paste0("Average pupil size relative to trial start (",sub," ",strategy,")"),
         x = "Seconds",
         y = "Average Pupil Size",
         color = "Valence Rating")
  
  fig
  ggsave(filename = paste0("/Volumes/x9/results/",sub,"/figures/",sub,"-",strategy,"-valenceVSpupilPerBlock.png"),  # The name of the output file
         plot = fig,                    # The ggplot object you created
         width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
         dpi = 300,                  # Resolution in dots per inch
         type = "cairo"              # This can ensure better quality on some systems
  )
  
  
  # Find the position of the maximum normalized_avg_pupil_size
  max_position <- plot_data %>%
    filter(normalized_avg_pupil_size == max(normalized_avg_pupil_size, na.rm = TRUE)) %>%
    pull(position) %>%
    unique()
  
  # Retrieve normalized_avg_pupil_size and effort_category for that position
  result <- plot_data %>%
    filter(position %in% max_position) %>%
    select(normalized_avg_pupil_size, effort)
  
  result_unique <- result[!duplicated(result$effort), ]
  #bdf$max_ps <- result_unique$normalized_avg_pupil_size
  if(FALSE){
  fig <- ggplot(plot_data, aes(x = position, y = normalized_avg_pupil_size, group = effort_category, color = as.factor(effort_category))) +
    #geom_line() +
    geom_smooth(method = "loess", span = 0.1, se = TRUE) +
    scale_x_continuous(breaks = c(0,200, 400), labels = c("0","1", "2")) +
    theme_minimal() +
    labs(title = paste0("Average pupil size relative to trial start (",sub," ",strategy,")"),
         x = "Seconds",
         y = "Average Pupil Size",
         color = "Effort Rating")
  fig
  ggsave(filename = paste0("/Volumes/x9/results/",sub,"/figures/",sub,"-",strategy,"-effortVSpupil.png"),  # The name of the output file
         plot = fig,                    # The ggplot object you created
         width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
         dpi = 300,                  # Resolution in dots per inch
         type = "cairo"              # This can ensure better quality on some systems
  )
  
  fig <- ggplot(plot_data, aes(x = position, y = normalized_avg_pupil_size, group = valence_category, color = as.factor(valence_category))) +
    #geom_line() +
    geom_smooth(method = "loess", span = 0.1, se = TRUE) +
    scale_x_continuous(breaks = c(0,200, 400), labels = c("0","1", "2")) +
    theme_minimal() +
    labs(title = paste0("Average pupil size relative to trial start (",sub," ",strategy,")"),
         x = "Seconds",
         y = "Average Pupil Size",
         color = "Valence Rating")
  fig
  ggsave(filename = paste0("/Volumes/x9/results/",sub,"/figures/",sub,"-",strategy,"-valenceVSpupil.png"),  # The name of the output file
         plot = fig,                    # The ggplot object you created
         width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
         dpi = 300,                  # Resolution in dots per inch
         type = "cairo"              # This can ensure better quality on some systems
  )
  
  fig <- ggplot(plot_data, aes(x = position, y = normalized_avg_pupil_size, group = effort_valence_category, color = as.factor(effort_valence_category))) +
    #geom_line() +
    geom_smooth(method = "loess", span = 0.1, se = FALSE) +
    scale_x_continuous(breaks = c(0,200, 400), labels = c("0","1", "2")) +
    theme_minimal() +
    labs(title = paste0("Average pupil size relative to trial start (",sub," ",strategy,")"),
         x = "Seconds",
         y = "Average Pupil Size",
         color = "Effort and Valence Rating")
  fig
  ggsave(filename = paste0("/Volumes/x9/results/",sub,"/figures/",sub,"-",strategy,"-effort_valenceMIXVSpupil.png"),  # The name of the output file
         plot = fig,                    # The ggplot object you created
         width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
         dpi = 300,                  # Resolution in dots per inch
         type = "cairo"              # This can ensure better quality on some systems
  )
  
  
  # Plot with combined smooth lines
  fig <- ggplot(plot_data, aes(x = position, y = normalized_avg_pupil_size)) +
    geom_smooth(aes(color = as.factor(valence_category)), method = "loess", span = 0.1, se = TRUE) +
    geom_smooth(aes(color = as.factor(effort_category)), method = "loess", span = 0.1, se = TRUE) +
    scale_x_continuous(breaks = c(0, 200, 400), labels = c("0", "1", "2")) +
    labs(color = "Category") +
    theme_minimal() +
    labs(title = paste0("Average pupil size relative to trial start (",sub," ",strategy,")"),
         x = "Seconds",
         y = "Average Pupil Size",
         color = "Effort and Valence Rating")
  
  print(fig)
  ggsave(filename = paste0("/Volumes/x9/results/",sub,"/figures/",sub,"-",strategy,"-effort_valenceSEPVSpupil.png"),  # The name of the output file
         plot = fig,                    # The ggplot object you created
         width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
         dpi = 300,                  # Resolution in dots per inch
         type = "cairo"              # This can ensure better quality on some systems
  )
  
}
  
  # Process and plot the data
  plot_data <- process_block_data(df, bdf$effort,df$ms)
  plot_data <- left_join(plot_data, bdf %>% select(effort, valence), by='effort')
  plot_data <- left_join(plot_data, bdf %>% select(effort, effort_category), by='effort')
  plot_data <- left_join(plot_data, bdf %>% select(effort, valence_category), by='effort')
  plot_data <- left_join(plot_data, bdf %>% select(effort, ps_pupil_rest), by='effort')
  
  
  fig <- ggplot(plot_data, aes(x = position, y = avg_pupil_size, group = effort, color = as.factor(round(effort,1)))) +
    #geom_line() +
    geom_smooth(method = "loess", span = 0.1, se =FALSE) +
    theme_minimal() +
    scale_x_continuous(breaks = c(0,200, 400), labels = c("0","1", "2")) +
    labs(title = paste0("Average pupil size relative to trial start (",sub," ",strategy,")"),
         x = "Seconds",
         y = "Frequency rate of microsaccade",
         color = "Effort Rating")
  
  fig
  ggsave(filename = paste0("/Volumes/x9/results/",sub,"/figures/",sub,"-",strategy,"-microssaceByEffortRating.png"),  # The name of the output file
         plot = fig,                    # The ggplot object you created
         width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
         dpi = 300,                  # Resolution in dots per inch
         type = "cairo"              # This can ensure better quality on some systems
  )
 
  fig <- ggplot(plot_data, aes(x = position, y = avg_pupil_size, group = effort_category, color = effort_category)) +
    #geom_line() +
    geom_smooth(method = "loess", span = 0.1, se =TRUE) +
    theme_minimal() +
    scale_x_continuous(breaks = c(0,200, 400), labels = c("0","1", "2")) +
    labs(title = paste0("Average pupil size relative to trial start (",sub," ",strategy,")"),
         x = "Seconds",
         y = "Frequency rate of microsaccade",
         color = "Effort Category")
  
  fig
  ggsave(filename = paste0("/Volumes/x9/results/",sub,"/figures/",sub,"-",strategy,"-microssaceByEffortCategory.png"),  # The name of the output file
         plot = fig,                    # The ggplot object you created
         width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
         dpi = 300,                  # Resolution in dots per inch
         type = "cairo"              # This can ensure better quality on some systems
  )
  
  
  if(FALSE){
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
  plot_data <- left_join(plot_data, bdf %>% select(effort, valence), by="effort")
  plot_data <- left_join(plot_data, bdf %>% select(effort, effort_category), by="effort")
  plot_data <- left_join(plot_data, bdf %>% select(effort, valence_category), by="effort")
  plot_data <- left_join(plot_data, bdf %>% select(effort, ps_pupil_rest), by="effort")
  
  plot_data$valence_category <- paste(plot_data$valence_category, "valence", sep=" ")
  plot_data$effort_category <- paste(plot_data$effort_category, "effort", sep=" ")
  plot_data$effort_valence_category <- paste(plot_data$effort_category, plot_data$valence_category, sep=" ")
  
  
  plot_data$normalized_avg_pupil_size <- (plot_data$avg_pupil_size - plot_data$ps_pupil_rest)/plot_data$ps_pupil_rest
  
  fig <- ggplot(plot_data, aes(x = position, y = normalized_avg_pupil_size, group = effort, color = as.factor(round(effort,1)))) +
    #geom_line() +
    geom_smooth(method = "loess", span = 0.1, se =FALSE) +
    theme_minimal() +
    scale_x_continuous(breaks = c(0,200, 400), labels = c("0","1", "2")) +
    labs(title = paste0("Average pupil size relative to trial start (",sub," ",strategy,")"),
         x = "Seconds",
         y = "Average Pupil Size",
         color = "Effort Rating")
  
  fig
  ggsave(filename = paste0("/Volumes/x9/results/",sub,"/figures/",sub,"-",strategy,"-FDeffortVSpupilPerBlock.png"),  # The name of the output file
         plot = fig,                    # The ggplot object you created
         width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
         dpi = 300,                  # Resolution in dots per inch
         type = "cairo"              # This can ensure better quality on some systems
  )
  
  fig <- ggplot(plot_data, aes(x = position, y = normalized_avg_pupil_size, group = valence, color = as.factor(round(valence,1)))) +
    #geom_line() +
    geom_smooth(method = "loess", span = 0.1, se =FALSE) +
    theme_minimal() +
    scale_x_continuous(breaks = c(0,200, 400), labels = c("0","1", "2")) +
    labs(title = paste0("Average pupil size relative to Score Feedback (",sub," ",strategy,")"),
         x = "Seconds",
         y = "Average Pupil Size",
         color = "Valence Rating")
  
  fig
  ggsave(filename = paste0("/Volumes/x9/results/",sub,"/figures/",sub,"-",strategy,"-FDvalenceVSpupilPerBlock.png"),  # The name of the output file
         plot = fig,                    # The ggplot object you created
         width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
         dpi = 300,                  # Resolution in dots per inch
         type = "cairo"              # This can ensure better quality on some systems
  )
  
  
  
  fig <- ggplot(plot_data, aes(x = position, y = normalized_avg_pupil_size, group = effort_category, color = as.factor(effort_category))) +
    #geom_line() +
    geom_smooth(method = "loess", span = 0.1, se = TRUE) +
    scale_x_continuous(breaks = c(0,200, 400), labels = c("0","1", "2")) +
    theme_minimal() +
    labs(title = paste0("Average pupil size relative to Score Feedback (",sub," ",strategy,")"),
         x = "Seconds",
         y = "Average Pupil Size",
         color = "Effort Rating")
  fig
  ggsave(filename = paste0("/Volumes/x9/results/",sub,"/figures/",sub,"-",strategy,"-FDeffortVSpupil.png"),  # The name of the output file
         plot = fig,                    # The ggplot object you created
         width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
         dpi = 300,                  # Resolution in dots per inch
         type = "cairo"              # This can ensure better quality on some systems
  )
  
  fig <- ggplot(plot_data, aes(x = position, y = normalized_avg_pupil_size, group = valence_category, color = as.factor(valence_category))) +
    #geom_line() +
    geom_smooth(method = "loess", span = 0.1, se = TRUE) +
    scale_x_continuous(breaks = c(0,200, 400), labels = c("0","1", "2")) +
    theme_minimal() +
    labs(title = paste0("Average pupil size relative to Score Feedback (",sub," ",strategy,")"),
         x = "Seconds",
         y = "Average Pupil Size",
         color = "Valence Rating")
  fig
  ggsave(filename = paste0("/Volumes/x9/results/",sub,"/figures/",sub,"-",strategy,"-FDvalenceVSpupil.png"),  # The name of the output file
         plot = fig,                    # The ggplot object you created
         width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
         dpi = 300,                  # Resolution in dots per inch
         type = "cairo"              # This can ensure better quality on some systems
  )
  
  fig <- ggplot(plot_data, aes(x = position, y = normalized_avg_pupil_size, group = effort_valence_category, color = as.factor(effort_valence_category))) +
    #geom_line() +
    geom_smooth(method = "loess", span = 0.1, se = TRUE) +
    scale_x_continuous(breaks = c(0,200, 400), labels = c("0","1", "2")) +
    theme_minimal() +
    labs(title = paste0("Average pupil size relative to Score Feedback (",sub," ",strategy,")"),
         x = "Seconds",
         y = "Average Pupil Size",
         color = "Effort and Valence Rating")
  fig
  ggsave(filename = paste0("/Volumes/x9/results/",sub,"/figures/",sub,"-",strategy,"-FDeffort_valenceMIXVSpupil.png"),  # The name of the output file
         plot = fig,                    # The ggplot object you created
         width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
         dpi = 300,                  # Resolution in dots per inch
         type = "cairo"              # This can ensure better quality on some systems
  )
  }
  }
  
  write.csv(bdf,paste0("/Volumes/x9/INITIAL_DATABASE/",sub,"/",sub,"-",strategy,"-test.csv"))
  
  cat('|',rep("#", isub), rep("-", nrow(sub_list) - isub),'|\n', sep = "")
}

#group_level_analysis---------------------------------
process_block_data <- function(df,bf,event,pre,post) {
  block_indices <- which(df$text == "block")
  post_position = post
  pre_position = pre
  if(length(block_indices) != length(bf$n_back)) {
    stop("The number of blocks and effort ratings do not match.")
  }
  
  all_data <- list()
  
  for (i in seq_along(block_indices)) {
    block_index <- block_indices[i]
    trial_indices <- which(df$text == event & seq_along(df$text) > block_index)
    pupil_sizes <- vector("list", post_position+1)
    microsacc <- vector("list", post_position+1)
    magnitude <- vector("list", post_position+1)
    blink <- vector("list", post_position+1)

    #pupil
    for (j in pre_position:post_position) {
      temp <- numeric()
      
      for (trial_index in trial_indices) {
        if ((trial_index + j) > 0 && (trial_index + j) <= nrow(df)) {
          temp <- c(temp, df$ps[trial_index + j])
        }
      }
      pupil_sizes[[j + post_position+1]] <- mean(temp, na.rm = TRUE)
    }
    
    #microsaccade
    for (j in pre_position:post_position) {
      temp <- numeric()
      
      for (trial_index in trial_indices) {
        if ((trial_index + j) > 0 && (trial_index + j) <= nrow(df)) {
          temp <- c(temp, df$ms[trial_index + j])
        }
      }
      microsacc[[j + post_position+1]] <- mean(temp, na.rm = TRUE)
    }
    
    #magnitude
    for (j in pre_position:post_position) {
      temp <- numeric()
      
      for (trial_index in trial_indices) {
        if ((trial_index + j) > 0 && (trial_index + j) <= nrow(df)) {
          temp <- c(temp, df$magnitude[trial_index + j])
        }
      }
      magnitude[[j + post_position+1]] <- mean(temp, na.rm = TRUE)
    }
    
    #blink
    for (j in pre_position:post_position) {
      temp <- numeric()
      
      for (trial_index in trial_indices) {
        if ((trial_index + j) > 0 && (trial_index + j) <= nrow(df)) {
          temp <- c(temp, df$blink[trial_index + j])
        }
      }
      blink[[j + post_position+1]] <- mean(temp, na.rm = TRUE)
    }
    
    block_data <- data.frame(
      block = bf$n_block[i],
      n_back = bf$n_back[i],
      position = pre_position:post_position,
      pupil_size = unlist(pupil_sizes),
      microsacc = unlist(microsacc),
      magnitude = unlist(magnitude),
      blink = unlist(blink),
      effort = round(bf$slider_effort.response[i]/10),
      effort_category = bf$effort_category[i],
      performance_category = bf$performance_category[i],
      pac1 = bf$pac1[i],
      pac2 = bf$pac2[i]
    )
    all_data[[i]] <- block_data
  }
  return(do.call(rbind, all_data))
}

# Min-Max normalization function
minmax_normalize <- function(x,a) {(x - min(a, na.rm = TRUE)) / (max(a, na.rm = TRUE) - min(a, na.rm = TRUE))}


sub_list <- read_csv("/Users/ali/Documents/Experiment/check_list.csv")

rows_to_remove <- c(1, 2, 3, 4, 5, 6, 10, 19, 20, 32, 33, 34, 39, 41, 42, 43, 45, 46, 53, 54) #bad files
sub_list <- sub_list[-rows_to_remove, ] # recordings to remove
sub_list <- sub_list[!is.na(sub_list$sub), ] # removing the NaN

dfplot_list <- list()

# Loop through the data
for (isub in 1:nrow(sub_list)) {
  
  sub <- sub_list$sub[isub]
  strategy <- sub_list$strategy[isub]
  
  df <- read.csv(paste0("/Volumes/x9/INITIAL_DATABASE/",sub,"/",sub,"-",strategy,"-test-pupil.csv"), encoding = "UTF-8")
  bf <- read.csv(paste0("/Volumes/x9/INITIAL_DATABASE/",sub,"/",sub,"-",strategy,"-test.csv"), encoding = "UTF-8")
  
  # Update "10", "20", or "30" to "stim"
  df$text <- ifelse(grepl("10|20|30", df$text), "stim", df$text)
  df$text <- ifelse(grepl("2", df$text), "rest", df$text)
  
  # ERP
  plot_data_stim <- process_block_data(df, bf, 'stim',0,1000)
  plot_data_rest <- process_block_data(df, bf, 'rest',0,1000)
  

  # removing outlier
  repeat {
    # Initialize a variable to track if NAs were added
    previous_na_count <- sum(is.na(plot_data_stim))
    
    # remove the outlier
    plot_data_stim[['pupil_size']][abs(scale(plot_data_stim[['pupil_size']])) >= 3] <- NA
    plot_data_rest[['pupil_size']][abs(scale(plot_data_rest[['pupil_size']])) >= 3] <- NA
    # Check if no more NAs were added
    current_na_count <- sum(is.na(plot_data_stim))
    
    # Exit the loop if no new NAs were added
    if (current_na_count == previous_na_count) {
      break
    }
  }
  
  # renaming 
  # List of columns to modify
  columns_to_modify <- c("pupil_size", "microsacc", "magnitude", "blink")
  
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
  
  #adding sub charachtrestics
  plot_data <- plot_data %>% mutate(
    sub = sub,
    sex = sub_list$sex[isub],
    age = sub_list$age[isub],
    dO = sub_list$dominancO[isub],
    wc = sub_list$workingcapacity[isub],
    bc = sub_list$besoincogni[isub],
    strategy = strategy
  )
  
  # Add the processed data frame to the list
  dfplot_list[[isub]] <- plot_data
  
  #################### adding ps and ms to bf
  dfp_pac <-  plot_data
  dfp_pac <- dfp_pac %>% filter(position >= 1 & position <= 600)
  
  # Number of rows per batch
  batch_size <- 300
  
  # Number of batches
  n_batches <- floor(nrow(dfp_pac) / batch_size)
  
  # Initialize empty lists for ps1, ps2, ms1, and ms2
  ps1 <- list()
  ps2 <- list()
  ms1 <- list()
  ms2 <- list()
  mg1 <- list()
  mg2 <- list()
  
  # Loop through each batch and calculate the averages
  for (i in seq(1, n_batches, by = 2)) {
    ps1[[length(ps1) + 1]] <- mean(dfp_pac$pupil_size_stim_norm[((i - 1) * batch_size + 1):(i * batch_size)], na.rm = TRUE)
    
    ms1[[length(ms1) + 1]] <- mean(dfp_pac$microsacc_stim[((i - 1) * batch_size + 1):(i * batch_size)], na.rm = TRUE)
    
    mg1[[length(mg1) + 1]] <- mean(dfp_pac$magnitude[((i - 1) * batch_size + 1):(i * batch_size)], na.rm = TRUE)
    
    # Check if there is a second 200 rows batch to calculate the average
    if (i + 1 <= n_batches) {
      ps2[[length(ps2) + 1]] <- mean(dfp_pac$pupil_size_stim_norm[(i * batch_size + 1):((i + 1) * batch_size)], na.rm = TRUE)
      
      ms2[[length(ms2) + 1]] <- mean(dfp_pac$microsacc_stim[(i * batch_size + 1):((i + 1) * batch_size)], na.rm = TRUE)

      mg2[[length(mg2) + 1]] <- mean(dfp_pac$magnitude[(i * batch_size + 1):((i + 1) * batch_size)], na.rm = TRUE)
    }
  }
  
  # Convert lists to vectors if needed
  bf$ps1 <- unlist(ps1)
  bf$ps2 <- unlist(ps2)
  bf$ms1 <- unlist(ms1)
  bf$ms2 <- unlist(ms2)
  bf$mg1 <- unlist(mg1)
  bf$mg2 <- unlist(mg2)
  
  # removing errorly created index column
  score_col_index <- which(names(bf) == "score")
  bf <- bf[, score_col_index:ncol(bf)]
  
  # exporting back bf
  write.csv(bf, paste0("/Volumes/x9/INITIAL_DATABASE/",sub,"/",sub,"-",strategy,"-test.csv"))
  
  cat('|',rep("#", isub), rep("-", nrow(sub_list) - isub),'|\n', sep = "")
  }


# Combine all data frames into one
dfplot <- bind_rows(dfplot_list)

dfplot <- dfplot %>%
  mutate(task = ifelse(strategy %in% c('static', 'rolling'), 'n-back', 'ax-cpt'))

dfplot$block <- dfplot$block + 1

# save the final dfplot
write.csv(dfplot, paste0("/Volumes/x9/INITIAL_DATABASE/dfplot_stim_lambda_6.csv"))

#============================================= plotting =================
dfplot <- read_csv(paste0('/Volumes/x9/INITIAL_DATABASE/dfplot_stim_lambda_6.csv'))


#reorgnising rest and stim
df_stim <- dfplot[, c("pupil_size_stim_norm", "microsacc_stim", "blink_stim", 
                   "magnitude_stim", "position", "strategy", "sub", 
                   "block", "effort")]
colnames(df_stim) <- gsub("_stim", "", colnames(df_stim))

df_stim$effort_category <- ifelse(df_stim$effort > median(df_stim$effort, na.rm = TRUE),
                                  "High effort", "Low effort")

df_rest <- dfplot[, c("pupil_size_rest_norm", "microsacc_rest", "blink_rest", 
                   "magnitude_rest", "position", "strategy", "sub", 
                   "block", "effort")]
colnames(df_rest) <- gsub("_rest", "", colnames(df_rest))

df_rest$effort_category <- "Rest"
df_rest$effort <- -1

df_eye <- rbind(df_stim, df_rest)

# converting to time and hz
df_eye$time <- round(df_eye$position/500,1)

df_agg <- df_eye %>%
  group_by(time, strategy, block, effort_category) %>%
  summarize(ps = mean(pupil_size_norm, na.rm = TRUE),
            ms = mean(microsacc, na.rm = TRUE)*100,
            bl = mean(blink, na.rm = TRUE),
            mag = mean(magnitude, na.rm = TRUE))


# Generate the plot
fig <- ggplot(df_agg, aes(x = time, y = ps, 
                          group = effort_category,
                          color = effort_category)) +
  geom_smooth(method = "loess", span = 0.3, se = TRUE) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Trial onset line
  annotate("text", x = 0, y = 0.61, label = "Trial Onset",  vjust = -0.5, hjust = 0, size = 6, fontface = "bold") +  # Label for the line
  theme_minimal(base_size = 16) +  # Increase font size
  scale_color_manual(values = c("Rest" = "#00008B", 
                                "High effort" = "#8B0000", 
                                "Low effort" = "#FFC0CB")) +
  labs(title = "Stim-locked EPR of Pupil Size",
       x = "Seconds",
       y = "Pupil Size (%)",
       color = "") +
  theme(legend.position = c(0.8, 0.2),
        legend.key = element_rect(fill = NA))


# Print the plot
print(fig)

ggsave(filename = "/Volumes/x9/results/allsubs/figure/EPR_ps_6_all.png",  # The name of the output file
       plot = fig,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300)                     # Resolution in dots per inch

# Generate the plot
fig <- ggplot(df_agg, aes(x = time, y = ms, 
                          group = effort_category,
                          color = effort_category)) +
  geom_smooth(method = "loess", span = 0.3, se = TRUE) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Trial onset line
  annotate("text", x = 0, y = 0.8, label = "Trial Onset",  vjust = -0.5, hjust = 0, size = 6, fontface = "bold") +  # Label for the line
  theme_minimal(base_size = 16) +  # Increase font size
  scale_color_manual(values = c("Rest" = "#00008B", 
                                "High effort" = "#8B0000", 
                                "Low effort" = "#FFC0CB")) +
  labs(title = "Stim-locked EPR of Microsaccade",
       x = "Seconds",
       y = "Microsaccade (Hz)",
       color = "") +
  theme(legend.position = c(0.8, 0.2),
        legend.key = element_rect(fill = NA))

# Print the plot
print(fig)

ggsave(filename = "/Volumes/x9/results/allsubs/figure/EPR_ms_6_all.png",  # The name of the output file
       plot = fig,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300)                     # Resolution in dots per inch

# statistics ========
dfplot <- read_csv('/Volumes/x9/INITIAL_DATABASE/dfplot_stim_lambda_6.csv', locale = locale(encoding = "UTF-8"))

#dfplot <- dfplot %>%left_join(dfa %>% dplyr::select(sub, strategy, block, m_correct), by = c("sub", "strategy", "block"))

#normlizing microsaccade
dfplot$microsacc_stim_norm <- (dfplot$microsacc_stim - dfplot$microsacc_rest)/ dfplot$microsacc_rest
#dfplot$microsacc_stim_norm <- minmax_normalize(dfplot$microsacc_stim,c(dfplot$microsacc_rest, dfplot$microsacc_stim))

# removing inf
dfplot <- dfplot %>% filter_all(all_vars(!is.infinite(.)))
# removing NA
dfplot_clean <- na.omit(dfplot)

# effort ---------------
# microsaccade
model2 <-lmer( microsacc_stim_norm ~ effort*strategy + n_back + block + (1|position) + (1|sub), subset(dfplot_clean,task=='ax-cpt'))
summary(model2)
model1 <-lmer( microsacc_stim_norm ~ effort*strategy + n_back + block + (1|position) + (1|sub), subset(dfplot_clean,task=='n-back'))
summary(model1)


# (further analysis) comparing stim vs rest for microsaccade

df_long <- tidyr::pivot_longer(dfplot, cols = c(microsacc_stim, microsacc_rest),
                               names_to = "condition", values_to = "microsacc")

# Convert condition to a factor
df_long$condition <- factor(df_long$condition, levels = c("microsacc_rest", "microsacc_stim"))  # Reference: rest


model2 <-lmer( microsacc ~ condition + block + (1|position) + (1|sub), subset(df_long,task=='ax-cpt'))
summary(model2)
model1 <-lmer( microsacc ~ condition + block + (1|position) + (1|sub), subset(df_long,task=='n-back'))
summary(model1)


# pupil size
model2 <-lmer( pupil_size_stim_norm ~ effort*strategy + n_back + block + (1|position)  + (1|sub), subset(dfplot_clean,task=='ax-cpt'))
summary(model2)
model1 <-lmer( pupil_size_stim_norm ~ effort*strategy + n_back + block + (1|position) + (1|sub), subset(dfplot_clean,task=='n-back'))
summary(model1)


plot_model(model1, type = "int", show.data = FALSE) +  # Effect plot for fixed effects
  theme_minimal(base_size = 14) +
  labs(title = "N-back",
       x = "Reported effort",
       y = "Predicted pupil size") +
  theme(legend.position = "top")

plot_model(model2, type = "int", show.data = FALSE) +  # Effect plot for fixed effects
  theme_minimal(base_size = 14) +
  labs(title = "AX-CPT",
       x = "Reported effort",
       y = "Predicted pupil size") +
  theme(legend.position = "top")


# pac ---------
dfa <- read_csv(paste0("/Users/ali/Documents/Experiment/dfa-test.csv"))
names(dfa)[names(dfa) == "n_block"] <- "block"
dfa$block <- dfa$block + 1


# Merge the computed averages into dfa
dfplot_clean <- dfplot_clean %>%
  left_join(dfa, by = c("sub", "strategy", "block"))

# ps ~ pac
model1 <-lmer( pupil_size_stim_norm ~  pac3_high*strategy + n_back.x + block + (1|position) + (1|sub), subset(dfplot_clean,task.x=='n-back'))
summary(model1)

model2 <-lmer( pupil_size_stim_norm ~  pac3_high*strategy + n_back.x + block + (1|position)  + (1|sub), subset(dfplot_clean,task.x=='ax-cpt'))
summary(model2)

plot_model(model1, type = "int", show.data = FALSE) +  # Effect plot for fixed effects
  theme_minimal(base_size = 14) +
  labs(title = "N-back",
       x = "Predicted PAC",
       y = "Pupil size" ) +
  theme(legend.position = "top")

plot_model(model2, type = "int", show.data = FALSE) +  # Effect plot for fixed effects
  theme_minimal(base_size = 14) +
  labs(title = "AX-CPT",
       x = "Predicted PAC",
       y = "Pupil size") +
  theme(legend.position = "top")

dfplot_clean$delta_pac <- I(dfplot_clean$pac2.x - dfplot_clean$pac1.x)

model1 <-lmer( pupil_size_stim_norm  ~ delta_pac*strategy + n_back.x + block + (1|position) + (1|sub), subset(dfplot_clean,task.x=='n-back'))
summary(model1)

model2 <-lmer(  pupil_size_stim_norm ~ delta_pac*strategy + n_back.x + block + (1|position)  + (1|sub), subset(dfplot_clean,task.x=='ax-cpt'))
summary(model2)


plot_model(model1, type = "int", show.data = FALSE) +  # Effect plot for fixed effects
  theme_minimal(base_size = 14) +
  labs(title = "N-back",
       x = "Predicted delta PAC",
       y = "Pupil size" ) +
  theme(legend.position = "top")

plot_model(model2, type = "int", show.data = FALSE) +  # Effect plot for fixed effects
  theme_minimal(base_size = 14) +
  labs(title = "AX-CPT",
       x = "Predicted delta PAC" ,
       y = "Pupil size") +
  theme(legend.position = "top")


# ms ~ pac
model1 <-lmer( microsacc_stim ~  pac3*strategy + n_back.x + block + (1|position) + (1|sub), subset(dfplot_clean,task.x=='n-back'))
summary(model1)

model2 <-lmer( microsacc_stim_norm ~  pac3_low*strategy + n_back.x + block + (1|position)  + (1|sub), subset(dfplot_clean,task.x=='ax-cpt'))
summary(model2)