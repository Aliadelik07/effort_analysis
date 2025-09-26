#importing modules =====================
library(readr)
library("readxl")
require(ggplot2)
library(investr)
library(dplyr)
library(lmerTest)




sub_list <- read_csv("/Users/ali/Documents/Experiment/check_list.csv")
rows_to_remove <- c(1, 2, 3, 4, 5, 6, 10, 19, 20, 32, 33, 34, 39, 42, 43, 45, 46, 53, 54) #bad files
sub_list <- sub_list[-rows_to_remove, ] # recordings to remove
sub_list <- sub_list[!is.na(sub_list$sub), ] # removing the NaN

# Function to normalize between 0 and 1
minmax_normalize <- function(x, ref) {
  (x - min(ref, na.rm = TRUE)) / (max(ref, na.rm = TRUE) - min(ref, na.rm = TRUE))
}

all_results <- list()
subject_counter <- 1

sample = 1000
# Loop through the participants ----------
for (isub in 1:nrow(sub_list)) {
  
  sub <- sub_list$sub[isub]
  strategy <- sub_list$strategy[isub]
  
  pupil_path <- paste0("/Volumes/x9/INITIAL_DATABASE/", sub, "/", sub, "-", strategy, "-test-pupil.csv")
  behav_path <- paste0("/Volumes/x9/INITIAL_DATABASE/", sub, "/", sub, "-", strategy, "-test.csv")
  
  if (!file.exists(pupil_path) || !file.exists(behav_path)) {
    cat("Missing file(s) for", sub, strategy, "- Skipping\n")
    next
  }
  
  df <- read.csv(pupil_path, encoding = "UTF-8")
  bf <- read.csv(behav_path, encoding = "UTF-8")
  
  # Rename event codes
  df$text[df$text %in% c("10", "20", "30")] <- "stim"
  df$text[df$text == "2"] <- "rest"
  df$text[df$text == "8"] <- "resp"
  
  # Normalize pupil signal
  df$ps_norm <- minmax_normalize(df$ps, df$ps)
  
  block_indices <- which(df$text == "block")
  
  if (length(block_indices) != length(bf$n_block)) {
    cat("Mismatch in block count for", sub, strategy, "- Skipping\n")
    next
  }
  
  results_list <- list()
  counter <- 1
  
  for (i in seq_along(block_indices)) {
    
    block_index <- block_indices[i]
    upper_limit <- if (i < length(block_indices)) block_indices[i + 1] else length(df$text) + 1
    trial_indices <- which(df$text == "stim" & seq_along(df$text) > block_index & seq_along(df$text) < upper_limit)
    
    if (length(trial_indices) == 0) next
    
    avg_ps <- numeric(length(trial_indices))
    avg_ms <- numeric(length(trial_indices))
    ms_offset <- rep(NA_real_, length(trial_indices))
    
    
    for (j in seq_along(trial_indices)) {
      idx <- trial_indices[j]
      
      if ((idx + sample) <= length(df$ps_norm)) {
        avg_ps[j] <- mean(df$ps_norm[idx:(idx + sample)], na.rm = TRUE)
       
        avg_ms[j] <- sum(df$ms[idx:(idx + sample)], na.rm = TRUE)* 0.5  # to make it Hz
        
        ms_window <- df$ms[idx:(idx + sample)]
        one_indices <- which(ms_window == 1)
        if (length(one_indices) > 0) {
          ms_offset[j] <- (one_indices[1] - 1) * 0.002  # in seconds
        }
      } else {
        avg_ps[j] <- NA
        avg_ms[j] <- NA
        ms_offset[j] <- NA
      }
    }
    
    # trial number
    trial <- seq_along(avg_ps)
    
    block_df <- data.frame(
      sub = sub,
      strategy = strategy,
      n_block = bf$n_block[i],
      trial = trial,
      avg_ps = avg_ps,
      avg_ms = avg_ms,
      ms_offset = ms_offset
    )
    
    results_list[[counter]] <- block_df
    counter <- counter + 1
  }
  
  if (length(results_list) > 0) {
    results_df <- do.call(rbind, results_list)
    all_results[[subject_counter]] <- results_df
    subject_counter <- subject_counter + 1
    
    cat('|',rep("#", isub), rep("-", nrow(sub_list) - isub),'|\n', sep = "")
  }
}

# Combine all participants' data
final_df <- do.call(rbind, all_results)


# Save the result
write.csv(final_df, "/Volumes/x9/INITIAL_DATABASE/dfa-trials-pupil-2s.csv", row.names = FALSE)
