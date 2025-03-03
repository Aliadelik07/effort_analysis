library(readr)
library("readxl")
library(ggrepel)
require(ggplot2)
library(broom)
library(investr)
library(dplyr)
library(tidyr)
library(pracma)
library(lme4)
library(R.matlab)
library(gridExtra)
library(eegkit)

indir <- "/Volumes/x9/INITIAL_DATABASE/"

sub_list <- read_csv("/Users/ali/Desktop/Experiment/check_list.csv")
rows_to_remove <- c(1, 2, 3, 4, 5, 6, 10, 19, 20, 32, 33, 34, 45, 46, 53, 54) #bad files
sub_list <- sub_list[-rows_to_remove, ] # recordings to remove
sub_list <- sub_list[!is.na(sub_list$sub), ] # removing the NaN


# Loop through the data
for (isub in 1:nrow(sub_list)) {
  
  sub <- sub_list$sub[isub]
  strategy <- sub_list$strategy[isub]
  
  # Attempt to change the working directory
  setwd_result <- try(setwd(paste0("/Volumes/x9/brainstorm_db/ADOEFFORT_protocol_local_asd/data/",sub)), silent = TRUE)
  
  # Check if the directory change was successful
  if (inherits(setwd_result, "try-error")) { message(sub,"_",strategy,". Skipping this iteration.")
    next
  }

  
  bdf <- read_csv(paste0(indir,sub,"/",sub,"-",strategy,"-test.csv"))
  
  #-------------------------- finding ch indices -------------------------------------
  full_path_ch <- file.path(getwd(), list.files(recursive = TRUE, full.names = TRUE)[grepl("channel\\.mat$", list.files(recursive = TRUE))])
  mat_data_ch <- readMat(full_path_ch[1])
  channel <- mat_data_ch[["Channel"]][1,1,]
  
  channel <- as.data.frame(lapply(channel, unlist))
  channel <- t(channel)
  rownames(channel) <- NULL
  
  
  ch_names_pac <- c('F3','F4','Fz') #dlpfc #c('F7','FT7') language
  ch_names_theta <- ch_names_pac
  ch_names_alpha <- c('P3','P4','Pz') 
  ch_names_erp <- c('Cz','FCz')
  
  ch_idx_erp <- c()
  for (i in 1:length(ch_names_erp)) {
    # Find the column index where the current row name exists
    currentIndex <- which(channel == ch_names_erp[i])
    
    if (length(currentIndex) > 0) {
      # Add current index to the list of column indices
      ch_idx_erp <- c(ch_idx_erp, currentIndex)
    } else {
      cat(ch_names_erp[i], "' not found.\n")
    }
  }
  
  ch_idx_pac <- c()
  for (i in 1:length(ch_names_pac)) {
    # Find the column index where the current row name exists
    currentIndex <- which(channel == ch_names_pac[i])
    
    if (length(currentIndex) > 0) {
      # Add current index to the list of column indices
      ch_idx_pac <- c(ch_idx_pac, currentIndex)
    } else {
      cat(ch_names_pac[i], "' not found.\n")
    }
  }
  
  ch_idx_theta <- c()
  for (i in 1:length(ch_names_theta)) {
    # Find the column index where the current row name exists
    currentIndex <- which(channel == ch_names_theta[i])
    
    if (length(currentIndex) > 0) {
      # Add current index to the list of column indices
      ch_idx_theta <- c(ch_idx_theta, currentIndex)
    } else {
      cat(ch_names_theta[i], "' not found.\n")
    }
  }
  
  ch_idx_alpha <- c()
  for (i in 1:length(ch_names_alpha)) {
    # Find the column index where the current row name exists
    currentIndex <- which(channel == ch_names_alpha[i])
    
    if (length(currentIndex) > 0) {
      # Add current index to the list of column indices
      ch_idx_alpha <- c(ch_idx_alpha, currentIndex)
    } else {
      cat(ch_names_alpha[i], "' not found.\n")
    }
  }
  
  if(FALSE){
  #-------------------------- PSD analysis -------------------------------------
  # funciton for indexing the 1/f
  find_closest_balance_point <- function(TF) {
    # Compute the cumulative sum of the vector
    cum_sum <- cumsum(TF)
    total_sum <- sum(TF)
    
    # Initialize variables to track the closest balance point
    closest_x <- NA
    closest_diff <- Inf
    
    # Find the closest balance point
    for (x in 1:length(TF)) {
      diff <- abs(cum_sum[x] - (total_sum - cum_sum[x]))
      if (diff < closest_diff) {
        closest_diff <- diff
        closest_x <- x
      }
    }
    
    return(closest_x)
  }
  
  # setting the directory
  files_in_directory <- list.files(paste0("./start_",strategy))
  
  list_theta <- list()
  list_gamma <- list()
  list_beta <- list()
  list_alpha <- list()
  list_beta <- list()
  list_onef <- list()
  
  for (ipsd in 1:nrow(bdf)) {
    tryCatch({
      # importing the psd of the block
      trial_number <- sprintf("%03d", ipsd) 
      target_file <- grep(paste0("timefreq_trial", trial_number, "_psd"), basename(files_in_directory), value = TRUE)
      mat_data <- readMat(paste0('start_',strategy,'/', target_file[2]))
      # 5-8 htz of F3,Fz,F4
      list_theta[[ipsd]] <- mean(mat_data$TF[ch_idx_theta, 1, 6:9], na.rm = TRUE)
      # 13-30 htz of F3,Fz,F4
      list_beta[[ipsd]] <- mean(mat_data$TF[ch_idx_theta, 1, 14:31], na.rm = TRUE)
      # 9-13 htz of P1,Pz,F2
      list_alpha[[ipsd]] <- mean(mat_data$TF[ch_idx_alpha, 1, 10:14], na.rm = TRUE)
      # 32-100
      list_gamma[[ipsd]] <- mean(mat_data$TF[ch_idx_pac, 1, 32:94], na.rm = TRUE)
      mat_data <- readMat(paste0('start_',strategy,'/', target_file[1]))
      list_onef[[ipsd]] <- find_closest_balance_point(apply(mat_data$TF, c(2, 3), mean))
    })
  }
  
  
  bdf$theta <- list_theta
  bdf$theta <- as.numeric(as.character(bdf$theta))
  bdf$beta <- list_beta
  bdf$beta <- as.numeric(as.character(bdf$beta))
  bdf$alpha <- list_alpha
  bdf$alpha <- as.numeric(as.character(bdf$alpha))
  bdf$gamma <- list_gamma
  bdf$gamma <- as.numeric(as.character(bdf$gamma))
  bdf$onef <- list_onef
  bdf$onef <- as.numeric(as.character(bdf$onef))
  }
  #-------------------------- P3a analysis -------------------------------------
  if(FALSE){
    p3_list <- list()
    
    for (ip3 in 1:nrow(bdf)) {
      files_in_directory <- list.files(paste0("stim_block_", ip3,"_",strategy), full.names = TRUE)
      target_file <- grep(paste0("data_stim_block_", ip3,"_",strategy,'_average_'), basename(files_in_directory), value = TRUE)
      if (length(target_file) == 0) {
        cat("not found: ", paste0("stim_block_", ip3,"_",strategy))
        p3_list[[ip3]] <- NaN
      } else {
        mat_data <- readMat(paste0("stim_block_", ip3,"_",strategy, "/", target_file))
        p3_list[[ip3]] <- mat_data[["F"]][ch_idx_erp[1],370:450][which.max(mat_data[["F"]][ch_idx_erp[1],370:450])] #Cz
        local_max_idx <- which.max(mat_data[["F"]][ch_idx_erp[1], 370:450]) + 369 
        if (local_max_idx == 370 || local_max_idx == 450) {p3_list[[ip3]] <- NaN} # Check if local_max_idx is at the boundary
      }
    }
    
    
    
    df_p3 <- as.data.frame(lapply(t(p3_list),unlist))
    df_p3 <- t(df_p3)
    row.names(df_p3) <- NULL
    bdf$p3 <- df_p3
    
  }
  #-------------------------- N2 response -------------------------------------
  if(FALSE){
    n2_list <- list()
    
    for (in2 in 1:nrow(bdf)) {
      files_in_directory <- list.files(paste0("resp_block_", in2,"_",strategy), full.names = TRUE)
      target_file <- grep(paste0("data_resp_block_", in2,"_",strategy,'_average_'), basename(files_in_directory), value = TRUE)
      if (length(target_file) == 0) {
        cat("not found: ", paste0("resp_block_", in2,"_",strategy))
        n2_list[[in2]] <- NaN
      } else {
        mat_data <- readMat(paste0("resp_block_", in2,"_",strategy, "/", target_file))
        n2_list[[in2]] <- mat_data[["F"]][ch_idx_erp[2],330:390][which.min(mat_data[["F"]][ch_idx_erp[2],330:390])] #FCz (0.14 - 0.25)
        local_min_idx <- which.min(mat_data[["F"]][ch_idx_erp[2],330:390]) + 329 
        if (local_min_idx == 330 || local_min_idx == 390) {n2_list[[in2]] <- NaN} # Check if local_max_idx is at the boundary
      }
    }
    
    
    
    df_n2 <- as.data.frame(lapply(t(n2_list),unlist))
    df_n2 <- t(df_n2)
    row.names(df_n2) <- NULL
    bdf$n2r <- df_n2
  }
  #-------------------------- N2 stimuli -------------------------------------
  if(FALSE){
    n2_list <- list()
    
    for (in2 in 1:nrow(bdf)) {
      files_in_directory <- list.files(paste0("stim_block_", in2,"_",strategy), full.names = TRUE)
      target_file <- grep(paste0("data_stim_block_", in2,"_",strategy,'_average_'), basename(files_in_directory), value = TRUE)
      if (length(target_file) == 0) {
        cat("not found: ", paste0("stim_block_", in2,"_",strategy))
        n2_list[[in2]] <- NaN
      } else {
        mat_data <- readMat(paste0("stim_block_", in2,"_",strategy, "/", target_file))
        n2_list[[in2]] <- mat_data[["F"]][ch_idx_erp[2],330:390][which.min(mat_data[["F"]][ch_idx_erp[2],330:390])] #FCz (0.14 - 0.25)
        local_min_idx <- which.min(mat_data[["F"]][ch_idx_erp[2],330:390]) + 329 
        if (local_min_idx == 330 || local_min_idx == 390) {n2_list[[in2]] <- NaN} # Check if local_max_idx is at the boundary
      }
    }
    
    
    
    df_n2 <- as.data.frame(lapply(t(n2_list),unlist))
    df_n2 <- t(df_n2)
    row.names(df_n2) <- NULL
    bdf$n2s <- df_n2
  }
  #-------------------------- RewP analysis -------------------------------------
  if(TRUE){
    p2_list <- list()
    
    for (ip2 in 1:nrow(bdf)) {
      files_in_directory <- list.files(paste0("fdbk_block_", ip2,"_",strategy), full.names = TRUE)
      target_file <- grep(paste0("data_fdbk_block_", ip2,"_",strategy), basename(files_in_directory), value = TRUE)
      if (length(target_file) == 0) {
        cat("not found: ", paste0("fdbk_block_", ip2,"_",strategy))
        p2_list[[ip2]] <- NaN
      } else {
        mat_data <- readMat(paste0("fdbk_block_", ip2,"_",strategy, "/", target_file))
        p2_list[[ip2]] <- mat_data[["F"]][ch_idx_erp[2],360:480][which.max(mat_data[["F"]][ch_idx_erp[2],360:480])] #FCz (0.25 - 0.35)
        local_max_idx <- which.max(mat_data[["F"]][ch_idx_erp[2],390:450]) + 329 
        if (local_max_idx == 360 || local_max_idx == 480) {p2_list[[ip2]] <- NaN} # Check if local_max_idx is at the boundary
      }
    }
    
    
    
    df_p2 <- as.data.frame(lapply(t(p2_list),unlist))
    df_p2 <- t(df_p2)
    row.names(df_p2) <- NULL
    bdf$p2 <- df_p2
  }
  #-------------------------- pac -------------------------------------
  if(FALSE){
  #importing all PACs
  pac1_list <- list()
  pac1_list_low <- list()
  pac1_list_high <- list()
  pac2_list <- list()
  pac2_list_low <- list()
  pac2_list_high <- list()
  
  for (ipac in 1:nrow(bdf)) {
      files_in_directory <- list.files(paste0("stim_block_", ipac,"_",strategy), full.names = TRUE)
      target_file <- grep("^timefreq_pac_", basename(files_in_directory), value = TRUE)
      if (length(target_file) == 0) {
        cat("not found: ", paste0("stim_block_", ipac,"_",strategy))
        pac1_list[[ipac]] <- NaN
        pac1_list_low[[ipac]] <- NaN
        pac1_list_high[[ipac]] <- NaN
        pac2_list[[ipac]] <- NaN
        pac2_list_low[[ipac]] <- NaN
        pac2_list_high[[ipac]] <- NaN
      } else {
      mat_data <- readMat(paste0("stim_block_", ipac,"_",strategy, "/", target_file[1]))
      pac1_list[[ipac]] <- mean(mat_data[["TF"]][ch_idx_pac,])
      pac1_list_low[[ipac]] <- mean(mat_data[["sPAC"]][[2]][ch_idx_pac,])
      pac1_list_high[[ipac]] <- mean(mat_data[["sPAC"]][[3]][ch_idx_pac,])
      mat_data <- readMat(paste0("stim_block_", ipac,"_",strategy, "/", target_file[2]))
      pac2_list[[ipac]] <- mean(mat_data[["TF"]][ch_idx_pac,])
      pac2_list_low[[ipac]] <- mean(mat_data[["sPAC"]][[2]][ch_idx_pac,])
      pac2_list_high[[ipac]] <- mean(mat_data[["sPAC"]][[3]][ch_idx_pac,])
      }
  }
  
  
  df_pac1 <- as.data.frame(lapply(t(pac1_list),unlist))
  df_pac1 <- t(df_pac1)
  row.names(df_pac1) <- NULL
  bdf$pac1 <- df_pac1
  
  df_pac1_low <- as.data.frame(lapply(t(pac1_list_low),unlist))
  df_pac1_low <- t(df_pac1_low)
  row.names(df_pac1_low) <- NULL
  bdf$pac1_low <- df_pac1_low
  
  df_pac1_high <- as.data.frame(lapply(t(pac1_list_high),unlist))
  df_pac1_high <- t(df_pac1_high)
  row.names(df_pac1_high) <- NULL
  bdf$pac1_high <- df_pac1_high
  
  df_pac2 <- as.data.frame(lapply(t(pac2_list),unlist))
  df_pac2 <- t(df_pac2)
  row.names(df_pac2) <- NULL
  bdf$pac2 <- df_pac2
  
  df_pac2_low <- as.data.frame(lapply(t(pac2_list_low),unlist))
  df_pac2_low <- t(df_pac2_low)
  row.names(df_pac2_low) <- NULL
  bdf$pac2_low <- df_pac2_low
  
  df_pac2_high <- as.data.frame(lapply(t(pac2_list_high),unlist))
  df_pac2_high <- t(df_pac2_high)
  row.names(df_pac2_high) <- NULL
  bdf$pac2_high <- df_pac2_high
  }

  if(FALSE){
  # saving sd and mean of pac
  bdf$pac_std <- sapply(pac_list, sd)
  bdf$pac_mean <- sapply(pac_list, mean)
  
  
  # searching for the PAC of channels that are corrolated with midfrontal theta #pac_list[[block]][ch]
  significant_channels <- c()
  significant_channel_data <- data.frame()
  for (ch in 1:size(pac_list[[1]])[1]){
    
    # PAC of a channel for every block
    ch_list <- list()
    for (block in 1:nrow(bdf)){ch_list[[block]] <- pac_list[[block]][ch]}
    
    # regressing against that
    data <- data.frame(ch_list = unlist(ch_list), theta = bdf$theta)
    model <- lm(ch_list ~ theta, data = data)
    p_value <- summary(model)$coefficients["theta", "Pr(>|t|)"]
    
    print(p_value)
    # Check if p-value is less than 0.05
    if (p_value <= 0.1) {
      bdf[paste0("pac_", channel[ch])] <- data$ch_list
      significant_channels <- c(significant_channels, ch)
    }
  }
  
  channel[significant_channels]
  }
  #-------------------------- export and save -------------------------------------
  write.csv(bdf, paste0(indir,sub,"/",sub,"-",strategy,"-test.csv"))
}

