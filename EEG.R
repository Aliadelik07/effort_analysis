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

sub_list <- read_csv("/Volumes/x9/INITIAL_DATABASE/check_list_to_process.csv")

# Loop through the data
for (isub in 1:nrow(sub_list)) {
  
  #-------------------------- setting up the profile --------------------------
  sub <- sub_list$sub[isub]
  strategy <- sub_list$strategy[isub]
  
  # Attempt to change the working directory
  setwd_result <- try(setwd(paste0("/Volumes/x9/ADOEFFORT_protocol_local_20250120/data/",sub)), silent = TRUE)
  
  # Check if the directory change was successful
  if (inherits(setwd_result, "try-error")) { message(sub,"_",strategy,". Skipping this iteration.")
    next
  }

  
  bdf <- read_csv(paste0(indir,sub,"/",sub,"-",strategy,"-test.csv"))
  
  #-------------------------- finding channel indices --------------------------
  if(TRUE){
  full_path_ch <- file.path(getwd(), list.files(recursive = TRUE, full.names = TRUE)[grepl("channel\\.mat$", list.files(recursive = TRUE))])
  mat_data_ch <- readMat(full_path_ch[1])
  channel <- mat_data_ch[["Channel"]][1,1,]
  
  channel <- as.data.frame(lapply(channel, unlist))
  channel <- t(channel)
  rownames(channel) <- NULL
  
  
  ch_names_pac_f <- c('F3','F4','Fz') #dlpfc #c('F7','FT7') language
  ch_names_pac_o <- c('O1','O2','Oz')
  ch_names_theta <- ch_names_pac_f
  ch_names_alpha <- c('P3','P4','Pz') 
  ch_names_erp <- c('Pz','Cz','FCz')
  ch_names_gamma <- c('Fz','Pz','CPz')
  
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
  
  ch_idx_pac_f <- c()
  for (i in 1:length(ch_names_pac_f)) {
    # Find the column index where the current row name exists
    currentIndex <- which(channel == ch_names_pac_f[i])
    
    if (length(currentIndex) > 0) {
      # Add current index to the list of column indices
      ch_idx_pac_f <- c(ch_idx_pac_f, currentIndex)
    } else {
      cat(ch_names_pac_f[i], "' not found.\n")
    }
  }
  
  ch_idx_pac_o <- c()
  for (i in 1:length(ch_names_pac_o)) {
    # Find the column index where the current row name exists
    currentIndex <- which(channel == ch_names_pac_o[i])
    
    if (length(currentIndex) > 0) {
      # Add current index to the list of column indices
      ch_idx_pac_o <- c(ch_idx_pac_o, currentIndex)
    } else {
      cat(ch_names_pac_o[i], "' not found.\n")
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
  
  ch_idx_gamma <- c()
  for (i in 1:length(ch_names_gamma)) {
    # Find the column index where the current row name exists
    currentIndex <- which(channel == ch_names_gamma[i])
    
    if (length(currentIndex) > 0) {
      # Add current index to the list of column indices
      ch_idx_gamma <- c(ch_idx_gamma, currentIndex)
    } else {
      cat(ch_names_gamma[i], "' not found.\n")
    }
  }
  
  }
  
  #-------------------------- PSD analysis -------------------------------------
  if(FALSE){
  
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
    
      # Importing the PSD of the block
      trial_number <- sprintf("%03d", ipsd) 
      target_file <- grep(paste0("timefreq_trial", trial_number, "_psd"), basename(files_in_directory), value = TRUE)
      
      if (length(target_file) == 0) {
        cat("not found: ", paste0("stim_block_", ipsd,"_",strategy))
        list_theta[[ipsd]] <- NaN
        list_beta[[ipsd]] <- NaN
        list_alpha[[ipsd]] <- NaN
        list_gamma[[ipsd]] <- NaN
        list_onef[[ipsd]] <- NaN
      } else {
      
      mat_data <- readMat(paste0('start_', strategy, '/', target_file[1]))
      # 4-8 Hz of F3, Fz, F4
      list_theta[[ipsd]] <- mean(mat_data$TF[ch_idx_theta, 1, 5:9], na.rm = TRUE)
      
      # 13-30 Hz of F3, Fz, F4
      list_beta[[ipsd]] <- mean(mat_data$TF[ch_idx_theta, 1, 14:31], na.rm = TRUE)
      
      # 8-12 Hz of P3, Pz, F4
      list_alpha[[ipsd]] <- mean(mat_data$TF[ch_idx_alpha, 1, 9:13], na.rm = TRUE)
      
      # 32-100 Hz of F3, Fz, F4
      list_gamma[[ipsd]] <- mean(mat_data$TF[ch_idx_pac_f, 1, 55:94], na.rm = TRUE)
      
      # Finding closest balance point
      mat_data <- readMat(paste0('start_', strategy, '/', target_file[2]))
      list_onef[[ipsd]] <- find_closest_balance_point(apply(mat_data$TF, c(2, 3), mean))
      }
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
        
        # time [0.28 , 0.4] seconds
        p3_list[[ip3]] <- mat_data[["F"]][ch_idx_erp[2],380:450][which.max(mat_data[["F"]][ch_idx_erp[2],380:450])] #Cz
        
        # 380 is the number of time points before the beginning of the domain
        local_max_idx <- which.max(mat_data[["F"]][ch_idx_erp[2], 380:450]) + 379
        
        # removing max if they are on the borders
        if (local_max_idx == 380 || local_max_idx == 450) {p3_list[[ip3]] <- NaN} # Check if local_max_idx is at the boundary
        
        # exerting mean instead of max (comment out if you want the peak)
        #p3_list[[ip3]] <- mean(mat_data[["F"]][ch_idx_erp[2],380:450][which.max(mat_data[["F"]][ch_idx_erp[2],380:450])]) #Cz
        
      }
    }
    
    
    
    df_p3 <- as.data.frame(lapply(t(p3_list),unlist))
    df_p3 <- t(df_p3)
    row.names(df_p3) <- NULL
    bdf$p3a <- df_p3
    
  }
  
  #-------------------------- P3b analysis -------------------------------------
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
        
        # time [0.28 , 0.4] seconds
        p3_list[[ip3]] <- mat_data[["F"]][ch_idx_erp[1],560:620][which.max(mat_data[["F"]][ch_idx_erp[1],560:620])] #Cz
        
        # 399 is the number of time points before the beginning of the domain
        local_max_idx <- which.max(mat_data[["F"]][ch_idx_erp[1], 560:620]) + 559
        
        # removing max if they are on the borders
        if (local_max_idx == 560 || local_max_idx == 620) {p3_list[[ip3]] <- NaN} # Check if local_max_idx is at the boundary
        
        # exerting mean instead of max (comment out if you want the peak)
        #p3_list[[ip3]] <- mean(mat_data[["F"]][ch_idx_erp[1],560:620][which.max(mat_data[["F"]][ch_idx_erp[1],380:450])]) #Cz
        
      }
    }
    
    
    
    df_p3 <- as.data.frame(lapply(t(p3_list),unlist))
    df_p3 <- t(df_p3)
    row.names(df_p3) <- NULL
    bdf$p3b <- df_p3
    
  }
  
  #-------------------------- Pe response --------------------------------------
  if(TRUE){
    n2_list <- list()
    
    for (in2 in 1:nrow(bdf)) {
      files_in_directory <- list.files(paste0("resp_block_", in2,"_",strategy), full.names = TRUE)
      target_file <- grep(paste0("data_resp_block_", in2,"_",strategy,'_average_'), basename(files_in_directory), value = TRUE)
      if (length(target_file) == 0) {
        cat("not found: ", paste0("resp_block_", in2,"_",strategy))
        n2_list[[in2]] <- NaN
      } else {
        mat_data <- readMat(paste0("resp_block_", in2,"_",strategy, "/", target_file))
        
        # time window of [0.1,0.2] s
        n2_list[[in2]] <- mat_data[["F"]][ch_idx_erp[3],300:360][which.min(mat_data[["F"]][ch_idx_erp[3],300:360])] #FCz
        local_min_idx <- which.min(mat_data[["F"]][ch_idx_erp[3],300:360]) + 299 
        if (local_min_idx == 300 || local_min_idx == 360) {n2_list[[in2]] <- NaN} # Check if local_max_idx is at the boundary
        
        # exerting mean instead of max (comment out if you want the peak)
        n2_list[[in2]] <- mean(mat_data[["F"]][ch_idx_erp[3],300:360])
        
      }
    }
    
    
    
    df_n2 <- as.data.frame(lapply(t(n2_list),unlist))
    df_n2 <- t(df_n2)
    row.names(df_n2) <- NULL
    bdf$pe <- df_n2
  }
  
  #-------------------------- ern response --------------------------------------
  if(TRUE){
    n2_list <- list()
    
    for (in2 in 1:nrow(bdf)) {
      files_in_directory <- list.files(paste0("resp_block_", in2,"_",strategy), full.names = TRUE)
      target_file <- grep(paste0("data_resp_block_", in2,"_",strategy,'_average_'), basename(files_in_directory), value = TRUE)
      if (length(target_file) == 0) {
        cat("not found: ", paste0("resp_block_", in2,"_",strategy))
        n2_list[[in2]] <- NaN
      } else {
        mat_data <- readMat(paste0("resp_block_", in2,"_",strategy, "/", target_file))
        
        # time window of [-0.05,0.05]
        n2_list[[in2]] <- mat_data[["F"]][ch_idx_erp[3],231:283][which.min(mat_data[["F"]][ch_idx_erp[3],231:283])] #FCz
        local_min_idx <- which.min(mat_data[["F"]][ch_idx_erp[3],231:283]) + 230
        if (local_min_idx == 231 || local_min_idx == 283) {n2_list[[in2]] <- NaN} # Check if local_max_idx is at the boundary
      
        # exerting mean instead of max (comment out if you want the peak)
        n2_list[[in2]] <- mean(mat_data[["F"]][ch_idx_erp[3],231:283])
        }
    }
    
    
    
    df_n2 <- as.data.frame(lapply(t(n2_list),unlist))
    df_n2 <- t(df_n2)
    row.names(df_n2) <- NULL
    bdf$ern <- df_n2
  }
  
  #-------------------------- N2 stimuli ---------------------------------------
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
        
        # time window of [0.2,0.25]
        n2_list[[in2]] <- mat_data[["F"]][ch_idx_erp[3],365:375][which.min(mat_data[["F"]][ch_idx_erp[3],365:375])] #FCz
        local_min_idx <- which.min(mat_data[["F"]][ch_idx_erp[3],365:375]) + 364 
        if (local_min_idx == 365 || local_min_idx == 375) {n2_list[[in2]] <- NaN} # Check if local_max_idx is at the boundary
      
        n2_list[[in2]] <- mean(mat_data[["F"]][ch_idx_erp[3],365:375])
        }
    }
    
    
    
    df_n2 <- as.data.frame(lapply(t(n2_list),unlist))
    df_n2 <- t(df_n2)
    row.names(df_n2) <- NULL
    bdf$n2s <- df_n2
  }
  
  #-------------------------- FRN analysis ------------------------------------
  if(TRUE){
    p2_list <- list()
    
    for (ip2 in 1:nrow(bdf)) {
      files_in_directory <- list.files(paste0("fdbk_block_", ip2,"_",strategy), full.names = TRUE)
      target_file <- grep(paste0("data_fdbk_block_", ip2,"_",strategy), basename(files_in_directory), value = TRUE)
      if (length(target_file) == 0) {
        cat("not found: ", paste0("fdbk_block_", ip2,"_",strategy))
        p2_list[[ip2]] <- NaN
      } else {
        mat_data <- readMat(paste0("fdbk_block_", ip2,"_",strategy, "/", target_file)) # (0.3 - 0.4) = 413:460. (0.2 - 0.4) = 360:460
        p2_list[[ip2]] <- mat_data[["F"]][ch_idx_erp[3],360:460][which.max(mat_data[["F"]][ch_idx_erp[3],360:460])] #FCz 
        #local_max_idx <- which.max(mat_data[["F"]][ch_idx_erp[3],360:460]) + 412
        #if (local_max_idx == 413 || local_max_idx == 465) {p2_list[[ip2]] <- NaN} # Check if local_max_idx is at the boundary
        
        # mean, in case you don't want the mean comment out
        p2_list[[ip2]] <- mean(mat_data[["F"]][ch_idx_erp[3],360:460])
      }
    }
    
    
    
    df_p2 <- as.data.frame(lapply(t(p2_list),unlist))
    df_p2 <- t(df_p2)
    row.names(df_p2) <- NULL
    bdf$p2 <- df_p2
  }
  
  #-------------------------- pac_f ---------------------------------------------
  if(FALSE){
  #importing all PACs
  pac1_list <- list()
  pac1_list_low <- list()
  pac1_list_high <- list()
  
  pac2_list <- list()
  pac2_list_low <- list()
  pac2_list_high <- list()
  
  pac3_list <- list()
  pac3_list_low <- list()
  pac3_list_high <- list()
  
  for (ipac in 1:nrow(bdf)) {
      files_in_directory <- list.files(paste0("stim_block_", ipac,"_",strategy), full.names = TRUE)
      target_file <- grep("^timefreq_pac_", basename(files_in_directory), value = TRUE)
      if (length(target_file) != 3) {
        cat("not found: ", paste0("stim_block_", ipac,"_",strategy))
        pac1_list[[ipac]] <- NaN
        pac1_list_low[[ipac]] <- NaN
        pac1_list_high[[ipac]] <- NaN
        pac2_list[[ipac]] <- NaN
        pac2_list_low[[ipac]] <- NaN
        pac2_list_high[[ipac]] <- NaN
        pac3_list[[ipac]] <- NaN
        pac3_list_low[[ipac]] <- NaN
        pac3_list_high[[ipac]] <- NaN
      } else {
      mat_data <- readMat(paste0("stim_block_", ipac,"_",strategy, "/", target_file[1]))
      pac1_list[[ipac]] <- mean(mat_data[["TF"]][ch_idx_pac_f,])
      pac1_list_low[[ipac]] <- mean(mat_data[["sPAC"]][[2]][ch_idx_pac_f,])
      pac1_list_high[[ipac]] <- mean(mat_data[["sPAC"]][[3]][ch_idx_pac_f,])
      
      mat_data <- readMat(paste0("stim_block_", ipac,"_",strategy, "/", target_file[2]))
      pac2_list[[ipac]] <- mean(mat_data[["TF"]][ch_idx_pac_f,])
      pac2_list_low[[ipac]] <- mean(mat_data[["sPAC"]][[2]][ch_idx_pac_f,])
      pac2_list_high[[ipac]] <- mean(mat_data[["sPAC"]][[3]][ch_idx_pac_f,])
      
      mat_data <- readMat(paste0("stim_block_", ipac,"_",strategy, "/", target_file[3]))
      pac3_list[[ipac]] <- mean(mat_data[["TF"]][ch_idx_pac_f,])
      pac3_list_low[[ipac]] <- mean(mat_data[["sPAC"]][[2]][ch_idx_pac_f,])
      pac3_list_high[[ipac]] <- mean(mat_data[["sPAC"]][[3]][ch_idx_pac_f,])
      }
  }
  
  
  df_pac1 <- as.data.frame(lapply(t(pac1_list),unlist))
  df_pac1 <- t(df_pac1)
  row.names(df_pac1) <- NULL
  bdf$pac1_f <- df_pac1
  
  df_pac1_low <- as.data.frame(lapply(t(pac1_list_low),unlist))
  df_pac1_low <- t(df_pac1_low)
  row.names(df_pac1_low) <- NULL
  bdf$pac1_low_f <- df_pac1_low
  
  df_pac1_high <- as.data.frame(lapply(t(pac1_list_high),unlist))
  df_pac1_high <- t(df_pac1_high)
  row.names(df_pac1_high) <- NULL
  bdf$pac1_high_f <- df_pac1_high
  
  df_pac2 <- as.data.frame(lapply(t(pac2_list),unlist))
  df_pac2 <- t(df_pac2)
  row.names(df_pac2) <- NULL
  bdf$pac2_f <- df_pac2
  
  df_pac2_low <- as.data.frame(lapply(t(pac2_list_low),unlist))
  df_pac2_low <- t(df_pac2_low)
  row.names(df_pac2_low) <- NULL
  bdf$pac2_low_f <- df_pac2_low
  
  df_pac2_high <- as.data.frame(lapply(t(pac2_list_high),unlist))
  df_pac2_high <- t(df_pac2_high)
  row.names(df_pac2_high) <- NULL
  bdf$pac2_high_f <- df_pac2_high
  
  # pac 3
  df_pac3 <- as.data.frame(lapply(t(pac3_list),unlist))
  df_pac3 <- t(df_pac3)
  row.names(df_pac3) <- NULL
  bdf$pac3_f <- df_pac3
  
  df_pac3_low <- as.data.frame(lapply(t(pac3_list_low),unlist))
  df_pac3_low <- t(df_pac3_low)
  row.names(df_pac3_low) <- NULL
  bdf$pac3_low_f <- df_pac3_low
  
  df_pac3_high <- as.data.frame(lapply(t(pac3_list_high),unlist))
  df_pac3_high <- t(df_pac3_high)
  row.names(df_pac3_high) <- NULL
  bdf$pac3_high_f <- df_pac3_high
  }
  
  #-------------------------- pac_o ---------------------------------------------
  if(FALSE){
    #importing all PACs
    pac1_list <- list()
    pac1_list_low <- list()
    pac1_list_high <- list()
    
    pac2_list <- list()
    pac2_list_low <- list()
    pac2_list_high <- list()
    
    pac3_list <- list()
    pac3_list_low <- list()
    pac3_list_high <- list()
    
    for (ipac in 1:nrow(bdf)) {
      files_in_directory <- list.files(paste0("stim_block_", ipac,"_",strategy), full.names = TRUE)
      target_file <- grep("^timefreq_pac_", basename(files_in_directory), value = TRUE)
      if (length(target_file) != 3) {
        cat("not found: ", paste0("stim_block_", ipac,"_",strategy))
        pac1_list[[ipac]] <- NaN
        pac1_list_low[[ipac]] <- NaN
        pac1_list_high[[ipac]] <- NaN
        pac2_list[[ipac]] <- NaN
        pac2_list_low[[ipac]] <- NaN
        pac2_list_high[[ipac]] <- NaN
        pac3_list[[ipac]] <- NaN
        pac3_list_low[[ipac]] <- NaN
        pac3_list_high[[ipac]] <- NaN
      } else {
        mat_data <- readMat(paste0("stim_block_", ipac,"_",strategy, "/", target_file[1]))
        pac1_list[[ipac]] <- mean(mat_data[["TF"]][ch_idx_pac_o,])
        pac1_list_low[[ipac]] <- mean(mat_data[["sPAC"]][[2]][ch_idx_pac_o,])
        pac1_list_high[[ipac]] <- mean(mat_data[["sPAC"]][[3]][ch_idx_pac_o,])
        
        mat_data <- readMat(paste0("stim_block_", ipac,"_",strategy, "/", target_file[2]))
        pac2_list[[ipac]] <- mean(mat_data[["TF"]][ch_idx_pac_o,])
        pac2_list_low[[ipac]] <- mean(mat_data[["sPAC"]][[2]][ch_idx_pac_o,])
        pac2_list_high[[ipac]] <- mean(mat_data[["sPAC"]][[3]][ch_idx_pac_o,])
        
        mat_data <- readMat(paste0("stim_block_", ipac,"_",strategy, "/", target_file[3]))
        pac3_list[[ipac]] <- mean(mat_data[["TF"]][ch_idx_pac_o,])
        pac3_list_low[[ipac]] <- mean(mat_data[["sPAC"]][[2]][ch_idx_pac_o,])
        pac3_list_high[[ipac]] <- mean(mat_data[["sPAC"]][[3]][ch_idx_pac_o,])
      }
    }
    
    
    df_pac1 <- as.data.frame(lapply(t(pac1_list),unlist))
    df_pac1 <- t(df_pac1)
    row.names(df_pac1) <- NULL
    bdf$pac1_o <- df_pac1
    
    df_pac1_low <- as.data.frame(lapply(t(pac1_list_low),unlist))
    df_pac1_low <- t(df_pac1_low)
    row.names(df_pac1_low) <- NULL
    bdf$pac1_low_o <- df_pac1_low
    
    df_pac1_high <- as.data.frame(lapply(t(pac1_list_high),unlist))
    df_pac1_high <- t(df_pac1_high)
    row.names(df_pac1_high) <- NULL
    bdf$pac1_high_o <- df_pac1_high
    
    df_pac2 <- as.data.frame(lapply(t(pac2_list),unlist))
    df_pac2 <- t(df_pac2)
    row.names(df_pac2) <- NULL
    bdf$pac2_o <- df_pac2
    
    df_pac2_low <- as.data.frame(lapply(t(pac2_list_low),unlist))
    df_pac2_low <- t(df_pac2_low)
    row.names(df_pac2_low) <- NULL
    bdf$pac2_low_o <- df_pac2_low
    
    df_pac2_high <- as.data.frame(lapply(t(pac2_list_high),unlist))
    df_pac2_high <- t(df_pac2_high)
    row.names(df_pac2_high) <- NULL
    bdf$pac2_high_o <- df_pac2_high
    
    # pac 3
    df_pac3 <- as.data.frame(lapply(t(pac3_list),unlist))
    df_pac3 <- t(df_pac3)
    row.names(df_pac3) <- NULL
    bdf$pac3_o <- df_pac3
    
    df_pac3_low <- as.data.frame(lapply(t(pac3_list_low),unlist))
    df_pac3_low <- t(df_pac3_low)
    row.names(df_pac3_low) <- NULL
    bdf$pac3_low_o <- df_pac3_low
    
    df_pac3_high <- as.data.frame(lapply(t(pac3_list_high),unlist))
    df_pac3_high <- t(df_pac3_high)
    row.names(df_pac3_high) <- NULL
    bdf$pac3_high_o <- df_pac3_high
  }
  
  
  #-------------------------- export and save ----------------------------------
  write.csv(bdf, paste0(indir,sub,"/",sub,"-",strategy,"-test.csv"))
}

