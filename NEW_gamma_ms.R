# modules -------------
library(readr)
library("readxl")
library(ggrepel)
require(ggplot2)
library(broom)
library(investr)
library(dplyr)
library(tidyr)
library(pracma)
library(lmerTest)
library(R.matlab)
library(gridExtra)
library(mediation)
library(vars)
library(lavaan)
library(ppcor)
library(vars)
library(tseries)
library(metafor)
library(meta)

indir <- "/Volumes/x9/INITIAL_DATABASE/"

sub_list <- read_csv("/Volumes/x9/INITIAL_DATABASE/check_list_to_process.csv")

# -------------------------- importing pupil data -----------------------
dfms <- read_csv(paste0('/Volumes/x9/INITIAL_DATABASE/dfplot_stim_lambda_6.csv'))

#downsampling to match the sampling rate of EEG

dfms <- dfms %>%
  mutate(time = floor(position / 5) * 0.01) # Create numeric time bins starting from 0

dfms <- dfms[dfms$time >= 0 & dfms$time <= 1.2, ]

range(dfms$time)

# Summarize data by time_bin
dfms <- dfms %>%
  group_by(sub, strategy, block, time, effort) %>%
  summarise(
    microsacc = sum(microsacc_stim, na.rm = TRUE)
  )

grand_df_list <- list()
# Loop through the data
for (isub in 1:nrow(sub_list)) {
  
  #-------------------------- profile --------------------------
  sub <- sub_list$sub[isub]
  strategy <- sub_list$strategy[isub]
  
  # Attempt to change the working directory
  setwd_result <- try(setwd(paste0("/Volumes/x9/ADOEFFORT_protocol_local_20250120/data/",sub)), silent = TRUE)
  
  # Check if the directory change was successful
  if (inherits(setwd_result, "try-error")) { message(sub,"_",strategy,". Skipping this iteration.")
    next
  }
  
  bdf <- read_csv(paste0(indir,sub,"/",sub,"-",strategy,"-test.csv"))
  #-------------------------- channels --------------------------
    full_path_ch <- file.path(getwd(), list.files(recursive = TRUE, full.names = TRUE)[grepl("channel\\.mat$", list.files(recursive = TRUE))])
    mat_data_ch <- readMat(full_path_ch[1])
    channel <- mat_data_ch[["Channel"]][1,1,]
    
    channel <- as.data.frame(lapply(channel, unlist))
    channel <- t(channel)
    rownames(channel) <- NULL
    
    ch_names <- c('Fz','F3','F4','CPz')

    
    ch_idx <- c()
    for (i in 1:length(ch_names)) {
      # Find the column index where the current row name exists
      currentIndex <- which(channel == ch_names[i])
      
      if (length(currentIndex) > 0) {
        # Add current index to the list of column indices
        ch_idx <- c(ch_idx, currentIndex)
      } else {
        cat(ch_names[i], "' not found.\n")
      }
    }
     
    
    #-------------------------- (time,gamma,ms) --------------------------
    
    sub_df_list <- list()
    
    for (ig in 1:nrow(bdf)) {
      files_in_directory <- list.files(paste0("stim_block_", ig,"_",strategy), full.names = TRUE)
      target_file <- grep(paste0('_zscore.mat'), basename(files_in_directory), value = TRUE)
      if (length(target_file) == 0) {
        cat("not found: ", paste0("stim_block_", ig,"_",strategy))
      } else {
        mat_data <- readMat(paste0("stim_block_", ig,"_",strategy, "/", target_file))
        
        df <- data.frame(
          sub = rep(sub, 616),
          strategy = rep(strategy, 616),
          block = rep(ig, 616)
        )
        
        df$time <- mat_data[["Time"]][257:872]
        df$gamma_frontal <- apply(mat_data[["TF"]][ch_idx[1:3], 257:872, 27:39], 2, mean)
        df$gamma_cpz <- apply(mat_data[["TF"]][ch_idx[4],257:872, 27:39],1,mean)
        
        #downsampling df
        df <- df %>%
          mutate(time = floor(time / 0.01) * 0.01)
        
        unique(df$time)
        
        # limiting to the time window that we have pac1 [0,0.6] and [0.6, 1.2]
        df <- df[df$time >= 0 & df$time <= 1.2, ]
        
        # Summarize data by time_bin
        df <- df %>%
          group_by(sub, strategy, block, time) %>%
          summarise(
            gamma_frontal = sum(gamma_frontal, na.rm = TRUE),
            gamma_cpz = sum(gamma_cpz, na.rm = TRUE)
          )
        
        #combining gamma with microsacc and save
        df <- df %>%
          left_join(dfms, by = c("sub", "strategy","block", "time"))

        sub_df_list[[ig]] <- df
        
      }
    }
    
    sub_df_list <- do.call(rbind, sub_df_list)
    
    grand_df_list[[isub]] <- sub_df_list
}  

grand_df_list <- do.call(rbind, grand_df_list)

grand_df_list$task <- ifelse(grand_df_list$strategy %in% c("rolling", "static"), "n-back", "ax-cpt")

write.csv(grand_df_list, paste0("/Volumes/x9/INITIAL_DATABASE/grand_df_microsacc_gamma.csv"))

#-------------------------- stats --------------------------

data <-read_csv("/Volumes/x9/INITIAL_DATABASE/grand_df_microsacc_gamma.csv")

data <- na.omit(data)



#replicating yuval-greenberg ---------
subset_data <- data %>%filter(time >= 0.15 & time <= 0.35)
model0 <- lmer( gamma_cpz ~ microsacc  + (1|sub) , subset(subset_data,task=='n-back' ))
summary(model0)

model0 <- lmer( gamma_cpz ~ microsacc  + (1|sub) , subset(subset_data,task=='ax-cpt' ))
summary(model0)

# our frontal
model1 <- lmer( gamma_frontal ~ microsacc + (1|time)  + (1|sub) , subset(data,task=='n-back' ))
summary(model1)

model1 <- lmer( gamma_frontal ~ microsacc + (1|time) + (1|sub) , subset(data,task=='ax-cpt' ))
summary(model1)


#granger causing
# Create time-series object
subset_data <- subset(data,task=='n-back' )

# Extract the time series data for microsacc and gamma_frontal
microsacc_ts <- subset_data$microsacc
gamma_frontal_ts <- subset_data$gamma_frontal

# Perform Augmented Dickey-Fuller (ADF) Test
adf_microsacc <- adf.test(microsacc_ts)
adf_gamma_frontal <- adf.test(gamma_frontal_ts)

# Perform KPSS Test
kpss_microsacc <- kpss.test(microsacc_ts)
kpss_gamma_frontal <- kpss.test(gamma_frontal_ts)

# Print results
print(adf_microsacc)
print(adf_gamma_frontal)
print(kpss_microsacc)
print(kpss_gamma_frontal)


ts_data <- subset_data[ c("microsacc", "gamma_cpz")]

# Fit a VAR model with lag 1 (you may need to determine the best lag using AIC)
var_model <- VAR(ts_data, p = 1, type = "const")

# Granger causality test
granger_test <- causality(var_model, cause = "microsacc")
print(granger_test)




# effort mediating microsaccade
mediator_model <- lm(effort ~  microsacc, data = data)
outcome_model <- lm(gamma_frontal ~ effort + microsacc, data = data)
mediator_result <- mediate(mediator_model, outcome_model, treat = "microsacc", mediator = "effort")
summary(mediator_result)

# granger causality  ----------------
data <-read_csv("/Volumes/x9/INITIAL_DATABASE/grand_df_microsacc_gamma.csv")
data <- na.omit(data)
data<- subset(data,task=='n-back')

results <- lapply(unique(data$block), function(b) {
  block_results <- lapply(unique(data$sub[data$block == b]), function(s) {
    sub_block_data <- subset(data, block == b & sub == s)
    
    # Ensure we have enough rows for VAR estimation
    if (nrow(sub_block_data) > 10) {
      # Check for constant variables
      if (var(sub_block_data$microsacc, na.rm = TRUE) == 0 || var(sub_block_data$gamma_cpz, na.rm = TRUE) == 0) {
        return(data.frame(block = b, sub = s, p_value = NA, warning = "Constant variable"))
      }
      
      # Check for perfect correlation
      if (abs(cor(sub_block_data$microsacc, sub_block_data$gamma_cpz, use = "complete.obs")) > 0.99) {
        return(data.frame(block = b, sub = s, p_value = NA, warning = "Perfect correlation"))
      }
      
      # Check for stationarity using ADF test (Augmented Dickey-Fuller Test)
      adf_microsacc <- adf.test(sub_block_data$microsacc, alternative = "stationary")
      adf_gamma <- adf.test(sub_block_data$gamma_cpz, alternative = "stationary")
      
      # If either variable is non-stationary, return a warning
      if (adf_microsacc$p.value > 0.05 || adf_gamma$p.value > 0.05) {
        return(data.frame(block = b, sub = s, p_value = NA, warning = "Non-stationary data"))
      }
      
      # Fit VAR model
      var_model <- try(VAR(sub_block_data[, c("microsacc", "gamma_cpz")], p = 3, type = "const"), silent = TRUE)
      
      # Handle singular matrix errors
      if (inherits(var_model, "try-error")) {
        return(data.frame(block = b, sub = s, p_value = NA, warning = "Singular matrix"))
      }
      
      # Compute Granger causality
      p_value <- try(causality(var_model, cause = "microsacc")$Granger$p.value, silent = TRUE)
      if (inherits(p_value, "try-error")) {
        return(data.frame(block = b, sub = s, p_value = NA, warning = "Granger test failed"))
      }
      
      return(data.frame(block = b, sub = s, p_value = p_value, warning = NA))
    } else {
      return(NULL)  # Skip if not enough data
    }
  })
  
  return(do.call(rbind, block_results))
})

# Convert results to a data frame
results_df <- do.call(rbind, results)

# View problematic cases
subset(results_df, !is.na(warning))


hist(results_df$p_value, 
     main = "Distribution of Granger Causality p-values in N-back task", 
     xlab = "p-value", 
     breaks = 20, 
     cex.main = 1.5,  # Increase title font size
     cex.lab = 1.5,   # Increase axis labels font size
     cex.axis = 1.5)  # Increase axis ticks font size

#final verdict
# Remove NAs from the p-values
valid_p_values <- results_df$p_value[!is.na(results_df$p_value)]

# Convert p-values to z-scores
z_scores <- qnorm(1 - valid_p_values)

# Number of tests
N <- length(z_scores)

# Calculate the combined Z-score
z_combined <- sum(z_scores) / sqrt(N)

# Calculate the p-value for the combined Z-score (two-tailed test)
combined_p_value <- 2 * (1 - pnorm(abs(z_combined)))

# Final verdict
if (combined_p_value < 0.05) {
  cat("Granger causality is significant overall.\n")
} else {
  cat("No significant Granger causality found overall.\n")
}


# Plotting -------------------------
fig <- ggplot(data) +
  geom_smooth(aes(x = time, y = (microsacc-mean(microsacc, na.rm = TRUE))*50, color = "Microsaccades"), method = "loess", span = 0.1, se = FALSE) + 
  geom_smooth(aes(x = time, y = (gamma_cpz-mean(gamma_cpz, na.rm = TRUE)), color = "Gamma CPz"), method = "loess", span = 0.1, se = FALSE) +
  geom_smooth(aes(x = time, y = (gamma_frontal-mean(gamma_frontal, na.rm = TRUE)), color = "Gamma midfrontal"), method = "loess", span = 0.1, se = FALSE) +
  labs(x = "Time", y = "Values", color = "Values") +
  theme(#legend.position = c(0, 0), legend.justification = c(0, 0),
    axis.title = element_text(size = 18),    # Increase axis title font size
    axis.text = element_text(size = 18),      # Increase axis labels font size
    legend.title = element_text(size = 18),   # Increase legend title font size
    legend.text = element_text(size = 18),    # Increase legend text font size
    strip.text = element_text(size = 18)) +
  theme_minimal() + facet_wrap(~ task, scales = "fixed")

print(fig)


ggsave(paste0("/Volumes/x9/results/allsubs/figure/gamma_microsaccade_all.png"), plot = fig, width = 10, height = 6, dpi = 300)

