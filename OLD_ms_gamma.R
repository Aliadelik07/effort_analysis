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


# combining microsacc with gamma timeseries
#importing ms df
dfms <- read_csv(paste0('/Volumes/x9/INITIAL_DATABASE/dfplot_stim_lambda_6.csv'))
dfms$idx <- paste(dfms$block, dfms$strategy, sep = "_")

#downsampling dfms
dfms <- dfms %>%
  mutate(time = floor(position / 5) * 0.01) # Create numeric time bins starting from 0

dfms <- dfms[dfms$time >= 0 & dfms$time <= 1.2, ]

range(dfms$time)

# Summarize data by time_bin
dfms <- dfms %>%
  group_by(sub,idx,time, effort) %>%
  summarise(
    microsacc = sum(microsacc_stim, na.rm = TRUE)
  )

# importing gamma df
#what channel you want?
channel = 'Fz'

path <- paste0("/Volumes/x9/TF_Output_CSV/",channel)
txt_files <- list.files(path, pattern = "\\.txt$", full.names = TRUE)
dfga <- txt_files %>%
  lapply(read.table, header = TRUE, sep = ",") %>%
  bind_rows()

#downsampling dfga
dfga <- dfga %>%
  mutate(time = floor(time / 0.01) * 0.01)

unique(dfga$time)
unique(dfms$time)

# limiting to the time window that we have pac1 [0,0.6] and [0.6, 1.2]
dfga <- dfga[dfga$time >= 0 & dfga$time <= 1.2, ]

range(dfga$time)

# Summarize data by time_bin
dfga <- dfga %>%
  group_by(sub,idx,time) %>%
  summarise(
    power = mean(power, na.rm = TRUE)
  )


#combining gamma with microsacc and save
combined_df <- dfga %>%
  left_join(dfms, by = c("sub", "idx", "time"))

combined_df <- combined_df %>%
  separate(idx, into = c("block", "strategy"), sep = "_")

combined_df$block <- as.integer(combined_df$block)

combined_df[["power"]][abs(scale(combined_df[["power"]])) >= 3] <- NA

write.csv(combined_df, paste0("/Volumes/x9/INITIAL_DATABASE/combined_df_microsacc_gamma_",channel,".csv"))

# ------ add pac ------

combined_df <- combined_df %>%
  left_join(dfa %>% select(sub, strategy, block, pac3), 
            by = c("sub", "strategy", "block"))

# stat
summary(lmer( pac3 ~ microsacc*strategy  + (1|sub), combined_df))

ggplot(combined_df, aes(x = microsacc, y = pac3)) +
  geom_point(aes(color = strategy), alpha = 1)+
  #stat_smooth(aes(group = factor(n_back)), method = "lm", formula = y ~ poly(x, 1), se = FALSE) +
  #geom_line(aes(group = factor(n_back))) +  # Connect points with a line
  stat_smooth(aes(), method = "lm", formula = y ~ poly(x, 1), se = TRUE, col='black') +
  theme_minimal() #+ facet_wrap(~ strategy, scales = "fixed")

#---- reproducing yuval-greenberg 2008 ----
channel = 'CPz'
combined_df <- read_csv(paste0("/Volumes/x9/INITIAL_DATABASE/combined_df_microsacc_gamma_",channel,".csv"))

combined_df$task <- ifelse(combined_df$strategy %in% c("rolling", "static"), "n-back", "ax-cpt")

#combined_df <- subset(combined_df, task =='n-back')


#burst of gamma only in CPz for around 0.2 second
subset_combined_df <- combined_df %>%filter(time >= 0.2 & time <= 0.3)
model0 <- lmer( power ~ microsacc + (1|sub) , subset_combined_df )
summary(model0)

#---- our frontal gamma ~ microsacc ----
channel = 'CPz'
combined_df <- read_csv(paste0("/Volumes/x9/INITIAL_DATABASE/combined_df_microsacc_gamma_",channel,".csv"))

combined_df$task <- ifelse(combined_df$strategy %in% c("rolling", "static"), "n-back", "ax-cpt")

# showing the relation of each variable together
model1 <- lmer( microsacc ~ effort*strategy + (1|sub) , combined_df )
model2 <- lmer( power ~  effort*strategy + (1|sub) , combined_df )
model3 <- lmer( power ~  microsacc*strategy + (1|sub) , combined_df )
model4 <- lmer( power ~  effort*microsacc + (1|sub) , combined_df )
summary(model4)

# triangle corrolation
library(GGally)

ggpairs(combined_df[, c("microsacc", "effort", "power")], 
        upper = list(continuous = wrap("cor", size = 5)),
        diag = list(continuous = wrap("densityDiag")))



library(semPlot)
library(lavaan)

# Define the relationships
model <- "
  microsacc ~ effort
  power ~ effort
  power ~ microsacc
"
fit <- sem(model, data = data_clean)
#summary(fit, standardized = TRUE, fit.measures = TRUE)
semPaths(fit, what = "std", layout = "circle", edge.label.cex = 1, nCharNodes = 2)


# mediating model ---------------
channel = 'Fz'
combined_df <- read_csv(paste0("/Volumes/x9/INITIAL_DATABASE/combined_df_microsacc_gamma_",channel,".csv"))
data_clean <- na.omit(combined_df)
#subset_data_clean <- data_clean %>%filter(time >= 0.15 & time <= 0.35)

# effort mediating microsaccade
mediator_model <- lm(effort ~  microsacc, data = data_clean)
outcome_model <- lm(power ~ effort  + microsacc, data = data_clean)
mediator_result <- mediate(mediator_model, outcome_model, treat = "microsacc", mediator = "effort")
summary(mediator_result)

# microsaccade mediating effort
mediator_model <- lm(microsacc ~  effort, data = data_clean)
outcome_model <- lm(power ~ effort  + microsacc, data = data_clean)
mediator_result <- mediate(mediator_model, outcome_model, treat = "effort" , mediator = "microsacc")
summary(mediator_result)


# Plotting -------------------------
fig <- ggplot(combined_df) +
  geom_smooth(aes(x = time, y = (microsacc-mean(microsacc, na.rm = TRUE))*10, color = "Microsaccades"), method = "loess", span = 0.1, se = FALSE) + 
  geom_smooth(aes(x = time, y = (power-mean(power, na.rm = TRUE)), color = "Gamma Power"), method = "loess", span = 0.1, se = FALSE) +
  labs(title = channel,
       x = "Time",
       y = "Value") +
  scale_color_manual(name = "Legend", values = c("Microsaccades" = "blue", "Gamma Power" = "red")) +
  theme_minimal() #+ facet_wrap(~ strategy, scales = "fixed")

print(fig)


ggsave(paste0("/Volumes/x9/results/allsubs/figure/gamma_microsaccade_all_",channel,".png"), plot = fig, width = 10, height = 6, dpi = 300)


# to make sure microsaccade data is correct
median_effort <- median(combined_df$effort, na.rm = TRUE)
combined_df$effort_category <- ifelse(combined_df$effort > median_effort, "2 High effort", "1 Low effort")

fig <- ggplot(combined_df, aes(x = time, y = microsacc, 
                                  group = effort_category,
                                  color = effort_category)) +
  geom_smooth(method = "loess", span = 0.2, se = TRUE) +
  theme_minimal() +
  #scale_color_gradient(low = "blue", high = "red") +
  #scale_x_continuous(breaks = c(0, 500, 1000), labels = c("0", "1", "2")) +
  labs(title = "",
       x = "Seconds",
       y = "Microsacc",
       color = "Effort")

# Display the plot
print(fig)



# Plot microsacc vs. power by effort bin
ggplot(subset_data_clean, aes(x = microsacc, y = power, color = as.factor(effort_bin))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(color = "Effort Level") +
  theme_minimal()

# Plot microsacc vs. power by effort bin
ggplot(subset_data_clean, aes(x = microsacc, y = power)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(color = "Effort Level") +
  theme_minimal()


# Define SEM model
model <- '
  microsacc ~ effort
  power ~ effort + microsacc
'

# Fit the SEM
fit <- sem(model, data = subset_data_clean)

# Summary of results
summary(fit, fit.measures = TRUE, standardized = TRUE)


# granger causality detailed ----------------
# A bivariate Vector Autoregression (VAR) -------------
channel = 'CPz'#'Fz'or 'CPz'

file_path <- paste0("/Volumes/x9/INITIAL_DATABASE/combined_df_microsacc_gamma_",channel,".csv")
data <-read_csv(file_path)
data_clean <- na.omit(data)

kendall_list <- list()

# Loop through the data
sub_list <- read_csv("/Volumes/x9/INITIAL_DATABASE/check_list_to_process.csv")
for (isub in 1:nrow(sub_list)) {

  sub_data <- data_clean %>%
    filter(strategy == unique(sub_list$strategy[isub]), sub == unique(sub_list$sub[isub]))
  
  # Loop over each unique block
  for (i in unique(sub_data$block)) {
  
  data <- sub_data %>% filter(block == i)
  
  #checking for stationarity
  stationary_ms <- adf.test(data$microsacc)
  stationary_gamma <- adf.test(data$power)
  # indicate if both timeseries are stationaries
  
  if (is.na(stationary_ms$p.value)) {next}
  
  if (stationary_ms$p.value < 0.05 && stationary_gamma$p.value < 0.05) {stationary_idx <- 1} else {stationary_idx <- 0}
  
  # Prepare a function for lagging
  lag_variable <- function(x, k) {
    if (k > 0) {
      c(rep(NA, k), x[1:(length(x) - k)])
    } else if (k < 0) {
      c(x[(-k + 1):length(x)], rep(NA, -k))
    } else {
      x
    }
}
  
  # Create a list to store plots
  plot_list <- list()
  
  
  # Initialize an empty data frame to store results
  kendall_result <- data.frame(
    k = integer(), 
    sub = integer(), 
    strategy = character(), 
    block = integer(), 
    stationary = integer(),
    tau = numeric(), 
    p_value = numeric(),
    stringsAsFactors = FALSE
  )
  # Generate scatter plots for lags from -10 to +10
  for (k in -10:10) {
    # Add lagged power column
    data$power_lagged <- lag_variable(data$power, k)
    
    # Remove NA rows caused by lagging
    lagged_data <- na.omit(data)
    
    # Calculate Kendall's test
    kendall_corr <- cor.test(lagged_data$power_lagged, lagged_data$microsacc, method = "kendall")
    
    #extract result
    kendall_result <- rbind(
      kendall_result,
      data.frame(
        k = k,
        sub = unique(sub_data$sub),          
        strategy = unique(sub_data$strategy),
        block = unique(sub_data$block),
        stationary = stationary_idx,
        tau = kendall_corr$estimate,
        p_value = kendall_corr$p.value,
        stringsAsFactors = FALSE
      )
    )
    
    # Generate scatter plot with ggplot2
    p <- ggplot(lagged_data, aes(x = power_lagged, y = microsacc)) +
      geom_point(color = "blue", alpha = 0.7) +
      geom_smooth(method = "lm", color = "red", se = FALSE) +
      labs(
        title = paste("Lag =", k, "\nKendall's τ =", round(kendall_corr$estimate, 3)),
        x = "Lagged Power",
        y = "Microsacc"
      ) +
      theme(
        plot.title = element_text(size = 10),         # Title font size
        axis.title = element_text(size = 8),         # Axis titles font size
        axis.text = element_text(size = 7),          # Axis tick labels font size
        legend.text = element_text(size = 7),        # Legend text font size
        legend.title = element_text(size = 8)        # Legend title font size
      )
    
    # Add to plot list
    plot_list[[k + 11]] <- p  
  }
  
  kendall_list[[isub]] <- kendall_result
  # Arrange plots in a 10x10 grid (or save individually)
  #fig <- grid.arrange(grobs = plot_list, ncol = 5, top = paste0(unique(data$sub),'|',unique(data$strategy),'|',unique(data$block)))
  #ggsave(paste0("/Volumes/x9/results/allsubs/figure/gamma_microsaccade/",unique(data$sub),'_',unique(data$strategy),'_',unique(data$block),".png"), plot = fig, width = 10, height = 6, dpi = 300)
  
  }
}

# exporting
kendall <- bind_rows(kendall_list)
write.csv(kendall, paste0("/Volumes/x9/INITIAL_DATABASE/kendall_",channel,".csv"))

# removing NA
kendall <- na.omit(kendall)

# Basic histogram with some customization
fig <- ggplot(kendall, aes(x = tau)) +
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "black", alpha = 0.7) + 
  labs(title = "Histogram of Kendall's Tau", 
       x = "Kendall's Tau", 
       y = "Frequency") +
  theme_minimal() +
  theme(text = element_text(size = 14),  # Increase text size
        plot.title = element_text(hjust = 0.5, size = 16),  # Center title and change font size
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  theme(plot.margin = margin(1, 1, 1, 1, "cm"))
fig
ggsave(paste0("/Volumes/x9/results/allsubs/figure/kendall_hist_",channel,".png"), plot = fig, width = 10, height = 6, dpi = 300)

# meta analysis of granger ----------------
channel = 'CPz' #'Fz' or 'CPz'

file_path <- paste0("/Volumes/x9/INITIAL_DATABASE/combined_df_microsacc_gamma_",channel,".csv")
main_data <-read_csv(file_path)

data <- main_data
  
data$task <- ifelse(data$strategy %in% c("rolling", "static"), "n-back", "ax-cpt")

data <- subset(data, task =='n-back')

data$microsacc_lag1 <- ave(data$microsacc, 
                            data$block, data$sub, data$strategy, 
                            FUN = function(x) c(NA, head(x, 1)))

data$microsacc_lag2 <- ave(data$microsacc, 
                           data$block, data$sub, data$strategy, 
                           FUN = function(x) c(NA, head(x, 2)))

data$microsacc_lag3 <- ave(data$microsacc, 
                           data$block, data$sub, data$strategy, 
                           FUN = function(x) c(NA, head(x, 3)))

data$microsacc_lag4 <- ave(data$microsacc, 
                           data$block, data$sub, data$strategy, 
                           FUN = function(x) c(NA, head(x, 4)))

data$microsacc_lag5 <- ave(data$microsacc, 
                           data$block, data$sub, data$strategy, 
                           FUN = function(x) c(NA, head(x, 5)))


data$power_lag1 <- ave(data$power, 
                            data$block, data$sub, data$strategy, 
                            FUN = function(x) c(NA, head(x, 1)))

data$power_lag2 <- ave(data$power, 
                       data$block, data$sub, data$strategy, 
                       FUN = function(x) c(NA, head(x, 2)))

data$power_lag3 <- ave(data$power, 
                       data$block, data$sub, data$strategy, 
                       FUN = function(x) c(NA, head(x, 3)))

data$power_lag4 <- ave(data$power, 
                       data$block, data$sub, data$strategy, 
                       FUN = function(x) c(NA, head(x, 4)))

data$power_lag5 <- ave(data$power, 
                       data$block, data$sub, data$strategy, 
                       FUN = function(x) c(NA, head(x, 5)))
  
  
# Remove NA rows caused by lagging
lagged_data <- na.omit(data)

#adf.test(lagged_data$microsacc)
#adf.test(lagged_data$power)
  
# lmer model
model <- lmer(power ~ power_lag1 + microsacc_lag1 +
                power_lag2 + microsacc_lag2 +
                power_lag3 + microsacc_lag3 +
                power_lag4 + microsacc_lag4 +
                power_lag5 + microsacc_lag5 +
                block + strategy + (1|sub), lagged_data)

summary(model)


# altogether granger causality  ----------------
channel = 'Fz' #'Fz' or 'CPz'

file_path <- paste0("/Volumes/x9/INITIAL_DATABASE/combined_df_microsacc_gamma_",channel,".csv")
main_data <-read_csv(file_path)

data <- main_data
data$task <- ifelse(data$strategy %in% c("rolling", "static"), "n-back", "ax-cpt")

#data <- subset(data, task =='n-back')
data  <- na.omit(data)

lag_selection <- VARselect(data, lag.max = 10, type = "const")
optimal_lag <- lag_selection$selection["AIC(n)"]
var_model <- VAR(data, p = optimal_lag, type = "const")
# microsaccade causing gamma
causality(var_model, cause = "microsacc")
# gamma causing microsaccade
causality(var_model, cause = "power")


#burst of gamma only in CPz for around 0.2 second
subset_combined_df <- data %>%filter(time >= 0.15 & time <= 0.35)


model1 <- lmer( microsacc ~ effort* strategy + (1|sub) , data )
model2 <- lmer( power ~  effort* strategy + (1|sub) , data )
model3 <- lmer( power ~  microsacc * strategy + (1|sub) , data )
summary(model3)

ggplot(subset_combined_df, aes(x = effort, y = microsacc, color = strategy)) +
  #geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE) #+facet_wrap(~ strategy)
  

# Calculating Phase Locking Value (PLV) ----------
library(tuneR)
# Hilbert Transform using FFT
hilbert <- function(x) {
  N <- length(x)
  X <- fft(x)  # FFT of signal
  H <- rep(0, N)  # Create a filter
  H[1] <- 1
  if (N %% 2 == 0) {
    H[2:(N/2)] <- 2
  } else {
    H[2:((N+1)/2)] <- 2
  }
  X <- X * H  # Apply filter
  Re(fft(X, inverse = TRUE) / N)  # Inverse FFT
}

data <- main_data
data  <- na.omit(data)

# Hilbert Transform to extract phase
x_phase <- angle(hilbert(data$microsacc))
y_phase <- angle(hilbert(data$power))

# Compute phase differences
phase_diff <- x_phase - y_phase

# Phase Locking Value (PLV)
PLV <- abs(mean(exp(1i * phase_diff)))



# Function to compute PLV for a given window
timeseries_plv <- function(x, y, window_size, overlap) {
  n <- length(x)
  step_size <- floor(window_size * (1 - overlap))  # Calculate step size for overlap
  
  PLV_series <- numeric()
  
  # Sliding window to calculate PLV
  for (start in seq(1, n - window_size, by = step_size)) {
    end <- start + window_size - 1
    x_window <- x[start:end]
    y_window <- y[start:end]
    
    # Compute phase using Hilbert transform
    x_phase <- angle(hilbert(x_window))
    y_phase <- angle(hilbert(y_window))
    
    # Compute phase difference
    phase_diff <- x_phase - y_phase
    
    # Compute PLV
    PLV <- abs(mean(exp(1i * phase_diff), na.rm = TRUE))
    
    # Store PLV value
    PLV_series <- c(PLV_series, PLV)
  }
  
  return(PLV_series)
}

# Parameters
window_size <- 10 # Number of samples per window (adjust as needed)
overlap <- 0.2  # 50% overlap

# Compute the time-varying PLV
plv_series <- timeseries_plv(main_data$microsacc, main_data$power, window_size, overlap)

# Plot the time-varying PLV
plot(plv_series, type = 'l', col = 'blue', xlab = 'Time (Windows)', ylab = 'PLV',
     main = 'Time-varying Phase Locking Value')



data$sub_strategy_block <- paste(data$sub, data$strategy, data$block, sep = "_")

# Define the PLV function
compute_plv <- function(x, y) {
  x_phase <- angle(hilbert(x))  # Hilbert Transform for x
  y_phase <- angle(hilbert(y))  # Hilbert Transform for y
  
  phase_diff <- x_phase - y_phase  # Compute phase differences
  PLV <- abs(mean(exp(1i * phase_diff), na.rm = TRUE))  # Calculate PLV
  return(PLV)
}

# Compute PLV for each sub_strategy_block and add it as a new column
data <- data %>%
  group_by(sub,strategy,block) %>%
  mutate(plv = compute_plv(power, microsacc))

model <- lmer(effort ~ plv*strategy + (1|sub), subset(data))
summary(model)

model <- lmer(power ~ microsacc*strategy + (1|sub), subset(data))
summary(model)
