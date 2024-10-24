#modules------
library(readr)
library("readxl")
require(ggplot2)
library(ggrepel)
library(investr)
library(dplyr)
library(ggpubr)
library(purrr)
library(lmerTest)
library(ggpmisc)
library(kableExtra)
library(webshot)
library(xtable)
library(broom.mixed)
library(knitr)
library(plotly)
library(gridExtra)


minmax_normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# save the final dfa-test-cleaned.csv
#write.csv(dfa, paste0("/Volumes/x9/INITIAL_DATABASE/dfa-test.csv")) #dfa-test-norepetition
#write.csv(dfa, paste0("/Users/ali/Desktop/Experiment/dfa-test.csv"))
# if you already combined and just want to load again
dfa <- read_csv(paste0("/Users/ali/Desktop/Experiment/dfa-test.csv"))


# if you want only to see one relation
ggplot(subset(dfa,n_back>0), aes(x =alpha , y = g)) +
  geom_point(alpha = 0.1) +
  #stat_smooth(aes(group = sub), method = "lm", formula = y ~ poly(x, 1),col = 'blue', se = FALSE) +
  stat_smooth(aes(), method = "lm", formula = y ~ poly(x, 1), se = TRUE) +
  theme_minimal() +facet_wrap(~strategy, scales = "fixed") 
#import -----------------------------------------------------------------------------------

indir <- "/Volumes/x9/INITIAL_DATABASE/"

sub_list <- read_csv("/Users/ali/Desktop/Experiment/check_list.csv")
rows_to_remove <- c(1, 2, 3, 4, 5, 6, 10, 19, 20,  32, 33, 34, 45, 46, 53, 54) #bad files
sub_list <- sub_list[-rows_to_remove, ]
sub_list <- sub_list[!is.na(sub_list$sub), ]

# Initialize an empty list to store data frames
df_list <- list()

# Loop through the data
for (isub in 1:nrow(sub_list)) {

  sub <- sub_list$sub[isub]
  strategy <- sub_list$strategy[isub]
  
  df <- read_csv(paste0(indir,sub,"/",sub,"-",strategy,"-test.csv"))
  
  
  # in case this subject didn't have something....
  if (!"pac" %in% names(df)) {df$pac <- NA}
  if (!"ps_bs" %in% names(df)) {df$ps_bs <- NA}
  if (!"ps" %in% names(df)) {df$ps <- NA}
  if (!"microsacc" %in% names(df)) {df$microsacc <- NA}
  if (!"magnitude" %in% names(df)) {df$magnitude <- NA}
  if (!"dps" %in% names(df)) {df$dps <- NA}
  if (!"gamma" %in% names(df)) {df$gamma <- NA}
  if (!"beta" %in% names(df)) {df$beta <- NA}
  if (!"alpha" %in% names(df)) {df$alpha <- NA}
  if (!"theta" %in% names(df)) {df$theta <- NA}
  if (!"onef" %in% names(df)) {df$onef <- NA}
  if (!"p3" %in% names(df)) {df$p3 <- NA}
  if (!"p2" %in% names(df)) {df$p2 <- NA}
  if (!"n2" %in% names(df)) {df$n2 <- NA}
  if (!"pac1" %in% names(df)) {df$pac1 <- NA}
  if (!"pac2" %in% names(df)) {df$pac2 <- NA}
  if (!"ps1" %in% names(df)) {df$ps1 <- NA}
  if (!"ps2" %in% names(df)) {df$ps2 <- NA}
  if (!"ms1" %in% names(df)) {df$ms1 <- NA}
  if (!"ms2" %in% names(df)) {df$ms2 <- NA}
  
  #normlizing pupil
  df$ps_norm <- minmax_normalize(df$ps)
  df$ms_norm <- minmax_normalize(df$microsacc)
  df$mg_norm <- minmax_normalize(df$magnitude)
  df$p3_norm <- minmax_normalize(df$p3)
  df$p2_norm <- minmax_normalize(df$p2)
  df$n2_norm <- minmax_normalize(df$n2)
  df$pac1_norm <- minmax_normalize(df$pac1)
  df$pac2_norm <- minmax_normalize(df$pac2)
  
  # how much this subject take time to finish the task
  df$total_n_block <- max(df$n_block)+1
  
  
  #define task
  df$task <- ifelse(strategy %in% c("rolling", "static"), "n-back", "ax-cpt")
  # Normalizing and adding 'sub' and 'strategy' columns
  df <- df %>% mutate(
                      sub = sub,
                      sex = sub_list$sex[isub],
                      age = sub_list$age[isub],
                      dO = sub_list$dominancO[isub],
                      wc = sub_list$workingcapacity[isub],
                      bc = sub_list$besoincogni[isub],
                      strategy = strategy
                      )
  
  # lagging
  df$lagged_score <- lag(df$score, 1)
  df$lagged_valence <- lag(df$slider_valenc.response, 1)
  df$lagged_effort <- lag(df$slider_effort.response, 1)
  df$lagged_p2 <- lag(df$p2, 1)
  df$lagged_p3 <- lag(df$p3, 1)
  df$lagged_n2 <- lag(df$n2, 1)
  
  # Add the processed data frame to the list
  df_list[[isub]] <- df
}

# Combine all data frames into one
dfa <- bind_rows(lapply(df_list, select,'sub','sex','age','wc','bc','dO','task','strategy','total_n_block',
                        'n_block','n_back','m_correct','DifficultyLevel','dprime','g','score','false_alarm','hit','correct_rejection','miss',
                        'lagged_score','lagged_valence','lagged_effort','lagged_p2','lagged_p3','lagged_n2',
                        'p3','n2','p2','pac1','pac2','ps1','ps2','ms1','ms2',
                        'p3_norm','n2_norm','p2_norm','pac1_norm','pac2_norm',
                        'ps_bs','ps','ps_norm','dps','microsacc','magnitude','mg_norm','ms_norm','effort', 'arousal','valence','time','rt_h','rt_f',
                        'slider_valenc.response','effort_category','valence_category','arousal_category','performance_category',
                        'slider_arous.response','slider_effort.response','slider_time.response',
                        'gamma','pac','beta','theta','alpha','onef'
                        ))

dfa$delta_ps_ms <- dfa$ps_norm-dfa$ms_norm 
dfa$theta_beta <- dfa$theta / dfa$beta
dfa$delta_pac <- dfa$pac2 - dfa$pac1
dfa$delta_ms <- dfa$ms2 - dfa$ms1
dfa$delta_ps <- dfa$ps2 - dfa$ps1

na1 <- sum(is.na(dfa))
repeat {
  # Initialize a variable to track if NAs were added
  previous_na_count <- sum(is.na(dfa))
  
  # Iterate through each column of dfa
  for (col in names(dfa)) {
    # Check if the column is numeric
    if (is.numeric(dfa[[col]])) {
      # Replace outliers with NA for the current column
      dfa[[col]][abs(scale(dfa[[col]])) >= 3] <- NA
    }
  }
  
  # Check if no more NAs were added
  current_na_count <- sum(is.na(dfa))
  
  # Exit the loop if no new NAs were added
  if (current_na_count == previous_na_count) {
    break
  }
}

na2 <- sum(is.na(dfa))

print((na2 - na1)/(nrow(dfa)*ncol(dfa)))

hist(dfa$microsacc, ylab="Frequency", col="blue", border="black")

dfa$recording <- interaction(sub,strategy)

mother_dfa <- dfa

# Remove rows where m_correct > score
#dfa <- dfa[dfa$m_correct <= dfa$score, ]

#modeling ------------------------------------ 

##n_back-----
model <- lmer(slider_valenc.response ~ 0 + factor(n_back)  + (1|sub), subset(dfa,strategy == 'rolling'))
AIC(model)
summary(model)
model <- lmer(slider_valenc.response ~ 0 + factor(n_back)  + (1|sub), subset(dfa,strategy == 'static'))
AIC(model)
summary(model)

##reinforcment learning of performance and valence----
dfa <- mother_dfa
dfa <- subset(dfa, strategy == 'rolling')
#dfa <- subset(dfa, n_back == 3)
# Step 1: Estimate y(t) using a mixed-effects model
model_p_mixed <- lmer(score ~ slider_effort.response + (1 | sub), data = dfa[-nrow(dfa), ])
# Step 2: Predict p(t) using the mixed-effects model
dfa$p_estimated <- predict(model_p_mixed, dfa)
# Step 3: Compute the difference v(t+1) - v(t) and (y(t) - v(t))
dfa$delta_v <- dfa$slider_valenc.response - dfa$lagged_valence  # v(t) - v(t-1)
dfa$delta_e <- dfa$slider_effort.response - dfa$lagged_effort
dfa$delta_pv <- dfa$p_estimated - dfa$score  # y(t) - s(t-1)
dfa$delta_p <- dfa$score - dfa$lagged_score  # s(t) - s(t-1)
dfa$delta_sp <- dfa$score - dfa$p_estimated  # s(t) - y(t)
dfa$delta_sv <- dfa$score - dfa$slider_valenc.response  # s(t) - y(t)
dfa$delta_p3 <- dfa$p3 - dfa$lagged_p3
dfa$delta_n2 <- dfa$n2 - dfa$lagged_n2

# Step 5: explaining RewP
model<- lmer(  p2 ~ 0 + strategy + I(delta_p - delta_v)  + (1 | sub), data = dfa[-nrow(dfa), ])
AIC(model)
summary(model)

model<- lmer( slider_valenc.response ~ score + (1 | sub), data = dfa[-nrow(dfa), ])
AIC(model)
summary(model)


# relationship between phase amplitude coupling and micro-saccade 
dfa <- mother_dfa
dfa <- subset(dfa, task == 'n-back')

dfa <- subset(dfa, n_back > 1)


summary(lmer(delta_pac ~ delta_ms + n_block + (1|sub), dfa))
summary(lmer(slider_effort.response ~ delta_pac + (1|sub), dfa))
summary(lmer(ps ~ microsacc + (1|sub), dfa))

# higher pac1 faster response time
summary(lmer( rt_h ~ 0 + strategy + pac1 + (1|sub), dfa))


##phenomenology-----
dfa <- mother_dfa
dfa <- dfa[!is.na(dfa$lagged_score), ]

dfa$valence_dt <- dfa$slider_valenc.response - lm(slider_valenc.response ~ 0 +  n_block + score  + lagged_score  , data = dfa)$fitted.values
dfa$arousal_dt <- dfa$slider_arous.response - lm(slider_arous.response ~ 0 + n_block + score + lagged_score , data = dfa)$fitted.values
dfa$effort_dt <- dfa$slider_effort.response - lm(slider_effort.response ~ 0 + n_block + score  + lagged_score , data = dfa)$fitted.values

model <- lmer(valence_dt ~ slider_effort.response + (1|sub), dfa)
AIC(model)
summary_output <- summary(model)

model <- lmer(arousal_dt ~ slider_effort.response + (1|sub), dfa)
AIC(model)
summary_output <- summary(model)

model <- lmer(effort_dt ~  arousal_dt + (1|sub), dfa)
AIC(model)
summary_output <- summary(model)

##Table for pacs -----
model1 <- lmer(pac1  ~ 0  + strategy  + (1|sub), data = subset(dfa,n_back==1))
model2 <- lmer(pac1 ~ 0 + strategy   + (1|sub), data = subset(dfa,n_back==2))
model3 <- lmer(pac1 ~ 0 + strategy  + (1|sub), data = subset(dfa,n_back==3))
model4 <- lmer(pac2  ~ 0  + strategy  + (1|sub), data = subset(dfa,n_back==1))
model5 <- lmer(pac2 ~ 0 + strategy   + (1|sub), data = subset(dfa,n_back==2))
model6 <- lmer(pac2 ~ 0 + strategy  + (1|sub), data = subset(dfa,n_back==3))


models <- list( model1,model2,model3,model4,model5,model6)
model_names <- c('1 t1','2 t1','3 t1','1 t2','2 t2','3 t2')

visu_lmr_combined(models,model_names)

model <- lmer(dprime ~ 0 + strategy + (1|sub), dfa)
AIC(model)
summary(model)

## performance-----
dfa_rolling <- subset(dfa,strategy=='rolling')
summary_output <- summary(lmer( g  ~  slider_effort.response + (1|sub), subset(dfa_rolling,n_back ==3)))
summary_output <- summary(lmer( g  ~  slider_effort.response + (1|sub), subset(dfa_rolling,n_back ==2)))
summary_output <- summary(lmer( g  ~  slider_effort.response + (1|sub), subset(dfa_rolling,n_back ==1)))


dfa_static <- subset(dfa,strategy=='proactive')
summary_output <- summary(lmer( g  ~  slider_effort.response + (1|sub), subset(dfa_static,n_back ==3)))
summary_output <- summary(lmer( g  ~  slider_effort.response + (1|sub), subset(dfa_static,n_back ==2)))
summary_output <- summary(lmer( g  ~  slider_effort.response + (1|sub), subset(dfa_static,n_back ==1)))


summary_output <- summary(lmer( g  ~  slider_effort.response + (1|sub), subset(dfa,strategy=='proactive')))
summary_output <- summary(lmer( g  ~  slider_effort.response + (1|sub), subset(dfa,strategy=='reactive')))

##p3----

model <- lmer( slider_effort.response ~  p3+ (1|sub), dfa)
AIC(model)

summary_output <- summary(model)

##n2----
model <- lmer( slider_effort.response ~ n2  + (1|sub), dfa)
AIC(model)

model <- lmer( g ~ n2 + (1|sub), dfa)
AIC(model)

summary(model)
summary_output <- summary(model)

##ps----
model <- lmer(slider_effort.response ~ ps  + (1|sub), dfa)
AIC(model)
summary_output <- summary(model)
summary(lmer( ps1  ~  slider_effort.response + (1|sub), dfa)) #positive
summary(lmer( ps2  ~  slider_effort.response + (1|sub), dfa)) #positive
summary(lmer( delta_ps  ~  slider_effort.response + (1|sub), dfa)) #negative

##ms----
summary(lmer(  slider_effort.response ~ microsacc  + (1|sub), dfa)) #negative
summary(lmer( ms1 ~  slider_effort.response + (1|sub), dfa)) #positive
summary(lmer( ms2 ~  slider_effort.response + (1|sub), dfa)) #positive

summary(lmer( delta_ms ~  ps + (1|sub), dfa)) #positive (delta_ms > 0)

##pac----
summary(lmer( pac1 ~  slider_effort.response + (1|sub), dfa)) #negative
summary(lmer( pac2 ~  slider_effort.response + (1|sub), dfa)) #negative
summary(lmer( delta_pac ~  slider_effort.response + (1|sub), dfa)) #positive

## delta_pac vs ps-----
dfa <- mother_dfa

summary(lmer( delta_pac ~ 0 +strategy +  ps_norm + (1|sub), dfa))

## pac vs ms-----
dfa <- mother_dfa

summary(lmer( delta_pac ~  microsacc + (1|sub), dfa)) 
#microsacc for some reason has positive relation with effort as well
#even though ms1 and ms2 have different releation

summary(lmer( pac1 ~ ms1 + ms2 + (1|sub), dfa))
summary(lmer( pac2 ~ ms1 + ms2 + (1|sub), dfa)) #negative ms1 positive ms2

summary(lmer( ms1 ~ pac1 + pac2 + (1|sub), dfa))
summary(lmer( ms2 ~ pac1 + pac2  + (1|sub), dfa))

summary(lmer( delta_ms ~ 0 + strategy + delta_pac + (1|sub), dfa))

##frequencies-----
summary(lmer(  slider_effort.response ~ alpha  + (1|sub), dfa))
summary(lmer(  slider_effort.response ~ theta  + (1|sub), dfa))
summary(lmer(  slider_effort.response ~ beta  + (1|sub), dfa))
summary(lmer(  slider_effort.response ~ I(theta/beta)  + (1|sub), dfa))
summary(lmer(  slider_effort.response ~ onef  + (1|sub), dfa))

##working memory capacity------
summary(lm( g ~ bc + wc + slider_effort.response, subset(dfa,task == 'n-back')))

## time -------
summary(lmer( slider_time.response ~  ms1 + ms2 + (1|sub), dfa))
summary(lmer( slider_time.response ~ 0 + delta_pac*delta_ms + strategy + (1|sub), dfa))

## response time ------
summary(lmer( rt_h ~ delta_pac*delta_ms + (1|sub), dfa))
#Kernel density of parameters------------------------------------ 

# --------------- don't forget to change the strategy!!!

dfa_long <- tidyr::gather(dfa, key = "variable", value = "value", 
                          slider_valenc.response, slider_arous.response, 
                          slider_effort.response, slider_time.response)

s <- "reactive"
gg <- ggplot(subset(dfa_long,strategy == s), aes(x = value, fill = variable))
gg <- gg + geom_density(alpha = 0.5)
gg <- gg + xlim(0, 100)
# Facet by both sub and n_back
gg <- gg + facet_wrap(~sub + n_back, scales = "free_y")
gg <- gg + labs(title = paste0("Kernel Density Estimates ",s),
                x = "Value",
                y = "Density")
print(gg)
ggsave(paste0("/Volumes/x9/results/allsubs/figure/density_",s,".png"), plot = gg, width = 10, height = 6, dpi = 300)

# Kernel density of visual scale analogs after normalization
dfa_long <- tidyr::gather(dfa, key = "variable", value = "value", 
                          valence, arousal, 
                          effort, time)

s <- "reactive"
gg <- ggplot(subset(dfa_long,strategy == s), aes(x = value, fill = variable))
gg <- gg + geom_density(alpha = 0.5)
gg <- gg + xlim(0, 1)
# Facet by both sub and n_back
gg <- gg + facet_wrap(~ sub + n_back, scales = "free_y")
gg <- gg + labs(title = paste0("Kernel Density Estimates ",s),
                x = "Value",
                y = "Density")
print(gg)
ggsave(paste0("/Volumes/x9/results/allsubs/figure/density_",s,"_minmaxed.png"), plot = gg, width = 10, height = 6, dpi = 300)


#n_back --------------------------- 
dfa$valence_dt <- dfa$slider_valenc.response - lm(slider_valenc.response ~  0 + n_block + score  , data = dfa)$fitted.values
dfa$valence_dt <-minmax_normalize(dfa$valence_dt)

y_values <- c('microsacc','ps_norm','ms_norm','delta_ps_ms','theta','alpha','beta','theta_beta','gamma','pac1','pac2','diff_pac','p3','p3_norm','p2','p2_norm','n2','n2_norm','time',"effort","valence", "dprime",'g','rt_h','ps1','ps2','ms1','ms2','pac1','pac2','delta_ps','delta_ms','delta_pac')

y_values <- c('valence_dt')
# Loop over each y value
for (y in y_values) {
  # Create the plot
  # Calculate means and standard errors
  dfa_summary <- dfa %>%
    group_by(n_back, strategy, task) %>%
    summarise(
      mean_y = mean(.data[[y]], na.rm = TRUE),
      se_y = sd(.data[[y]], na.rm = TRUE) / sqrt(n())
    ) %>%
    ungroup()
  
  # Plot with error bars and connecting lines
  plot <- ggplot(dfa_summary, aes(x = n_back, y = mean_y, color = strategy)) +
    geom_errorbar(aes(ymin = mean_y - se_y, ymax = mean_y + se_y), width = 0.2) +
    geom_line(aes(group = strategy), size = 1) +
    facet_wrap(~task) +
    labs(
      title = paste("Difficulty and", y),
      x = "Qualitative Difficulty Level",
      y = y
    ) +
    theme_minimal() +
    scale_x_continuous(breaks = c(1, 2, 3)) +  # Only show 1, 2, 3 on the x-axis
    theme(
      panel.grid.major.x = element_line(color = "grey80"),  # Remove existing vertical grid lines
      panel.grid.minor.x = element_blank(),  # Remove minor vertical grid lines
      panel.grid.major.y = element_blank(),  # Keep horizontal grid lines
      axis.ticks.x = element_line(color = "black"),  # Add x-axis ticks
      axis.ticks.length = unit(5, "pt")  # Customize tick length if desired
    )
  
  # Fit linear model
  #lm_model <- lm(data = dfa, formula = as.formula(paste(y, "~ poly(n_back, 2)")))
  
  # Extract p-value
  #p_value <- summary(lm_model)$coefficients["poly(n_back, 2)1", "Pr(>|t|)"]
  
  # Add caption
  #plot <- plot + labs(caption = paste("p-value total: ", round(p_value,3)))
  print(plot)
  
  file_path <- file.path("/Volumes/x9/results/allsubs/figure/", paste("n_back_", y, "_by_task.png", sep = ""))
  #ggsave(filename = file_path, plot = plot, width = 10, height = 6, dpi = 300)
}




#phenomenology ------------------------------ 
# Define the x values
x_values <- c("n_block","effort", "dprime", "score", "ps_norm","pac",'microsacc','p3','n2','p2')

x_values <- c("effort")
for (x in x_values) {
  # Create the plot
  phenom <- ggplot(dfa) + 
    geom_point(aes_string(x = x, y = "arousal_dt", color = "'Arousal'",alpha = 0.01)) + 
    geom_smooth(aes_string(x = x, y = "arousal_dt", color = "'Arousal'"), method = "lm", se = TRUE) +
    geom_point(aes_string(x = x, y = "valence_dt", color = "'Valence'"),alpha = 0.1) +
    geom_smooth(aes_string(x = x, y = "valence_dt", color = "'Valence'"), method = "lm", se = TRUE) +
    labs(
      title = paste("Phenomenal ", x),
      
      y = "Arousal and valence"
    ) + 
    scale_color_manual(
      name = "Measures",
      values = c("Arousal" = "orange", "Valence" = "purple")
    ) +
    theme(
      legend.position = c(.25, .95),
      legend.justification = c("right", "top"),
      legend.box.just = "right"
    ) + theme_minimal() #+ facet_wrap(~ strategy, scales = "fixed")
    #xlim(-0.4, 0.4) +  # Set x-axis limits
    #ylim(-0.4, 0.4)
  
  # Extract p-values
  p_value_arousal <- summary(lm(arousal ~ ., data = dfa[, c(x, "arousal")]))$coefficients[2, 4]
  p_value_valence <- summary(lm(valence ~ ., data = dfa[, c(x, "valence")]))$coefficients[2, 4]
  
  # Annotate p-values onto the plot
  #phenom <- phenom + geom_text(x = Inf, y = Inf, label = paste("p-value:", round(p_value_arousal, 3)), hjust = 1, vjust = 1, color = "orange") +geom_text(x = Inf, y = Inf, label = paste("p-value:", round(p_value_valence, 3)), hjust = 1, vjust = 2.5, color = "purple")
  
  # Print the plot
  print(phenom)
  
  # Construct the file path
  file_path <- file.path("/Volumes/x9/results/allsubs/figure/", paste("phenom_", x, "_strategy.png", sep = ""))
  
  # Save the plot
  #ggsave(filename = file_path, plot = phenom, width = 10, height = 6, dpi = 300)
}


#performance ----------------------------- 

# Define x_values and y_values
x_values <- c("effort",'p3','n2','ps_norm','microsacc','ms_norm','rt_h','diff_ps_ms','diff_pac')
y_values <- c("score","dprime",'g', "time", "rt_h", "effort") 


# Loop over each y_value
for (x_val in x_values) {
  
  # Loop over each x_value
  for (y_val in y_values) {
    
    # Skip if the dependent and independent variables are the same
    if (y_val == x_val) {
      next
    }
    
    # Initialize empty data frame to store p-values
    p_values <- data.frame(n_back = character(), p_value = numeric())
    
    # Loop over each level of n_back
    for (n_level in unique(dfa$n_back)) {
      # Subset the data for the current level of n_back
      subset_data <- dfa[dfa$n_back == n_level, ]
      
      # Perform linear regression using dynamic y and x variables
      lm_formula <- as.formula(paste(y_val, "~", x_val))
      lm_model <- lm(lm_formula, data = subset_data)
      
      # Extract the p-value
      p_value <- summary(lm_model)$coefficients[2, 4]
      
      # Add the p-value to the data frame
      p_values <- rbind(p_values, data.frame(n_back = n_level, p_value = p_value))
    }
    
    # Perform linear regression for the total dataset
    lm_model_total <- lm(lm_formula, data = dfa)
    # Extract the p-value for the total dataset
    p_value_total <- summary(lm_model_total)$coefficients[2, 4]
    
    # Create the plot
    perform <- ggplot(dfa, aes_string(x = x_val, y = y_val, col = "factor(n_back)")) +
      stat_smooth(aes_string(group = "factor(n_back)"), method = "lm", formula = "y ~ poly(x, 1)", se = FALSE) +
      geom_smooth(method = "lm", se = TRUE, color = "black") + # Add regression line for all data
      #geom_point() +
      facet_wrap(~ strategy, scale="free") +
      theme_minimal() +
      scale_color_manual(values = c("blue", "green", "red"))#+labs(caption = paste("p-value 1:", round(p_values[,2][1], 3),"p-value 2:", round(p_values[,2][2], 3),"p-value 3:", round(p_values[,2][3], 3),"p-value total:", round(p_value_total, 3))) 
        
    
    # Print the plot
    print(perform)
    
    # Construct the file path to include both y_val and x_val
    file_path <- file.path("/Volumes/x9/results/allsubs/figure/", paste( "performance_",y_val, "_vs_", x_val, "by_n_back.png", sep = ""))
    
    # Save the plot
    #ggsave(filename = file_path, plot = perform, width = 10, height = 6, dpi = 300)
  }
}


#sub.strategy ----------------------------- 

ggplot(dfa, aes(x = g, y = slider_valenc.response, col = factor(n_back))) +
  stat_smooth(aes(group = factor(n_back)), method = "lm",formula =  y ~ poly(x,1), se = TRUE) 
  #+ geom_point() #+ facet_wrap(~ sub)

ggplot(dfa, aes(x = strategy, y = valence)) +
  stat_smooth(method = "lm", formula =  y ~ 1 + poly(x,1), se = FALSE) +
  geom_point() + 
  #facet_wrap(~ sub, scales = "free") +
  stat_poly_eq(
    aes(label = paste(formatC(..p.value.., digits = 2, format = "f"))), 
    formula = y ~ poly(x, 1), 
    parse = TRUE, 
    geom = "text", 
    size = 5,
    label.x = "left",  # Adjust the x position of the label
    label.y = "top",
    color = "blue" # Adjust the y position of the label
  ) +
  theme_minimal()



#strategy ----------------------------- 

x_values <- c("effort", "pac", 'ps_bs','ps_norm',"rt_h",'microsacc','ms_norm')
y_values <- c("dprime", "effort", "ps_bs",'ps_norm', "time", "rt_h","n_block",'microsacc')



for (x_val in x_values) {
  for (y_val in y_values) {
    # Check if x_val and y_val are the same
    if (x_val == y_val) {
      next  # Skip to the next iteration if x_val and y_val are the same
    }
    # Perform the regression analysis
    lm1 <- lm(as.formula(paste(y_val,"~", x_val)), data = subset(dfa, strategy == "rolling"))
    p_values1 <- summary(lm1)$coefficients[2, "Pr(>|t|)"][1]
    
    lm2 <- lm(as.formula(paste(y_val,"~", x_val)), data = subset(dfa, strategy == "static"))
    p_values2 <- summary(lm2)$coefficients[2, "Pr(>|t|)"][1]
    
    # Create the plot
    plot <- ggplot(dfa, aes_string(x = x_val, y = y_val, col = "factor(strategy)")) +
      stat_smooth(aes_string(group = "factor(strategy)"), method = "lm", formula = "y ~ poly(x, 1)", se = TRUE) +
      # Add legend title
      labs(col = "Strategies") +
      # Extract the variables used for x and y axes
      labs(
        title = paste(y_val, " and ", x_val),
        x = x_val,
        y = y_val
      ) +
      # Add other plot settings
      theme_minimal() + facet_wrap(~ strategy,scale="fixed")
    
    # Print the plot
    print(plot)
    
    # Construct the file path to include both y_val and x_val
    file_path <- file.path("/Volumes/x9/results/allsubs/figure/", paste(y_val, "_vs_", x_val, "by_strategy.png", sep = ""))
    
    # Save the plot
    ggsave(filename = file_path, plot = plot, width = 10, height = 6, dpi = 300)
  }
}


#wilcoxon ---------------------------- 

# choose a task!!!
task = "nback"
task = "axcpt"
# List of y_values
y_values <- c("score", "dprime", "rt_h",'microsacc', "time","effort", "valence", "arousal","slider_effort.response","slider_arous.response","slider_valenc.response")
x_values <- c("effort_category")  # , "effort_category","arousal_category","valence_category"

dfa$block_category <- interaction(dfa$valence_category,dfa$performance_category)
y_values <- c('p2')
x_values <- c('block_category') 

#dfa_task <- dfa[dfa$strategy %in% c("rolling", "static"), ]
#dfa_task <- dfa[dfa$strategy %in% c("proactive", "reactive"), ]
# Iterate over each y_value
for (x_value in x_values) {
  for (y_value in y_values) {  # Loop over x_values
    # Run Wilcoxon test
    test_result <- wilcox.test(get(y_value) ~ get(x_value), data = dfa)
    
    # Create a box plot and add a significance annotation
    wilcox_plot <- ggplot(dfa, aes_string(x = x_value, y = y_value)) +
      geom_boxplot(fill = "#69b3a2", color = "#2e4053", alpha = 0.8) +
      geom_point(position = position_jitter(width = 0.2), alpha = 0.5, color = "#2e4053") +
      geom_hline(yintercept = median(dfa[[y_value]]), color = "#2e4053", linetype = "dashed", size = 1) +
      labs(title = paste(toupper(y_value), "by", toupper(x_value)),
           x = toupper(x_value),
           y = toupper(y_value),
           caption = paste("Wilcoxon p-value:", round(test_result$p.value, 3))) +
      theme_minimal() +
      theme(plot.title = element_text(size = 20, hjust = 0.5),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 14),
            legend.position = "none")
    
    print(wilcox_plot)
    # Define file path
    file_path <- file.path("/Volumes/x9/results/allsubs/figure/", paste("wilcoxon_test_", y_value, "_", x_value, ".png", sep = ""))
    
    # Save the plot
    ggsave(filename = file_path, plot = wilcox_plot, width = 10, height = 6, dpi = 300)
  }
}



#violin_plot -------------------------------- 

# Vector of y values
y_values <- c( "pac", "ps_bs",'ps_norm',"effort", "dprime", "time", "valence", "rt_h")

y_values <- c("p2")
# Loop through y values
for (y in y_values) {
  
  # Create the violin plot
  violin_plot <- ggplot(subset(dfa,n_back>1), aes(x = block_category, y = !!sym(y), fill = block_category)) +
    geom_violin(trim = FALSE) +
    scale_fill_brewer(palette = "Set3") + 
    geom_boxplot(width = 0.1, fill = "white") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") +
    labs(
      title = paste("Parameters by Strategy (Y =", y, ")"),
      x = "Strategy",
      y = y
    ) + theme_minimal() + facet_wrap(~strategy)
  
  print(violin_plot)
  
  file_path <- file.path("/Volumes/x9/results/allsubs/figure/", paste("violin_block_cateogry_", y, ".png", sep = ""))
  # Save the plot
  #ggsave(filename = file_path, plot = violin_plot, width = 10, height = 6, dpi = 300)
}


#elasticity ------------------------------------------ 

compute_elasticity <- function(data, x_value) {
  pct_change_g <- (data$g - lag(data$g)) / lag(data$g)
  pct_change_x <- (data[[x_value]] - lag(data[[x_value]])) / lag(data[[x_value]])
  elasticity <- mean(pct_change_g / pct_change_x, na.rm = TRUE)
  return(elasticity)
}

# Create an empty list to store elasticity results
elasticity_results <- list()

# Loop through all combinations of strategy, sub, and n_back
for (strategy_val in unique(dfa$strategy)) {
  for (sub_val in unique(dfa$sub)) {
    for (n_back_val in unique(dfa$n_back)) {
      # Subset the dataframe
      subset_df <- subset(dfa, strategy == strategy_val & sub == sub_val & n_back == n_back_val)
      
      # Compute elasticity for the subset
      elasticity <- compute_elasticity(subset_df, "effort")
      
      # Store the elasticity result along with strategy, sub, and n_back
      result_entry <- data.frame(strategy = strategy_val,
                                 sub = sub_val,
                                 n_back = n_back_val,
                                 elasticity = elasticity)
      
      # Append the result to the elasticity_results list
      elasticity_results[[length(elasticity_results) + 1]] <- result_entry
    }
  }
}

# Combine the dataframes in elasticity_results into a single dataframe
elasticity_df <- do.call(rbind, elasticity_results)

# Plot with smoothed trend line
ggplot(subset(elasticity_df,-1<elasticity & elasticity<1), aes(x = factor(n_back), y = elasticity, group = strategy, color = strategy)) +
  #geom_line() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add smoothed trend line
  labs(x = "Difficulty Level", y = "Elasticity", title = "Pupil elasticity of performance") +
  theme_minimal()

# Plot with violin plot
ggplot(subset(elasticity_df,-1<elasticity & elasticity<1), aes(x = factor(n_back), y = elasticity, fill = strategy)) +
  geom_violin() +
  labs(x = "Difficulty Level", y = "Elasticity", title = "Pupil elasticity of performance") +
  theme_minimal()


# ------------------------------------showing the leontief relation between strategy effort and performance
# Remove rows with missing values
df3d <- na.omit(dfa)
df3d <- (dfa)
# Impute missing values with mean
# Replace "column_name" with the actual column containing missing values
df3d$effort[is.na(df3d$effort)] <- mean(df3d$effort, na.rm = TRUE)
df3d$dprime[is.na(df3d$dprime)] <- mean(df3d$dprime, na.rm = TRUE)
df3d$ps_bs[is.na(df3d$ps_bs)] <- mean(df3d$ps_bs, na.rm = TRUE)



# Create 3D scatterplot with grouping by dfa$n_back
plot_ly(data = subset(dfa,n_back>0), x = ~effort, y = ~strategy, z = ~ dprime, color = ~factor(round(n_back,1)), type = "scatter3d", mode = "markers") %>%
  layout(scene = list(xaxis = list(title = "n back"),
                      yaxis = list(title = "Strategy"),
                      zaxis = list(title = "Dprime"),
                      aspectmode = "cube"))

# Fit a linear model
lm_model <- lm(dprime ~ effort + strategy, data = subset(dfa, n_back > 0))

# Create 3D scatterplot with grouping by dfa$n_back
plot_ly(data = subset(dfa, n_back > 0), x = ~effort, y = ~strategy, z = ~dprime, color = ~factor(round(n_back, 1)), type = "scatter3d", mode = "markers") %>%
  add_trace(x = ~dfa$n_back, y = ~dfa$strategy, z = predict(lm_model), 
            type = "scatter3d", mode = "lines", line = list(color = "blue")) %>%
  layout(scene = list(xaxis = list(title = "Effort"),
                      yaxis = list(title = "Strategy"),
                      zaxis = list(title = "Dprime"),
                      aspectmode = "cube"))




