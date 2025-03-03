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
library(moments)
library(plotly)
library(sjPlot)
library(ggforce)


minmax_normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# if you already combined and just want to load again
dfa <- read_csv(paste0("/Users/ali/Documents/Experiment/dfa-test.csv"))


dfa_clean <- dfa[!is.na(dfa$p2), ] # Remove rows with NA in p2
model <- lm(p2 ~ 0 + outcome, data = dfa_clean)
dfa_clean$residuals <- dfa_clean$p2 - model$fitted.values


fig <- ggplot(dfa, aes(x = slider_valenc.response, y = score, color = strategy)) +
  stat_smooth(aes(group = factor(strategy)), method = "lm", formula = y ~ poly(x, 1), se = TRUE) +
  theme_minimal() +
  labs(x = "Reported Valence", y = "Score", color = "Strategy") +
  theme(legend.position = c(0, 0), legend.justification = c(0, 0),
        axis.title = element_text(size = 18),    # Increase axis title font size
        axis.text = element_text(size = 18),      # Increase axis labels font size
        legend.title = element_text(size = 18),   # Increase legend title font size
        legend.text = element_text(size = 18),    # Increase legend text font size
        strip.text = element_text(size = 18)) + facet_wrap(~task, scales = 'fixed')
fig


dfa_task <- subset(dfa,task=="n-back")

model <- lmer(  p2 ~ pe + (1|sub) , data = subset(dfa,task=="n-back"))
summary(model)

##import -----------------------------------------------------------------------------------

indir <- "/Volumes/x9/INITIAL_DATABASE/"

sub_list <- read_csv("/Users/ali/Documents/Experiment/check_list.csv")
rows_to_remove <- c(1, 2, 3, 4, 5, 6, 10, 19, 20,  32, 33, 34, 45, 46, 53, 54) #bad files
sub_list <- sub_list[-rows_to_remove, ]
sub_list <- sub_list[!is.na(sub_list$sub), ]

#write.csv(sub_list, paste0("/Volumes/x9/INITIAL_DATABASE/check_list_to_process.csv"))

# importing sub
sub_list <- read_csv("/Volumes/x9/INITIAL_DATABASE/check_list_to_process.csv")
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
  if (!"ms_bs" %in% names(df)) {df$ms_bs <- NA}
  if (!"pupil_stim" %in% names(df)) {df$pupil_stim <- NA}
  if (!"pupil_rest" %in% names(df)) {df$pupil_rest <- NA}
  if (!"microsacc_stim" %in% names(df)) {df$microsacc_stim <- NA}
  if (!"microsacc_rest" %in% names(df)) {df$microsacc_rest <- NA}
  if (!"magnitude" %in% names(df)) {df$magnitude <- NA}
  if (!"dps" %in% names(df)) {df$dps <- NA}
  if (!"gamma" %in% names(df)) {df$gamma <- NA}
  if (!"beta" %in% names(df)) {df$beta <- NA}
  if (!"alpha" %in% names(df)) {df$alpha <- NA}
  if (!"theta" %in% names(df)) {df$theta <- NA}
  if (!"onef" %in% names(df)) {df$onef <- NA}
  if (!"p3a" %in% names(df)) {df$p3a <- NA}
  if (!"p3b" %in% names(df)) {df$p3b <- NA}
  if (!"p2" %in% names(df)) {df$p2 <- NA}
  if (!"pe" %in% names(df)) {df$pe <- NA}
  if (!"ern" %in% names(df)) {df$ern <- NA}
  if (!"n2s" %in% names(df)) {df$n2s <- NA}
  if (!"pac1" %in% names(df)) {df$pac1 <- NA}
  if (!"pac2" %in% names(df)) {df$pac2 <- NA}
  if (!"pac3" %in% names(df)) {df$pac3 <- NA}
  if (!"pac1_low" %in% names(df)) {df$pac1_low <- NA}
  if (!"pac2_low" %in% names(df)) {df$pac2_low <- NA}
  if (!"pac3_low" %in% names(df)) {df$pac3_low <- NA}
  if (!"pac1_high" %in% names(df)) {df$pac1_high <- NA}
  if (!"pac2_high" %in% names(df)) {df$pac2_high <- NA}
  if (!"pac3_high" %in% names(df)) {df$pac3_high <- NA}
  if (!"ps1" %in% names(df)) {df$ps1 <- NA}
  if (!"ps2" %in% names(df)) {df$ps2 <- NA}
  if (!"ms1" %in% names(df)) {df$ms1 <- NA}
  if (!"ms2" %in% names(df)) {df$ms2 <- NA}
  
  #normlizing
  #df$pupil_stim_norm <- minmax_normalize(df$pupil_stim)
  #df$pupil_rest_norm <- minmax_normalize(df$pupil_rest)
  #df$ms_norm <- minmax_normalize(df$microsacc)
  #df$mg_norm <- minmax_normalize(df$magnitude)
  df$p3a_norm <- minmax_normalize(df$p3a)
  df$p3b_norm <- minmax_normalize(df$p3b)
  #df$p2_norm <- minmax_normalize(df$p2)
  df$pe_norm <- minmax_normalize(df$pe)
  df$ern_norm <- minmax_normalize(df$ern)
  #df$pac1_norm <- minmax_normalize(df$pac1)
  #df$pac2_norm <- minmax_normalize(df$pac2)
  #df$alpha_norm <- minmax_normalize(df$alpha)
  #df$theta_norm <- minmax_normalize(df$theta)
  #df$beta_norm <- minmax_normalize(df$beta)
  #df$gamma_norm <- minmax_normalize(df$gamma)
  
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
  df$lagged_p3a <- lag(df$p3a, 1)
  df$lagged_p3b <- lag(df$p3b, 1)
  
  # Add the processed data frame to the list
  df_list[[isub]] <- df
}

# Combine all data frames into one
dfa <- bind_rows(lapply(df_list, select,'sub','sex','age','wc','bc','dO','task','strategy','total_n_block',
                        'n_block','n_back','m_correct','DifficultyLevel','dprime','g','score','false_alarm','hit','correct_rejection','miss',
                        'lagged_score','lagged_valence','lagged_effort','lagged_p2','lagged_p3a','lagged_p3b',
                        'p3a','p3b','pe','ern','n2s','p2','pac1','pac2','pac3','pac1_low','pac2_low','pac3_low','pac1_high','pac2_high','pac3_high',
                        'p3a_norm','p3b_norm','pe_norm','ern_norm',
                        'ps1','ps2','ms1','ms2',
                        'effort', 'arousal','valence','time','rt_h','rt_f',
                        'slider_valenc.response','effort_category','valence_category','arousal_category','performance_category',
                        'slider_arous.response','slider_effort.response','slider_time.response',
                        'gamma','pac','beta','theta','alpha','onef'
                        ))


dfa$theta_beta <- dfa$theta / dfa$beta
dfa$delta_pac <- dfa$pac2 - dfa$pac1
dfa$delta_pac_low <- dfa$pac2_low - dfa$pac1_low
dfa$delta_pac_high <- dfa$pac2_high - dfa$pac1_high
dfa$delta_ms <- dfa$ms2 - dfa$ms1
dfa$delta_ps <- dfa$ps2 - dfa$ps1

# Create a new column 'outcome' based on the condition
dfa$outcome <- ifelse(dfa$m_correct > dfa$score, "fail", "success")



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

hist(dfa$ern, ylab="Frequency", col="blue", border="black")

dfa$recording <- interaction(sub,strategy)

# Add a new column `group` based on the condition
dfa$group <- ifelse(dfa$strategy %in% c("static", "reactive"), 
                    "static & reactive", 
                    "rolling & proactive")

mother_dfa <- dfa
dfa<-mother_dfa

write.csv(dfa, paste0("/Volumes/x9/INITIAL_DATABASE/dfa-test.csv"))
write.csv(dfa, paste0("/Users/ali/Documents/Experiment/dfa-test.csv"))

#dfa <- dfa[dfa$m_correct <= dfa$score, ] # Remove rows where m_correct > score

#modeling ------------------------------------ 


##strategy -------
library(emmeans)
dfa <- mother_dfa
model<- lmer(slider_valenc.response ~ 0 +  strategy + (1|sub), subset(dfa,n_back==3))
summary(model)

emmeans(model, pairwise ~ strategy, adjust = "tukey")


plot_model(model, type = "pred", show.data = FALSE) +  # Effect plot for fixed effects
  theme_minimal(base_size = 14) +
  labs(title = "Reported valence",
       x = "Strategies",
       y = "Predicted variable") +
  theme(legend.position = "top")

##n_back-----
model <- lmer(slider_valenc.response ~ 0 + factor(n_back) + (1|sub), subset(dfa,strategy == 'rolling'))
AIC(model)
summary(model)
model <- lmer(slider_valenc.response ~ 0 + factor(n_back) + (1|sub), subset(dfa,strategy == 'static'))
AIC(model)
summary(model)

##FCE (free choice of effort) -----
dfa$fce <- (dfa$score - dfa$m_correct)/(20 - dfa$m_correct)

dfa_nofail <- dfa[dfa$m_correct <= dfa$score, ] # Remove rows where m_correct > score

df_nback <- subset(dfa_nofail,task=='n-back')
df_axcpt <- subset(dfa_nofail,task=='ax-cpt')

shapiro.test(df_nback$free_choice)
hist(df_nback$free_choice)
hist(df_axcpt$free_choice)


wilcox.test(df_nback$free_choice, mu < 0)
wilcox.test(df_axcpt$free_choice, mu = 0)


dfa_nofail %>%
  group_by(sub, strategy) %>%
  summarise(mean_free_choice = mean(free_choice, na.rm = TRUE))



summary(lm(free_choice ~ bc + wc, subset(dfa_nofail,task=='n-back')))
summary(lm(free_choice ~ bc + wc, subset(dfa_nofail,task=='ax-cpt')))

# Boxplot of free_choice by strategy
ggplot(dfa_nofail, aes(x = factor(m_correct), y = free_choice, fill = factor(m_correct))) +
  geom_boxplot(alpha = 0.6) +
  labs(title = "Free choice effort",
       x = "Required score",
       y = "FCE") +
  theme_minimal() +
  theme(
    legend.position = "none",  # Hide legend
    plot.title = element_text(size = 18, face = "bold"),  # Bigger title
    axis.title.x = element_text(size = 18),  # Bigger x-axis label
    axis.title.y = element_text(size = 18),  # Bigger y-axis label
    axis.text.x = element_text(size = 18),  # Bigger x-axis ticks
    axis.text.y = element_text(size = 18)   # Bigger y-axis ticks
  )  + facet_wrap(~ task, scales = "fixed")  

summary(lmer( free_choice ~  slider_valenc.response*strategy  + n_back + n_block  + (1|sub), subset(dfa_nofail,task=='n-back')))
summary(lmer( free_choice ~  slider_valenc.response*strategy  + n_back + n_block   + (1|sub), subset(dfa_nofail,task=='ax-cpt')))


summary(lmer( p2 ~  free_choice*strategy + (1|sub), subset(dfa_nofail,task=='n-back')))
summary(lmer( p2 ~  free_choice*strategy + (1|sub), subset(dfa_nofail,task=='ax-cpt')))


##reinforcment learning of performance and valence----
dfa <- mother_dfa
#dfa <- subset(dfa, strategy == 'rolling')
#dfa <- subset(dfa, n_back == 3)
# Step 1: Estimate y(t) using a mixed-effects model
model_p_mixed <- lmer(score ~ slider_effort.response + (1 | sub), data = dfa[-nrow(dfa), ])
# Step 2: Predict p(t) using the mixed-effects model
dfa$p_estimated <- predict(model_p_mixed, dfa)
# Step 3: Compute the difference v(t+1) - v(t) and (y(t) - v(t))
dfa$delta_v <- (dfa$slider_valenc.response - dfa$lagged_valence)  # v(t) - v(t-1)
dfa$delta_e <- (dfa$slider_effort.response - dfa$lagged_effort)
dfa$delta_pv <- dfa$p_estimated - dfa$score  # y(t) - s(t-1)
dfa$delta_p <- (dfa$score - dfa$lagged_score) # s(t) - s(t-1)
dfa$delta_sp <- dfa$score - dfa$p_estimated  # s(t) - y(t)
dfa$delta_sv <- dfa$score - dfa$slider_valenc.response  # s(t) - y(t)
dfa$delta_p3 <- dfa$p3 - dfa$lagged_p3
dfa$delta_n2 <- dfa$n2 - dfa$lagged_n2

# Step 5: explaining RewP
dfa <- dfa[is.finite(dfa$p2) & is.finite(dfa$delta_p) & is.finite(dfa$delta_v), ]
model<- lmer( p2 ~ delta_p + delta_v + (1 | sub), data = subset(dfa,task == 'n-back'))
summary(model)
AIC(model)

#median_effort <- median(dfa$effort, na.rm = TRUE)
#dfa$effort_category_post <- ifelse(dfa$effort > median_effort, "High effort", "Low effort")

##FRN----

summary(lmer( p2 ~ 0 + outcome * slider_effort.response + block + n_back + (1|sub), subset(dfa, task == 'n-back')))

summary(lm( p2 ~ bc + wc, subset(dfa, task == 'n-back')))


##phenomenology-----
dfa <- mother_dfa
dfa <- dfa[!is.na(dfa$lagged_score), ]

dfa$valence_dt <- dfa$slider_valenc.response - lm(slider_valenc.response ~ 0 +  n_block + score  + lagged_score  , data = dfa)$fitted.values
dfa$arousal_dt <- dfa$slider_arous.response - lm(slider_arous.response ~ 0 + n_block + score + lagged_score , data = dfa)$fitted.values
dfa$effort_dt <- dfa$slider_effort.response - lm(slider_effort.response ~ 0 + n_block + score  + lagged_score , data = dfa)$fitted.values


dfa_nofail <- dfa[dfa$m_correct <= dfa$score, ] # Remove rows where m_correct > score

model <- lmer(effort_dt ~ arousal_dt*strategy + valence_dt*strategy  + (1|sub), data = dfa)
summary(model)

model <- lmer( effort_dt ~ 0 + free_choice*strategy  + (1|sub), data = dfa)
summary(model)




fig<-ggplot(dfa, aes(x = valence_dt, y = arousal_dt, color = effort_dt)) +
  geom_point() +
  labs(
    title = "Effect of Arousal and Valence on Effort for both tasks per participants",
    x = "Valence",
    y = "Arousal",
    color = "Effort"
  ) + 
  scale_color_gradient(low = "blue", high = "red") +
  geom_hline(yintercept = 0, color = "black") + # Horizontal line at y=0
  geom_vline(xintercept = 0, color = "black") + # Vertical line at x=0
  geom_circle(
    aes(x0 = 0, y0 = 0, r = 100), # Center (x0, y0) and radius (r)
    color = "black", linetype = "solid"
  ) +
  theme_minimal() + facet_wrap(~ sub, scales = "fixed")


ggsave(paste0("/Volumes/x9/results/allsubs/figure/phenomenology_sub.png"), plot = fig, width = 10, height = 6, dpi = 300)


plot_ly(dfa, x = ~arousal_dt, y = ~valence_dt, z = ~effort_dt,
        type = 'scatter3d', mode = 'markers',
        marker = list(size = 5, color = ~effort_dt, colorscale = 'Viridis')) %>%
  layout(title = "3D Plot of Effort by Arousal and Valence",
         scene = list(xaxis = list(title = "Arousal"),
                      yaxis = list(title = "Valence"),
                      zaxis = list(title = "Effort")))


##Table for strategy comparison -----
summary(model1 <- lmer(rt_h  ~ 0  + strategy  + (1|sub), data = dfa))

model1 <- lmer(pac1  ~ 0  + strategy  + (1|sub), data = subset(dfa,n_back==1))
model2 <- lmer(pac1 ~ 0 + strategy   + (1|sub), data = subset(dfa,n_back==2))
model3 <- lmer(pac1 ~ 0 + strategy  + (1|sub), data = subset(dfa,n_back==3))
model4 <- lmer(pac2  ~ 0  + strategy  + (1|sub), data = subset(dfa,n_back==1))
model5 <- lmer(pac2 ~ 0 + strategy   + (1|sub), data = subset(dfa,n_back==2))
model6 <- lmer(pac2 ~ 0 + strategy  + (1|sub), data = subset(dfa,n_back==3))


models <- list( model1,model2,model3,model4,model5,model6)
model_names <- c('1 t1','2 t1','3 t1','1 t2','2 t2','3 t2')

visu_lmr_combined(models,model_names)

model <- lmer(slider_valenc.response ~ 0 + strategy + (1|sub), dfa)
AIC(model)
summary(model)

## performance-----
summary(lmer(g ~  slider_effort.response*strategy + n_back + n_block + (1|sub), subset(dfa,task=='ax-cpt')))
summary(lmer(g ~  slider_effort.response*strategy + n_back + n_block + (1|sub), subset(dfa,task=='n-back')))


summary(lmer( g  ~  slider_effort.response + n_block + (1|sub), subset(dfa,strategy=='proactive')))
summary(lmer( g  ~  slider_effort.response + n_block + (1|sub), subset(dfa,strategy=='reactive')))

# divided by difficulty level
dfa_rolling <- subset(dfa,strategy=='rolling')
summary(lmer( g  ~  slider_effort.response + block + (1|sub), subset(dfa_rolling,n_back ==1)))
summary(lmer( g  ~  slider_effort.response + block + (1|sub), subset(dfa_rolling,n_back ==2)))
summary(lmer( g  ~  slider_effort.response + block + (1|sub), subset(dfa_rolling,n_back ==3)))


dfa_static <- subset(dfa,strategy=='static')
summary(lmer( g  ~  slider_effort.response + block + (1|sub), subset(dfa_static,n_back ==1)))
summary(lmer( g  ~  slider_effort.response + block + (1|sub), subset(dfa_static,n_back ==2)))
summary(lmer( g  ~  slider_effort.response + block + (1|sub), subset(dfa_static,n_back ==3)))

dfa_proactive <- subset(dfa,strategy=='proactive')
summary(lmer( g  ~  slider_effort.response + n_block + (1|sub), subset(dfa_proactive,n_back ==1)))
summary(lmer( g  ~  slider_effort.response + n_block + (1|sub), subset(dfa_proactive,n_back ==2)))
summary(lmer( g  ~  slider_effort.response + n_block + (1|sub), subset(dfa_proactive,n_back ==3)))


dfa_reactive <- subset(dfa,strategy=='reactive')
summary(lmer( g  ~  slider_effort.response + n_block + (1|sub), subset(dfa_reactive,n_back ==1)))
summary(lmer( g  ~  slider_effort.response + n_block + (1|sub), subset(dfa_reactive,n_back ==2)))
summary(lmer( g  ~  slider_effort.response + n_block + (1|sub), subset(dfa_reactive,n_back ==3)))

##P3a and P3b -----------------
summary(lmer( p3a ~  slider_effort.response*strategy + n_back + n_block + (1|sub), subset(dfa,task=='ax-cpt')))
summary(lmer( p3a ~ slider_effort.response*strategy + n_back + n_block + (1|sub) , subset(dfa,task=='n-back')))

summary(lmer( p3b ~  slider_effort.response*strategy + n_back + n_block + (1|sub), subset(dfa,task=='ax-cpt')))
summary(lmer( p3b ~  slider_effort.response*strategy + n_back + n_block + (1|sub), subset(dfa,task=='n-back')))


summary(lmer( p3a ~  slider_effort.response*strategy + n_block + (1|sub), dfa))
summary(lmer( p3b ~  slider_effort.response*strategy + n_block + (1|sub), dfa))

##n2s -----------------
summary(lmer( n2s ~  slider_effort.response*strategy + n_back + n_block + (1|sub), subset(dfa,task=='n-back')))
summary(lmer( n2s ~  score*strategy + n_back + n_block + (1|sub), subset(dfa,task=='n-back')))

summary(lmer( n2s ~  slider_effort.response*strategy + n_back + n_block + (1|sub), subset(dfa,task=='ax-cpt')))
summary(lmer( n2s ~  score*strategy + n_back + n_block + (1|sub), subset(dfa,task=='ax-cpt')))

##pe -----------------
summary(lmer(  pe ~ score  + n_back + n_block  + (1|sub), subset(dfa,task=='n-back')))
summary(lmer(  pe ~  score + n_back + n_block  + (1|sub), subset(dfa,task=='ax-cpt')))

summary(lmer( pe ~ score + n_back + n_block + (1|sub), dfa))

##ern -----------------
summary(lmer( ern  ~ score  + n_back + n_block + (1|sub), subset(dfa,task=='n-back')))
summary(lmer(  ern ~  score + n_back + n_block  + (1|sub), subset(dfa,task=='ax-cpt')))

summary(lmer( ern ~  score + (1|sub), dfa))

##ps----
model <- lmer(slider_effort.response ~ I(pupil_stim-pupil_rest/pupil_rest) + (1+strategy_numeric|sub), dfa)
AIC(model)
summary(model)
summary(lmer( ps1  ~  slider_effort.response + (1|sub), dfa)) #positive
summary(lmer( ps2  ~  slider_effort.response + (1|sub), dfa)) #positive
summary(lmer( delta_ps  ~  slider_effort.response + (1|sub), dfa)) #negative

##ms----
dfa <- mother_dfa
model <- lmer( n2 ~ ms1 + ms2 + (1|sub), dfa)
AIC(model)
summary(model)


dfa$ms_ratio <- (dfa$microsacc_stim - dfa$microsacc_rest)/dfa$microsacc_rest
dfa$ms_ratio[!is.finite(dfa$ms_ratio)] <- NA

model <- lmer( ms_ratio ~  slider_effort.response + (1|sub), data = dfa[-nrow(dfa), ])
summary(model)
AIC(model)
summary(lmer( ms1 ~  slider_effort.response + (1|sub), dfa)) #positive
summary(lmer( ms2 ~  slider_effort.response + (1|sub), dfa)) #positive

summary(lmer( ms2 - ms1 ~ slider_effort.response*strategy + (1|sub), dfa))

summary(lmer( delta_ms ~ delta_pac*strategy + (1|sub), dfa))

##pac----

fig <- ggplot(dfa, aes(x = slider_valenc.response, y = score, color = strategy)) +
  stat_smooth(aes(group = factor(strategy)), method = "lm", formula = y ~ poly(x, 1), se = TRUE) +
  theme_minimal() +
  labs(x = "Pupil Size", y = "Delta PAC", color = "Strategy") +
  theme(legend.position = c(0, 0), legend.justification = c(0, 0),
    axis.title = element_text(size = 18),    # Increase axis title font size
    axis.text = element_text(size = 18),      # Increase axis labels font size
    legend.title = element_text(size = 18),   # Increase legend title font size
    legend.text = element_text(size = 18),    # Increase legend text font size
    strip.text = element_text(size = 18)) + facet_wrap(~task, scales = 'fixed')
fig

ggsave(filename = "/Volumes/x9/results/allsubs/figure/ps~pac2-pac1.png",  # The name of the output file
       plot = fig,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300)   


# reported effort

summary(lmer( pac3 ~ slider_effort.response*strategy + n_block + n_back + (1 | sub), data = subset(dfa,task=="ax-cpt")))
summary(lmer( pac3 ~ slider_effort.response*strategy + n_block + n_back + (1 | sub), data = subset(dfa,task=="n-back")))

summary(lmer( pac3_high ~ slider_effort.response*strategy + n_block + n_back + (1 | sub), data = subset(dfa,task=="ax-cpt")))
summary(lmer( pac3_high ~ slider_effort.response*strategy + n_block + n_back + (1 | sub), data = subset(dfa,task=="n-back")))

summary(lmer( pac2 - pac1 ~ slider_effort.response*strategy + n_block + n_back + (1 | sub), data = subset(dfa,task=="ax-cpt")))
summary(lmer( pac2 - pac1 ~ slider_effort.response*strategy + n_block + n_back + (1 | sub), data = subset(dfa,task=="n-back")))


plot_model(model1, type = "int", show.data = FALSE) +  # Effect plot for fixed effects
  theme_minimal(base_size = 14) +
  labs(title = "N-back",
       x = "Reported effort",
       y = "Predicted PAC") +
  theme(legend.position = "top")

plot_model(model2, type = "int", show.data = FALSE) +  # Effect plot for fixed effects
  theme_minimal(base_size = 14) +
  labs(title = "AX-CPT",
       x = "Reported effort",
       y = "Predicted PAC") +
  theme(legend.position = "top")


# reported effort
summary(lmer(  score ~ pac3*strategy + n_block + n_back + (1 | sub), data = subset(dfa,task=="ax-cpt")))
summary(lmer(  score ~ pac3*strategy + n_block + n_back + (1 | sub), data = subset(dfa,task=="n-back")))

summary(lmer(   pac3 ~ g + strategy + (1 | sub), data = dfa))
summary(lmer(   g ~ pac2 + pac1 + strategy + (1 | sub), data = dfa))

summary(lmer(  g ~ pac2 + pac1 + (1 | sub), data = subset(dfa,task=="n-back")))



#strategy
model <- lmer( pac3 ~ 0 + strategy + n_block + n_back + (1 | sub), data = dfa)
summary(model)
model <- lmer( pac3_low ~ 0 + strategy + n_block + n_back + (1 | sub), data = dfa)
summary(model)
model <- lmer( pac3_high ~ 0 + strategy + n_block + n_back + (1 | sub), data = dfa)
summary(model)

model <- lmer( pac3_high ~ slider_effort.response*strategy + n_block + (1 | sub), data = dfa)
summary(model)

##frequencies-----

summary(lmer(  I(theta/beta) ~ slider_effort.response*strategy + block + n_back + (1|sub), subset(dfa,task=='n-back')))
summary(lmer(  I(theta/beta) ~ slider_effort.response*strategy + block + n_back + (1|sub), subset(dfa,task=='ax-cpt')))

summary(lmer(  alpha ~ slider_effort.response*strategy + block + n_back + (1|sub), subset(dfa,task=='n-back')))
summary(lmer(  alpha ~ slider_effort.response*strategy + block + n_back + (1|sub), subset(dfa,task=='ax-cpt')))

summary(lmer(  onef ~ slider_effort.response*strategy + block + n_back + (1|sub), subset(dfa,task=='n-back')))
summary(lmer(  onef ~ slider_effort.response*strategy + block + n_back + (1|sub), subset(dfa,task=='ax-cpt')))

summary(lmer(  pac3_low ~ slider_effort.response*strategy + n_block + n_back + (1|sub), subset(dfa,task=='n-back')))
summary(lmer(  pac3_low ~ slider_effort.response*strategy + n_block + n_back + (1|sub), subset(dfa,task=='ax-cpt')))

summary(lmer(  pac3_low ~ slider_effort.response*strategy + n_block + n_back + (1|sub), dfa))


summary(lmer(  pac3_high ~ slider_effort.response*strategy + n_block + n_back + (1|sub), subset(dfa,task=='n-back')))
summary(lmer(  pac3_high ~ slider_effort.response*strategy + n_block + n_back + (1|sub), subset(dfa,task=='ax-cpt')))

##working memory capacity------
summary(lm(free_choice ~ bc  + wc  , dfa_nofail))


summary(lmer(slider_effort.response ~ bc + wc + (1|n_block) + (1|sub), dfa))
summary(lmer(g ~ bc * strategy + wc * strategy + (1|n_block) + (1|sub), dfa))
summary(lmer(ps_norm ~ bc * strategy + wc * strategy + (1|n_block) + (1|sub), dfa))

library(ez)
anova_model <- ezANOVA(
  data = dfa_nofail,
  dv = free_choice,
  wid = sub,
  within = strategy,
  between = bc,
  detailed = TRUE
)
anova_model

## time -------
summary(lmer(  pac3_low ~ slider_time.response*strategy + block + n_back + (1|sub), subset(dfa,task=='n-back')))
summary(lmer(  pac3_low ~ slider_time.response*strategy + block + n_back + (1|sub), subset(dfa,task=='ax-cpt')))

summary(lmer(  pac3_high ~ slider_time.response*strategy + block + n_back + (1|sub), subset(dfa,task=='n-back')))
summary(lmer(  pac3_high ~ slider_time.response*strategy + block + n_back + (1|sub), subset(dfa,task=='ax-cpt')))

summary(lmer(  slider_time.response ~ pac3*strategy + block + n_back + (1|sub), subset(dfa,task=='n-back')))
summary(lmer(  slider_time.response ~ pac3*strategy + block + n_back + (1|sub), subset(dfa,task=='ax-cpt')))

summary(lmer(  slider_time.response ~ I(pac2-pac1)*strategy + block + n_back + (1|sub), subset(dfa,task=='n-back')))
summary(lmer(  slider_time.response ~ I(pac2-pac1)*strategy + block + n_back + (1|sub), subset(dfa,task=='ax-cpt')))


## RT ------

summary(lmer( rt_h ~ slider_effort.response*strategy + n_back + n_block + (1|sub), subset(dfa,task=='ax-cpt')))
summary(lmer( rt_h ~ slider_effort.response*strategy + n_back + n_block + (1|sub), subset(dfa,task=='n-back')))

summary(lmer( rt_h ~ p3b*strategy + n_back + block + (1|sub), subset(dfa,task=='ax-cpt')))
summary(lmer( rt_h ~ p3b*strategy + n_back + block + (1|sub), subset(dfa,task=='n-back')))

summary(lmer( rt_h ~ ps_norm*strategy + n_back + block + (1|sub), subset(dfa,task=='ax-cpt')))
summary(lmer( rt_h ~ ps_norm*strategy + n_back + block + (1|sub), subset(dfa,task=='n-back')))

summary(lmer( rt_h ~ ms_norm*strategy + n_back + block + (1|sub), subset(dfa,task=='ax-cpt')))
summary(lmer( rt_h ~ ms_norm*strategy + n_back + block + (1|sub), subset(dfa,task=='n-back')))


## pupil size ------

summary(lmer( ps_norm ~ slider_effort.response*strategy + n_back + block + (1|sub), subset(dfa,task=='ax-cpt')))
summary(lmer( ps_norm ~ slider_effort.response*strategy + n_back + block + (1|sub), subset(dfa,task=='n-back')))

#Kernel density of parameters------------------------------------ 

dfa_long <- tidyr::gather(dfa, key = "variable", value = "value", 
                          slider_valenc.response, slider_arous.response, 
                          slider_effort.response)

gg <- ggplot(dfa_long, aes(x = value, fill = variable))
gg <- gg + geom_density(alpha = 0.5)
#gg <- gg + xlim(0, 100)
# Facet by both sub and n_back
gg <- gg + facet_wrap(~strategy, scales = "fixed")
gg <- gg + labs(title = paste0("Kernel Density Estimates ",s),
                x = "Value",
                y = "Density")
print(gg)
ggsave(paste0("/Volumes/x9/results/allsubs/figure/density_all.png"), plot = gg, width = 10, height = 6, dpi = 300)

grouped_stats <- dfa %>%
  group_by(strategy) %>%
  summarise(
    mean = mean(slider_effort.response, na.rm = TRUE),
    median = median(slider_effort.response, na.rm = TRUE),
    sd = sd(slider_effort.response, na.rm = TRUE),
    var = var(slider_effort.response, na.rm = TRUE),
    skewness = skewness(slider_effort.response, na.rm = TRUE),
    kurtosis = kurtosis(slider_effort.response, na.rm = TRUE),
    min = min(slider_effort.response, na.rm = TRUE),
    max = max(slider_effort.response, na.rm = TRUE),
    range = max(slider_effort.response, na.rm = TRUE) - min(slider_effort.response, na.rm = TRUE),
    n = sum(!is.na(slider_effort.response)),
    n_na = sum(is.na(slider_effort.response))
  )
print(grouped_stats)

# Calculate the mean of slider_effort.response by strategy
grouped_stats <- dfa %>%
  group_by(strategy) %>%
  summarise(strategy_mean = mean(slider_effort.response, na.rm = TRUE)) %>%
  ungroup()

# Now, join grouped_stats with dfa by strategy and center responses
dfa <- dfa %>%
  left_join(grouped_stats, by = "strategy") %>%
  group_by(sub) %>%
  mutate(
    # Center by the strategy mean
    effort_centered = slider_effort.response - strategy_mean
  ) %>%
  ungroup()

dfa$strategy_numeric <- ifelse(
  dfa$strategy %in% c("rolling", "static"), 1,
  ifelse(dfa$strategy %in% c("proactive", "reactive"), 0, NA)
)

# Calculate Spearman correlation within each subset of dfa$sub
result <- subset(dfa,task=='n-back') %>%
  group_by(sub) %>%
  summarize(
    corr = cor(slider_effort.response, n_back, method = "spearman", use = "complete.obs")
  )

mean_corr <- mean(result$corr, na.rm = TRUE)
hist(result$corr, 
     main = "Distribution of subjective correlations in the n-back task",  # Title
     xlab = "Correlation between self-reported effort and difficulty level",                 # X-axis label
     ylab = "Frequency",                          # Y-axis label
     col = "grey",                                # Bar color
     border = "black")                            # Border color

print(mean_corr)

#n_back --------------------------- 
dfa$valence_dt <- dfa$slider_valenc.response - lm(slider_valenc.response ~  0 + n_block + score  , data = dfa)$fitted.values
dfa$valence_dt <-minmax_normalize(dfa$valence_dt)

y_values <- c('microsacc','ps_norm','ms_norm','delta_ps_ms','theta','alpha','beta','theta_beta','gamma','pac1','pac2','diff_pac','p3','p3_norm','p2','p2_norm','n2','n2_norm','time',"effort","valence", "dprime",'g','rt_h','ps1','ps2','ms1','ms2','pac1','pac2','delta_ps','delta_ms','delta_pac')

y_values <- c('slider_valenc.response','slider_time.response','slider_effort.response','g','pac1','pac2','pac1_low','pac2_low','pac1_high','pac2_high','ms1','ms2','delta_ms','delta_pac')

y_values <- c('microsacc')

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

x_values <- c("effort_dt")
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
    ) + theme_minimal() + facet_wrap(~ strategy, scales = "fixed")
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
  ggsave(filename = file_path, plot = phenom, width = 10, height = 6, dpi = 300)
}


#performance ----------------------------- 

# Define x_values and y_values
x_values <- c("effort",'p3','n2','ps_norm','microsacc','ms_norm','rt_h','diff_ps_ms','diff_pac')
y_values <- c("score","dprime",'g', "time", "rt_h", "effort") 

x_values <- c("slider_effort.response")
y_values <- c("g") 

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
    perform <- ggplot(dfa, aes_string(x = x_val, y = y_val, col = "factor(n_back)",alpha=0.1)) +
      stat_smooth(aes_string(group = "factor(n_back)"), method = "lm", formula = "y ~ poly(x, 2)", se = FALSE) +
      geom_smooth(method = "lm", se = TRUE, color = "black") + # Add regression line for all data
      geom_point() +
      facet_wrap(~ task, scale="free") +
      theme_minimal() +
      scale_color_manual(values = c("blue", "green", "red"))#+labs(caption = paste("p-value 1:", round(p_values[,2][1], 3),"p-value 2:", round(p_values[,2][2], 3),"p-value 3:", round(p_values[,2][3], 3),"p-value total:", round(p_value_total, 3))) 
        
    
    # Print the plot
    print(perform)
    
    # Construct the file path to include both y_val and x_val
    file_path <- file.path("/Volumes/x9/results/allsubs/figure/", paste( "performance_",y_val, "_vs_", x_val, "by_n_back.png", sep = ""))
    
    # Save the plot
    ggsave(filename = file_path, plot = perform, width = 10, height = 6, dpi = 300)
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

x_values <- c('pac3','ps_norm','microsacc','ms_norm')
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

# leontief ------------------
#set the difficulty level
dfa_nback_subset <- subset(dfa_nback, n_back>1)
# Categorize slider_effort.response into bins of 10
dfa_nback_subset$effort_category <- cut(
  dfa_nback_subset$slider_effort.response,
  breaks = seq(floor(min(dfa_nback_subset$slider_effort.response, na.rm = TRUE)), 
               ceiling(max(dfa_nback_subset$slider_effort.response, na.rm = TRUE)), 
               by = 10),
  include.lowest = TRUE,
  right = FALSE
)

# Create meaningful labels for the effort categories
levels(dfa_nback_subset$effort_category) <- paste0(
  seq(floor(min(dfa_nback_subset$slider_effort.response, na.rm = TRUE)), 
      ceiling(max(dfa_nback_subset$slider_effort.response, na.rm = TRUE)) - 10, by = 10),
  "-", 
  seq(floor(min(dfa_nback_subset$slider_effort.response, na.rm = TRUE)) + 10, 
      ceiling(max(dfa_nback_subset$slider_effort.response, na.rm = TRUE)), by = 10)
)

# Plotting the categorized data
ggplot(data = dfa_nback_subset, aes(x = effort_category, y = strategy)) +
  geom_point(aes(color = g), size = 2, position = position_jitter(width = 0.2)) +  # Add jitter to avoid overplotting
  labs(x = "Effort Category (Slider Response)", y = "Strategy", color = "G Value") + 
  theme_minimal() +  # Optional: adds a clean theme
  theme(legend.position = "right")  # Adjust legend position if necessary

# Create a base plot
p <- ggplot(data = dfa_nback_subset, aes(x = effort_category, y = strategy)) +
  geom_point(aes(color = g), size = 2, position = position_jitter(width = 0.2)) +  # Add jitter to avoid overplotting
  labs(x = "Effort Category (Slider Response)", y = "Strategy", color = "G Value") + 
  theme_minimal() +  # Optional: adds a clean theme
  theme(legend.position = "right")  # Adjust legend position if necessary

# Adding horizontal lines for each strategy level
p <- p + geom_hline(yintercept = unique(dfa_nback_subset$strategy), linetype = "dotted", color = "grey")

# Adding vertical lines for each effort category level
p <- p + geom_vline(xintercept = unique(dfa_nback_subset$effort_category), linetype = "dotted", color = "grey")

# Display the plot
print(p)

# Calculate the mean of g to determine the threshold
g_threshold <- mean(dfa_nback_subset$g, na.rm = TRUE)

# Create a new categorical variable for good and bad
dfa_nback_subset$g_category <- ifelse(dfa_nback_subset$g >= g_threshold, "Good", "Bad")

# Create a base plot
p <- ggplot(data = dfa_nback_subset, aes(x = effort_category, y = strategy, color = g_category)) +
  geom_point(aes(size = g), position = position_jitter(width = 0.2), alpha = 0.6) +  # Add jitter and transparency
  labs(x = "Effort", y = "Strategy", color = "G Category", size = "G Value") + 
  theme_minimal() +  # Optional: adds a clean theme
  theme(legend.position = "right") + geom_point(data = average_g2, aes(x = avg_g/10, y = strategy), color = "red", size = 3, shape = 17)+
  geom_point(data = average_g3, aes(x = avg_g/10, y = strategy), color = "red", size = 3, shape = 17)

# Adding horizontal lines for each strategy level
p <- p + geom_hline(yintercept = unique(dfa_nback_subset$strategy), linetype = "dotted", color = "grey")

# Adding vertical lines for each effort category level
p <- p + geom_vline(xintercept = unique(dfa_nback_subset$effort_category), linetype = "dotted", color = "grey")

# Display the plot
print(p)

# Save the plot to a file
ggsave("/Users/ali/Desktop/Figures_/leontief_data_2_3.png", plot = p, width = 10, height = 6, dpi = 300)


#