# Modules -------
library(dplyr)
library(tidyr)
library(tidyverse)
library(mediation)
library(purrr)
library(ggplot2)
library(lme4)
library(lmerTest) 
library(dplyr)
library(purrr)
library(effects)


# combining all dfs -------
# Define file paths
behave_dir <- '/Volumes/x9/INITIAL_DATABASE/dfa-trials-behave.csv'
pupil_dir  <- '/Volumes/x9/INITIAL_DATABASE/dfa-trials-pupil-2s.csv'
pac_dir    <- '/Volumes/x9/INITIAL_DATABASE/dfa-trials-pac-2s.csv'

# Read CSV files into data frames
df_behave <- read_csv(behave_dir)
df_behave <- df_behave[, !names(df_behave) %in% "task"]

df_behave$n_back <- as.numeric(gsub(",", ".", df_behave$n_back))
df_behave$trial <- as.numeric(gsub(",", ".", df_behave$trial))
df_behave$n_block <- as.numeric(gsub(",", ".", df_behave$n_block))
df_behave <- df_behave[!is.na(df_behave$trial), ]
df_behave$trial <- df_behave$trial + 1
df_behave$sub <- as.numeric(gsub("sub", "", df_behave$sub))


df_pupil <- read_csv(pupil_dir)
df_pupil$sub <- as.numeric(gsub("sub", "", df_pupil$sub))

df_behave$trial <- df_behave$trial + 1
df_pac <- read_csv(pac_dir)
df_pac <- df_pac %>% rename(n_block = block)
df_pac$n_block <- df_pac$n_block - 1
#df_pac <- df_pac %>% mutate(sub = sprintf("sub%02d", as.integer(sub)))


# Now join
df_all <- df_behave %>%
  left_join(df_pupil, by = c("sub", "strategy", "n_block", "trial")) %>%
  left_join(df_pac,   by = c("sub", "strategy", "n_block", "trial"))

write.csv(df_all, file = "/Volumes/x9/INITIAL_DATABASE/dfa-trials-2s.csv", row.names = FALSE)

#importing dfa-trials -------

df_all<- read_csv("/Volumes/x9/INITIAL_DATABASE/dfa-trials-2s.csv")






# outliers ------------------


na1 <- sum(is.na(df_all))
repeat {
  # Initialize a variable to track if NAs were added
  previous_na_count <- sum(is.na(df_all))
  
  # Iterate through each column of dfa
  for (col in names(df_all)) {
    # Check if the column is numeric
    if (is.numeric(df_all[[col]])) {
      # Replace outliers with NA for the current column
      df_all[[col]][abs(scale(df_all[[col]])) >= 3] <- NA
    }
  }
  
  # Check if no more NAs were added
  current_na_count <- sum(is.na(df_all))
  
  # Exit the loop if no new NAs were added
  if (current_na_count == previous_na_count) {
    break
  }
}

na2 <- sum(is.na(df_all))

print((na2 - na1)/(nrow(df_all)*ncol(df_all)))


# bootstraping ------------------

# Step 1: Split into groups
df_hit <- df_all %>% filter(feedback == "hit")
df_fa  <- df_all %>% filter(feedback == "false_alarm")
df_miss  <- df_all %>% filter(feedback == "miss")
df_cr  <- df_all %>% filter(feedback == "correct_rejection")

# Step 2: Get how many 'hit' trials exist
n_hit <- nrow(df_hit)

# Step 3: Bootstrap 'false_alarm' to match number of 'hit'
df_fa_boot <- df_fa %>%
  slice_sample(n = n_hit, replace = TRUE)

df_miss_boot <- df_miss %>%
  slice_sample(n = n_hit, replace = TRUE)

df_cr_boot <- df_cr %>%
  slice_sample(n = n_hit, replace = TRUE)

# Step 4: Combine back with original 'hit'
df_balanced <- bind_rows(
  df_hit,
  df_fa_boot
)





# plot -------
df_balanced  <- df_balanced [df_balanced$strategy %in% c("rolling", "static"), ]

ggplot(df_balanced, aes(x = factor(n_back), y = avg_ps, color = factor(feedback))) +
  stat_summary(fun = mean, geom = "point", size = 3, 
               position = position_dodge(width = 0.4)) +  # mean points with dodge
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.3, 
               position = position_dodge(width = 0.4)) +  # error bars with dodge
  facet_wrap(~strategy, scales = "fixed") +
  scale_color_manual(
    #values = c("hit" = "forestgreen", "false_alarm" = "#d10f90" ),
    values = c("hit" = "forestgreen", "miss" = "firebrick", "false_alarm" = "#d10f90" ),
    #labels = c( "False Alarm","Hit", "Miss")
  ) +
  labs(
    x = "Difficulty Level",
    y = "Pupil Size (%)",
    title = " ",
    color = " "  # legend title
  ) + scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
  #theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 16, margin = margin(t = 10)),
    axis.title.y = element_text(size = 16, margin = margin(r = 10)),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 16),
    legend.position = "top",
    legend.direction = "horizontal"
  )



#logestic model --------------

df_balanced$feedback <- ifelse(df_balanced$feedback == "hit", 1, 0)


# pac_f 
model <- glmer(feedback ~ 0 + pac_f : avg_ps +   n_back : strategy + n_block + (1|sub) , data = df_balanced, family = binomial)
summary(model)


# ms_offset
model <- lmer(ms_offset ~ feedback + n_back + (1|sub), data = df_balanced)
summary(model)
model <- lmer(ms_offset ~ n_back + responseTime+ (1|sub) , data = df_balanced)
summary(model)

# avg_ms
model <- lmer(avg_ms ~ 0 + n_back : strategy + (1|sub) , data = subset(df_balanced, feedback=='hit'))
summary(model)


ggplot(subset(df_balanced, feedback == 'hit'), 
       aes(x = n_back, y = avg_ms, color = strategy)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  #stat_summary(fun.data = mean_se, geom = "errorbar", size = 0.3) +
  scale_color_manual(values = c("rolling" = "#d60f60" , "static" = "#1b4e77"))+
  labs(title = "",
       x = "Difficulty Level", y = "Microsaccade Rate") +
  theme(legend.position = "top",
        legend.direction = "horizontal")




# mediation analysis ------------------

# Mediator model: pac_f ~ avg_ps
med.fit <- lm(pac_f ~ avg_ps*strategy, data = df)

# Outcome model: feedback ~ avg_ps + pac_f
out.fit <- glm(feedback ~ avg_ps*strategy + pac_f*strategy, data = df, family = binomial(link = "logit"))

# Outcome model: feedback ~ avg_ps + pac_f
out.fit <- glm(feedback ~ avg_ps *strategy + pac_f *strategy, data = df, family = binomial(link = "logit"))

med.out <- mediate(med.fit, out.fit,
                   treat = "pac_f" ,
                   mediator = "avg_ps",
                   boot = TRUE, sims = 100)  # bootstrapping for robustness

summary(med.out)

# dprime score -----------------------

# Define a function to compute d-prime
compute_dprime <- function(hits, misses, fa, cr) {
  # Calculate hit rate and false alarm rate with log-linear correction
  hit_rate <- (hits + 0.5) / (hits + misses + 1)
  fa_rate  <- (fa + 0.5) / (fa + cr + 1)
  
  # Apply z-transform (qnorm is inverse CDF of normal)
  d_prime <- qnorm(hit_rate) - qnorm(fa_rate)
  return(d_prime)
}



df_dprime <- df_all %>%
  group_by(sub, strategy, n_back, n_block) %>%
  summarise(
    hit = sum(feedback == "hit"),
    miss = sum(feedback == "miss"),
    false_alarm = sum(feedback == "false_alarm"),
    correct_rejection = sum(feedback == "correct_rejection"),
    max_score = max(score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    d_prime = pmap_dbl(
      list(hit, miss, false_alarm, correct_rejection),
      compute_dprime
    ),
    max_score = as.integer(max_score)
  )
ggplot(df_dprime, aes(x = d_prime, y = as.integer(max_score))) +
  geom_point(aes(color = strategy)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  labs(
    x = "d-prime",
    y = "Score",
    color = "Strategy",
    title = "d-prime vs. Score"
  ) +
  theme_minimal(base_size = 14) +  # Increase overall font size
  theme(
    legend.position = c(0.38, 0.98),         # Top-right corner inside plot
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = "white", color = "grey80"),
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12)
  )


# summary dprime
dprime_table <- df_dprime %>%
  group_by(strategy, n_back) %>%
  summarise(
    average_d_prime = mean(d_prime, na.rm = TRUE),
    .groups = "drop"
  )
print(dprime_table)
