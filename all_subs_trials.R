# Load required libraries -------
library(tidyverse)
library(dplyr)
library(mediation)
library(purrr)
library(ggplot2)
library(lme4)
library(lmerTest) 

# combining all dfs -------
# Define file paths
behave_dir <- '/Volumes/x9/INITIAL_DATABASE/dfa-trials-behave.csv'
pupil_dir  <- '/Volumes/x9/INITIAL_DATABASE/dfa-trials-pupil-2s.csv'
pac_dir    <- '/Volumes/x9/INITIAL_DATABASE/dfa-trials-pac-2s.csv'

# Read CSV files into data frames
df_behave <- read_csv(behave_dir)
df_behave$n_back <- as.numeric(gsub(",", ".", df_behave$n_back))
df_behave$trial <- as.numeric(gsub(",", ".", df_behave$trial))
df_behave$n_block <- as.numeric(gsub(",", ".", df_behave$n_block))
df_behave <- df_behave[!is.na(df_behave$trial), ]
df_behave$trial <- df_behave$trial + 1

df_pupil <- read_csv(pupil_dir)

df_pac <- read_csv(pac_dir)
df_pac <- df_pac %>% rename(n_block = block)
df_pac <- df_pac %>%
  mutate(sub = sprintf("sub%02d", as.integer(sub)),
         n_block = as.numeric(gsub(",", ".", n_block)) - 1)


# Now join
df_all <- df_behave %>%
  left_join(df_pupil, by = c("sub", "strategy", "n_block", "trial")) %>%
  left_join(df_pac,   by = c("sub", "strategy", "n_block", "trial"))

write.csv(df_all, file = "/Volumes/x9/INITIAL_DATABASE/dfa-trials-2s.csv", row.names = FALSE)

#importing dfa-trials -------

df_all<- read_csv("/Volumes/x9/INITIAL_DATABASE/dfa-trials-2s.csv")


# violin plot -------


ggplot(subset(df_all, feedback == 'false_alarm'), aes(x = factor(n_back), y = pac_o_phase)) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5) +
  facet_wrap(~strategy, scales = "fixed") +
  labs(
    x = "Difficulty Level",
    y = "Occipital max MI - phase Frequency",
    title = "False alarm"
  ) +
  theme_minimal(base_size = 16) +  # sets base font size
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 16)
  )


# mixed model ---------------
df_all_n_back <- subset(df_all,task == 'n-back')
model <- lmer(pac_f ~ 0 +  avg_ps*strategy + n_block  + (1|sub), data = subset(df_all_n_back,feedback=='hit'))

summary(model)

ggplot(df_all, aes(x = avg_ps, y = ms_offset, color = as.factor(n_back))) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ strategy)


summary_df <- df_all %>%
  mutate(
    strategy = as.factor(strategy),
    n_back = as.factor(n_back),
    feedback = as.factor(feedback)
  ) %>%
  group_by(strategy, n_back, feedback) %>%
  summarise(
    mean = mean(pac_f, na.rm = TRUE),
    se = sd(pac_f, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

subset_df <- subset(summary_df, feedback %in% c("hit"))
ggplot(summary_df, aes(x = n_back, y = mean, color = feedback, group = feedback)) +
  geom_point(position = position_dodge(width = 0.4), size = 3) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(width = 0.4), width = 0.2) +
  facet_wrap(~ strategy) +
  theme_minimal() +
  labs(
    title = "mid-frontal",
    y = "Mean ± SE",
  ) #+ coord_cartesian(ylim = c(0.2, 0.21)) 


# Define a function that performs mediation analysis on a subset
run_mediation <- function(data_subset) {
  med_model <- lm(pac_f ~ avg_ps , data = data_subset)
  out_model <- lm(feedback_bin ~ avg_ps + pac_f, data = data_subset)
  
  mediate(med_model, out_model,
          treat = "avg_ps", mediator = "pac_f",
          boot = TRUE, sims = 1000)
}

# Apply the mediation analysis by strategy
results_by_strategy <- df_filtered %>%
  group_split(strategy) %>%
  set_names(levels(df_filtered$strategy)) %>%
  map(run_mediation)

# View summaries for each strategy
summary_list <- map(results_by_strategy, summary)

# Print summaries
for (name in names(summary_list)) {
  cat("\n\n--- Strategy:", name, "---\n")
  print(summary_list[[name]])
}

