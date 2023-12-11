#importing the reposonses cleaned by python code with the same name
library(readr)
library("readxl")
library(ggrepel)
require(ggplot2)
library(investr)

df1 <- read_csv("/Users/ali/Desktop/Experiment/sub01/sub01-rolling-test.csv")
df1$'Unnamed: 0' <- NULL
df1$sub <- "sub01"
df1$strategy <- "rolling"
df2 <- read_csv("/Users/ali/Desktop/Experiment/sub01/sub01-proactive-test.csv")
df2$sub <- "sub01"
df2$strategy <- "proactive"
df3 <- read_csv("/Users/ali/Desktop/Experiment/sub02/sub02-proactive-test.csv")
df3$sub <- "sub02"
df3$strategy <- "proactive"
df3$g <- 1
df4 <- read_csv("/Users/ali/Desktop/Experiment/sub02/sub02-rolling-test.csv")
df4$sub <- "sub02"
df4$strategy <- "rolling"

common_columns <- colnames(df1)
dfa <- bind_rows(lapply(list(df1, df2, df3, df4), select, common_columns))



phenom <- ggplot(subset(dfa,0<ps)) + 
  geom_point(aes(x = effort, y = arousal, color = "Arousal")) + 
  geom_smooth(aes(x = effort, y = arousal, color = "Arousal"), method = "lm", se = TRUE) +
  
  geom_point(aes(x = effort, y = valence, color = "Valence")) +
  geom_smooth(aes(x = effort, y = valence, color = "Valence"), method = "lm", se = TRUE) +
  
  labs(
    title = "Phenomenal effort",
    x = "effort",
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
  )

phenom

phenom <- ggplot(dfa) + 
  geom_point(aes(x = n_block, y = arousal, color = "Arousal")) + 
  geom_smooth(aes(x = n_block, y = arousal, color = "Arousal"), method = "lm", se = TRUE) +
  
  geom_point(aes(x = n_block, y = valence, color = "Valence")) +
  geom_smooth(aes(x = n_block, y = valence, color = "Valence"), method = "lm", se = TRUE) +
  
  labs(
    title = "Phenomenal task",
    x = "block number",
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
  )

phenom

g <- ggplot(dfa, aes(x = n_block, y = arousal, col = interaction(sub,strategy))) +
  stat_smooth(aes(group = interaction(sub,strategy)), method = "lm", se = FALSE) +
  geom_point()

g <- g + labs(title = "Phenomonal effort",
              x = "block number",
              y = "arousal",
              color = "Group")
g


g <- ggplot(subset(dfa,strategy=="rolling"), aes(x = effort, y = rt_h, col = factor(n_back))) +
  stat_smooth(aes(group = factor(n_back)), method = "lm", se = FALSE) +
  geom_point()

g <- g + labs(title = "Perfromance and effort",
              x = "Effort report",
              y = "RT",
              color = "Group")
g


g <- ggplot(subset(dfa,strategy=="rolling"), aes(x = ps, y = g, col = factor(n_back))) +
  stat_smooth(aes(group = factor(n_back)), method = "lm", se = FALSE) +
  geom_point()

g <- g + labs(title = "Effort and Performance",
              x = "Effort report",
              y = "Normlized dprime",
              color = "Group")
g


phenom <- ggplot(dfa) + 
  geom_point(aes(x = effort, y = arousal, color = "Arousal",col = interaction(sub, strategy) )) + 
  geom_smooth(aes(x = effort, y = arousal, color = "Arousal"), method = "lm", se = FALSE) +
  
  geom_point(aes(x = effort, y = valence, color = "Valence")) +
  geom_smooth(aes(x = effort, y = valence, color = "Valence"), method = "lm", se = TRUE) +
  
  labs(
    title = "Phenomenal effort",
    x = "effort",
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
  )

phenom

dfa <- dfa %>%
  mutate(strategy_sub = interaction(strategy, sub))



violin_plot <- ggplot(dfa, aes(x = strategy_sub, y = dps, fill = strategy_sub)) +
  geom_violin(trim = FALSE) +
  scale_fill_brewer(palette = "Set3") + 
  geom_boxplot(width = 0.1, fill = "white") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        legend.position = "none") +
  labs(
    title = "Mean pupil size by Strategy-Subject Interaction",
    x = "Strategy-Subject Interaction",
    y = "Mean pupil siz"
  )

violin_plot


# Perform the ANOVA
res.aov <- aov(effort ~ sub, data = dfa)
anova_summary <- summary(res.aov)

# Obtain the p-value
p_value <- anova_summary[["sub"]][["Pr(>F)"]][1]

# Check if the p-value is available and less than the significance level
if (!is.null(p_value) && p_value < 0.05) {
  # Since we have a significant effect, we perform a post-hoc test
  Tukey_test <- TukeyHSD(res.aov, which = "sub")
  
  # Create the violin plot
  violin_plot <- ggplot(dfa, aes(x = sub, y = effort)) +
    geom_violin(trim = FALSE) +
    geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
    labs(
      title = "Effort by Strategy-Subject Interaction",
      x = "Strategy-Subject Interaction",
      y = "Effort"
    )
  
  # Determine the position for the annotation based on the violins' position and the maximum effort value
  max_effort <- max(dfa$effort, na.rm = TRUE)
  
  # Assume we are annotating the significance between two specific groups, modify as needed
  group1 <- "group1"
  group2 <- "group2"
  
  signif_level <- ifelse(Tukey_test[["sub"]][group1 - group2, "p adj"] < 0.001, "***",
                         ifelse(Tukey_test[["sub"]][group1 - group2, "p adj"] < 0.01, "**",
                                ifelse(Tukey_test[["sub"]][group1 - group2, "p adj"] < 0.05, "*", "ns")))
  
  # Add the annotation to the plot
  violin_plot <- violin_plot +
    geom_text(aes(x = 1.5, y = max_effort, label = signif_level), vjust = -1)
  
  # Print the plot
  print(violin_plot)
  
} else {
  print("No significant differences found by ANOVA")
}