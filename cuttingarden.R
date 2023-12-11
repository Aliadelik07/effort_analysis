library(readr)
library("readxl")
library(ggrepel)
require(ggplot2)
library(investr)
library(dplyr)

df <- read_csv("/Users/ali/Desktop/Experiment/sub01/Sub01-nbackRolling-test.csv")

lm <- lm(p_size ~ poly(effort), data = subset(df,n_back<3))

summary(lm)

phenom <- ggplot(subset(df,-1<score)) + 
  geom_point(aes(x = effort, y = arousal, color = "Arousal")) + 
  geom_smooth(aes(x = effort, y = arousal, color = "Arousal"), method = "lm", se = TRUE) +
  
  geom_point(aes(x = effort, y = valence, color = "Valence")) +
  geom_smooth(aes(x = effort, y = valence, color = "Valence"), method = "lm", se = TRUE) +
  
  labs(
    title = "Phenomenal Effort",
    x = "Effort report",
    y = "Arousal and Valence"
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

perform <- ggplot(subset(df,n_back>0), aes(x = dpsize_mean, y = effort, color = factor(round(n_back)))) +
  stat_smooth(aes(group = factor(round(n_back))), method = "lm", formula = y ~ x,  se = FALSE) +
  geom_point()+ labs(title = "Perfromance and Effort",
              x = "Effort Self-Report",
              y = "Pupil size",
              color = "AX")+
  theme(
    legend.position = c(.15, .35),
    legend.justification = c("right", "top"),
    legend.box.just = "right"
  )
perform

ggplot(df, aes(n_block, valence)) +    
  geom_point() + stat_smooth(method = "lm",formula = y ~ x) + 
  labs(title = "valence and task progress",
       x = "block",
       y = "valence")

ggplot(subset(dfp,1<n_back), aes(effort, mean_pupil_size)) +    
  geom_point() + stat_smooth(method = "lm",formula = y ~ x) + 
  labs(title = "Pupil Size and Effort",
       x = "Effort Self-Report",
       y = "Mean Pupil Size")


sub_n = "nback-04"
dfe <- read_excel("/Users/ali/Desktop/Experiment/Preliminary result.xlsx", sheet = sub_n)


ggplot(subset(dfe,AUC<0.0000000001), aes(x = AUC, y = effort)) +
  geom_point() +
  stat_smooth(method = "lm",formula = y ~ x )+ 
  labs(title = "Effort and Power Spectrum Density",
       x = "Area Under the Curve of Power Spectrum Density",
       y = "Effort Self-Report")

ggplot(subset(dfe,0<TBR_std), aes(x = ao_mean, y = effort, color = factor(round(n_back)))) +
  stat_smooth(aes(group = factor(round(n_back))), method = "lm", se = FALSE) +
  geom_point()+
  labs(title = "Effort and Parietal Alpha",
              x = "Mean Amplitude of Parietal Alpha",
              y = "Effort Self-Report",
              color = "Load")+
  theme(
    legend.position = c(.12, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right"
  )
