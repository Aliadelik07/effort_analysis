#importing the reposonses cleaned by python code with the same name
library(readr)
library("readxl")
library(ggrepel)
require(ggplot2)
library(investr)

n <- "04"
strategy <- "static"


df <- read_csv(paste0("/Users/ali/Desktop/Experiment/sub",n,"/sub",n,"-",strategy,"-test.csv"))
df


phenom <- ggplot(subset(df, effort < 1)) + 
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

ggsave(filename = paste0("/Users/ali/Desktop/Experiment/sub",n,"/figures/sub",n,"-",strategy,"-effort with arousal and valence.png" ),  # The name of the output file
       plot = phenom,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300,                  # Resolution in dots per inch
       type = "cairo"              # This can ensure better quality on some systems
)

phenom <- ggplot(subset(df,-1<score)) + 
  geom_point(aes(x = n_block, y = arousal, color = "Arousal")) + 
  geom_smooth(aes(x = n_block, y = arousal, color = "Arousal"), method = "lm", se = TRUE) +
  
  geom_point(aes(x = n_block, y = valence, color = "Valence")) +
  geom_smooth(aes(x = n_block, y = valence, color = "Valence"), method = "lm", se = TRUE) +
  
  labs(
    title = "Phenomenal Task",
    x = "Number of blocks",
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
ggsave(filename = paste0("/Users/ali/Desktop/Experiment/sub",n,"/figures/sub",n,"-",strategy,"-n_block with arousal and valence.png" ),  # The name of the output file
       plot = phenom,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300,                  # Resolution in dots per inch
       type = "cairo"              # This can ensure better quality on some systems
)


#just seeing among n_backs
ggplot(df, aes(x = n_block, y = valence)) +geom_point(alpha = 0.8) + facet_wrap(~n_back)+
  # Show dots
  geom_text(
    label=round(df$rt_h,1), 
    nudge_x = 0.05, nudge_y = 0.05, 
    check_overlap = T
  )

ggplot(df, aes(x = n_block, y = valence, col=factor(round(rt_h,1)))) +
  stat_smooth(aes(group = factor(round(rt_h,1))), method = "lm", se = FALSE) + geom_point()


# regressing performance vs effort
lm <- lm(valence ~ score:n_back  , data = subset(df,valence<1.5)) #linear
summary(lm)
valence_dt <- df$valence - fitted(lm)

summary(lm(arousal ~ effort , data = subset(df,effort< 2)))

# Plotting regression of performance vs effort 
perform <- ggplot(subset(df,effort<1), aes(effort, g)) +    
  geom_point() + stat_smooth(method = "lm",se = TRUE,formula = y ~ poly(x,1)) +
  labs(
    title = "Performance and effort",
    x = "Effort report",
    y = "Normlized dprime"
  ) + 
  theme(
    legend.position = c(.2, .25),
    legend.justification = c("right", "top"),
    legend.box.just = "right"
  )

perform
ggsave(filename = paste0("/Users/ali/Desktop/Experiment/sub",n,"/figures/sub",n,"-",strategy,"-effort and g.png" ),  # The name of the output file
       plot = perform,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300,                  # Resolution in dots per inch
       type = "cairo"              # This can ensure better quality on some systems
)


# Plotting regression of performance vs effort 
perform <- ggplot(subset(df,effort <1), aes(effort, rt_h)) +    
  geom_point() + stat_smooth(method = "lm",se = TRUE,formula = y ~ poly(x,1)) +
  labs(
    title = "Performance and effort",
    x = "Effort report",
    y = "Response Time on Target trials"
  ) + 
  theme(
    legend.position = c(.2, .25),
    legend.justification = c("right", "top"),
    legend.box.just = "right"
  )

perform
ggsave(filename = paste0("/Users/ali/Desktop/Experiment/sub",n,"/figures/sub",n,"-",strategy,"-effort and RT.png" ),  # The name of the output file
       plot = perform,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300,                  # Resolution in dots per inch
       type = "cairo"              # This can ensure better quality on some systems
)

temporal <- ggplot(subset(df,effort<1), aes(effort, time)) +    
  geom_point() + stat_smooth(method = "lm",se = TRUE,formula = y ~ poly(x,1)) +
  labs(
    title = "Time perception and Effort",
    x = "Effort report",
    y = "Time report",
    color = "n back"
  ) + 
  theme(
    legend.position = c(.2, .4),
    legend.justification = c("right", "top"),
    legend.box.just = "right"
  )

temporal
ggsave(filename = paste0("/Users/ali/Desktop/Experiment/sub",n,"/figures/sub",n,"-",strategy,"-effort and timePerception.png" ),  # The name of the output file
       plot = temporal,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300,                  # Resolution in dots per inch
       type = "cairo"              # This can ensure better quality on some systems
)


fig <- ggplot(subset(df,effort<1), aes(effort, time)) +    
  geom_point() + stat_smooth(method = "lm",formula = y ~ poly(x,1)) #+ geom_text_repel(aes(label = n_block))
fig
ggsave(filename = paste0("/Users/ali/Desktop/Experiment/sub",n,"/figures/sub",n,"-",strategy,"- time and RT.png" ),  # The name of the output file
       plot = fig,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300,                  # Resolution in dots per inch
       type = "cairo"              # This can ensure better quality on some systems
)
ggplot(subset(df,effort<1), aes(effort, rt_h)) +    
  geom_point() + stat_smooth(method = "lm",formula = y ~ x) #+geom_text_repel(aes(label = round(valence,1))) #+ xlab("effort") + ylab("perfromance") 

g <- ggplot(subset(df,effort<1), aes(x = effort, y = rt_h, col=factor(n_back))) +
  stat_smooth(aes(group = factor(n_back)), method = "lm",formula = y ~ poly(x,1), se = FALSE) + geom_point()

g <- g + labs(title = "effort and performance",
              x = "Effort self-report",
              y = "Response Time",
              color = "n_back")
g

ggsave(filename = paste0("/Users/ali/Desktop/Experiment/sub",n,"/figures/sub",n,"-",strategy,"- time and RT with quality.png" ),  # The name of the output file
       plot = g,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300,                  # Resolution in dots per inch
       type = "cairo"              # This can ensure better quality on some systems
)

g <- ggplot(subset(df,0<score), aes(x = effort, y = score, col=factor(n_back))) +
  stat_smooth(aes(group = factor(n_back)), method = "lm",formula = y ~ poly(x,1), se = FALSE) + geom_point()

g <- g + labs(title = "effort and performance",
              x = "Effort self-report",
              y = "normlized dprime",
              color = "n_back")+ 
  theme(
    legend.position = c(.2, .4),
    legend.justification = c("right", "top"),
    legend.box.just = "right"
  )
g

ggsave(filename = paste0("/Users/ali/Desktop/Experiment/sub",n,"/figures/sub",n,"-",strategy,"- time and g with quality.png" ),  # The name of the output file
       plot = g,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300,                  # Resolution in dots per inch
       type = "cairo"              # This can ensure better quality on some systems
)



summary(lm (valence_dt ~ hit + false_alarm, df))

# regressing performance vs effort
lm <- lm(valence ~ score:n_back , data = df) #linear
summary(lm)
valence_dt <- df$valence - fitted(lm)

summary(lm(valence_dt ~ n_block , data = subset(df, 0 < n_back)))

g <- ggplot(df, aes(x = score, y = valence_dt, col=factor(n_back))) +
  stat_smooth(aes(group = factor(n_back)), method = "lm", se = FALSE) + geom_point()

g <- g + labs(title = "Valence and Performance",
              x = "Score",
              y = "Detrended valence",
              color = "load")+ 
  theme(
    legend.position = c(.2, .4),
    legend.justification = c("right", "top"),
    legend.box.just = "right"
  )
g

ggsave(filename = paste0("/Users/ali/Desktop/Experiment/sub",n,"/figures/sub",n,"-",strategy,"- valence and performance.png" ),  # The name of the output file
       plot = g,                    # The ggplot object you created
       width = 8, height = 6, unit = "in",  # Size of the output (can also be in cm or mm)
       dpi = 300,                  # Resolution in dots per inch
       type = "cairo"              # This can ensure better quality on some systems
)
