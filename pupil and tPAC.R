library(readr)
library("readxl")
library(ggrepel)
require(ggplot2)
library(investr)
library(dplyr)

n <- "02"
strategy <- "rolling"
tidy_ps <- read_csv(paste0("/Volumes/Effort data/subs/sub",n,"/sub",n,"-",strategy,"-test-pupil ERP per block.csv" ))
tidy_pac <- read_csv(paste0("/Volumes/Effort data/subs/sub",n,"/sub",n,"-",strategy,"-test- tPAC ERP per block.csv" ))


# removing negative time points of the baseline
tidy_ps <- tidy_ps[tidy_ps$position >= 0, ]
tidy_ps <- tidy_ps[tidy_ps$position < 300, ]

tidy_pac <- tidy_pac[tidy_pac$time >= 0, ]#tidy_pac$time[tidy_pac$time < 0] <- abs(tidy_pac$time[tidy_pac$time < 0] - 1.4)


set.seed(123) # Set seed for reproducibility
downsampled_tidy_ps <- tidy_ps[sample(nrow(tidy_ps), size = 420), ]

# Divide all values in the 'position' column by 200
downsampled_tidy_ps$position <- downsampled_tidy_ps$position / 200
# Round all values in the 'position' column to one decimal place
downsampled_tidy_ps$position <- round(downsampled_tidy_ps$position, digits = 1)


downsampled_tidy_ps <- downsampled_tidy_ps[order(downsampled_tidy_ps$effort, downsampled_tidy_ps$position), ]



ps_pac <- cbind(tidy_pac, downsampled_tidy_ps[c("avg_pupil_size")])
names(ps_pac)[names(ps_pac) == "avg_pupil_size"] <- "ps"


# Create scatter plot with grouped regression lines: 1 second lag is 15
ggplot(ps_pac, aes(x = pac, y = lag(ps,15),color = time))+
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "pac", y = "pupil_size") +
  theme_minimal()


summary(lm(lag(ps,15) ~ pac, data = ps_pac))


model <- lmer(lag(ps,15) ~ pac + (1 | block), data = ps_pac)
summary(model)
plot_resid_panel(model)
