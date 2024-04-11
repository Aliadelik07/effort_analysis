
#First create the dfa in all subs.R
#importing the reposonses cleaned by python code with the same name
library(readr)
library("readxl")
library(ggrepel)
require(ggplot2)
library(investr)
library(dplyr)
library(ggpubr)
library(lmerTest)
library(ggpmisc)
library(kableExtra)
library(webshot)
library(xtable)
library(broom.mixed)
library(knitr)
library(plotly)
library(gridExtra)
library(knitr)

dfa <- read_csv(paste0("/Users/ali/Desktop/Experiment/allsubs-nback-test.csv"))

# Create an empty dataframe to store the variances
df_sample_size <- data.frame(
  variable = c("Pupil_size", "Dprime","Alpha"),
  variance = numeric(3)
)

# variance of each variable
df_sample_size$variance[1] <- var(dfa$ps_norm[dfa$effort_category == "high effort"], na.rm = TRUE)
df_sample_size$variance[2] <- var(dfa$dprime[dfa$effort_category == "high effort"], na.rm = TRUE)
df_sample_size$variance[3] <- var(dfa$alpha_norm[dfa$effort_category == "high effort"], na.rm = TRUE)

# squared mean difference of each variable
df_sample_size$squared_mean_difference[1] <- (mean(dfa$ps_norm[dfa$effort_category == "low effort"], na.rm = TRUE)-mean(dfa$ps_norm[dfa$effort_category == "high effort"], na.rm = TRUE))^2
df_sample_size$squared_mean_difference[2] <- (mean(dfa$dprime[dfa$effort_category == "low effort"], na.rm = TRUE)-mean(dfa$dprime[dfa$effort_category == "high effort"], na.rm = TRUE))^2
df_sample_size$squared_mean_difference[3] <- (mean(dfa$alpha_norm[dfa$effort_category == "low effort"], na.rm = TRUE)-mean(dfa$alpha_norm[dfa$effort_category == "high effort"], na.rm = TRUE))^2

# computing the sample size # 4((z_alpha = 0.05) * (z_beta = 0.1))^2 = ~17
df_sample_size$sample_size[1] <- (df_sample_size$variance[1]/df_sample_size$squared_mean_difference[1])*17
df_sample_size$sample_size[2] <- (df_sample_size$variance[2]/df_sample_size$squared_mean_difference[2])*17
df_sample_size$sample_size[3] <- (df_sample_size$variance[3]/df_sample_size$squared_mean_difference[3])*17

print(df_sample_size)
write.csv(df_sample_size, paste0("/Users/ali/Desktop/Experiment/sample_size_computation.csv"))

# Render the dataframe as HTML table
html_table <- knitr::kable(df_sample_size, format = "html")

# Save the HTML table as an image
webshot(html_table, file = "table.png", cliprect = "viewport")
