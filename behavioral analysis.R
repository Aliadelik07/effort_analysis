library(readr)
library("readxl")

df <- read_csv("/Users/ali/Desktop/Experiment/sub01/Sub01-nbackRolling-test.csv")


library(Matrix)
library(lme4)
library(foo)

lm <- lm(dprime ~ slider_effort.response, data = df)

summary(lm)
library(ggrepel)
require(ggplot2)
library(investr)
ggplot(df, aes(x=slider_effort.response, y=dprime)) + geom_line ()+ geom_point() +facet_wrap(~n_back)


ggplot(df, aes(x = slider_effort.response, y = dprime)) +geom_point(alpha = 0.5) + facet_wrap(~n_back) 

lm <- lm(dprime ~ (n_back * poly(slider_effort.response,2)) , data = df)
summary(lm)
plotFit(lm)

lm <- lm(dprime ~ poly(slider_effort.response,2) , data = subset(df, n_back>1))
summary(lm)



ggplot(df, aes(x = n_block, y = effort)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(subset(df, n_back>0), aes(x = effort_norm, y = score)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(df, aes(x = effort, y=g)) + geom_line ()+ geom_point() +facet_wrap(~n_back)


#performance and effort
sub_n = "nback-03"

df <- read_excel("/Users/ali/Desktop/experiment/Preliminary result.xlsx", sheet = sub_n)

df$effort = df$slider_effort.response
df$effort <- (df$effort - min(df$effort)) / (max(df$effort) - min(df$effort))


g <- ggplot(subset(df,n_back>1), aes(effort, dprime)) +    
  geom_point() + stat_smooth(method = "lm",formula = y ~ poly(x,2)) +
  labs (title = "Perfromance and Effort",
        x = "Effort Self-Report",
        y = "Performance (d')")


g<- g+scale_color_gradientn(colours = rainbow(5)) + labs(title = "Perfromance and Effort",
                                                         x = "Effort Self-Report",
                                                         y = "Performance (d')")

g

#hit and false alarm
ggplot(subset(df, n_back>1), aes(effort_norm, cr )) +    
  geom_point() + stat_smooth(method = "lm",formula = y ~ poly(x,2)) + ggtitle("cr")


