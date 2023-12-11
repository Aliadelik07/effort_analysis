library(readr)
library("readxl")

path = '/Users/ali/Desktop/Experiment/sub01/'
df <- read_excel(path + "Sub01-nbackRolling-test.csv", sheet = "nback-04") #12 is removed

b1 <- read_csv("/Users/ali/Desktop/Experiment/EEG/from matlab for r/nback/trial001_psd_230718_1351_avgrows.csv", colClasses = c("AVG"))

b1 <- as.data.frame(t(b1[-(49:257),-3]))
View(b1)

library(dplyr)
files <- list.files(path = "/Users/ali/Desktop/Experiment/EEG/from matlab for r/nback/", pattern = ".csv")


library("data.table")
data <- fread("/Users/ali/Desktop/Experiment/EEG/from matlab for r/nback/trial001_psd_230718_1351_avgrows.csv", select = c("AVG"))
data <- as.data.frame(t(data))
data <- data[-(49:257)]

###______________________________________adding_PSD__to_behavioral_data____________________________________
library(dplyr)
library(readr)
library("data.table")
library("readxl")

files <- list.files(path = "/Users/ali/Desktop/Experiment/EEG/from matlab for r/nback/", pattern = ".csv")

library(gtools)
setwd("/Users/ali/Desktop/Experiment/EEG/from matlab for r/nback/")

mydata = read.csv(files[1], header = TRUE, stringsAsFactors = FALSE)
mydata = data.frame(mydata[1,])

for (record in files) {
  temp <- fread(record, select = c("AVG"))
  temp <- as.data.frame(t(temp[-(49:257)]))
  mydata = smartbind(mydata, temp)
}

library("writexl")
write_xlsx(mydata,"mydata.xlsx")
###_______________________________Power Spectrum Density_______________________________________________


library("readxl")
sub_n = "nback-04"
df <- read_excel("/Users/ali/Desktop/Experiment/Preliminary result.xlsx", sheet = sub_n)

library(Matrix)
library(lme4)
library(foo)
library(aod)

ml <- lm(effort ~  V1+	V2+	V3+	V4+	V5+	V6+	V7+	V8+	V9+	V10+	V11+	V12+	V13+	V14+	V15+	V16+	V17+	V18+	V19+	V20+	V21+	V22+	V23+	V24+	V25+	V26+	V27+	V28+	V29+	V30+	V31+	V32+	V33+	V34+	V35+V36+	V37+	V38+	V39+	V40+	V41+	V42+	V43+	V44 +	V45 +	V46	+ V47 +	V48,
          data = df)

summary(ml)


summary (glm(effort ~ V1+ V2+	V3+	V4+	V5+	V6+	V7+	V8+	V9+	V10+	V11+	V12+	V13+	V14+	V15+	V16+	V17+	V18+	V19+	V20
            ,data = df))

model <- lm (effort ~ log(delta) + log(theta) + log(alpha) + log(beta) + log(gamma),data = df)
model <- lm (effort ~ delta + theta + alpha + beta + gamma ,data = subset(df,AUC<1e-10))

summary (model)

summary (lm (effort ~ AUC + difficulty  ,data = subset(df,AUC<1e-10)))

summary (lm (effort ~ alpha + difficulty,data = subset(df,AUC<1e-10)))

plot(df$alpha, df$effort)

summary (lm (effort ~ I(beta/theta)  ,data = subset(df,AUC<1e-10)))

require(ggplot2)
ggplot(subset(df,AUC<1e-10), aes(x = AUC, y = effort)) +
  geom_point() +
  stat_smooth(method = "lm",formula = y ~ x )

###_______________________________TimeFrequency_______________________________________________
library("readxl")
sub_n = "nback-04"
df <- read_excel("/Users/ali/Desktop/Experiment/Preliminary result.xlsx", sheet = sub_n)

summary (lm (effort ~ TBR_h  , data = subset(df,n_back>1)))

require(ggplot2)
g <- ggplot(subset(df,n_back=3), aes(x = ao_mean, y = effort)) +
  geom_point() +
  stat_smooth(method = "lm",formula = y ~ x )

g <- g + scale_color_gradientn(colours = rainbow(5)) + geom_text_repel(aes(label = round(n_back)), df,fontface ="plain", size = 3)

g

g <- ggplot(subset(df,TBR_std<2.75), aes(x = ap_h, y = effort, color = factor(round(n_back)))) +
  stat_smooth(aes(group = factor(round(n_back))), method = "lm", se = FALSE) + geom_point()
g <- g + labs(title = "effort and parietal alpha",
              x = "Alpha_mean",
              y = "Effort",
              color = "n_back")
g

###_______________________________Hurst Exponent_______________________________________________
library(R.matlab)
TBRnA <- readMat("/Users/ali/Documents/MatlabDir/brainstorm3/TBRandAlpha.mat")

library(pracma)


for (i in 1:24) {
  hurst_exponent <- hurstexp(TBRnA[["ao"]][[i]])
  df$ao_H[i] <- hurst_exponent[1]
  
  hurst_exponent <- hurstexp(TBRnA[["ap"]][[i]])
  df$ap_H[i] <- hurst_exponent[1]
  
  hurst_exponent <- hurstexp(TBRnA[["tbr"]][[i]])
  df$TBR_H[i] <- hurst_exponent[1]  
  
}

dr <- data.frame(df$ao_H,df$ap_H, df$TBR_H)
library(openxlsx)
write.xlsx(dr, file = "Hurst.xlsx")

library("readxl")
sub_n = "nback-04"
dfe <- read_excel("/Users/ali/Desktop/Experiment/Preliminary result.xlsx", sheet = sub_n)

summary (lm (dprime ~ poly(ap_h,2)  , data = subset(dfe,dprime>1.5)))


require(ggplot2)
ggplot(subset(df,n_back>1), aes(x = TBR_h, y = effort)) +
  geom_point() +
  stat_smooth(method = "lm",formula = y ~ x )
