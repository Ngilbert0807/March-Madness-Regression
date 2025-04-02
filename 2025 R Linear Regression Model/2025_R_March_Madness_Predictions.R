
data <- read.csv("C:/Users/natha/Downloads/KenPom Barttorvik.csv", header = TRUE, sep = ",")

library(tidyverse)
library(MASS)

data2 <- data %>%mutate(ROUND=case_when(
  ROUND==64 ~0,
  ROUND==68 ~0,
  ROUND==32 ~1,
  ROUND==16~2,
  ROUND==8~3,
  ROUND==4~4,
  ROUND==2 ~5,
  ROUND==1~6
))
str(data2)
factorize <-c('CONF.ID','TEAM.ID')

data2[, factorize] <- lapply(data2[,factorize], as.factor)
to_remove <-c('CONF', 'YEAR', 'QUAD.NO', 'QUAD.ID', 'TEAM.NO', 'TEAM' )
data2 <- data2%>%select(-to_remove)
data2 <-data2%>%select(-factorize)

model <-lm(ROUND~., data2)

model_summary <- summary(model)
coefficients_table <- model_summary$coefficients
significant_predictors <- coefficients_table[coefficients_table[, "Pr(>|t|)"] < 0.1,]
sig_pred_names <-rownames(significant_predictors)
sig_pred_names <-append(sig_pred_names, 'ROUND')
sig_data <-data2[,sig_pred_names]
model2<-lm(ROUND~., sig_data)
summary(model2)

plot(model2)
model3<-lm(sqrt(ROUND)~., sig_data)
par(mfrow=c(2,2))
plot(model3)
df_squared <-sig_data
df_squared <-x(df_squared[,-df_squared$ROUND])
model4<-lm(sqrt(ROUND)~., df_squared)
sig_data <-sig_data %>%slice(-184,-637,-82)
plot(model3)

summary(model3)

this_year <-read.csv('C:/Users/natha/Downloads/KenPom Barttorvik (1).csv')
this_year <-this_year %>% filter(YEAR==2025)
this_year_sig <-this_year [,sig_pred_names]
pred <-predict(model3, this_year_sig)
teams <-this_year$TEAM

df<-data.frame(teams, pred)
df <-df%>%arrange(desc(pred))
df$Rank <- rank(-df$pred)
df

sum(df$pred)
