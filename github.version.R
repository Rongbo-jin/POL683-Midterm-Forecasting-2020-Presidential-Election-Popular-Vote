library(foreign)
library(xlsx)
library(ggplot2)
library(car)
library(dplyr)
library(stargazer)
library(haven)
library(R.utils)
df <- loadObject("forecast2020.RData")

# Alan Abramowitz Time for Change model 
lm1 <- lm(incvote ~ q2gdp + juneapp + inc, df)
summary(lm1)

# social movement model
lm2 <- lm(incvote ~ q2gdp + juneapp + inc + soc_mov, df)
summary(lm2)

# polarization model
lm3 <- lm(incvote ~ q2gdp + juneapp + inc + polarization, df)
summary(lm3)

# regression table 
stargazer(lm1, lm2, lm3, dep.var.labels=c("Time for Change Model", "Social Movement Model", "Polarization Model"),
          covariate.labels=c("Q2 GDP", "Net Approval Rate", "Incumbency", "Social Movement", "Polarization"), 
          type="text", out="word")

# Alan Abramowitz 2020
# dfx <- as.data.frame(read.spss("/Users/macintoshhd/Desktop/SGPP/2020 Fall/POL 683/forecasting/replication data/Incumbent forecast data.sav",
# use.value.labels=F, use.missing=T))



# the political economy model 
# Lewis-Beck
lm5 <- lm(pop2pvot ~ julypop + gnpchan, df)
summary(lm5)
# delet duplicated colunm name
ls(df)
colnames(df)
df <- df[, !duplicated(colnames(df))]
colnames(df)

# Modified Political Economy Model--combination of Abramowitz and Lewis-Beck
lm7 <-  lm(pop2pvot ~ julypop + gnpchan + inc, df)
summary(lm7)
lm8 <-  lm(pop2pvot ~ julypop + gnpchan + inc + soc_mov, df)
summary(lm8)
lm9 <- lm(pop2pvot ~ julypop + gnpchan + inc + polarization, df)
summary(lm9)


# regression table 
stargazer(lm5, lm7, lm8, lm9, dep.var.labels=c("Time for Change Model", "Social Movement Model", "Polarization Model"),
          covariate.labels=c("Approval Rate", "GNP", "Incumbency", "Social Movement", "Polarization"), 
          type="text", out="word")

# predictions from lm7
df$pre1 <- predict(lm7)
# create a new vriable to indicate whether lm7 correctly predict previous elections
df$correct <- c("Yes", "Yes", "Yes", "No", "Yes", "Yes", "Yes", "No", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", 
                    "Yes", "No")


# visualization of the results of lm7 prediction
ggplot(df, aes(x = pre1, y = incvote, label = year)) +
  geom_point(aes(color = factor(correct)), position = "jitter", size = 2) +
  geom_text(check_overlap = TRUE) +
  geom_abline(color = "darkblue", size = 1) +
  #ggtitle("Popular vote share for the incubent party vs. linear model prediction") +
  scale_y_continuous(name="Popular Vote Share for the Incubent Party") + 
  scale_x_continuous(name="Modified Political Economy Model Prediction") +
  theme(legend.position="bottom", legend.box = "horizontal")

# simulation 
pred_sim = MASS::mvrnorm(1010, coef(lm7), vcov(lm7))
nd <- cbind(1, seq(0, 100, by=1), -4.14, 1)
mean = nd %*% t(pred_sim) %>% apply(2, mean)
upper = nd %*% t(pred_sim) %>% apply(2, quantile, 0.975)
lower = nd %*% t(pred_sim) %>% apply(2, quantile, 0.025)
plot.data<-data.frame(mean, upper, lower, nd[, 2:4])
names(plot.data)<-c("mean", "upper", "lower", "Popularity", "Economic Growth", "Incubent")

ggplot(plot.data, aes(x = Popularity, y=mean, 
                      ymin=lower, ymax=upper))+
  geom_point(position = "jitter") +
  geom_abline(color = "darkgreen", size=1.5) +
  geom_errorbar(position = "dodge", width = 0.25) +
  #geom_ribbon(col='brown', alpha=0.10) +
  ylim(limit=c(0,100))+
  geom_hline(yintercept=50, linetype="dashed", 
             color = "red", size=2) #+
#ggtitle("Predicting Popular Vote Share for Trump")


