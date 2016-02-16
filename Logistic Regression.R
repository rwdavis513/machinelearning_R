#Logistic Regression Example
http://www.ats.ucla.edu/stat/r/dae/logit.htm

library(aod)
library(ggplot2)
mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
str(mydata)

## view the first few rows of the data
head(mydata)


sapply(mydata, sd)

## two-way contingency table of categorical outcome and predictors we want
## to make sure there are not 0 cells
xtabs(~admit + rank, data = mydata)

mydata$rank = factor(mydata$rank)


mylogit = glm(admit ~ gre + gpa + rank, data=mydata,family=binomial)
summary(mylogit)

## CIs using profiled log-likelihood
round(confint(mylogit),3)

wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)

terms <- cbind(0, 0, 0, 1, -1, 0)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), L = terms)

exp(coef(mylogit))

round(exp(cbind(OR = coef(mylogit), confint(mylogit))),3)
newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))

newdata1$RankPred = predict(mylogit,newdata=newdata1, type="response")
newdata1

newdata2 <- with(mydata, data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100),4), gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))
newdata2


newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link", se = TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

## view first few rows of final dataset
head(newdata3)

ggplot(newdata3, aes(x = gre, y = PredictedProb)) + geom_ribbon(aes(ymin = LL, ymax = UL, fill = rank), alpha = 0.2) + geom_line(aes(colour = rank),size = 1)

with(mylogit, null.deviance - deviance)
with(mylogit, df.null - df.residual)
with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
logLik(mylogit)


-----
library(class)
library(gmodels)
  #  adding a prediction
mydata$admitProb = exp(predict(mylogit))

cutoffThreshold = 0.9

mydata$admitPred = ifelse(mydata$admitProb>cutoffThreshold,1,0)
myCT = CrossTable(mydata$admit,mydata$admitPred)
  
  

