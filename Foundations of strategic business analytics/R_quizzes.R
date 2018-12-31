##### FOUNDATIONS OF STRATEGIC BUSINESS ANALYTICS
##### ALL QUIZES

#TRAINING QUIZ 1

library(dplyr)
library(tidyr)
rm(list=ls(all=TRUE))
setwd("C:/Users/Nino/Desktop/Strategic Business Analytics/Foundations of strategic business analytics/Week 1")
data=read.table('_eba2c079135882131db3690701bc9c97_PASTAPURCHASE_EDITED.csv', header = T,sep=',')

#1
message("1: Mean = ", mean(data$PASTA), "     StD = ", sd(data$PASTA))

#2
i <- data %>% 
  filter(INCOME==min(INCOME)) %>% 
  select (AREA) %>% 
  unique
ii <- data %>% 
  filter(INCOME==max(INCOME)) %>% 
  select (AREA) %>%
  unique
message("2: (i) = ", i, "     (ii) = ", ii)

#3
max_pasta <- data %>% 
  select(HHID,PASTA) %>% 
  group_by(HHID) %>% 
  summarise(sum=sum(PASTA)) %>% 
  filter(sum==max(sum)) %>%
  select(sum) %>%
  round(digits=0)
message("3: ", max_pasta)

#4
avg_income <- data %>% 
  select(INCOME,AREA) %>% 
  group_by(AREA) %>% 
  summarise(ave=mean(INCOME)) %>% 
  filter(AREA==4) %>% 
  select(ave) %>%
  round(digits=2)
message("4: ", avg_income)

#5
N <- data %>% 
  select(HHID, AREA, INCOME, PASTA) %>% 
  group_by(HHID) %>% 
  summarise(INCOME=mean(INCOME), PASTA=sum(PASTA), AREA=mean(AREA)) %>%  
  filter(AREA==2 & INCOME > 20000 & PASTA >30) %>% 
  summarise(N=n())
message("5: ", N)


#6
message("6: ", cor(data$PASTA,data$EXPOS))

#7
data_filtered <- data %>% select(HHID, PASTA) %>% group_by(HHID) %>% summarise(PASTA=sum(PASTA))
hist(data_filtered$PASTA)
message("7: ", "A")

#8
data_filtered <- data %>% select(TIME, PASTA) %>% group_by(TIME) %>% summarise(PASTA=sum(PASTA))
plot(PASTA ~TIME, data_filtered)
message("8: ", "C")



# QUIZ 1 - Module 2
rm(list=ls(all=TRUE))
setwd("C:/Users/Nino/Desktop/Strategic Business Analytics/Foundations of strategic business analytics/Week 1")
library(ggplot2)
library(dplyr)

#1
data1 <- read.table('DATA_2.01_SKU.csv', header = T,sep=',') 
question1 <- list(mean=mean(data1$CV), median=median(data1$CV))
message("1: Mean = ", question1$mean, "     Media = ", question1$median)

#2
data1 <- read.table('DATA_2.01_SKU.csv', header = T,sep=',') 
data1_scaled <- scale(data1)
hclust1 <- hclust(dist(data1_scaled, method="euclidean"), method="ward.D")
data1$segments_new <- cutree(hclust1,2)
data1$segments_old <- cutree(hclust1,3)
qplot(CV,ADS, data=data1, col=segments_old, size=segments_new) + 
annotate("text", x = 0.15, y = 9.7, label = "Horses") +
annotate("text", x = 0.65, y = 9, label = "Wild Bulls") +
annotate("text", x = 0.8, y = 2, label = "Crikets")
message("2: The segments Crikets and Wild Bulls are merged")

#3
data2 <- read.table('DATA_2.02_HR.csv', header = T,sep=',') 
plot(NP~LPE, data=data2)
message("3: ", "C")

#4
data2 <- read.table('DATA_2.02_HR.csv', header = T,sep=',') 
data2_reduced <- select(data2, S, LPE, NP)
data2_red_scaled <- scale(data2_reduced)
hclust2 <- hclust(dist(data2_red_scaled, method="euclidean"), method="ward.D")
data2_reduced$segment <- cutree(hclust2,2) 
question4 <- data2_reduced %>% 
  group_by(segment) %>% 
  summarise(median=median(S))
message("4: 1 = ",question4$median [1], "     2 = ", question4$median [2])


#5
data3 <- read.table('DATA_2.03_Telco.csv', header = T,sep=',')
data3_scaled <- scale(data3)
hclust3 = hclust(dist(data3_scaled, method = "euclidean"), method="ward.D") 
data3$segment= cutree(hclust3,k=5) 
data3 %>% 
  group_by(segment) %>% 
  summarise_all(mean) %>% 
  mutate(group=c("Silver", "Heavy User", "Pro", "Light User", "YA"))
data3 %>% 
  group_by(segment) %>% 
  summarise(n=n()) %>% 
  mutate(group=c("Silver", "Heavy User", "Pro", "Light User", "YA"))
min(data3$Intern)
message("5: a) The Young Adoult (YA) segment uses more data and text than any other segment     
   d) All the customers in the sample made at least one international call")



# QUIZ 2 - Module 3
rm(list=ls(all=TRUE))
setwd("C:/Users/Nino/Desktop/Strategic Business Analytics/Foundations of strategic business analytics/Week 2")
library(dplyr)

#1
data1 <- read.table('DATA_3.01_CREDIT.csv',sep=',',header=TRUE) 
lreg <- lm(Rating~., data=data1)
summary(lreg)
cor(lreg$fitted.values, data1$Rating)
message("1: a) The regression predicts correctly 98.67% of the observations
   c) The variable balance has a positive impact on the rating, eveything else being equal
   e) Students have, everything else being equal, a weaker rating
   f) People with lerger income have, everything else being equal, a better rating")

#2
data1 <- read.table('DATA_3.01_CREDIT.csv',sep=',',header=TRUE) 
lreg <- lm(Rating~., data=data1)
summary(lreg)
print(c("Positive", "NS", "NS"))
message("2: ","F")

#3
data1 <- read.table('DATA_3.01_CREDIT.csv',sep=',',header=TRUE) 
lreg2 <- lm(Rating~Income+Cards+Married, data=data1)
summary(lreg2)
print(c("Positive", "Positive", "NS"))
message("3: ","D")

#4
data2 <- read.table('DATA_3.02_HR2.csv',sep=',',header=TRUE)
logit <- glm(left ~ ., family=binomial(logit), data=data2)
cutoff <- 0.5
data2$fitted <- logit$fitted.values
data2 %>% 
  mutate(group=ifelse(fitted > 0.5,1,0)) %>% 
  mutate(correct=ifelse(left==group,1,0)) %>% 
  summarise(stay=1-mean(group), leave=mean(group), correctly=mean(correct)) %>%
  mutate_all(round, digits=2)
message("4: ", "C")

#5
data2 <- read.table('DATA_3.02_HR2.csv',sep=',',header=TRUE)
logit <- glm(left ~ ., family=binomial(logit), data=data2)
summary(data2) #We can only know if more than 50% of employes will leave with Survival analysis (next module)
summary(logit)
data2 %>% group_by(TIC) %>% summarise(mean(left))
message("5: b) B
   c) C
   d) D")



# QUIZ 3 - Module 4
setwd("C:/Users/Nino/Desktop/Strategic Business Analytics/Foundations of strategic business analytics/Week 3")
rm(list=ls(all=TRUE))
library(dplyr)
library(survival) 

#1
dataold <- read.table('DATA_3.02_HR2.csv', header = T,sep=',') 
datanew <- read.table('DATA_4.02_HR3.csv', header = T,sep=',') 
logreg <- glm(left ~ ., family=binomial(logit), data=dataold) 
probaToLeave <- predict(logreg,newdata=datanew,type="response") 
datanew$Prob2Leave <- probaToLeave
datanew$ID <- seq_along(probaToLeave)
datanew %>% 
  filter(Prob2Leave==min(Prob2Leave)) %>% 
  select(ID)
message("1: 572")

#2
dataold <- read.table('DATA_3.02_HR2.csv', header = T,sep=',') 
datanew <- read.table('DATA_4.02_HR3.csv', header = T,sep=',') 
logreg <- glm(left ~ ., family=binomial(logit), data=dataold) 
probaToLeave <- predict(logreg,newdata=datanew,type="response") 
datanew$Prob2Leave <- probaToLeave
datanew$ID <- seq_along(probaToLeave)
datanew %>% 
  filter(LPE > 0.9) %>% 
  filter(Prob2Leave==min(Prob2Leave)) %>% 
  select(ID)
message("2: 322")

#3
data <- read.table('DATA_4.03_MNT.csv',sep=',',header=TRUE)
dependantvars <- Surv(data$lifetime, data$broken) 
survreg <- survreg(dependantvars~pressureInd+moistureInd+temperatureInd+team+provider, dist="gaussian",data=data) 
summary(survreg)  
print(c("Non-Significant", "Non-Significant", "Significant"))
message("3: B")

#4
data <- read.table('DATA_4.03_MNT.csv',sep=',',header=TRUE)
dependantvars <- Surv(data$lifetime, data$broken) 
survreg <- survreg(dependantvars~pressureInd+moistureInd+temperatureInd, dist="gaussian",data=data) 
summary(survreg) 
print(c("Non-Significant", "Non-Significant", "Non-Significant"))
message("4: A")

#5
data <- read.table('DATA_4.03_MNT.csv',sep=',',header=TRUE)
dependantvars <- Surv(data$lifetime, data$broken)
survreg <- survreg(dependantvars~pressureInd+moistureInd+temperatureInd+team+provider, dist="gaussian",data=data) 
summary(survreg)
data$Elifetime <- predict(survreg, newdata=data, type="quantile", p=.5)
data %>% 
  mutate(time.left=Elifetime-lifetime, ID=seq_along(lifetime)) %>% 
  filter(time.left==max(time.left)) %>% 
  select(ID)
message("5: 53")
