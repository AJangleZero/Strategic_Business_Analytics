##### FOUNDATIONS OF STRATEGIC BUSINESS ANALYTICS
##### ALL QUIZESlibrary(dplyr)

# QUIZ 1 - Module 1
setwd("C:/Users/Nino/Desktop/Strategic Business Analytics/Foundations of marketing analytics")
rm(list=ls(all=TRUE))
library(dplyr)
library(sqldf)

data <- read.table('purchases.txt', header = FALSE, sep = '\t')
colnames(data) <- c('customer_id', 'purchase_amount', 'date_of_purchase')
data$date_of_purchase <- as.Date(data$date_of_purchase, "%Y-%m-%d")
data$days_since <- as.numeric(difftime(time1="2016-01-01",
                                        time2=data$date_of_purchase, units="days"))
customers <- data %>% 
  group_by(customer_id) %>% 
  summarise(recency=min(days_since), frequency=n(), amount=mean(purchase_amount))
new_data <- customers %>%
  mutate(amount=log(amount), frequency=log(frequency)) %>%
  mutate_at(vars(-customer_id),funs(scale))
sample <- seq(1, length(customers$customer_id), by = 10)
customers_sample <- customers[sample, ]
new_data_sample  <- new_data[sample, ]
d <- dist(new_data_sample[,2:4], method = "euclidean")
c = hclust(d, method="ward.D2")
customers_sample$cluster <- cutree(c,k=5)
customers_sample %>% select(-customer_id) %>% group_by(cluster) %>% summarise_all(mean)

#1
customers_sample %>% 
  group_by(cluster) %>% 
  summarise(size=n()) %>% 
  filter(size==max(size)) %>% 
  select(size)
message("1: 555")

#2
customers_sample %>% 
  select(-customer_id) %>% 
  group_by(cluster) %>% 
  summarise_all(mean) %>% 
  filter(recency==min(recency)) %>% 
  select(amount)
message("2: 38")

#3
message("3: The number of segmentation variables")

#4
customers_sample %>% 
  filter(customer_id==260 | customer_id==5920) %>% 
  select(cluster)
message("4: No, they belong to different segments")

#5
customers_sample %>% 
  select(-customer_id) %>% 
  group_by(cluster) %>% 
  summarise_all(mean)
message("5: Customers with poor prospects in terms of loyalty and profitability, due to the few purchases they have made so far")



# QUIZ 2 - Module 2
setwd("C:/Users/Nino/Desktop/Strategic Business Analytics/Foundations of marketing analytics")
rm(list=ls(all=TRUE))
library(dplyr)
library(sqldf)

data <- read.table('purchases.txt', header = FALSE, sep = '\t')
colnames(data) <- c('customer_id', 'purchase_amount', 'date_of_purchase')
data$date_of_purchase <- as.Date(data$date_of_purchase, "%Y-%m-%d")
data$year_of_purchase <- as.numeric(format(data$date_of_purchase, "%Y"))
data$days_since <- as.numeric(difftime(time1="2016-01-01",
                                        time2=data$date_of_purchase, units="days"))
customers_2015 <- data %>% 
  group_by(customer_id) %>% 
  summarise(recency=min(days_since), first_purchase=max(days_since), frequency=n(), amount=mean(purchase_amount))
customers_2015$segment = "NA"
customers_2015$segment[customers_2015$recency > 365*3] = "inactive"
customers_2015$segment[customers_2015$recency <= 365*3 & customers_2015$recency > 365*2] = "cold"
customers_2015$segment[customers_2015$recency <= 365*2 & customers_2015$recency > 365*1] = "warm"
customers_2015$segment[customers_2015$recency <= 365] = "active"
customers_2015$segment[customers_2015$segment == "warm" & customers_2015$first_purchase <= 365*2] = "new warm"
customers_2015$segment[customers_2015$segment == "warm" & customers_2015$amount < 100] = "warm low value"
customers_2015$segment[customers_2015$segment == "warm" & customers_2015$amount >= 100] = "warm high value"
customers_2015$segment[customers_2015$segment == "active" & customers_2015$first_purchase <= 365] = "new active"
customers_2015$segment[customers_2015$segment == "active" & customers_2015$amount < 100] = "active low value"
customers_2015$segment[customers_2015$segment == "active" & customers_2015$amount >= 100] = "active high value"
customers_2015$segment[customers_2015$segment == "new active" & customers_2015$amount >= 100] = "new active high"
customers_2015$segment[customers_2015$segment == "new active" & customers_2015$amount <= 100] = "new active low"
customers_2014 <- data %>% 
  filter(days_since>365) %>%
  group_by(customer_id) %>% 
  summarise(recency=min(days_since)-365, first_purchase=max(days_since)-365, frequency=n(), amount=mean(purchase_amount)) 
customers_2014$segment = "NA"
customers_2014$segment[customers_2014$recency > 365*3] = "inactive"
customers_2014$segment[customers_2014$recency <= 365*3 & customers_2014$recency > 365*2] = "cold"
customers_2014$segment[customers_2014$recency <= 365*2 & customers_2014$recency > 365*1] = "warm"
customers_2014$segment[customers_2014$recency <= 365] = "active"
customers_2014$segment[customers_2014$segment == "warm" & customers_2014$first_purchase <= 365*2] = "new warm"
customers_2014$segment[customers_2014$segment == "warm" & customers_2014$amount < 100] = "warm low value"
customers_2014$segment[customers_2014$segment == "warm" & customers_2014$amount >= 100] = "warm high value"
customers_2014$segment[customers_2014$segment == "active" & customers_2014$first_purchase <= 365] = "new active"
customers_2014$segment[customers_2014$segment == "active" & customers_2014$amount < 100] = "active low value"
customers_2014$segment[customers_2014$segment == "active" & customers_2014$amount >= 100] = "active high value"
customers_2014$segment[customers_2014$segment == "new active" & customers_2014$amount >= 100] = "new active high"
customers_2014$segment[customers_2014$segment == "new active" & customers_2014$amount <= 100] = "new active low"
customers_2015$segment = factor(x = customers_2015$segment, levels = c("inactive", "cold",
                                                                           "warm high value", "warm low value", "new warm",
                                                                           "active high value", "active low value", 
                                                                           "new active high", "new active low"))
customers_2014$segment = factor(x = customers_2014$segment, levels = c("inactive", "cold",
                                                                           "warm high value", "warm low value", "new warm",
                                                                           "active high value", "active low value",
                                                                           "new active high", "new active low"))
revenue_2015 <- data %>% 
  filter(year_of_purchase==2015) %>% 
  group_by(customer_id) %>% 
  summarise(revenue=sum(purchase_amount))
actual <- customers_2015 %>% left_join(revenue_2015, by="customer_id") 
actual$revenue[is.na(actual$revenue)] = 0
forward <- left_join(customers_2014, revenue_2015, by="customer_id")
forward$revenue[is.na(forward$revenue)] = 0
actual %>% 
  group_by(segment) %>% 
  summarise(avg=mean(revenue))
forward %>% 
  group_by(segment) %>% 
  summarise(avg=mean(revenue))

#1
customers_2015 %>% 
  group_by(segment) %>% 
  summarise(N=n()) %>% 
  filter(segment=="new active low")
message("1: 1249")

#2
Nnah_2015 <- customers_2015 %>% 
  group_by(segment) %>% 
  summarise(N=n()) %>% 
  filter(segment=="new active high") %>% 
  select(N)
Nnah_2014 <- customers_2014 %>% 
  group_by(segment) %>% 
  summarise(N=n()) %>% 
  filter(segment=="new active high") %>% 
  select(N)
Nnah_2015/Nnah_2014-1
message("2: 29.5%")

#3
forward %>% 
  group_by(segment) %>% 
  summarise(avg=mean(revenue)) %>% 
  filter(segment=="new warm")
message("3: $5.06")

#4
forward %>% 
  group_by(segment) %>% 
  summarise(avg=mean(revenue)) %>% 
  arrange(avg)
message("4: Warm low value")

#5
customers_2015 %>% 
  group_by(segment) %>% 
  summarise(avg=mean(amount)) %>% 
  filter(segment=="new active high")
message("5: $283.38")


# QUIZ 3 - Module 3
setwd("C:/Users/Nino/Desktop/Strategic Business Analytics/Foundations of marketing analytics")
rm(list=ls(all=TRUE))
library(nnet)
library(dplyr)

data <- read.table('purchases.txt', header = FALSE, sep = '\t')
names(data) <- c('customer_id', 'purchase_amount', 'date_of_purchase')
data$date_of_purchase <- as.Date(data$date_of_purchase, "%Y-%m-%d")
data$year_of_purchase <- as.numeric(format(data$date_of_purchase, "%Y"))
data$days_since <- as.numeric(difftime(time1="2016-01-01", time2 = data$date_of_purchase, units="days"))
customers_2014 <- data %>% 
  filter(days_since>365) %>% 
  group_by(customer_id) %>%
  summarise(recency=min(days_since)-365, first_purchase=max(days_since)-365, 
            frequency=n(), avg_amount=mean(purchase_amount), max_amount=max(purchase_amount))
revenue_2015 <- data %>% 
  filter(year_of_purchase==2015) %>% 
  group_by(customer_id) %>% 
  summarise(revenue_2015=sum(purchase_amount))
in_sample <- left_join(customers_2014,revenue_2015, by="customer_id")
in_sample$revenue_2015 [is.na(in_sample$revenue_2015)] <- 0
in_sample$active_2015 <- as.numeric(in_sample$revenue_2015>0)
prob.model <- multinom(formula = active_2015 ~ recency + log(recency) + frequency + log(frequency),
                         data = in_sample)
coef <- summary(prob.model)$coefficients
std  <- summary(prob.model)$standard.errors
z <- in_sample$active_2015==1
amount.model <- lm(formula = log(revenue_2015) ~ log(avg_amount) + log(max_amount), data = in_sample[z, ])
customers_2015 <- data %>% 
  group_by(customer_id) %>%
  summarise(recency=min(days_since), first_purchase=max(days_since), 
            frequency=n(), avg_amount=mean(purchase_amount), max_amount=max(purchase_amount))
customers_2015$prob_predicted <- predict(prob.model, newdata = customers_2015, type="probs")
customers_2015$revenue_predicted <- exp(predict(amount.model, newdata = customers_2015))
customers_2015$score_predicted <- customers_2015$prob_predicted*customers_2015$revenue_predicted
head(customers_2015,10)

#1
I(coef/std) [abs(coef/std)<2]
message("1: frequency")

#2
message("2: This customer has a 20% chance of buying for $1000, which will generate on average a revenue of $200")

#3
customers_2015 %>% filter(customer_id==80) %>% select(prob_predicted)
customers_2015 %>% filter(customer_id==190) %>% select(frequency)
customers_2015 %>% filter(customer_id==230) %>% select(score_predicted) #This is NOT percentage, but $
customers_2015 %>% filter(customer_id==130) %>% select(revenue_predicted)
1-customers_2015 %>% filter(customer_id==130) %>% select(prob_predicted)
message("3: Customer #230 has a score of 5.64%")

#4
customers_2015[1:10,] %>% filter(score_predicted==max(score_predicted)) %>% select(customer_id)
message("4: Customer #90")

#5
message("5: Because a high value for recency means that the customer has made his last purchase a long time ago")



# QUIZ 4 - Module 4
setwd("C:/Users/Nino/Desktop/Strategic Business Analytics/Foundations of marketing analytics")
rm(list=ls(all=TRUE))
library(dplyr)
library(data.table)
library(sqldf)

data <- read.table('purchases.txt', header = FALSE, sep = '\t')
colnames(data) <- c('customer_id', 'purchase_amount', 'date_of_purchase')
data$date_of_purchase <- as.Date(data$date_of_purchase, "%Y-%m-%d")
data$year_of_purchase <- as.numeric(format(data$date_of_purchase, "%Y"))
data$days_since <- as.numeric(difftime(time1="2016-01-01",
                                       time2=data$date_of_purchase, units="days"))
customers_2015 <- data %>% 
  group_by(customer_id) %>% 
  summarise(recency=min(days_since), first_purchase=max(days_since), frequency=n(), amount=mean(purchase_amount))
customers_2015$segment = "NA"
customers_2015$segment[customers_2015$recency > 365*3] = "inactive"
customers_2015$segment[customers_2015$recency <= 365*3 & customers_2015$recency > 365*2] = "cold"
customers_2015$segment[customers_2015$recency <= 365*2 & customers_2015$recency > 365*1] = "warm"
customers_2015$segment[customers_2015$recency <= 365] = "active"
customers_2015$segment[customers_2015$segment == "warm" & customers_2015$first_purchase <= 365*2] = "new warm"
customers_2015$segment[customers_2015$segment == "warm" & customers_2015$amount < 100] = "warm low value"
customers_2015$segment[customers_2015$segment == "warm" & customers_2015$amount >= 100] = "warm high value"
customers_2015$segment[customers_2015$segment == "active" & customers_2015$first_purchase <= 365] = "new active"
customers_2015$segment[customers_2015$segment == "active" & customers_2015$amount < 100] = "active low value"
customers_2015$segment[customers_2015$segment == "active" & customers_2015$amount >= 100] = "active high value"
customers_2014 <- data %>% 
  filter(days_since>365) %>%
  group_by(customer_id) %>% 
  summarise(recency=min(days_since)-365, first_purchase=max(days_since)-365, frequency=n(), amount=mean(purchase_amount)) 
customers_2014$segment = "NA"
customers_2014$segment[customers_2014$recency > 365*3] = "inactive"
customers_2014$segment[customers_2014$recency <= 365*3 & customers_2014$recency > 365*2] = "cold"
customers_2014$segment[customers_2014$recency <= 365*2 & customers_2014$recency > 365*1] = "warm"
customers_2014$segment[customers_2014$recency <= 365] = "active"
customers_2014$segment[customers_2014$segment == "warm" & customers_2014$first_purchase <= 365*2] = "new warm"
customers_2014$segment[customers_2014$segment == "warm" & customers_2014$amount < 100] = "warm low value"
customers_2014$segment[customers_2014$segment == "warm" & customers_2014$amount >= 100] = "warm high value"
customers_2014$segment[customers_2014$segment == "active" & customers_2014$first_purchase <= 365] = "new active"
customers_2014$segment[customers_2014$segment == "active" & customers_2014$amount < 100] = "active low value"
customers_2014$segment[customers_2014$segment == "active" & customers_2014$amount >= 100] = "active high value"
customers_2015$segment = factor(x = customers_2015$segment, levels = c("inactive", "cold",
                                                                       "warm high value", "warm low value", "new warm",
                                                                       "active high value", "active low value", 
                                                                       "new active"))
customers_2014$segment = factor(x = customers_2014$segment, levels = c("inactive", "cold",
                                                                       "warm high value", "warm low value", "new warm",
                                                                       "active high value", "active low value",
                                                                       "new active"))
new_data <- left_join(customers_2014,customers_2015, by="customer_id")
transition <- table(new_data$segment.x,new_data$segment.y)
transition <- transition/rowSums(transition)
segments <- matrix(nrow=8, ncol=11)
row.names(segments) <- levels(customers_2015$segment)
colnames(segments) <- 2015:2025
segments[,1] <- table(customers_2015$segment)
for (i in 2:11) {
  segments[,i] <- segments[,i-1] %*% transition
  segments [8,i] <- 1000
}
yearly_revenue <- c(0, 0, 0, 0, 0, 323.57, 52.31, 79.17)
revenue_per_segment <- yearly_revenue * segments
yearly_revenue <- colSums(revenue_per_segment)
cumulated_revenue <- cumsum(yearly_revenue)
discount_rate <- 0.10
discount <- 1 / ((1 + discount_rate) ^ (0:10) )
disc_yearly_revenue <- yearly_revenue * discount
disc_cumulated_revenue <- cumsum(disc_yearly_revenue)

#1
segments1 <- as.data.frame(segments)
segments1 <- setDT(segments1, keep.rownames = TRUE)
segments1 %>% filter(rn=="inactive") %>% select("2025")
message("1: 18275")

#2
disc_cumulated_revenue[length(disc_cumulated_revenue)] - yearly_revenue[1]
message("2: $3.3 millions")

#3
transition [5,2]
message("3: 91.1%")

#4
message("4: All of the above")

#5
colSums(segments) [8]
message("5: 25417")
