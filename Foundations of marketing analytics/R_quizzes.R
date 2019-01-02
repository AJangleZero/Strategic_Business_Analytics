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

          