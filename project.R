### CS544 Project ##################################################

### preparing the data

setwd("~/Desktop/CS544_Project/")

airbnb <- read.csv(file = 'AB_NYC_2019.csv')

ncol(airbnb)
nrow(airbnb)

library(plotly)
library(sampling)

#### Data analyzing ############################################################

### I.	Categorical variable analysis of the number of airbnbs in different boroughs in NYC

## 1. setting data

names_groupsInNYC <- table(airbnb$neighbourhood_group)

numberOfAirbnbs <- as.numeric(table(airbnb$neighbourhood_group))

proportionEachBoroughs <- names_groupsInNYC / nrow(airbnb)


## 2. Visulization by barplot

# •	Barplot of the number of Airbnbs in each borough in NYC

group_plot_number<-barplot(names_groupsInNYC, names = names(names_groupsInNYC), 
                    main = "The number of Airbnbs in each borough in NYC",
                    xlab = "Boroughs of New York City", 
                    ylab = "The number of Airbnbs",
                    ylim = c(0,25000), cex.names = 0.95, cex.main = 1)

text(x = group_plot_number, y = numberOfAirbnbs, label = numberOfAirbnbs,
     pos = 3, cex = 0.8)


# •	Pie chart for the proportion of airbnbs in each borough

percent_labels <- names(proportionEachBoroughs)
percent <- paste(round(proportionEachBoroughs,4)*100,"%")
percent_labels<-paste(percent_labels, percent)

group_plot_percent<-pie(proportionEachBoroughs, labels = percent_labels,
                        main = "The proportion in each boroughs in NYC", 
                        cex = 0.8)


### II.	Numerical variable analysis of the price of airbnbs in NYC

## setting data

manhattan <- subset(airbnb, airbnb$neighbourhood_group == "Manhattan")
brooklyn <- subset(airbnb, airbnb$neighbourhood_group == "Brooklyn")
bronx <- subset(airbnb, airbnb$neighbourhood_group == "Bronx")
queens <- subset(airbnb, airbnb$neighbourhood_group == "Queens")
staten <- subset(airbnb, airbnb$neighbourhood_group == "Staten Island")

## Visualization for the price

# •	Barplot of the frequency of the price

barplot(table(airbnb$price), main = "The frequency of prices",
      xlab = "price", ylab = "frequency", cex.names = 0.8)


# •	Barplots of the frequency of the price in each borough

par(mfrow = c(2,3))

barplot(table(manhattan$price), main = "Airbnb prices of Manhattan", xlab = "prices", ylab = "frequency")
barplot(table(brooklyn$price), main = "Airbnb prices of brooklyn", xlab = "prices", ylab = "frequency")
barplot(table(bronx$price), main = "Airbnb prices of bronx", xlab = "prices", ylab = "frequency")
barplot(table(queens$price), main = "Airbnb prices of queens", xlab = "prices", ylab = "frequency")
barplot(table(staten$price), main = "Airbnb prices of staten", xlab = "prices", ylab = "frequency")

# •	Summaries of the price in each borough

summary(manhattan$price)
summary(brooklyn$price)
summary(bronx$price)
summary(queens$price)
summary(staten$price)


# •	Boxplots of the price in each borough in one diagram

NYC_price <- plot_ly(manhattan, x = ~manhattan$price, type="box", name = 'Manhattan')

NYC_price <- add_trace(NYC_price, x = ~brooklyn$price, name = 'brooklyn')

NYC_price <- add_trace(NYC_price, x = ~bronx$price, name = 'Bronx')

NYC_price <- add_trace(NYC_price, x = ~queens$price, name = 'Queens')

NYC_price <- add_trace(NYC_price, x = ~staten$price, name = 'Staten Island')

NYC_price <- layout(NYC_price, xaxis = list(title = 'Price'))

NYC_price




### III.	Cooperation of the relationship between price, minimum night, number of reviews

## 1.	Pairs diagram of the combination of the relationship of three variables

pairs(airbnb[,10:12], main = "Comparation of price, minimum night, and number of reviews", pch=16)

## 2.	Scatterplot of the relationship of price and minimum night

plot(airbnb$price, airbnb$minimum_nights, 
     main = "The relationship of the price and minimum nights",
     xlab = "Price", ylab = "Minimum nights")

## 3.	Scatterplot for relationship of price and number of reviews

plot(airbnb$price, airbnb$number_of_reviews,
     main = "The relationship of the price and the number of reivews",
     xlab = "Price", ylab = "Number of Reviews")

## 4.	Boxplots of price, minimum nights, and number of reviews

boxplot(airbnb[,10:12], col = c("red", "blue", "green"), 
        main = "Boxplots of the price, minimum nights and the number of reviews", 
        cex.main = 1, cex.axis = 0.9)




### distribution analysis

## 1.	Plotting the data of number of reviews in airbnb open data

barplot(table(airbnb$number_of_reviews),
     main = "Number of reviews of each airbnbs in NYC",
     xlab = "Reviews each airbnb", ylab = "Number of reviews")

## 2.	Scatterplot for the probability of each number of reviews appeared

plot(table(airbnb$number_of_reviews)/nrow(airbnb), type = "p",
     main = "The probability of each number of review appeared",
     xlab = "number of reviews", ylab = "Probability")

## 3.	Boxplot for the number of reviews

boxplot(airbnb$number_of_reviews, main = "Barplot of the number of reviews")

## 4.	Summary of the number of reviews and its standard deviation

summary(airbnb$number_of_reviews)
sd(airbnb$number_of_reviews)

## 5.	Probability density function for the number reviews as normal distribution

pdf<-dnorm(airbnb$number_of_reviews, mean = 23.27, sd = 44.55058)
plot(airbnb$number_of_reviews, pdf, type = "p",
     main = "PDF of the number of reviews",
     xlab = "Number of reviews", ylab = "PDF")

## 6.	Cumulative distribution function for the number reviews as normal distribution

cdf<-pnorm(airbnb$number_of_reviews, mean = 23.27, sd = 44.55058)

plot(airbnb$number_of_reviews, cdf, type = "p",
     main = "CDF of the number of reviews",
     xlab = "Number of reviews", ylab = "CDF")





### V.	Applicability of the Central Limit Theorem for Price

## setting data

sample_draw <- 10000
sample_size1 <- 10
sample_size2 <- 20
sample_size3 <- 30
sample_size4 <- 40

xbar_10 <- numeric(sample_draw)
xbar_20 <- numeric(sample_draw)
xbar_30 <- numeric(sample_draw)
xbar_40 <- numeric(sample_draw)


## 2.	Central Limit Theorem visualization for price

# •	Histogram for CLT of sample size 10, 20, 30, 40 for price

for (i in 1: sample_draw) {
  xbar_10[i] <- mean(sample(airbnb$price, size = sample_size1, replace = FALSE))
}

for (i in 1: sample_draw) {
  xbar_20[i] <- mean(sample(airbnb$price, size = sample_size2, replace = FALSE))
}

for (i in 1: sample_draw) {
  xbar_30[i] <- mean(sample(airbnb$price, size = sample_size3, replace = FALSE))
}

for (i in 1: sample_draw) {
  xbar_40[i] <- mean(sample(airbnb$price, size = sample_size4, replace = FALSE))
}

par(mfrow=c(2,2))
hist(xbar_10, main = "CLT for NYC with size 10", xlab = "sample means")
hist(xbar_20, main = "CLT for NYC with size 20", xlab = "sample means")
hist(xbar_30, main = "CLT for NYC with size 30", xlab = "sample means")
hist(xbar_40, main = "CLT for NYC with size 40", xlab = "sample means")


## 3.	Central Limit Theorem visualization for each borough

# •	CLT for Manhattan

mbar_10 <- numeric(sample_draw)
mbar_20 <- numeric(sample_draw)
mbar_30 <- numeric(sample_draw)
mbar_40 <- numeric(sample_draw)

for (i in 1: sample_draw) {
  mbar_10[i] <- mean(sample(manhattan$price, size = sample_size1, replace = FALSE))
}

for (i in 1: sample_draw) {
  mbar_20[i] <- mean(sample(manhattan$price, size = sample_size2, replace = FALSE))
}

for (i in 1: sample_draw) {
  mbar_30[i] <- mean(sample(manhattan$price, size = sample_size3, replace = FALSE))
}

for (i in 1: sample_draw) {
  mbar_40[i] <- mean(sample(manhattan$price, size = sample_size4, replace = FALSE))
}

par(mfrow=c(2,2))
hist(mbar_10, main = "CLT for Manhattan with size 10", xlab = "sample means")
hist(mbar_20, main = "CLT for Manhattan with size 20", xlab = "sample means")
hist(mbar_30, main = "CLT for Manhattan with size 30", xlab = "sample means")
hist(mbar_40, main = "CLT for Manhattan with size 40", xlab = "sample means")


# •	CLT for Brooklyn

bbar_10 <- numeric(sample_draw)
bbar_20 <- numeric(sample_draw)
bbar_30 <- numeric(sample_draw)
bbar_40 <- numeric(sample_draw)

for (i in 1: sample_draw) {
  bbar_10[i] <- mean(sample(brooklyn$price, size = sample_size1, replace = FALSE))
}

for (i in 1: sample_draw) {
  bbar_20[i] <- mean(sample(brooklyn$price, size = sample_size2, replace = FALSE))
}

for (i in 1: sample_draw) {
  bbar_30[i] <- mean(sample(brooklyn$price, size = sample_size3, replace = FALSE))
}

for (i in 1: sample_draw) {
  bbar_40[i] <- mean(sample(brooklyn$price, size = sample_size4, replace = FALSE))
}

par(mfrow=c(2,2))
hist(bbar_10, main = "CLT for Brooklyn with size 10", xlab = "sample means")
hist(bbar_20, main = "CLT for Brooklyn with size 20", xlab = "sample means")
hist(bbar_30, main = "CLT for Brooklyn with size 30", xlab = "sample means")
hist(bbar_40, main = "CLT for Brooklyn with size 40", xlab = "sample means")


# •	CLT for Bronx

brbar_10 <- numeric(sample_draw)
brbar_20 <- numeric(sample_draw)
brbar_30 <- numeric(sample_draw)
brbar_40 <- numeric(sample_draw)

for (i in 1: sample_draw) {
  brbar_10[i] <- mean(sample(bronx$price, size = sample_size1, replace = FALSE))
}

for (i in 1: sample_draw) {
  brbar_20[i] <- mean(sample(bronx$price, size = sample_size2, replace = FALSE))
}

for (i in 1: sample_draw) {
  brbar_30[i] <- mean(sample(bronx$price, size = sample_size3, replace = FALSE))
}

for (i in 1: sample_draw) {
  brbar_40[i] <- mean(sample(bronx$price, size = sample_size4, replace = FALSE))
}

par(mfrow=c(2,2))
hist(brbar_10, main = "CLT for Bronx with size 10", xlab = "sample means")
hist(brbar_20, main = "CLT for Bronx with size 20", xlab = "sample means")
hist(brbar_30, main = "CLT for Bronx with size 30", xlab = "sample means")
hist(brbar_40, main = "CLT for Bronx with size 40", xlab = "sample means")


# •	CLT for Queens

qbar_10 <- numeric(sample_draw)
qbar_20 <- numeric(sample_draw)
qbar_30 <- numeric(sample_draw)
qbar_40 <- numeric(sample_draw)

for (i in 1: sample_draw) {
  qbar_10[i] <- mean(sample(queens$price, size = sample_size1, replace = FALSE))
}

for (i in 1: sample_draw) {
  qbar_20[i] <- mean(sample(queens$price, size = sample_size2, replace = FALSE))
}

for (i in 1: sample_draw) {
  qbar_30[i] <- mean(sample(queens$price, size = sample_size3, replace = FALSE))
}

for (i in 1: sample_draw) {
  qbar_40[i] <- mean(sample(queens$price, size = sample_size4, replace = FALSE))
}

par(mfrow=c(2,2))
hist(qbar_10, main = "CLT for Queens with size 10", xlab = "sample means")
hist(qbar_20, main = "CLT for Queens with size 10", xlab = "sample means")
hist(qbar_30, main = "CLT for Queens with size 10", xlab = "sample means")
hist(qbar_40, main = "CLT for Queens with size 10", xlab = "sample means")


# •	CLT for Staten Island

sbar_10 <- numeric(sample_draw)
sbar_20 <- numeric(sample_draw)
sbar_30 <- numeric(sample_draw)
sbar_40 <- numeric(sample_draw)

for (i in 1: sample_draw) {
  sbar_10[i] <- mean(sample(staten$price, size = sample_size1, replace = FALSE))
}

for (i in 1: sample_draw) {
  sbar_20[i] <- mean(sample(staten$price, size = sample_size2, replace = FALSE))
}

for (i in 1: sample_draw) {
  sbar_30[i] <- mean(sample(staten$price, size = sample_size3, replace = FALSE))
}

for (i in 1: sample_draw) {
  sbar_40[i] <- mean(sample(staten$price, size = sample_size4, replace = FALSE))
}

par(mfrow=c(2,2))
hist(sbar_10, main = "CLT for Staten Island with size 10", xlab = "sample means")
hist(sbar_20, main = "CLT for Staten Island with size 20", xlab = "sample means")
hist(sbar_30, main = "CLT for Staten Island with size 30", xlab = "sample means")
hist(sbar_40, main = "CLT for Staten Island with size 40", xlab = "sample means")




### VI.	Sampling for airbnb open data

## 1. setting data

size5000<-5000
NrowAirbnb <- nrow(airbnb)
numItems <- ceiling(NrowAirbnb / size5000)

## 2.	Sample random sampling

# •	Table of 5000 random variables drew in Airbnb

NYC_sampling1 <- srswr(size5000, nrow(airbnb))

rows <- (1:nrow(airbnb))[NYC_sampling1!=0]
rows <- rep(rows, NYC_sampling1[NYC_sampling1 != 0])
rs_NYC <- airbnb[rows,]

table(rs_NYC$neighbourhood_group)

# •	Barpolot of 5000 random variables drew in airbnb

numberOfsrs <- as.numeric(table(rs_NYC$neighbourhood_group))

NYC_srs<-barplot(table(rs_NYC$neighbourhood_group), 
        main = "Barplot of the number of airbnbs in random varibles in each borough",
        ylab = "Number", ylim = c(0,2700), cex.main = 1, cex.names = 0.9)

text(x = NYC_srs, y = numberOfsrs, label = numberOfsrs,
     pos = 3, cex = 0.8)

# •	Table of proportion of random variables drew in each borough

table(rs_NYC$neighbourhood_group)/size5000

# •	Pie chart of the proportion of random variables drew in each borough

NYC_srs_prob <- table(rs_NYC$neighbourhood_group)/size5000

NYC_srs_prob_label <- names(NYC_srs_prob)
percent_srs <- paste(round(NYC_srs_prob,4)*100,"%")
NYC_srs_prob_label<-paste(NYC_srs_prob_label, percent_srs)


NYC_prob_srs<-pie(NYC_srs_prob, labels = NYC_srs_prob_label,
                                      main = "The proportion from sampling in each borough", 
                                      cex = 0.8)


## 3.	Systematic sampling

# •	Table of systematic sampling with size = 5000 for each borough

r <- sample(numItems,1)
NYC_sampling2 <- seq(r, by = numItems, length = size5000)
ss_NYC <- airbnb[NYC_sampling2, ]

table(ss_NYC$neighbourhood_group)

# •	Barplot of systematic sampling with size = 5000 for each borough

numberOfss <- as.numeric(table(ss_NYC$neighbourhood_group))

NYC_ss<-barplot(table(ss_NYC$neighbourhood_group), 
                 main = "Barplot of the number of airbnbs in systematic sampling in each borough",
                 ylab = "Number", ylim = c(0,2500), cex.main = 1, cex.names = 0.9)

text(x = NYC_ss, y = numberOfss, label = numberOfss,
     pos = 3, cex = 0.8)

# •	Table of the proportion of systematic sampling with size = 5000 for each borough

table(ss_NYC$neighbourhood_group)/size5000

NYC_ss_prob <- table(ss_NYC$neighbourhood_group)/size5000

NYC_ss_prob_label <- names(NYC_ss_prob)
percent_ss <- paste(round(NYC_ss_prob,4)*100,"%")
NYC_ss_prob_label<-paste(NYC_ss_prob_label, percent_ss)


NYC_prob_ss<-pie(NYC_ss_prob, labels = NYC_ss_prob_label,
                  main = "The proportion from systematic sampling in each borough", 
                  cex = 0.8)

## 4.	Inclusion probability

# •	Table of inclusion probability

inclusion <- inclusionprobabilities(airbnb$number_of_reviews, size5000)

NYC_sampling3 <- UPsystematic(inclusion)

ip_NYC <- airbnb[NYC_sampling3 != 0,]

table(ip_NYC$neighbourhood_group)

# •	Barplot of inclusion probability

numberOfip <- as.numeric(table(ip_NYC$neighbourhood_group))

NYC_ip<-barplot(table(ip_NYC$neighbourhood_group),
                main = "Barplot of the number of airbnbs in inclusion probability in each borough",
                ylab = "Number", ylim = c(0,2500), cex.main = 0.9, cex.names = 0.9)

text(x = NYC_ip, y = numberOfip, label = numberOfip,
     pos = 3, cex = 0.8)

# •	Table of the proportion of inclusion probability of the number of airbnbs

table(ip_NYC$neighbourhood_group)/size5000

NYC_ip_prob <- table(ip_NYC$neighbourhood_group)/size5000

NYC_ip_prob_label <- names(NYC_ip_prob)
percent_ip <- paste(round(NYC_ip_prob,4)*100,"%")
NYC_ip_prob_label<-paste(NYC_ip_prob_label, percent_ip)


NYC_prob_ip<-pie(NYC_ip_prob, labels = NYC_ip_prob_label,
                 main = "The proportion from inclusion probability in each borough", 
                 cex.main = 1)

## 5.	Stratified

# •	Setting data

boroughs <- rep(names(sort(table(airbnb$neighbourhood_group), each = nrow(airbnb))))

NYC_rows <- round(runif(500, 1, nrow(airbnb)))

NYC_data <- data.frame(
  Boroughs = boroughs, 
  Row = NYC_rows)

# •	Table and description of stratified data

table(NYC_data$Boroughs)

## 6. Means comparsion

mean(rs_NYC$price)
summary(ss_NYC$price)["Mean"]
mean(ip_NYC$price)


### These sampling could help researchers to save their times and obtain a reliable result.
### Without sampling, researchers need to spend a lot of time to get the data from real-time data.
### By using sampling, researchers could do the data analysis and expect the future result.
### In this project, the sample size is 10000 and 5000 which provide the idea that
### if there are 5000 or 10000 customers are looking for an airbnb to stay, which 
### price interval they probably choose.
### And it could help the company gets an accurate result because random variables simulated the number of real customers.
### The company could clearly see the mean, standard deviation and based on different variable such as price or number of reviews
### which one they choose the most. 
### Applying these samples to the whole data set will not provide an expectation, it shows the regular analysis.
