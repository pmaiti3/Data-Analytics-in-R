

# Project Title: Hotel Room Pricing In The Indian Market
# NAME: Pratyusha Maiti
# EMAIL: pratyusha.maiti@gmail.com
# COLLEGE: Jadavpur University




getwd()
setwd("E:/DATA ANALYTICS INTERNSHIP/Github/Data-Analytics-in-R")

#Read data into cities dataframe
cities.df <- read.csv(paste("file:///E:/DATA ANALYTICS INTERNSHIP/Capstone/Cities42.csv"))
summary(cities.df)

#Summarising data
library(psych)
describe(cities.df)

#Uniform formatting of Date column
table(cities.df$Date)
cities.df$Date <- gsub("18-Dec-16", "Dec 18 2016", cities.df$Date)
cities.df$Date <- gsub("21-Dec-16", "Dec 21 2016", cities.df$Date)
cities.df$Date <- gsub("24-Dec-16", "Dec 24 2016", cities.df$Date)
cities.df$Date <- gsub("25-Dec-16", "Dec 25 2016", cities.df$Date)
cities.df$Date <- gsub("28-Dec-16", "Dec 28 2016", cities.df$Date)
cities.df$Date <- gsub("31-Dec-16", "Dec 31 2016", cities.df$Date)
cities.df$Date <- gsub("4-Jan-16", "Jan 4 2017", cities.df$Date)
cities.df$Date <- gsub("4-Jan-17", "Jan 4 2017", cities.df$Date)
cities.df$Date <- gsub("Jan 04 2017", "Jan 4 2017", cities.df$Date)
cities.df$Date <- gsub("8-Jan-17", "Jan 8 2017", cities.df$Date)
cities.df$Date <- gsub("8-Jan-16", "Jan 8 2017", cities.df$Date)
cities.df$Date <- gsub("Jan 08 2017", "Jan 8 2017", cities.df$Date)
table(cities.df$Date)

#check formatting of other text columns
table(cities.df$CityName)
table(cities.df$HotelDescription)

###################################################
#                                                 #  
#    IDENTIFYING IMPORTANT DEPENDENT VARIABLES    #
#                                                 #
###################################################

#Use boruta feature selection algorithm to identify important independent variables
library(Boruta)
boruta.train <- Boruta(RoomRent ~ .-HotelAddress -HotelDescription -CityName -HotelName, data = cities.df)
boruta.train
#12 important factors found
#Plot the factors in order of median importance
plot(boruta.train, xlab = "", xaxt = "n")
columns<-lapply(1:ncol(boruta.train$ImpHistory),
           function(i)boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),
                                              i])
names(columns) <- colnames(boruta.train$ImpHistory)
xLabels <- sort(sapply(columns, median))
axis(side = 1, las = 2, labels = names(xLabels),
     at = 1:ncol(boruta.train$ImpHistory), cex = 0.7)

#In order of importance:
#Star Rating
#Airport
#Swimming Pool
#Hotel Capacity 
#Hotel Pincode
#Destination
#City Rank
#Free breakfast
#Population
#Free WiFi
#Metro City

attach(cities.df)
cor.test(RoomRent, StarRating, method = "pearson", conf.level = 0.95)
#Cor value: 0.3693734
cor.test(RoomRent, Airport, method = "pearson", conf.level = 0.95)
cor.test(RoomRent, HasSwimmingPool, method = "pearson", conf.level = 0.95)
#Cor value: 0.3116577 
cor.test(RoomRent, HotelCapacity, method = "pearson", conf.level = 0.95)
#Cor value: 0.1578733 
cor.test(RoomRent, HotelPincode, method = "pearson", conf.level = 0.95)
cor.test(RoomRent, IsTouristDestination, method = "pearson", conf.level = 0.95)
#Cor value: 0.122503 
cor.test(RoomRent, CityRank, method = "pearson", conf.level = 0.95)
#Cor value: 0.09398553
cor.test(RoomRent, FreeBreakfast, method = "pearson", conf.level = 0.95)
cor.test(RoomRent, Population, method = "pearson", conf.level = 0.95)
cor.test(RoomRent, FreeWifi, method = "pearson", conf.level = 0.95)
cor.test(RoomRent, IsMetroCity, method = "pearson", conf.level = 0.95)

#From Correlation test, top five important factors:
#Star Rating
#Swimming Pool
#Hotel Capacity
#Tourist Destination
#City Rank


#########################
#SELECTED VARIABLES:
#star Rating
#Tourist Destination
#Hotel Capacity
#########################



###################################################
#                                                 #  
#    VISUALISING IMPORTANT DEPENDENT VARIABLES    #
#                                                 #
###################################################

##Individual visualisation
library(ggplot2)
ggplot(cities.df, aes(x=RoomRent)) + geom_histogram() + scale_x_continuous(lim = c(0, 30000))
ggplot(cities.df, aes(x=StarRating)) + geom_histogram()
table(cities.df$IsTouristDestination)
ggplot(cities.df, aes(x=HotelCapacity)) + geom_histogram()


##Pairwise plots
attach(cities.df)
plot(RoomRent, StarRating, main = "Room rent vs Star rating")
abline(lm(RoomRent~StarRating), col="red")
plot(RoomRent, IsTouristDestination, main = "Room rent vs Star rating")
plot(RoomRent, HotelCapacity, main = "Room rent vs Star rating")


##Corrgrams
library(Hmisc)
library(car)
library(corrgram)
corrgram(cities.df[,c("StarRating", "IsTouristDestination", "HotelCapacity")], order=TRUE,
         main="Room Pricing",
         lower.panel=panel.pts, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt)

##Variance-Covariance Matrix
roomrents <- cities.df[c(6, 12, 19)]
var(roomrents)
cov(roomrents)


###################################################
#                                                 #  
#      TESTING FOR OTHER DEPENDENT VARIABLES      #
#                                                 #
###################################################

##Room Rate dependence on Weekend or Not
table(cities.df$IsWeekend)
barplot(table(cities.df$IsWeekend), main="Distribution of Weekend", xlab = "Weekend or not")
aggregate(RoomRent ~ IsWeekend, data=cities.df,mean)
boxplot(RoomRent~IsWeekend,data=cities.df[which(cities.df$RoomRent<100000),], main="Room rent vs. IsWeekend",xlab="Room Rent", col=c("red","blue"),horizontal=TRUE)

##RoomRent on different dates
table(cities.df$Date)
aggregate(RoomRent ~ Date, data=cities.df,mean)
boxplot(RoomRent~Date,data=cities.df[which(cities.df$RoomRent<100000),], main="Room rent vs. Date",xlab="Room Rent",horizontal=TRUE)

##RoomRent on whether Metro City or not
table(cities.df$IsMetroCity)
aggregate(RoomRent~IsMetroCity, data = cities.df, mean)

##RoomRent on whether close to Airport or not
aggregate(RoomRent~Airport, data = cities.df, mean)
qplot(RoomRent, Airport,
      data=cities.df[which(cities.df$RoomRent<100000),],
      main = "Room Rent vs distance from airport",
      ylab = "Distance from Airport",
      colour = HasSwimmingPool)



###################################################
#                                                 #  
#       HYPOTHESIS ON ROOM RENT DEPENDENCE        #
#                                                 #
###################################################

#1.RoomRent in hotels having swimming pool is more than that which don't have.
t.test(RoomRent~HasSwimmingPool,data = cities.df, alternative="less")

#2.RoomRent in hotels closer to airport is higher.
t.test(cities.df$RoomRent,cities.df$Airport)

#3.RoomRent in hotels with high star rating is higher.
t.test(cities.df$RoomRent,cities.df$StarRating)

#4.RoomRent in hotels providing Free Breakfast is higher.
t.test(RoomRent~FreeBreakfast, data = cities.df, alternative="less")

#5.RoomRent in metro cities hotels is higher.
t.test(RoomRent~IsMetroCity, data = cities.df, alternative="less")

#6.Average RoomRent in hotels having more hotel capacity is more compared to one with less capacity.
t.test(cities.df$RoomRent,cities.df$HotelCapacity)


##Regression models to test the dependence of room rents on various factors

#Star Rating, Tourist Destination, Hotel Capacity
model1 <- lm(RoomRent~StarRating+IsTouristDestination+HotelCapacity, data = cities.df)
summary(model1)

#Star Rating, Swimming Pool, Hotel Capacity
model2 <- lm(RoomRent~StarRating+HasSwimmingPool+HotelCapacity, data = cities.df)
summary(model2)

#Star Rating, Tourist Destination, Hotel Capacity, Swimming Pool
model3 <- lm(RoomRent~StarRating+IsTouristDestination+HasSwimmingPool+HotelCapacity, data = cities.df)
summary(model3)

#Star Rating, Tourist Destination, Hotel Capacity, Swimming Pool, Airport
model4 <- lm(RoomRent~StarRating+IsTouristDestination+HasSwimmingPool+HotelCapacity+Airport-1, data = cities.df)
summary(model4)

#Star Rating,Airport,Swimming Pool,Hotel Capacity ,Hotel Pincode,Destination,City Rank,Free breakfast,Population,Free WiFi, Metro City
model5 <- lm(RoomRent~StarRating+Airport+HasSwimmingPool+HotelCapacity+HotelPincode+IsTouristDestination+CityRank+Population+FreeWifi+IsMetroCity)
summary(model5)

#Adding terms to improve model
model6 <- lm(RoomRent~StarRating+StarRating:HasSwimmingPool+StarRating:FreeWifi+HasSwimmingPool:FreeWifi+StarRating:HasSwimmingPool:FreeWifi+Airport-1+HasSwimmingPool+HotelCapacity+HotelCapacity:StarRating+IsTouristDestination+CityRank+Population+FreeWifi+IsMetroCity)
summary(model6)
model6$coefficients


#StarRating                             Airport                     HasSwimmingPool 
#1.004182e+03                        9.320143e+00                       -7.217968e+03 
#HotelCapacity                IsTouristDestination                            CityRank 
#4.058440e+00                        1.860006e+03                        8.873910e+00 
#Population                            FreeWifi                         IsMetroCity 
#-9.793509e-05                       -4.553251e+03                       -7.618424e+02 
#StarRating:HasSwimmingPool                 StarRating:FreeWifi            HasSwimmingPool:FreeWifi 
#2.547979e+03                        1.444784e+03                       -7.758756e+02 
#StarRating:HotelCapacity StarRating:HasSwimmingPool:FreeWifi 
#-3.883444e+00                        3.421689e+02 


#Generated model has a Residual standard error: 6555 on 13218 degrees of freedom, Multiple R-squared:  0.4873,	Adjusted R-squared:  0.4868 