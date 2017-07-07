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

#########################################
#                                       #  
#    IDENTIFYING DEPENDENT VARIABLES    #
#                                       #
#########################################

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