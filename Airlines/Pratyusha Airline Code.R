
# Analysis of Airline Ticket Pricing
# NAME: Pratyusha Maiti
# EMAIL: pratyusha.maiti@gmail.com
# COLLEGE: Jadavpur University



setwd("E:/DATA ANALYTICS INTERNSHIP/Assignments")
#Read data into R
airlines <- read.csv(paste("E:/DATA ANALYTICS INTERNSHIP/Assignments/SixAirlines.csv"))

#Attach the dataset
attach(airlines)

#Viewing the attached dataset 
View(airlines)



#Summarising data to understand the distibution of variables
#===========================================================
library(psych)
describe(airlines)



#Boxplots for comparitive viewing of premium and economy features
#================================================================
par(mfrow = c(3,2))
boxplot(airlines$PRICE_PREMIUM, horizontal = TRUE, xlab = "Premium price")
boxplot(airlines$PRICE_ECONOMY, horizontal = TRUE, xlab = "Economy price")
boxplot(airlines$WIDTH_PREMIUM, horizontal = TRUE, xlab = "Width of premium seats")
boxplot(airlines$WIDTH_ECONOMY, horizontal = TRUE, xlab = "Width of economy seats")
boxplot(airlines$PITCH_PREMIUM, horizontal = TRUE, xlab = "Pitch of premium seats")
boxplot(airlines$PITCH_ECONOMY, horizontal = TRUE, xlab = "Pitch of economy seats")
par(mfrow = c(1,1))


#Study of variation of prices with number of seats
#=================================================
library(ggplot2)
ggplot(airlines, aes(x = PRICE_ECONOMY, fill = SEATS_ECONOMY)) + geom_histogram(binwidth = 50)
ggplot(airlines, aes(x = PRICE_PREMIUM, fill = SEATS_PREMIUM)) + geom_histogram(binwidth = 50)



#Scatterplot to study the effect of flight duration on pricing wrt individual airlines
#=====================================================================================
attach(airlines)

qplot(FLIGHT_DURATION,PRICE_ECONOMY,
      main="Price of economy seats vs flight hours",
      xlab="Hours", ylab="Price",
      color = AIRLINE)
qplot(FLIGHT_DURATION,PRICE_PREMIUM,
      main="Price of premium seats vs flight hours",
      xlab="Hours", ylab="Price",
      color = AIRLINE)
#Inference: Clearly see that SirFrance has highest price, British Airways is spread over entire graph
#and more or less constant distribution

plot(PRICE_PREMIUM,PRICE_ECONOMY, 
     col="blue",
     main="Price of economy vs premium seats",
     xlab="Premium Prices", ylab="Economy Prices")
abline(h=mean(PRICE_ECONOMY), col="dark blue", lty="dotted")
abline(v=mean(PRICE_PREMIUM), col="dark blue", lty="dotted")
abline(lm(PRICE_ECONOMY~PRICE_PREMIUM))
#Reiterates the observation from scatterplots that there is a roughly linear variation of prices



#Scatterplot Matrix for Economy and Premium features
#===================================================
library(car)
scatterplotMatrix(formula = ~ SEATS_ECONOMY + PITCH_ECONOMY + WIDTH_ECONOMY + PRICE_ECONOMY, cex=0.6,
                  data=airlines, diagonal="density")

scatterplotMatrix(formula = ~ SEATS_PREMIUM + PITCH_PREMIUM + WIDTH_PREMIUM + PRICE_PREMIUM, cex=0.6,
                  data=airlines, diagonal="density")



#Corrplot and Variance-covariance matrix for Economy and Premium seats
#=====================================================================
library(corrplot)
library(gplots)
economy <- cor(airlines[,c(2:6,8,10,12,15,17)], use="complete.obs")
corrplot.mixed(economy, 
               upper = "circle",
               lower = "ellipse",
               col = colorpanel(50, "red", "orange", "royalblue"),
               tl.pos="lt")
premium <- cor(airlines[, c(2:5,7,9,11,13,15,17)], use="complete.obs")
corrplot.mixed(premium, 
               upper = "circle",
               lower = "ellipse",
               col = colorpanel(50, "red", "orange", "royalblue"),
               tl.pos="lt")
#Variance and covariance matrix
cov_airlines <- airlines[-c(1)]
var(cov_airlines)
cov(cov_airlines)



#Hypothesis to test against regression models
#============================================

#Hypothesis 1a: Number of seats does not affect pricing of economy seats
cor.test(PRICE_ECONOMY,SEATS_ECONOMY+PITCH_ECONOMY+WIDTH_ECONOMY, data = airlines)
cor.test(PRICE_ECONOMY,PITCH_ECONOMY+WIDTH_ECONOMY, data = airlines)
#Hypothesis 1b: Number of seats affects pricing of premium seats
cor.test(PRICE_PREMIUM,SEATS_PREMIUM+PITCH_PREMIUM+WIDTH_PREMIUM, data = airlines)
cor.test(PRICE_PREMIUM,PITCH_PREMIUM+WIDTH_PREMIUM, data = airlines)



#REGRESSION MODEL to show the factors affecting price of economy and premium-economy seats
#=========================================================================================

#Economy
regression_economy<-lm(PRICE_ECONOMY~FLIGHT_DURATION+SEATS_PREMIUM+PITCH_PREMIUM+WIDTH_PREMIUM+QUALITY+MONTH+AIRCRAFT+AIRLINE,
                       data=airlines)
summary(regression_economy)
fitted(regression_economy)
coefficients(regression_economy)
#Premium-economy
regression_premium<-lm(PRICE_PREMIUM~FLIGHT_DURATION+SEATS_PREMIUM+PITCH_PREMIUM+WIDTH_PREMIUM+QUALITY+MONTH+AIRCRAFT+AIRLINE,
                       data=airlines)
summary(regression_premium)
fitted(regression_premium)
coefficients(regression_premium)

#Example regression without essential term flight_duration
regression_premium <- update(regression_premium, .~.-FLIGHT_DURATION, data = airlines)
summary(regression_premium)
coefficients(regression_premium)
