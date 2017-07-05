
# Analysis of MBA Salaries
# NAME: Pratyusha Maiti
# EMAIL: pratyusha.maiti@gmail.com
# COLLEGE: Jadavpur University



setwd("E:/DATA ANALYTICS INTERNSHIP/Assignments")
#Read data into R
mba <- read.csv(paste("E:/DATA ANALYTICS INTERNSHIP/Assignments/MBA Starting Salaries Data.csv"))
View(mba)
attach(mba)

#Summarising data to understand the distibution of variables
#===========================================================

library(psych)
describe(mba)



#Boxplots to visualise data
#==========================

par(mfrow = c(2,2))
#Individual variable visualisation
boxplot(gmat_tot, horizontal = TRUE, xlab = "GMAT Total score")
boxplot(gmat_tpc, horizontal = TRUE, xlab = "Overall GMAT Percentage")
boxplot(s_avg, horizontal = TRUE, xlab = "Spring MBA Average")
boxplot(f_avg, horizontal = TRUE, xlab = "Fall MBA Average")
par(mfrow = c(1,1))

#Remove items without salary data
revised_mba <- mba[which(mba$salary>999),]
attach(revised_mba)
par(mfrow = c(2,1))

#Visualise starting salary with respect to various factors
boxplot(salary~sex, horizontal = TRUE, xlab = "Starting salary", ylab = "Gender", main = "Gender vs Starting Salary")
boxplot(salary~frstlang, horizontal = TRUE, xlab = "Starting salary", ylab = "First Language", main = "First Language vs Starting Salary")
par(mfrow = c(1,1))



#Analysis of starting salary with respect to various factors
#===========================================================
library(ggplot2)
#Variation of salary with gmat total, sex
ggplot(revised_mba, aes(x=gmat_tot, y=salary)) + geom_jitter() + facet_grid(.~sex)
#Variation of salary with gmat total, first language
ggplot(revised_mba, aes(x=gmat_tot, y=salary)) + geom_jitter() + facet_grid(.~frstlang)
#Variation of salary with age, sex
ggplot(revised_mba, aes(x=age, y=salary)) + geom_jitter() + facet_grid(.~sex)
#Variation of salary with work years, sex
ggplot(revised_mba, aes(x=work_yrs, y=salary)) + geom_jitter() + facet_grid(.~sex)



#Scatterplot to visualise pairwise distribution of each variable independently
#==============================================================================
attach(revised_mba)

qplot(gmat_tot, salary,
      main="GMAT Total vs Starting salary",
      xlab="GMAT Total score", ylab="Starting salary",
      color = work_yrs)
qplot(gmat_tot, salary,
      main="GMAT Total vs Starting salary",
      xlab="GMAT Total score", ylab="Starting salary",
      color = satis)
qplot(gmat_tot,gmat_tpc,
      main="GMAT Total vs GMAT Percentile",
      xlab="GMAT Total", ylab="GMAT Percentile",
      color = salary)



#Corrplots and Variance-Covariance matrix for a complete comparative study
#=========================================================================
attach(revised_mba)
library(corrplot)
library(gplots)

study <- cor(revised_mba)
study
corrplot.mixed(study, 
               upper = "circle", 
               lower = "ellipse",
               col = colorpanel(50, "red", "lightblue", "blue"),
               tl.pos = "lt")

#Variance matrix
var(revised_mba)
var(mba)
#Covariance matrix
cov(revised_mba)
cov(mba)


#########################################
#                                       #  
#      STUDY OF PLACED STUDENTS         #
#                                       #
#########################################

attach(revised_mba)

#Model 1: No interaction term
#=============================
placed_model <- lm(salary ~ age + sex + gmat_tot + quarter + work_yrs + frstlang, data = revised_mba)
summary(placed_model)
step(placed_model)



#Chi-sq tests to check model dependances
#=======================================

#Test 1
chisq.test(table(salary,age))
#Test 2
chisq.test(table(salary,gmat_tot))
#Test 3
chisq.test(table(salary,gmat_tpc))
#Test 4
chisq.test(table(salary,s_avg))
#Test 5
chisq.test(table(salary,f_avg))
#Test 6
chisq.test(table(salary,work_yrs))
#Test 7
chisq.test(table(salary,frstlang))



#T-Tests
#=======
t.test(salary, work_yrs)
t.test(salary, frstlang)



#Regression Model 2:Adding interaction terms
#===========================================
placed_model1 <- lm(salary ~ age + sex + age:sex + gmat_tot + quarter + work_yrs + frstlang, data = revised_mba)
summary(placed_model1)
#RSE: 13390 on 95 degrees of freedom

placed_model1 <- lm(salary ~ age + sex + age:sex + gmat_tot + age:gmat_tot + quarter + work_yrs + frstlang, data = revised_mba)
summary(placed_model1)
#RSE: 13160 on 94 degrees of freedom

placed_model1 <- lm(salary ~ age + sex + age:sex + gmat_tot + age:gmat_tot + age:sex:gmat_tot + quarter + work_yrs + frstlang, data = revised_mba)
summary(placed_model1)
#RSE: 12870 on 93 degrees of freedom

placed_model1 <- lm(salary ~ age + sex + age:sex + gmat_tot + age:gmat_tot + age:sex:gmat_tot + quarter + age:quarter:sex + work_yrs + age:work_yrs + frstlang, data = revised_mba)
summary(placed_model1)
#RSE: 12330 on 91 degrees of freedom

placed_model1 <- lm(salary ~ age + sex + age:sex + gmat_tot + age:gmat_tot + age:sex:gmat_tot + quarter + age:quarter:sex + work_yrs + age:work_yrs + age:work_yrs:sex + frstlang, data = revised_mba)
summary(placed_model1)
#RSE: 11930 on 90 degrees of freedom
#Multiple R-squared:  0.6068,	Adjusted R-squared:  0.5543




#########################################
#                                       #  
#  COMPARISON WITH UNPLACED STUDENTS    #
#                                       #
#########################################


unplaced <- subset(mba, mba$salary==0)
placed <- revised_mba

attach(unplaced)
table(gmat_tot, satis)
xtabs(satis ~ gmat_tot + age + sex)

#Chi-squared tests 
chisq.test(table(gmat_tot, satis))
chisq.test(table(gmat_tot, age))
chisq.test(table(gmat_tot, work_yrs))
