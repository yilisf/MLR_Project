# MLR_Project
Spring 2023 STAT632 Final Project

```{r}
crab <- read.csv("CrabAgePrediction.csv")
summary(crab)
library(dplyr)
crab <- filter(crab,Height>0&Height<1)
crab$Sex <- as.factor(crab$Sex)

#Count Sex and Calculate the proportion
table(crab$Sex)
round(1224/(1224+1231+1434),2)
round(1231/(1224+1231+1434),2)
round(1434/(1224+1231+1434),2)

#Calculation for Numeric variables
round(sapply(crab,mean),2)
round(sapply(crab[,2:9],sd),2)

#Data Exploration with Scatter plot and box plot
pairs(Age~Length+Diameter+Height+Weight+Shucked.Weight+Viscera.Weight+Shell.Weight,crab)
boxplot(Age~Sex,crab)

#Model 1
lm1 <- lm(Age~.,crab)
summary(lm1)

#Assumption check for Model 1
par(mfrow=c(2,2))
plot(lm1,1:2)
shapiro.test(resid(lm1))
library(car)
ncvTest(lm1)
vif(lm1)

#Data Transformation
pacman::p_load(MASS,car)
boxcox(lm1,lambda=seq(-0.7,0.7,by=0.05))
summary(powerTransform(lm1))

#Model 2
lm2 <- lm(log(Age)~.,crab)
summary(lm2)

#Assumption check for Model 2
par(mfrow=c(2,2))
plot(lm1,1:2)
plot(lm2,1:2)
shapiro.test(resid(lm2))
library(lmtest)
ncvTest(lm2)
vif(lm2)

#Standardized Residual vs Predictors
par(mfrow=c(2,4))
plot(crab$Sex,rstandard(lm2),xlab="Sex",ylab="Standardized Residual")
plot(crab$Length ,rstandard(lm2),xlab="Length",ylab="Standardized Residual")
plot(crab$Diameter,rstandard(lm2),xlab="Diameter",ylab="Standardized Residual")
plot(crab$Height,rstandard(lm2),xlab="Height",ylab="Standardized Residual")
plot(crab$Weight,rstandard(lm2),xlab="Weight",ylab="Standardized Residual")
plot(crab$Shucked.Weight,rstandard(lm2),xlab="Shucked.Weight",ylab="Standardized Residual")
plot(crab$Viscera.Weight,rstandard(lm2),xlab="Viscera.Weight",ylab="Standardized Residual")
plot(crab$Shell.Weight,rstandard(lm2),xlab="Shell.Weight",ylab="Standardized Residual")
      
#Outliers and High Leverage Points
plot(hatvalues(lm2),rstandard(lm2))
abline(v=0.04,lty=2,col="red")
abline(h=-4.2,lty=2,col="blue")
ind <- which(rstandard(lm2)<(-4.2)|hatvalues(lm2)>0.05)
crab[ind,]

#Cross validation with 65% training data
set.seed(999) # set seed for reproducibility
n <- nrow(crab); n
floor(0.65*n)
# randomly sample 65% of rows for training set
train <- sample(1:n,2527)
# fit model using training data
lm_train <- lm(log(Age) ~., data=crab,subset = train)
summary(lm_train)
```

+----------------------------+-----------------+-----------------+
| ***Variables***            | ***Mean/N***    | ***SD/%***      |
+----------------------------+-----------------+-----------------+
| **Sex**                    | 3889            | 100             |
|                            |                 |                 |
| **-male**                  | 1224            | 31              |
|                            |                 |                 |
| **-female**                | 1231            | 32              |
|                            |                 |                 |
| **-indeterminate**         | 1434            | 37              |
+----------------------------+-----------------+-----------------+
| **Length(feet)**           | 1.31            | 0.30            |
+----------------------------+-----------------+-----------------+
| **Diameter(feet)**         | 1.02            | 0.25            |
+----------------------------+-----------------+-----------------+
| **Height(feet)**           | 0.35            | 0.10            |
+----------------------------+-----------------+-----------------+
| **Weight(ounces)**         | 23.57           | 13.88           |
+----------------------------+-----------------+-----------------+
| **Shucked Weight(ounces)** | 10.21           | 6.27            |
+----------------------------+-----------------+-----------------+
| **Viscera Weight(ounces)** | 5.14            | 3.10            |
+----------------------------+-----------------+-----------------+
| **Shell Weight(ounces)**   | 6.79            | 3.94            |
+----------------------------+-----------------+-----------------+
| **Age(months)**            | 9.96            | 3.22            |
+----------------------------+-----------------+-----------------+


+-----------------------------------+-----------------------------+----------------------------+
| ***Model***                       | ***1***                     | ***2***                    |
+-----------------------------------+-----------------------------+----------------------------+
| **No. of Significant Predictors** | 7                           | 8                          |
+-----------------------------------+-----------------------------+----------------------------+
| **Adjusted R-Squared**            | 0.5415                      | 0.6049                     |
+-----------------------------------+-----------------------------+----------------------------+
| **Normality: shapiro.test**       | W=0.92972, p-value\<2.2e-16 | W=0.9843, p-value\<2.2e-16 |
+-----------------------------------+-----------------------------+----------------------------+
| **Constant Variance: ncvTest**    | p-value\<2.22e-16           | p-value=0.032343           |
+-----------------------------------+-----------------------------+----------------------------+


|                                     |                  |                     |
|-------------------------------------|------------------|---------------------|
| ***Data***                          | ***Overall***    | ***Training(65%)*** |
| **No. of Observations**             | 3889             | 2527                |
| **Adjusted R-squared**              | 0.6049           | 0.6096              |
| **Signs of Estimated Coefficients** | Same as Training | Same as Overall     |
