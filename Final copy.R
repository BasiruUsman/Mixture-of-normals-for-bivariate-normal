#RESET
par(mfrow = c(1,1))
LiquidData <- read.csv("~/Desktop/DataScience/STAT6600/Final/Problem1/LiquidRocketEngineData.csv", na = ".")
View(LiquidData)
#solid <- solidmotor
#View(solid)

log_Weight <- log(LiquidData$Weight)
log_Thrust <- log(LiquidData$Thrust)

#1b.

## logWeight
#histogram with overlayed theoretical pdf
hist(log_Weight, freq = FALSE, col = "grey")
curve(dnorm(x, mean = mean(log_Weight), sd = sd(log_Weight)), from = 0, add = TRUE, col = "blue")
legend("topleft", c("Sample Distribution","Theoretical PDF"), fill=c("grey","blue"))

#empirical and theoretical CDF
plot(ecdf(log_Weight),main="Emperical vs. Theoretical CDF")
curve(pnorm(x, mean = mean(log_Weight),sd = sd(log_Weight)), add = TRUE, col = "blue")
legend("topleft", c("Empirical CDF","Theoretical CDF"), fill=c("black","blue"))

#qqnorm
qqnorm(log_Weight)
qqline(log_Weight, distribution = qnorm, col = "red")

#shapiro test
shapiro.test(log_Weight) 
#install.packages('nortest')
library(nortest)

#anderson-darling
ad.test(log_Weight)

#cramer
cvm.test(log_Weight)


## logThrust
#histogram with overlayed theoretical pdf
hist(log_Thrust, freq = FALSE, col = "grey")
curve(dnorm(x, mean = mean(log_Thrust), sd = sd(log_Thrust)), from = 0, add = TRUE, col = "blue")
legend("topleft", c("Sample Distribution","Theoretical PDF"), fill=c("grey","blue"))

#empirical and theoretical CDF
plot(ecdf(log_Thrust),main="Emperical vs. Theoretical CDF")
curve(pnorm(x, mean = mean(log_Thrust),sd = sd(log_Thrust)), add = TRUE, col = "blue")
legend("topleft", c("Empirical CDF","Theoretical CDF"), fill=c("black","blue"))

#qqnorm
qqnorm(log_Thrust)
qqline(log_Thrust, distribution = qnorm, col = "blue")

#shapiro test
shapiro.test(log_Thrust) 

#anderson-darling
ad.test(log_Thrust)

#cramer
cvm.test(log_Thrust)

#1c.
pi <- c(.2,.5,.3) # mixing coeffs
n <- 211
classes <- sample(1:3, n, replace=TRUE, prob=pi)
LiquidData2<-LiquidData
LiquidData2$log_weight<-log(LiquidData2$Weight)
LiquidData2$log_thrust<-log(LiquidData2$Thrust)
logs <- LiquidData2[,c(12,13)]

#Plotting the mixture of the two without labels 
plot(logs, col="black", xlab="logWeight", ylab="logThrust", pch = 19)

install.packages(mixtools)
library(mixtools)

#plotting models with multiple k values
par(mfrow = c(1,1))
model2 <- mvnormalmixEM(logs, k=2, epsilon=1e-04)
plot(model2, which=2)
model3 <- mvnormalmixEM(logs, k=3, epsilon=1e-04)
plot(model3, which=2)
model4 <- mvnormalmixEM(logs, k=4, epsilon=1e-04)
plot(model4, which=2)
model5 <- mvnormalmixEM(logs, k=5, epsilon=1e-04)
plot(model5, which=2)
model10 <- mvnormalmixEM(logs, k=10, epsilon=1e-04)
plot(model10, which=2)

#Choosing one that looks the best?
# k = 3 or 2,but I will prepare 3 because it seems to be the best

# k = 3
model3$mu
model3$sigma
model3$lambda
plot(model3, which=2)
head(model3$posterior)

#1d. 
test <- as.data.frame(round(model3$posterior), 0)
colnames(test) <- c("Group1", "Group2", "Group3")

x <- test[,1]
y <- test[,2]
z <- test[,3]

logs2 <- logs

logs2$First <- x
logs2$Second <- y
logs2$Third <- z
View(logs2)

library(dplyr)
group1 <- data.frame(filter(logs2, First == 1))
group1 <- group1[,1:2] 

group2 <- data.frame(filter(logs2, Second == 1))
group2 <- group2[,1:2]

group3 <- data.frame(filter(logs2, Third == 1))
group3 <- group3[,1:2]

## TEST FOR NORMALITY IN GROUP 1 

## log_weight
#histogram with overlayed theoretical pdf
par(mfrow = c(1,1))
hist(group1$log_weight, freq = FALSE, col = "grey")
curve(dnorm(x, mean = mean(group1$log_weight), sd = sd(group1$log_weight)), from = 0, add = TRUE, col = "blue")
legend("topleft", c("Sample Distribution","Theoretical PDF"), fill=c("grey","blue"))

#empirical and theoretical CDF
plot(ecdf(group1$log_weight),main="Emperical vs. Theoretical CDF")
curve(pnorm(x, mean = mean(group1$log_weight),sd = sd(group1$log_weight)), add = TRUE, col = "blue")
legend("topleft", c("Empirical CDF","Theoretical CDF"), fill=c("black","blue"))

#qqnorm
qqnorm(group1$log_weight)
qqline(group1$log_weight, distribution = qnorm, col = "blue")

#shapiro test
shapiro.test(group1$log_weight) 

#anderson-darling
ad.test(group1$log_weight)

#cramer
cvm.test(group1$log_weight)


## logThrust
#histogram with overlayed theoretical pdf
hist(group1$log_thrust, freq = FALSE, col = "grey")
curve(dnorm(x, mean = mean(group1$log_thrust), sd = sd(group1$log_thrust)), from = 0, add = TRUE, col = "blue")
legend("topleft", c("Sample Distribution","Theoretical PDF"), fill=c("grey","blue"))

#empirical and theoretical CDF
plot(ecdf(group1$log_thrust),main="Emperical vs. Theoretical CDF")
curve(pnorm(x, mean = mean(group1$log_thrust),sd = sd(group1$log_thrust)), add = TRUE, col = "blue")
legend("topleft", c("Empirical CDF","Theoretical CDF"), fill=c("black","blue"))

#qqnorm
qqnorm(group1$log_thrust)
qqline(group1$log_thrust, distribution = qnorm, col = "blue")

#shapiro test
shapiro.test(group1$log_thrust) 

#anderson-darling
ad.test(group1$log_thrust)

#cramer
cvm.test(group1$log_thrust)

## TEST FOR NORMALITY IN GROUP 2 
## logWeight
#histogram with overlayed theoretical pdf
hist(group2$log_weight, freq = FALSE, col = "grey")
curve(dnorm(x, mean = mean(group2$log_weight), sd = sd(group2$log_weight)), from = 0, add = TRUE, col = "blue")
legend("topleft", c("Sample Distribution","Theoretical PDF"), fill=c("lightgrey","blue"))

#empirical and theoretical CDF
plot(ecdf(group2$log_weight),main="Emperical vs. Theoretical CDF")
curve(pnorm(x, mean = mean(group2$log_weight),sd = sd(group2$log_weight)), add = TRUE, col = "blue")
legend("topleft", c("Empirical CDF","Theoretical CDF"), fill=c("black","blue"))

#qqnorm
qqnorm(group2$log_weight)
qqline(group2$log_weight, distribution = qnorm, col = "blue")

#shapiro test
shapiro.test(group2$log_weight) 

#anderson-darling
ad.test(group2$log_weight)

#cramer
cvm.test(group2$log_weight)


## logThrust
#histogram with overlayed theoretical pdf
hist(group2$log_thrust, freq = FALSE, col = "grey")
curve(dnorm(x, mean = mean(group2$log_thrust), sd = sd(group2$log_thrust)), from = 0, add = TRUE, col = "blue")
legend("topleft", c("Sample Distribution","Theoretical PDF"), fill=c("lightgrey","blue"))

#empirical and theoretical CDF
plot(ecdf(group2$log_thrust),main="Emperical vs. Theoretical CDF")
curve(pnorm(x, mean = mean(group2$log_thrust),sd = sd(group2$log_thrust)), add = TRUE, col = "blue")
legend("topleft", c("Empirical CDF","Theoretical CDF"), fill=c("black","blue"))

#qqnorm
qqnorm(group2$log_thrust)
qqline(group2$log_thrust, distribution = qnorm, col = "blue")

#shapiro test
shapiro.test(group2$log_thrust) 

#anderson-darling
ad.test(group2$log_thrust)

#cramer
cvm.test(group2$log_thrust)


## TEST FOR NORMALITY IN GROUP 3 
## logWeight 
#histogram with overlayed theoretical pdf
hist(group3$log_weight, freq = FALSE, col = "grey")
curve(dnorm(x, mean = mean(group3$log_weight), sd = sd(group3$log_weight)), from = 0, add = TRUE, col = "blue")
legend("topleft", c("Sample Distribution","Theoretical PDF"), fill=c("grey","blue"))

#empirical and theoretical CDF
plot(ecdf(group3$log_weight),main="Emperical vs. Theoretical CDF")
curve(pnorm(x, mean = mean(group3$log_weight),sd = sd(group3$log_weight)), add = TRUE, col = "blue")
legend("topleft", c("Empirical CDF","Theoretical CDF"), fill=c("black","blue"))

#qqnorm
qqnorm(group3$log_weight)
qqline(group3$log_weight, distribution = qnorm, col = "blue")

#shapiro test
shapiro.test(group3$log_weight) 

#anderson-darling
ad.test(group3$log_weight)

#cramer
cvm.test(group3$log_weight)


## logThrust
#histogram with overlayed theoretical pdf
hist(group3$log_thrust, freq = FALSE, col = "grey")
curve(dnorm(x, mean = mean(group3$log_thrust), sd = sd(group3$log_thrust)), from = 0, add = TRUE, col = "blue")
legend("topleft", c("Sample Distribution","Theoretical PDF"), fill=c("lightgrey","blue"))

#empirical and theoretical CDF
plot(ecdf(group3$log_thrust),main="Emperical vs. Theoretical CDF")
curve(pnorm(x, mean = mean(group3$log_thrust),sd = sd(group3$log_thrust)), add = TRUE, col = "blue")
legend("topleft", c("Empirical CDF","Theoretical CDF"), fill=c("black","blue"))

#qqnorm
qqnorm(group3$log_thrust)
qqline(group3$log_thrust, distribution = qnorm, col = "blue")

#shapiro test
shapiro.test(group3$log_thrust) 

#anderson-darling
ad.test(group3$log_thrust)

#cramer
cvm.test(group3$log_thrust)


#1e. 
logs3<-logs2
logs3$Groups<-ifelse(logs3$First==1,1,ifelse(logs3$Second==1,2,3)) 
logs3<-logs3[,c(1,2,6)]
#install.packages("caret")
library(MASS)
library(caret)
np <- 300    

nd.x = seq(from = min(logs3$log_weight), to = max(logs3$log_thrust), length.out = np)
nd.y = seq(from = min(logs3$log_thrust), to = max(logs3$log_thrust), length.out = np)
nd = expand.grid(log_weight = nd.x, log_thrust = nd.y)
model <- lda(Groups ~ ., data=logs3)
prd = as.numeric(predict(model, newdata = nd)$class)
p = predict(model, newdata= nd)
p.x = seq(from = min(p$x[,1]), to = max(p$x[,1]), length.out = np) #LD1 scores
p.y = seq(from = min(p$x[,2]), to = max(p$x[,2]), length.out = np) #LD2 scores
plot(model, panel = function(x, y, ...) { points(x, y, ...) },
     col = c(4,2,3)[factor(logs3$Groups)], 
     pch = c(17,19,15)[factor(logs3$Groups)],
     ylim=c(-3,3), xlim=c(-5,5))
contour(x = p.x, y = p.y, z = matrix(prd, nrow = np, ncol = np), 
        levels = c(1, 2, 3), add = TRUE, drawlabels = FALSE)

#1.f

LData1<-LiquidData2[,c(6,11)]
LData1$First <- x
LData1$Second <- y
LData1$Third <- z
Goup1_country<-(filter(LData1, First == 1))
summary(Goup1_country)
#it shows about 53.54% of group 1 are made from Russia and about 29.3% are made from USA 
#The value of the ISP has a minimum of  268 and mean of 339.2 
Goup2_country<-(filter(LData1, Second == 1))
#47% are made in USA 19% from Germany and 25% from Russia
summary(Goup2_country)
#Group 2 has anvarage ISP of 356.8
Goup3_country<-(filter(LData1, Third == 1))
#In group3 54% are made in Russia, 25% are made in USA 
summary(Goup3_country)
# In group 3 has an ISP averge of 373.7 which is the highest in the three groups.

#Q2.
#Lasso and Ridge regression are shrinkage methods, we can fit a model containing all p predictors using a technique
#that constrains or regularizes the coefficient estimates, or equivalently, that
#shrinks the coefficient estimates towards zero.
#Let us start with the Ridge Regression 
solidmotor <- read.csv("~/Desktop/DataScience/STAT6600/Final/Problem2/solidmotor.csv")
solidtrain<-(filter(solidmotor, Selected == "Train"))
solidtest<-(filter(solidmotor, Selected == "Test"))
#For y3
solidtrain1<-solidtrain[,-c(1,10,11,13,14)]
solidtest1<-solidtest[,-c(1,10,11,13,14)]
library(glmnet)
train.mat = model.matrix(y3~., data=solidtrain1)
test.mat = model.matrix(y3~., data=solidtest1)  
grid = 10 ^ seq(4, -2, length=100)
mod.ridge = cv.glmnet(train.mat, solidtrain1[, "y3"], alpha=0, lambda=grid, thresh=1e-12)
#we now select the best lambda 
lambda.best = mod.ridge$lambda.min
#now we check for the testing error
ridge.pred = predict(mod.ridge, newx=test.mat, s=lambda.best)
mean((solidtest1[, "y3"] - ridge.pred)^2)
#The test RSS is 0.5275427
#For y5
solidtrain2<-solidtrain[,-c(1,10,11,12,13)]
solidtest2<-solidtest[,-c(1,10,11,12,13)]
train.mat2 = model.matrix(y5~., data=solidtrain2)
test.mat2 = model.matrix(y5~., data=solidtest2) 
mod.ridge2 = cv.glmnet(train.mat2, solidtrain2[, "y5"], alpha=0, lambda=grid, thresh=1e-12)
#we now select the best lambda 
lambda.best2 = mod.ridge2$lambda.min
#We now check for the test RSS
ridge.pred2 = predict(mod.ridge2, newx=test.mat2, s=lambda.best2)
mean((solidtest2[, "y5"] - ridge.pred2)^2)
#The test RSS is 0.008394384 significanlty better than the one obtained while using y3
#Now for LASSO 
#we start with y3 as the responds variable
mod.lasso1 = cv.glmnet(train.mat, solidtrain1[, "y3"], alpha=1, lambda=grid, thresh=1e-12)
lambda.bestl1 = mod.lasso1$lambda.min
lambda.bestl1
lasso.pred1 = predict(mod.lasso1, newx=test.mat, s=lambda.bestl1)
mean((solidtest1[, "y3"] - lasso.pred1)^2)
#the test RSS is  0.5288618 is slightly more than the test RSS obtained in Ridge regression
#below is to check the variables that are shrunk to zero
solid2<-solidmotor[,-c(1,10,11,13,14)]
mod.lasso_full_1 = glmnet(model.matrix(y3~., data=solid2), solid2[, "y3"], alpha=1)
predict(mod.lasso_full_1, s=lambda.bestl1, type="coefficients")
#Now we y5 as the respones 
mod.lasso2 = cv.glmnet(train.mat2, solidtrain2[, "y5"], alpha=1, lambda=grid, thresh=1e-12)
lambda.bestl2 = mod.lasso2$lambda.min
lambda.bestl2
#For test RSS we have 
lasso.pred2 = predict(mod.lasso2, newx=test.mat2, s=lambda.bestl2)
mean((solidtest2[, "y5"] - lasso.pred2)^2)
#Here the test RSS 0.008776992 is slightly higher than the obtained using Rigde regression
solid3<-solidmotor[,-c(1,10,11,12,13)]
mod.lasso_full_2 = glmnet(model.matrix(y5~., data=solid3), solid3[, "y5"], alpha=1)
predict(mod.lasso_full_2, s=lambda.bestl2, type="coefficients")



