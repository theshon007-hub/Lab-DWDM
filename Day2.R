
# Q1. Covariance and Correlation
data1 <- data.frame(A=c(18,2,20), B=c(22,28,10), C=c(20,40,40))
print(cov(data1$B, data1$C))
print(cov(data1))
print(cor(data1$B, data1$C))
print(cor(data1))


# Q2. Equal-Frequency Binning and Smoothing
x <- sort(c(18,18,18,20,20,20,20,20,20,20,21,21,
            21,21,25,25,25,25,25,28,28,30,30,30))
bins <- split(x, rep(1:3, each=8))
print(bins)
print(unlist(lapply(bins, function(b) rep(mean(b), length(b)))))
print(unlist(lapply(bins, function(b)
  sapply(b, function(v)
    if(abs(v-min(b)) <= abs(v-max(b))) min(b) else max(b)))))
hist(x, breaks=3, col="lightblue", main="Equal-Frequency Data")


# Q2(b). All Electronics Prices
prices <- sort(c(1,1,5,5,5,5,5,8,8,10,10,10,10,12,
                 14,14,14,15,15,15,15,15,15,18,18,18,18,18))
pbins <- split(prices, cut(seq_along(prices), 3, labels=FALSE))
print(pbins)
print(unlist(lapply(pbins, function(b) rep(mean(b), length(b)))))
print(unlist(lapply(pbins, function(b)
  sapply(b, function(v)
    if(abs(v-min(b)) <= abs(v-max(b))) min(b) else max(b)))))
hist(prices, breaks=3, col="lightgreen", main="Electronics Prices")


# Q3. Class A and Class B
A <- c(76,35,47,64,95,66,89,36,84)
B <- c(51,56,84,60,59,70,63,66,50)
print(c(Mean=mean(A), Median=median(A), Range=diff(range(A))))
print(c(Mean=mean(B), Median=median(B), Range=diff(range(B))))
boxplot(A, B, names=c("Class A","Class B"),
        col=c("lightblue","lightgreen"), main="Class Comparison")


# Q4. Normalization
x <- c(200,300,400,600,1000)
print((x-min(x))/(max(x)-min(x)))
print((x-mean(x))/sd(x))

# Min-max normalization of 80000 between 50000 and 100000
print((80000-50000)/(100000-50000))


# Q5. AirPassengers Histogram
hist(AirPassengers, breaks=seq(100,700,by=150),
     xlim=c(100,700), col="lightblue",
     main="AirPassengers Histogram", xlab="Passengers")


# Q6. Multiple Lines using mtcars
plot(mtcars$mpg, type="l", col="blue",
     ylim=range(c(mtcars$mpg,mtcars$qsec)),
     xlab="Car Index", ylab="Values", main="MPG and QSEC")
lines(mtcars$qsec, col="red")
legend("topright", legend=c("MPG","QSEC"),
       col=c("blue","red"), lty=1)


# Q7. Water Dataset and Linear Regression
data(water, package="MASS")
plot(water$hardness, water$mortality,
     xlab="Hardness", ylab="Mortality", main="Water Dataset")
model <- lm(mortality ~ hardness, data=water)
abline(model, col="red")
print(summary(model))
print(predict(model, newdata=data.frame(hardness=88)))


# Q8. Boxplot of MPG and Cylinders
boxplot(mpg ~ factor(cyl), data=mtcars,
        col="lightblue", xlab="Cylinders", ylab="MPG",
        main="MPG by Number of Cylinders")


# Q9. Tennis Players' Scores and Outliers
points <- list(Player1=c(10,12,14,15,16,18,20),
               Player2=c(8,10,11,12,13,15,50),
               Player3=c(9,11,13,14,15,17,19))
boxplot(points, col="lightgreen",
        main="Tennis Players' Scores",
        ylab="Points Scored")


# Q10. Diabetes Dataset: Blood Pressure vs Age
# Keep diabetes.csv in the current working directory
diabetes <- read.csv("diabetes.csv")
plot(diabetes$Age, diabetes$BloodPressure,
     xlab="Age", ylab="Blood Pressure",
     main="Blood Pressure vs Age", pch=19, col="blue")
barplot(tapply(diabetes$BloodPressure, diabetes$Age, mean),
        las=2, col="lightblue",
        main="Average Blood Pressure by Age",
        xlab="Age", ylab="Mean Blood Pressure")

