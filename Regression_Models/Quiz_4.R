# Question 1
library(MASS)
data(shuttle)
mydata <- shuttle
mylogit <- glm(use ~ wind - 1, data = mydata, family = "binomial")
print(summary(mylogit))
print(exp(coef(mylogit)))

# Question 2
mydata <- shuttle
mylogit <- glm(use ~ wind + magn, data = mydata, family = "binomial")
print(summary(mylogit))
print(exp(coef(mylogit)))

# Question 4
data(InsectSprays)
fit <- glm(count ~ spray - 1, family="poisson", data=InsectSprays)
print(coef(fit))
ratio <- coef(fit)[1]/coef(fit)[2]
print(ratio)

# Question 6
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
splineTerms <- sapply(x, function(x) (x > 0)*(x-0))
xMat <- cbind(1, x, splineTerms)
fit <-lm(y ~ xMat -1)
yhat <- predict(fit)
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat, col = "red", lwd = 2)
print(summary(fit))