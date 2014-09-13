# Question 1
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit <- lm(y ~ x)
print(summary(fit)$coefficients)

# Question 2
print(summary(fit))

# Question 3
data(mtcars)
x <- mtcars$wt
y <- mtcars$mpg
fit <- lm(y ~ x)
newdata = data.frame(x=mean(x))
print(predict(fit, newdata, interval="confidence"))

# Question 5
newdata = data.frame(x=3)
print(predict(fit, newdata, interval="prediction"))

# Question 6
x <- mtcars$wt/2
fit <- lm(y ~ x)
print(confint(fit, 'x', level=0.95))

# Question 9
x <- mtcars$wt
y <- mtcars$mpg
fitNum <- lm(y ~ x)
SSEDen <- sum((y-mean(y))^2)
SSENum <- sum((y-predict(fitNum))^2)
print(SSENum/SSEDen)
