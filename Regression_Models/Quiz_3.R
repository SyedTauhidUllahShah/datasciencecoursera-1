# Question 1
data(mtcars)
fit <- lm(mpg ~ wt + factor(cyl), data = mtcars)
#print(coef(fit))

# Question 2
fit_adj <- lm(mpg ~ factor(cyl), data=mtcars)
fit_unadj <- lm(mpg ~ wt + factor(cyl), data=mtcars)
print(coef(fit_adj))
print(coef(fit_unadj))

# Question 3
fit1 <- lm(mpg ~ wt + factor(cyl), data=mtcars)
fit2 <- update(fit1, mpg ~ wt*factor(cyl))
print(anova(fit1, fit2))

# Question 4
fit <- lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
print(coef(fit))

# Question 5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
#print(hat(x, intercept = TRUE))

# Question 6
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit <- lm(y ~ x)
print(lm.influence(fit)$hat)
print(dfbetas(fit))