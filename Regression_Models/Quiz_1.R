# Question 1
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
mu <- .1471
SSE = sum(w*(x-mu)^2)
print(SSE)
mu <- 1.077
SSE = sum(w*(x-mu)^2)
print(SSE)
mu <- .0025
SSE = sum(w*(x-mu)^2)
print(SSE)
mu <- .3
SSE = sum(w*(x-mu)^2)
print(SSE)

# Question 2
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
q2 <- lm(y ~ x -1)
print(q2)

# Question 3
data(mtcars)
q3 <- lm(mpg ~ wt, data=mtcars)
print(q3)

# Question 6
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
norm6 <- (x-mean(x))/sd(x)
print(norm6)

# Question 7
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
q7 <- lm(y ~ x)
print(q7)

# Question 9
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
q9 <- mean(x)
print(q9)
