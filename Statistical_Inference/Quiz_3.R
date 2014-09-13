# Question 1
mu <- 1100
sd <- 30
n <- 9
error <- qt(.975, df=n-1)*sd/sqrt(n)
confint <- mu +c(-1, 1)*error
print(confint)

# Question 2
n <- 9
deltaWt <- -2
s <- sqrt(n)*(0-deltaWt)/qt(.975, df=n-1)
print(s)

# Question 4
mean.New <- 3
var.New <- .60
n.New <- 10
mean.Old <- 5
var.Old <- .68
n.Old <- 10
var.Pool <- ((n.New-1)*var.New + (n.Old-1)*var.Old)/(n.New + n.Old - 2)
error <- sqrt(var.Pool)*sqrt(1/n.New + 1/n.Old)
confint <- mean.New-mean.Old+c(-1,1)*qt(.975, df=n.New+n.Old-2)*error
print(confint)

# Question 6
n.New <- 100
n.Old <- 100
mean.New <- 4
sd.New <- .5
mean.Old <- 6
sd.Old <- 2
var.Pool <- ((n.New-1)*sd.New^2 + (n.Old-1)*sd.Old^2)/(n.New + n.Old - 2)
error <- sqrt(var.Pool)*sqrt(1/n.New + 1/n.Old)
confint <- mean.Old-mean.New+c(-1,1)*qnorm(.975)*error
print(confint)

# Question 7
n.Treated <- 9
n.Placebo <- 9
mean.Treated <- -3
mean.Placebo <- 1
sd.Treated <- 1.5
sd.Placebo <- 1.8
var.Pool <- ((n.Treated-1)*sd.Treated^2 + (n.Placebo-1)*sd.Placebo^2)/(n.Treated + n.Placebo - 2)
error <- sqrt(var.Pool)*sqrt(1/n.Treated + 1/n.Placebo)
confint <- mean.Treated-mean.Placebo+c(-1,1)*qt(.95, df=n.Treated+n.Placebo-2)*error
print(confint)