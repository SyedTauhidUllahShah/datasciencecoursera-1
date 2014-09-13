#Question 1
Baseline <- c(140, 138, 150, 148, 135)
Week2 <- c(132, 135, 151, 146, 130)
tt <- t.test(Baseline, Week2, paired=TRUE, alternative="two.sided")
print(tt)

#Question 2
avg <- 1100
sd <- 30
n <- 9
se <-  sd/sqrt(n)
qtile <- qt(.975, df=n-1)
print(avg + c(-1, 1) * qtile * se)

#Question 3
pCoke <- .75
p <- pbinom(3, size = 4, prob = .75, lower.tail = FALSE)
print(p)

# Question 4
n1 <- 100
m1 <- 1
n2 <- 1787
m2 <- 10
lambda <- m1/n1 * n2
p <- ppois(m2, lambda, lower.tail = TRUE)
print(p)

# Question 5
n.dp <- 9
n.pla <- 9
avg.dp <- -3
avg.pla <- 1
sd.dp <- 1.5
sd.pla <- 1.8
var.pool <- ((n.dp-1)*sd.dp^2+(n.pla-1)*sd.pla^2)/(n.dp+n.pla-2)
t <- (avg.dp-avg.pla)/sqrt(var.pool*(1/n.dp+1/n.pla))
print(2*pt(-abs(t), df=n.dp+n.pla-2))

# Question 6
n <- 9
ci.l <- 1077
ci.u <- 1123
alpha <- .05
mu <- 1078

# Question 7
n <- 100
avg <- .01
sd <- .04
alpha <- .05
df <- n-1
se <- sd / sqrt(n)
t <- qt(1-alpha, df=df)
ncp <- .01/(se)
#power <- pt(avg + t*se, df=df, lower.tail=FALSE)
power <- power.t.test(n=n, sd=sd, delta=avg, sig.level=0.05, type="one.sample", alternative="one.sided")$power
print(power)

# Question 8
avg <- .01
sd <- .04
n <- power.t.test(sd=sd, delta=avg, sig.level=0.05, alternative = "one.sided", type="one.sample", power = .90)
print(n)

# Question 10
n.Met <- 288
avg.Met <- 44
n.Got <- 288
avg.Got <- 42.04
sd <- 12
z <- (avg.Met - avg.Got)/sqrt(sd^2*(1/n.Met+1/n.Got))
p = 2*pnorm(-abs(z))
print(p)
