library(ggplot2)
library(manipulate)
mu0 = 30
myplot <- function(sigma, mua, n, alpha) {
  g = ggplot(data.frame(mu = c(27, 36)), aes(x = mu))
  g = g + stat_function(fun = dnorm, geom = "line", args = list(mean = mu0,
                                                                sd = sigma/sqrt(n)), size = 2, col = "red")
  g = g + stat_function(fun = dnorm, geom = "line", args = list(mean = mua,
                                                                sd = sigma/sqrt(n)), size = 2, col = "blue")
  xitc = mu0 + qnorm(1 - alpha) * sigma/sqrt(n)
  g = g + geom_vline(xintercept = xitc, size = 3)
  g
}
manipulate(myplot(sigma, mua, n, alpha), sigma = slider(1, 10, step = 1, initial = 4),
           mua = slider(30, 35, step = 1, initial = 32), n = slider(1, 50, step = 1,
                                                                    initial = 16), alpha = slider(0.01, 0.1, step = 0.01, initial = 0.05))

power.t.test(n=16, delta=2, sd=4, type="one.sample", alt="one.sided")

library(datasets)
data(mtcars)

m1 <- c(140,138,150,148,135)
m2 <- c(132,135,151,146,130)

# Quiz 4
# Q1
t.test(m1,m2, paired = TRUE, var.equal=TRUE, alternative="two.sided")$p.value

#Q2
n <- 9
μ <- 1100
σ <- 30
quantile = 0.975 # is 95% with 2.5% on both sides of the range
confidenceInterval = μ + c(-1, 1) * qt(quantile, df=n-1) * σ / sqrt(n)
confidenceInterval

#Q3
n <- 4
x <- 3
test <- binom.test(x=x, n=n, alt="greater")
round(test$p.value,2)

#Q4
rate <- 1/100
errors <- 10
days <- 1787
test <-  poisson.test(errors, T = days, r = rate, alt="less")
round(test$p.value,2)

#Q5
n_y <- 9 # subjects treated
n_x <- 9 # subjects placebo
σ_y <- 1.5# kg/m2 std.dev. treated 
σ_x <- 1.8# kg/m2 std.dev. placebo 
μ_y <- -3#  kg/m2 average difference treated
μ_x <- 1#  kg/m2 average difference placebo

# calculate pooled standard deviation
n1 = 9; 
n2 = 9; 
mu1 = -3; 
sd1 = 1.5; 
mu2 = 1; 
sd2 = 1.8

SE <- ( (n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2)
t <- (mu1 - mu2) / (SE * sqrt(1/n1 + 1/n2))
p.val = 2 * pt(t, n1+n2-2) 
p.val

#Q6

#Q7
n <- 100 #subject
μ <- 0.01# m^3 brain volume loss mean
σ <- 0.04# m^3 brain volume loss std. dev.
p <- 0.05 # sign level

pow <- power.t.test(n=n, delta=μ, sd=σ , sig.level=p, type="one.sample", alt="one.sided")$power
round(pow, 2)

#Q8
μ <- 0.01# m^3 brain volume loss mean
σ <- 0.04# m^3 brain volume loss std. dev.
p <- 0.05 # sign level
pow <- 0.9 #power

n <- power.t.test(power=pow, delta=μ, sd=σ , sig.level=p, type="one.sample", alt="one.sided")$n
ceiling(n/10)*10