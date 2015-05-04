


# Normal distribution
## If Z is standard normal
## X = mu + sigma*Z ~ N(mu, sigma^2)

## Quantiles
## 
plot(rnorm(10000, mean=0, sd=1))
hist(rnorm(10000, mean=0, sd=1))

# Uniform distribution
plot(runif(1000, min=0, max=1))
hist(runif(1000, min=0, max=1))

# binomial
## Suppose a friend has 8 children (oh my!), 7 of which are girls and none are twins
## If each gender has probability 0.5 for each birth, what is the probability of getting
## 7 or more girls out of 8 birth?
choose(8, 7) * 0.5^8 + choose(8, 8) * 0.5^8
### or
pbinom(6, size = 8, prob = 0.5, lower.tail = FALSE)

plot(rbinom(1000, size=10, prob=0.70))
hist(rbinom(1000, size=10, prob=0.70))

# poisson
plot(rpois(1000, lambda=10))
hist(rpois(1000, lambda=10))