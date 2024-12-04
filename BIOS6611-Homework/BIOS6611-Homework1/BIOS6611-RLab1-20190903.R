#BIOS6611 lab1 sampling and simulations
mock.sample.size <- 20
simval <- rnorm(n = mock.sample.size, mean = 8, sd = 1)
simval[1:5]
hist(simval, main = "", xlab = "Normal, N=20", xlim = c(4, 12))
mean(simval)
sd(simval)

#issue 1: seeds for pseudo-random number generators
set.seed(seed = 999)
rnorm(n = 3, mean = 0, sd = 2)
rnorm(n = 3, mean = 0, sd = 2)
set.seed(seed = 999)
rnorm(n = 3, mean = 0, sd = 2)
#issue 2: variation at low sample sizes: higher variability in the distribution of sample.
#the sample itself has a distribution. the sample from the population may different from another sample due to random sampling error

set.seed(555)
hist(x = rnorm(n=1000, mean = 8, sd = 1), 
     main = "",
     xlab = "Normal, N=1000",
     xlim = c(4,12))

x <- rnorm(n = 1000, mean = 8, sd = 1)
hist(x, main = "", xlab = "Normal distribution, N=1000", xlim = c(4, 12))

#uniform distribution is determined entirely by its minimum and maximum

set.seed(100)
uniform.example <- runif(n = 10000, min=0, max=1)
uniform.example[1:10]
par(cex = 0.8)
hist(uniform.example, main = "", xlab = "Uniform(0, 1) Sample")
# binomial distribution represents the number of "successes" of a binary outcome. 
# binomial distribution example
set.seed(111)
rbinom(n=1, size = 10, prob = 0.5)
# example to highlight difference between "n" and "size"
set.seed(111)
par(cex = 0.8)
binom_example <- rbinom(n = 200, size = 10, prob = 0.5)
# note: setting "breaks" argument to change x-axis defaults,
# in order to explicitly show that this is *discrete*
hist(binom_example, main = "",
     xlab = "Binomial(n = 10, p = 0.5)",
     breaks = seq(-0.5, 10.5, 0.5))

# figure 5-top
set.seed(303)
hist(rpois(n = 400, lambda = 2),
     main = "", xlab = "Poisson, lambda = 2",
     breaks = seq(0, 20, 1))
# figure 5-mid
hist(rpois(n = 400, lambda = 4),
     main = "", xlab = "Poisson, lambda = 4",
     breaks = seq(0, 20, 1))
# figure 5-bot
hist(rpois(n = 400, lambda = 10),
     main = "", xlab = "Poisson, lambda = 10",
     breaks = seq(0, 20, 1))
# "r"set of functions like rnorm() for simulation sampling
# dnorm() for density
# pnorm() for cumulative distribution function
# qnorm() for quantile

# d-funtion for probability density function PDF
# the density represnts this for a theoretical distribution
# density function - dnorm()
x.values <- seq(4, 12, 0.1)
density.of.normal <- dnorm(x = x.values,
                           mean = 8, sd = 1)
plot(x.values, density.of.normal,
     xlab = "X-Value", ylab = "Density", type = "l")
# discrete variables have an analogous "density" 
# the probability mass function p(X) for which p(x)=P(X=x).
# dbinom() example 1
dbinom(x = 0, size = 2, prob = 0.5) +
  dbinom(x = 1, size = 2, prob = 0.5) +
  dbinom(x = 2, size = 2, prob = 0.5)
# dbinom() example 2
dbinom(0.4, size = 2, prob = 0.5)

### p-funtions cumulative distibution functions CDF
#the cumulative distribution function (CDF)
#P(X ??? x). As it is a probability
#the CDF ranges between 0 and 1.
# cumulative probability (distribution function)
x_values <- seq(4, 10, 0.1)
density_of_normal <- pnorm(q = x_values,
                           mean = 8, sd = 1)
plot(x_values, density_of_normal,
     xlab = "X-Value", ylab = "CDF", type = "l")
# example 1 of using cdf
pnorm(q = 7, mean = 8, sd = 1)
# example 2 of using cdf
pbinom(q = 17, size = 20, prob = 0.5) -
  pbinom(q = 15, size = 20, prob = 0.5)
### q-funtion quantiles
qnorm(p = 0.95, mean = 8, sd = 1)


#### the sampling distribution of the mean
# the sample has its own distribution, with random samples potentially differing from one another by chance and chance alone.
# sampling error
# the sample mean itself varies from sample to sample and thus itself has a distribution. 
# this distibution is known as the sampling distribution of the mean.
# simulation parameters
number_of_sims <- 500
sample_size <- 20
set.seed(888)
# empty vector to store sample means
vector_of_sample_means <- rep(-9, number_of_sims)
# for i = 1, 2, ..., 500
# calculate the sample mean, and store in vector
for (i in 1:number_of_sims) {
  vector_of_sample_means[i] <-
    mean( rnorm(n = sample_size, 
                mean = 8, sd = 1) )
}
# check to make sure that our simulation worked
# print the first five entires
vector_of_sample_means[1:5]
# are the first five entries equal to -9?
vector_of_sample_means[1:5] == -9
# plot the sampling distribution of the mean
hist(vector_of_sample_means, xlim=c(7,9),
     xlab = "Values of Sample Mean",
     main = "")
# as n increases, the sampling distribution of the mean changes.
# the population is distributed as a normal with mean mu, and standard deviation sigma, 
# X ??? Normal(mu, sigma), sample mean is X.hat ??? Normal(mu, sigma/n)
# from sample to sample decreases as the sample size increases.
# variation at low sample sizes
# reiterate that the "distribution of a sample" is different from the "distribution of a sample's mean"

T + T + F + T + F +T
# select approximately 5 with runif()
set.seed(122)
select_with_uniform <- runif(n = 50) < 0.1
state.name[select_with_uniform]
### random assignment and the sample() function
# select approximately 5 with rbinom()
set.seed(321)
select_with_binom <-
  rbinom(n = 50, size = 1, prob = 0.1) == 1
state.name[select_with_binom]
# select exactly 5 with sample()
set.seed(123321)
sample(x = state.name, # features we want to sample from
       size = 5, # sample size
       replace = F) # don't do it with replacement

# although it may seem farfetched in this example, there are many situations where we may want an apporximate propotion of our sample to have a feature versus an exact proportion.
### Expected value and variance
# the expected value represents the "average" value of the random variable.
# the expected value of a discrete random variable
# the expected value of a continuous random variable
# variance represents the spread of a random variable


