#### Using a loop in R ####
set.seed(2345)
# Set input values
n <- # insert sample size value
mean <- # null or alternative hypothesis value
sd <- # for s.d.
numTrials <- # insert number
alpha <- # chosen significance level
# Set a counter to determine the number of rejected hypothesis tests
count<- 0
for( i in 1:numTrials){
# Generate data
y <- # generate sample from desired distribution
# Perform test
t <- #insert specific test as function of data, y
count <- count + (#insert the part of test object containing the
test's p-value < alpha)
}
# Power = proportion of rejections
power <- count/numTrials
power

#### Using a function in R ####
# The Function below will use sample size, mean, SD, number of trials
# and alpha as inputs
compute_power = function(n, mean, sigma, numTrials, alpha){
  # Generate a matrix with the data
  sample = matrix(#rdist(n*numTrials, mean, sigma), ncol=numTrials)
    # rdist is shorthand for the specific distribution from which random
    # variates are generated
    # Now, write out elements of the test statistic, e.g.
    # xbar <- apply(sample, 2, mean)#find mean of each column of matrix
    # variance <- apply(sample, 2, var)#find variance each column of
    matrix
    # df.num = n-1 #e.g. degrees of freedom might be needed
    # .
    # combine elements of test statistic, e.g. numerator, denominator
    # .
    test.stat = #test statistic formula based on elements above
      # Result of the function is the proportion of rejected hypothesis
      # tests over all of the trials
      return (mean(abs(test.stat) >= #qdist((1-(alpha/2)), parameter of
                     dist)))
  # qdist is shorthand for the specific quantile function for the
  # sampling distribution of the test statistic
}
# Now, call the function with the arguments it needs - e.g. n, mean,
# sd, number of trials, alpha
set.seed(2345)
compute_power(#n, #mean, #sd, #trials, #alpha)