####BIOS 6611 HW 1####

#############################################################################
#Exercise 1-RLab1 (Getting Started)
#
set.seed(123) #in order to reproducibly simulate data, need to set a seed

#1a
n <- 10000
norm <- rnorm(n, mean = 125, sd = 8)
pois <- rpois(n, lambda = 1.5)
binom <- rbinom(n, size = 5, prob = 0.15)


#1b
#normal: should be mean = 125, sd = 8
mean(norm)
sd(norm)
#poisson: should be mean = 1.5, sd = 1.22 (sqrt(1.5))
mean(pois)
sd(pois)
#binomal: should be mean = 0.75 (5*0.15), sd = 0.7984 (sqrt(5*0.15*0.85))   
mean(binom)
sd(binom)


#1c
par(mfrow=c(2,3)) #use this to make 1 figure with all 6 plots

hist(norm)
hist(pois)
hist(binom)

boxplot(norm, main='Box plot of norm')
boxplot(pois, main='Box plot of pois')
boxplot(binom, main='Box plot of binom')



#############################################################################
#Exercise 2-RLab1 (Sampling Distributions for Other Statistics)

#2a
set.seed(2)
nsim <- 1000
n <- 10
meanV <- rep(NA, 1000)
medianV <- rep(NA, 1000)
varV <- rep(NA, 1000)

for(i in 1:nsim){
  random <- rnorm(n, mean = 40, sd = 10)
  meanV[i] <- mean(random)
  medianV[i] <- median(random)
  varV[i] <- var(random)
}

par( mfrow=c(1,3) )

hist(meanV)
hist(medianV)
hist(varV)


#2b
#Normally distributed


#2c
varVshift <- varV*(9/10^2)
hist(varVshift)
curve(dchisq(x, df = 9), col="green", xlim = c(0,25),
      ylim = c(0,0.12))

plot(dchisq(0:25, df = 9), col="green", xlim = c(0,25),
      ylim = c(0,0.12))



#############################################################################
#Exercise 3-RLab1 (Properties of Estimators: Bias, Consistency, and Efficiency)

## 3a-Bias of median as estmiator
set.seed(0203) # Set seed for reproducibility
sim_norm <- rnorm(n=100,mean=70,sd=sqrt(15)) # Simulate a normal distribution
med <- median(sim_norm)  # Calculate the median
bias <- med-70  # Calculate the bias (Estimate - population mean)
bias


## 3b-Consistency of median as estimator
set.seed(0203) # Set seed for reproducibility

# Increasing sample size
ns<-seq(100,100000,by=100)
bias_median<-sapply(ns,function(x){
  sim<-rnorm(n=x,mean=70,sd=sqrt(15)) #Simulate normal distributions for each n
  medians<-median(sim) # Calculate the median of the samples
  bias<-medians-70 # Calculate the bias of the samples
  return(bias)
})

# Plot the results
plot(x=ns,y=bias_median,type='l')


## 3c-how variance changes with sample size
set.seed(0203) # Set seed the same as before

# Increasing sample size
ns<-seq(100,100000,by=100)
var_median<-sapply(ns,function(x){
  sim<-rnorm(n=x,mean=70,sd=sqrt(15))
  med<-median(sim)
  var_median<-sum((sim-med)^2)/length(sim)
  return(var_median)
})

# Plot the results
plot(x=ns,y=var_median,type='l')


## 3d-efficiency of estimators
set.seed(0203) # Set seed the same as before

# Increasing sample size
ns<-rep(1000,100000)
medians<-sapply(ns,function(x){
  sim<-rnorm(n=x,mean=70,sd=sqrt(15))
  med<-median(sim)
  return(med)
})


set.seed(0203) # Set seed the same as before

# Increasing sample size
ns<-rep(1000,100000)
means<-sapply(ns,function(x){
  sim<-rnorm(n=x,mean=70,sd=sqrt(15))
  m<-mean(sim)
  return(m)
})

var(medians)

var(means)

var(means)/var(medians) # Relative efficiency


#############################################################################
#Exercise 4---RLab1 (Study Design Sprints)

#4a, sampling from IDs
id.vec <- seq(from=9001, to=9250, by=1) #creates vector of record IDs

set.seed(5) #set seed to reproducibly sample from id.vec
id.sample <- sample(id.vec, 30, replace=F) #randomly samples 30 records for audit


#4b, manipulating names data frame
#4b.i
names <- read.table( 'filepath/names.txt')

#4b.ii (lots of ways one could do this)
names$team <- 'blue' #add column where everyone is on the "blue" team, we will write this over with our sample below
set.seed(52)
names$team[sample(1:nrow(names), size=nrow(names)/2, replace=F)] <- 'red'

#4b.iii
head(names, n=10)

#4b.iv
table(names$team) #should have 15 in each group


#4c, creating data frame and subsetting the data
set.seed(515)
df.5c <- data.frame( id=1:10, age=runif(10, min=20, max=60) )
older <- df.5c[ which(df.5c$age >= 45) ,]
younger <- df.5c[ which(df.5c$age < 45) ,]


#4d, create data frame and store information based on given criteria (lots of ways to do this one)
assignments <- data.frame( id=1:100, dietary_intervention=NA, pharma_intervention=NA )

set.seed(54)
for(i in 1:100){
	assignments$dietary_intervention[i] <- if( runif(1) < 0.30 ){ 'D' }else{ 'ND' }
	assignments$pharma_intervention[i] <- if( rbinom(n=1,size=1,prob=0.30)==1 ){ 'P' }else{ 'NP' }
}

table(assignments$dietary_intervention,assignments$pharma_intervention)


#4e. creating data frame to simulate trial and response
#4e.i
set.seed(55)
df.trial <- data.frame( center=rep(1:5, times=40), improve=rbinom(n=5*40,size=1,prob=0.70) )

#4e.ii
numimprove.vec <- NULL #initialize vector to store number improved at each hospital
for(k in 1:5){
	numimprove.vec <- c(numimprove.vec, sum(df.trial[which(df.trial$center==k),'improve']) )
}

#4e.iii, mean number of patients improving across all hospitals
mean( numimprove.vec )


#############################################################################
#Exercise 5---RLab1 (NAWS Farm Worker Survey Simulation)

#5a, read in data
NAWS <- read.csv("filepath/NAWS2014.csv", header=T)


#5b, create histogram of years of schooling
hist(NAWS$A09, main = "Histogram of Educational Attainment \n for Migrant Farmers", xlab = "Years")


#5d, create new column with A09 (years of schooling) represented as a categorical variable
NAWS$category_edu <- "00 - 05"
NAWS[NAWS$A09 >= 6 & NAWS$A09 <= 8, ]$category_edu <- "06 - 08"
NAWS[NAWS$A09 >= 9 & NAWS$A09 <= 11, ]$category_edu <- "09 - 11"
NAWS[NAWS$A09 >= 12, ]$category_edu <- "12+"


#5e, create table and check proportions are equal to 0.250, 0.291, 0.208 and 0.251
round( table(NAWS$category_edu) / sum(table(NAWS$category_edu)), 3)


#5f, mock data set creation
set.seed(6)
mockdata <- data.frame(subject = 1:800, random = runif(n = 800))

mockdata$educ_cat <- '00 - 05'
mockdata$educ_cat[which(mockdata$random >= 0.248 & mockdata$random < 0.248+0.289)] <- '06 - 08'
mockdata$educ_cat[which(mockdata$random >= 0.248+0.289 & mockdata$random < 0.248+0.289+0.212)] <- '09 - 11'
mockdata$educ_cat[which(mockdata$random >= 0.248+0.289+0.212)] <- '12+'

mockdata$educ_years <- NA #create column to store results in
mockdata[which(mockdata$educ_cat=='00 - 05'),'educ_years'] <- runif(n=sum(mockdata$educ_cat=='00 - 05'),0,6)
mockdata[which(mockdata$educ_cat=='06 - 08'),'educ_years'] <- runif(n=sum(mockdata$educ_cat=='06 - 08'),6,9)
mockdata[which(mockdata$educ_cat=='09 - 11'),'educ_years'] <- runif(n=sum(mockdata$educ_cat=='09 - 11'),9,12)
mockdata[which(mockdata$educ_cat=='12+'),'educ_years'] <- runif(n=sum(mockdata$educ_cat=='12+'),12,17)

mockdata$edu_stop_yn <- rbinom(n=800, size=1, prob=0.8)

mockdata$educ_years[which( mockdata$edu_stop_yn == 1 & mockdata$educ_cat=='06 - 08')] <- 6
mockdata$educ_years[which( mockdata$edu_stop_yn == 1 & mockdata$educ_cat=='09 - 11')] <- 9
mockdata$educ_years[which( mockdata$edu_stop_yn == 1 & mockdata$educ_cat=='12+')] <- 12

mockdata$educ_years2 <- as.integer(mockdata$educ_years)


#5f, create histogram of mockdata$educ_years
hist(mockdata$educ_years2, breaks=seq(0,16,1), main = "Histogram of Educational Attainment \n for Mock Data", xlab = "Years")



