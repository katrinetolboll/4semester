install.packages(c("coda","mvtnorm","devtools"))
library(devtools)
devtools::install_github("rmcelreath/rethinking")



                                            ##CHAPTER 1-2##
#Computing pausibilities
ways=c(0,3,8,9,0)
ways/sum(ways)

#Binomial distribution
dbinom(6,size=9,prob=0.5)

### Grid approximation

#define grid
p_grid = seq(from=0,to=1, length.out = 100)

#define prior
prior= rep(1,100)
prior=ifelse(p_grid<0.5,01)

#compute likelihood at each value in grid
likelihood = dbinom(5,size=7, prob=p_grid)

#compute product of likelihood and prior
unstd.posterior = likelihood *prior 

#standardize the posterir, so it sums to 1
posterior=(unstd.posterior / sum(unstd.posterior))

#pretty plots
plot(p_grid, posterior, type="b", xlab="probability of water", ylab="poterior probability")
mtext("100 points")

#Quadric approximation
library(rethinking)
globe.qa = map(
  alist(
    w~dbinom(9,p),
    p~dunif(0,1)
  ),
  data=list(w=6))

#display summary of quadric approximation
precis(globe.qa)

#analytic calculation
w=6
n=9
curve(dbeta(x,w+1,n-w+1),from=0, to=1)

#quadric approximation
curve(dnorm(x,0.67,0.16),lty=2, add=T)

#Excercises :) 

(0.3*0.5)/(0.3*0.5+1*0.5)

(4/12)/(2/12)

                                                ##CHAPTER 3##
##Vampire exsample
PrPV = 0.95
PrPM = 0.01
PrV = 0.001
PrP = PrPV*PrV+PrPM*(1-PrV)

#Applying Bayes Therom
PrVP=(PrPV*PrV)/PrP
PrVP

#Sampling from grid-approximate posterior

p_grid = seq( from = 0, to = 1, length.out= 1000)
prior = rep(1,1000)
likelihood = dbinom(6, size = 9, prob=p_grid)
posterior = likelihood*prior
posterior = posterior/sum(posterior)

samples = sample(p_grid, prob= posterior, size = 1e4, replace = T)

plot(samples)
library(rethinking)
dens(samples)

#intervals of defined boundaries
sum(posterior[p_grid < 0.5])

sum(samples<0.5)/1e4
sum(samples >0.5 & samples <0.75)/1e4

#intervals of defined mass
quantile(samples,0.8)
quantile(samples,c(0.1,0.9))

p_grid = seq( from = 0, to = 1, length.out= 1000)
prior = rep(1,1000)
likelihood = dbinom(3, size = 3, prob=p_grid)
posterior = likelihood*prior
posterior = posterior/sum(posterior)
samples=sample(p_grid,size=1e4,replace=T, prob=posterior)

PI(samples,prob=0.5)
HPDI(samples,prob=0.5)

##Point estimate
p_grid[which.max(posterior)]
chainmode(samples,adj=0.01)
mean(samples)
median(samples)

#Loss function
sum(posterior*abs(0.5-p_grid))
loss = sapply(p_grid,function(d) sum(posterior*abs (d-p_grid)))
p_grid [which.min(loss)]

#DUMMY DATA
dbinom(0:2,size=2,prob=0.7)
#generating 10 randomt stimulations
rbinom(10,size=2,prob=0.7)
#generating 100.000 random stimulations
dummy_w=rbinom(1e5,size=2,prob=0.7)
table(dummy_w)/1e5

dummy_w=rbinom(1e5,size=9,prob=0.7)
simplehist(dummy_w,xlab="dummy water count")

#model checking
w=rbinom(1e4,size=9,prob=samples)

#Exercises

#Easy
p_grid = seq(from=0, to=1,length.out = 1000)
prior=rep(1,1000)
likelihood=dbinom(6,size=9,prob=p_grid)
posterior=likelihood*prior
posterior=posterior/sum(posterior)
set.seed(100)
samples=sample(p_grid,prob=posterior,size=1e4,replace=T)

#3E1
sum(samples<0.2)/1e4
sum(posterior[p_grid<0.2])
 #0.00085 


#3E2
sum(posterior[p_grid>0.8])
 #0.12

#3E3
sum(samples>0.2 & samples<0.8)/1e4
 #0.89

#3E4
quantile(samples,0.2)
 #0.52

#3E5
quantile(samples,0.8)
 #0.76

#3E6
HPDI(samples,prob=0.66)
 #0.52 - 0.78

#3E7
PI(samples,prob=0.66)
 #0.50 - 0.77

#Medium
 
#3M1
p_grid = seq(from=0, to=1,length.out = 1000)
prior=rep(1,1000)
likelihood=dbinom(8,size=15,prob=p_grid)
posterior=likelihood*prior
posterior=posterior/sum(posterior)
set.seed(100)
samples=sample(p_grid,prob=posterior,size=1e4,replace=T)

#3M2
samples=sample(p_grid,prob=posterior,size=1e4,replace=T)
HPDI(samples, prob=0.9)
 #0.32 - 0.72

#3M3
posterior.predictive = rbinom(1e4,size=15,prob=samples)
mean(posterior.predictive == 8)

#eller 

table(posterior.predictive)/1e4
 #15% probability

#3M4
posterior.predictive = rbinom(1e4,size=9,prob=samples)
mean(posterior.predictive == 6)
 #0.18 

#3M5

# 3M1
w <- 8
n <- 15
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- ifelse(test = p_grid < .5, yes = 0, no = 1)
likelihood <- dbinom(x = w, size = n, prob = p_grid)
unstandardized.posterior <- likelihood * prior
posterior <- unstandardized.posterior / sum(unstandardized.posterior)
plot(posterior ~ p_grid, type = "l")

# 3M2
trials <- 1e4
samples <- sample(x = p_grid, size = trials, prob = posterior, replace = TRUE)
HPDI(samples = samples, prob = .9)

# 3M3
posterior.predictive.distribution <- rbinom(n = trials, size = n, prob = samples)
mean(posterior.predictive.distribution == 8)

# 3M4
posterior.predictive.distribution <- rbinom(n = trials, size = 9, prob = samples)
mean(posterior.predictive.distribution == 6)

#3H1
library(rethinking)
data=data(homeworkch3)

sum(birth1)+sum(birth2)


total.births=length(birth1)+length(birth2)
boys = sum(birth1)+sum(birth2)
girls=total.births-boys

p_grid = seq(from=0, to=1,length.out = 1000)
prior=rep(1,length(p_grid))
likelihood=dbinom(boys,size=total.births,prob=p_grid)
posterior=likelihood*prior
posterior=posterior/sum(posterior)

p_grid[which.max(posterior)]
 #0.55

#3H2
samples = sample(p_grid,prob=posterior,size=1e4,replace=T)
HPDI(samples,prob=0.5)
 #0.53 - 0.57
HPDI(samples,prob=0.89)
 #0.50 -0.61
HPDI(samples,prob=0.97)
 #0.48-0.63

#3H3
prediction=rbinom(1e4,size=200,prob=samples)
dens(prediction)
abline(v=111)

#3H4
birth1sum <- sum(birth1)
likelihood2 <- dbinom(birth1sum, size=100, prob=p_grid) 
prior2 <- rep( 1, 100) 
posterior2 <- likelihood2 * prior2 
posterior2 <- posterior2 / sum(posterior2) 
samples2 <- sample(p_grid, size=1e4, replace=TRUE, prob=posterior2)

simulation2 <- rbinom(1e4, size=100, prob=samples2)
dens(simulation2)
abline(v=51)

#3H5
firstgirls = 100 - sum(birth1)   
boysaftgirls = birth2[birth1==0]  
simulation4 = rbinom(1e4, size=firstgirls, prob=samples)
dens(simulation4)
abline(v=sum(boysaftgirls))

 #Tror det er den her. Modellen ser underlig ud - The actual number of male births that follow 
#the female births seems much higher than one would expect when assuming that first and second births are independent. 

OR !?!??

boys.born.after.girls = birth2[birth1 == 0]
posterior.predictive.distribution = rbinom(1e4, size = length(boys.born.after.girls), prob = samples)
dens(posterior.predictive.distribution)
abline(v = sum(boys.born.after.girls))
