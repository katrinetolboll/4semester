install.packages(c("coda","mvtnorm","devtools"))
library(devtools)
devtools::install_github("rmcelreath/rethinking")
library(rethinking)


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

                    #Chapter 4

pos = replicate(1000, sum(runif(16,-1,1)))
plot(density(pos))

prod(1+runif(12,0,0.1))

growth=replicate(10000,prod(1+runif(12,0,0.1)))
dens(growth,norm.comp=T)

big=replicate(10000,prod(1+runif(12,0,0.5)))
small=replicate(10000,prod(1+runif(12,0,0.01)))

dens(small,norm.comp=T)

#Howel example
library(rethinking)
data(Howell1)
d=Howell1

d2 = d[d$age >=18,]
dens(d2$height)

#plotting priors
curve(dnorm(x,178,20),from=100,to=250)
curve(dunif(x,0,50),from=-10,to=60)

sample_mu=rnorm(1e4,178,20)
sample_sigma=runif(1e4,0,50)
prior_h=rnorm(1e4,sample_mu,sample_sigma)
dens(prior_h)

#compute posterior #idk
mu.list = seq(from=140, to=160, length.out = 200)
sigma_list = seq(from=4, to=9, length.out = 200)
post=expand.grid(mu=mu.list,sigma=sigma_list)
post$LL=sapply(1:nrow(post),function(i) sum(dnorm(
  d2$height,
  mean=post$mu[i],
  sd=post$sigma[i],
  log= T)))
post$prod =post$LL + dnorm(post$mu,178,20,T)+dunif(post$sigma,0,50,T)
post$prob =exp(post$prod-max(post$prod))

contour_xyz(post$mu,post$sigma,post$prob)
image_xyz(post$mu,post$sigma,post$prob)

#sampleling from the posterior
sample.row=sample(1:nrow(post),size=1e4,replace=T,prob=post$prob)
sample.mu=post$mu[sample.row]
sample.sigma=post$sigma[sample.row]
plot(sample.mu,sample.sigma,pch=16,col=col.alpha(rangi2,0.1))

dens(sample.mu)
dens(sample.sigma)
HPDI(sample.mu)
HPDI(sample.sigma)

#Fitting models with map
library(rethinking)
data("Howell1")
d=Howell1
d2=d[d$age>=18,]
flist = alist(
  height~dnorm(mu,sigma),
  mu~dnorm(178,20),
  sigma~dunif(0,50)
)
m4.1=map(flist,data=d2)
precis(m4.1)

#An easier way
m4.2=map(
  alist(
    height~dnorm(mu,sigma),
    mu~dnorm(178,0.1),
    sigma~dunif(0,50)
  ),
  data=d2)
precis(m4.2)

#adding a predictor
plot(d2$height~d2$weight)


library(rethinking)
data("Howell1")
d=Howell1
d2=d[d$age>=18,]
m4.3=map(
  alist(
    height~dnorm(mu,sigma),
    mu<-a+b*weight,
    a~dnorm(156,100),
    b~dnorm(0,10),
    sigma~dunif(0,50)
  ),
  data=d2)
precis(m4.3)


#centering
d2$weight.c = d2$weight-mean(d2$weight)
m4.3=map(
  alist(
    height~dnorm(mu,sigma),
    mu<-a+b*weight.c,
    a~dnorm(156,100),
    b~dnorm(0,10),
    sigma~dunif(0,50)
  ),
  data=d2)
precis(m4.3,corr=T)


#plotting
plot(height~weight, data=d2)
abline(a=coef(m4.3)["a"],b=coef(m4.3)["b"])

post=extract.samples(m4.3)
post[1:5,]     

#add uncertanty
N=10
dN=d2[1:N,]
mN=map(
  alist(
    height~dnorm(mu,sigma),
    mu<-a+b*weight,
    a~dnorm(178,100),
    b~dnorm(0,10),
    sigma~dunif(0,50)
  ),
  data=dN)

post=extract.samples(mN,n=20)
plot(dN$weight,dN$height,
     xlim=range(d2$weight),ylim=range(d2$height),
     col=rangi2,xlab="weight",ylab="height")
mtext(concat("N=",N))

for (i in 1:20)
  abline(a=post$a[1],b=post$b[i],col=col.alpha("black",0.3))

mu=link(m4.3)
str(mu)

weight.seq=seq(from=25, to=70,by=1)
mu<-link(m4.3,data=data.frame(weight=weight.seq))

                #Exercises for chapter 4
#4E1
nr. 1

#4E2
2

#4E3
not gonna happen see page 78

#4E4
u=a+bx 

#4E5
3

#4M1
trials <- 1e3
mu.prior.samples <- rnorm(n = trials, mean = 0, sd = 10)
sigma.prior.samples <- runif(n = trials, min = 0, max = 10)
simulated.heights.from.prior <- rnorm(n = trials, mean = mu.prior.samples, sd = sigma.prior.samples)

#4M2
4m2=map(
  alist(
    height~dnorm(mu,sigma),
    mu~dnorm(0,10),
    sigma~dunif(0,10)
  ),

#4E3
For let

#4M4
height~normal(mu,sigma)
mu=height+year*x
a~normal(150,50)
b~normal(0,3)
sigma~uniform(0,20)

#4M5
My intercept for mean should be 120cm

#4M6
from varience i can calculate my sd and then ajust my sigma

#4H1
# load data
data(Howell1)
d <- Howell1
#Centering in order to aviod strong correlation
d$weight.centered <- (d$weight - mean(d$weight)) / sd(d$weight)

# build model
model <- map(
  alist(
    height ~ dnorm(mean = mu, sd = sigma),
    mu <- alpha + beta*weight.centered,
    alpha ~ dnorm(mean = 0, sd = 10),
    beta ~ dnorm(mean = 0, sd = 10),
    sigma ~ dunif(min = 0, max = 70)
  ),
  data = d
)

# simulate heights from model
individual.weights <- c(46.95, 43.72, 64.78, 32.59, 54.63)
individual.weights.centered <- (individual.weights - mean(d$weight)) / sd(d$weight)
simulated.heights <- sim(model, data = list(weight.centered = individual.weights.centered))

# summarize results
simulated.heights.PI <- apply(X = simulated.heights, MARGIN = 2, FUN = PI, prob = .89)

#intervals
simulated.heights.PI

posterior.samples <- extract.samples(model)
simulated.heights.first.individual <- rnorm(n = trials, mean = posterior.samples$alpha + posterior.samples$beta*individual.weights.centered[1], sd = posterior.samples$sigma)
simulated.heights.first.individual.mean <- mean(simulated.heights.first.individual)
simulated.heights.first.individual.PI <- PI(samples = simulated.heights.first.individual, prob = .89)

#expected height for 1
simulated.heights.first.individual.mean

#4H2
d_under18= d[d$age <18,]
weight=d_under18$weight

#a)
model <- map(
  alist(
    height ~ dnorm(mean = mu, sd = sigma),
    mu <- alpha + beta*weight,
    alpha ~ dnorm(mean = 100, sd = 100),
    beta ~ dnorm(mean = 0, sd = 10),
    sigma ~ dunif(min = 0, max = 50)
  ),
  data = d_under18
)

precis(model)

#For every 1kg the child is 2.7 cm higher

#b)

