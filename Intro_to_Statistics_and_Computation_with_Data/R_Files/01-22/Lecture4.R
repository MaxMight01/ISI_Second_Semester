#################################################
#Left over from lecture3.R
##################################################
install.packages("HSAUR")
require(HSAUR)
data("Forbes2000",package="HSAUR")
##### Pareto chart
install.packages("qcc")
library(qcc)
t<-table(Forbes2000[,"category"])
pareto.chart(t)
#######################################################
#4.3 Example of multivariate Mixed data
######################################################
babies<-read.table("https://www.stat.berkeley.edu/~statlabs/data/babies.data",header=TRUE)
attach(babies)
smoke[smoke==9]<-NA
replace(gestation,gestation==999,NA)
replace(bwt,bwt==999,NA)
tapply(bwt,smoke,mean)
boxplot(bwt~smoke)
qqplot(bwt[smoke==0],bwt[smoke==1])
a<-which(babies$smoke==1)
b<-which(babies$smoke==0)
par(mfrow=c(2,1))
plot(gestation[b], bwt[b],xlim=c(145,355),ylim=c(50,180))
plot(gestation[a], bwt[a],xlim=c(155,355),ylim=c(50,180))

########################################################################
#4.4 Example of Multivariate Qualitative data
#############################################################
data("HairEyeColor")
HairEyeColor
HairEyeColor["Black",,"Male"]
windows()
par(mfrow=c(2,2))
pie(HairEyeColor["Black",,"Male"],main="Black",las=2)
pie(HairEyeColor["Brown",,"Male"],main="Brown",las=2)
pie(HairEyeColor["Red",,"Male"],main="Red",las=2)
pie(HairEyeColor["Blond",,"Male"],main="Blond",las=2)



#########################################################
# Probability with R
##########################################################
#1 Random Sampling
##########################################################
#Pick 5 students from this class. Make alphabetical list. Simple Random Sampling Without Replacement (SRSWOR):
n<-37
sample(1:n,5)
#Permute all students. We could use this to assign students to projects randomly.
sample(1:n,n)
#Toss a coin
sample(c("Head","Tail"),1)
#Roll a die
sample(1:6,1)
#Roll the die 10 times. Numbers can repeat. Simple Random Sample With Replacement (SRSWR):
sample(1:6,10,replace=T)
# Histogram
x = sample(1:6,100,replace=T)
y = hist(x,0.5+0:6)
#Pie chart
pie(y$counts)
###########################################################
#3 Discrete distributions
############################################################
pbinom(4,10,0.2)  #P(X<=k)  pbinom(k,n,p) P(X<=4) when X~Bin(10,.2)
p<-dbinom(0:10,10,0.6) #P(X=k) X~Bin(10, 0.6)
barplot(p)
#Want that smallest q, so that P(X<=q)>p
qbinom(0.5, 10,.2)
windows()
x<-rbinom(1000,10,.6)
hist(x)
qgeom(p, prob, lower.tail = TRUE, log.p = FALSE)
qgeom(0.6,0.5) #Result is that number a, so that P(X<=a)>=p, a=inf(t:F(t)>=p)
rhyper(nn, m, n, k)
rhyper(10, 10, 20, 5)
dpois(x, lambda, log = FALSE)
dpois(3, 1, log = FALSE)
# (d p q r) (binom pois geom hyper)
############################################################
#4 Poisson approximation to binomial 
##############################################################
#Binom with n->infinity and p->0 in such a way that np->lambda
n=1000;p=.001;np=1
par(mfrow=c(1,2))
p<-hist(rpois(10000,1),0.5+-1:8)
b<-hist(rbinom(10000,1000,0.001),0.5+-1:8)
(b$counts-p$counts)/10000
#[1]  0.0014 -0.0007  0.0008 -0.0022  0.0019 -0.0008 -0.0005  0.0001  0.0000
##############################################################
#5 Continuous distributions
#############################################################
x<-0.6
q<-x
p<-0.6
dunif(x, min = 0, max = 2, log = FALSE)
punif(q, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE)
qunif(p, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE)
u<-runif(100000, min = 0, max = 1)
var(u)-1/12
#[1] 0.0002057412
mean(u)
pexp(q, rate = 1, lower.tail = TRUE, log.p = FALSE)
shape<-2
qgamma(p, shape, rate = 1, lower.tail = TRUE, log.p = FALSE) #P(X<=result)=p
dbeta(x, shape1=2, shape2=3, ncp = 0, log = FALSE)
pnorm(x)
qnorm(p)
dnorm(x)
rnorm(n, mu, sigma)
hist(rnorm(1000,0,1))
#################################################################
#Geometric as discrete exponential
###############################################
p=0.6
l=-log(1-p)
g<-dgeom(seq(1:20),p) #Y~Geom(p) P(Y=0) P(Y=1)... P(Y=19)
h<-pexp(seq(1:20),l) #X~Exp(l) P(X<=1) P(X<=2)...P(X<=20)
e<-h[2:20]-h[1:19] #h[2]-h[1],h[3]-h[2]
max(abs(e-g[1:19]))
##############################################
# The error is less when p is close to 0, 1/2 and 1
# This is purely computational error in evaluation of exponential, log, power etc  
###############################################
x<-rep(0,99)
for(i in 1:99){
  p=i/100
l=-log(p)
g<-dgeom(seq(1:20)-1,p) #Y~Geom(p) P(Y=0) P(Y=1)... P(Y=19)
h<-pexp(seq(1:20),l) #X~Exp(l) P(X<=1) P(X<=2)...P(X<=20)
e<-h[-1]-h[1:19] #h[2]-h[1],h[3]-h[2],
e=c(1-h[1],e) #h[1], and then append.
x[i]<-max(abs(e-g))
}
plot(x)
###################################################################
#Transformation from unif to beta
##################################################################
x<-runif(10000,0,1)
y<-x^2
s<-seq(0,1,by=.01)
hist(y,s,freq=FALSE)  #100 classes [(0,.01),(.01,.02),...(.99,1)]  
f<-dbeta(s-.005,1/2,1) #101 points of evaluation (-.005,....995) 
lines(s[-1]-.005,f[-1],col="red")  #100 points plotted [(.005,f(.005)),...]
#Alternatively I could evaluate f at s+.005 and drop the last one.