##########################################################

### An Introduction to R

##########################################################

# This works under R version 3.0.3 (2014-03-06) -- "Warm Puppy"
# Copyright (C) 2014 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# See http://cran.r-project.org/ for free download details

##########################################################


##########################################################
### Table of Contents
##########################################################

# 1. Basic R Commands
# 2. Data Export/Import
# 3. Data Structures
# 4. Data Manipulation
# 5. Data Visualization
# 6. Multiple Linear Regression 



##########################################################
### 1. Basic R Commands
##########################################################

# 1.1. A Few General Things
# 1.2. Command Line Evaluation in R
# 1.3. Write and Source R script
# 1.4. Packages in R
# 1.5. R workspace
# 1.6. Online R Help
# 1.7. Commonly used Arithmetic Operators
# 1.8. Numerical Vectors in R
# 1.9. Numerical Matrices in R
# 1.10. Character Vectors and Factors
# 1.11. Logical Operations in R 



##########################################################
### 1.1. A Few General Things
##########################################################

# Largest integer you can represent on your computer 
.Machine$integer.max    # differs based on computer

# output written out to "3" digits. Default is 7 
options(digits=3)


# R is case-sensitive.
x=5
X   # Error: object 'X' not found
x   #[1] 5
# Note: [1] prefixes the output and indicates this is item 1 in a vector of output

# Working Directory
# When you start R, it nominates one of the directories on your 
# hard drive as a working directory, where it looks for data files
# and user-written programs. To get this directory:
#
getwd()

# To change to a different one, use
setwd("C:\\Users\\jhump\\Desktop\\Newer\\StatI\\")  #use correct path for you

#OR# go to File, Change dir, and browse your way to the one you want.
# Then make sure you are in the working dir you want: 
getwd()



##########################################################
### 1.2. Command Line Evaluation in R
##########################################################

# R system evaluates commands typed at the R prompt and 
# returns the results of the computations.
sqrt(100)-5    # [1] 5

# assign a value to variable x
x<-sqrt(100)-5    
x  # print x at screen
print(x)

# A command can be continued on more than one line.
z=sqrt(100)-5 +1.5*(x-4)
z

# Can use semi-colon to separate statements on a single line.
# Not recommended for beginners.
x=5;y=sqrt(100)-5 +1.5*(x-4)
y

# You can stop R run by clicking on the red STOP button in top panel
#q() enables you to quit the R session.



##########################################################
### 1.3. Write and Source R script
##########################################################

# Go to File and click on New Script. Opens an untitled R editor.
# Type in commands:
# Example: x=5;y=sqrt(100)-5 +1.5*(x-4); y 
# Save into your local directory as Script.R
#
source("C:\\Users\\nra02001\\Dropbox\\ISBIS-VP-SciProg\\Chennai Workshop Jan 2015\\Script.R")
y    # Print y



##########################################################
### 1.4. Packages in R
##########################################################

# Install a package: You must be connected to the internet. 
# Example: install an R package called "car" 
install.packages("car")

# You must choose a CRAN mirror. Say, USA (CA1)
# R will install if it is a valid package.
#OR#
# You can also click on Packages, Installed package(s), 
# choose a CRAN mirror, and select a package from a complete list.


# Load installed packages using library or require
# library(package) and require(package) both load 
# the package with name package and put it on the search list. 
# require is designed for use inside other functions.
# They also load the help package for the function.
#
library(car)     
require(car)

# To see a list of your installed packages in a new window 
library() 

##########################################################
### 1.5. R workspace
##########################################################

# ls() displays names of objects in the current workspace.
ls()


# rm or remove help remove objects currently in the workspace.
# Use with caution
rm(x)
remove(y)
 
# Caution! Dont use!!!
# rm(list=ls()) will remove entire list of objects.



##########################################################
### 1.6. Online R Help
##########################################################

# Here are a few examples to try
help("mean")  # will open a new window with this info

#OR#
?mean

# Very extensive and useful.
# Generally gives a Description, Usage, Arguments, Value,  
# References, See Also, Examples.
#
help(package="car")
?car



##########################################################
### 1.7. Commonly used Arithmetic Operators
##########################################################

# Help Arithmetic gives you a list
?Arithmetic  # opens help in a new window


# Examples of commonly used R functions for arithmetic 
# operations. # You will pick up more as we progress. 
#
sqrt(144)    # square root
12^2   #square
abs(-144)  #absolute value
exp(1)   #exponential
log(exp(1))   #natural log
floor(12.156)   #round down
ceiling(12.156)  #round up
factorial(5)
7%%4   # x mod y
7%/%4  # integer division of x by y
27%/%4


##########################################################
### 1.8. Numerical Vectors in R
##########################################################

1:6
c(1,2,3,4,5,6)
c(1:6)
seq(from=1,to=6,by=1)
x=seq(1,6,1)

length(x) # should be 6
class(x)  # numeric

# Incorrect query! A vector has length only
dim(seq(1,6,1))   # answer will be NULL
rep(0,10)


##########################################################
### 1.9. Numerical Matrices in R
##########################################################

# Matrices are 2-dim. arrays which contain variables of a single type.
# Note: A data frame is not a matrix because its columns may have 
# different types of variables.
# In R, by default, a matrix is filled in column major order, so that 
# the row index changes faster than the column index.

# Create a 3 by 6 matrix 
amat=matrix(1:18,nrow=3,ncol=6)
amat
cmat=t(matrix(1:18,nrow=6,ncol=3))
bmat=matrix(c(2,5,7,18,4,6),nrow=2,ncol=3)
bmat

# Can change the default assignment of values by row and column.
# use byrow=TRUE option
bmat=matrix(c(2,5,7,18,4,6),nrow=2,ncol=3,byrow=T)
bmat   # different matrix than before

# Create a 3-dim Identity matrix
iden=matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,ncol=3)
iden

# Create a 2 by 4 zero matrix
zero=matrix(rep(0,8),nrow=2,ncol=4)
zero



##########################################################
### 1.10. Character Vectors and Factors
##########################################################

# Create a vector called city with three character elements and make 
# them into levels of a factor variable using the as.factor function.
city=c("New York","Los Angeles","Chicago")
city

as.factor(city)



##########################################################
### 1.11. Logical Operations in R
##########################################################

?Logic    #help in a new window

# & operator performs elementwise AND comparison
x=c(1:7)
1<x & x<5  #satisfied only by 2,3,4
# ouptut is a vector length 7 with TRUE or FALSE as elements

# && operator evaluates AND condition from left to right UNTIL a
# logical result is obtained
z<-1
1<x && x<5  # output is a singleton; FALSE
1 <=x && x<5  # TRUE

# & and any operators
any(1<x & x<5)  # is any element TRUE? Then TRUE

# && and any operators
any(1<x && x<5)  #FALSE

any(1<x) && any(x<5)   # TRUE

# all and && operators
all(1<x) && all(x<5)    # FALSE

##########################################################
##########################################################






##########################################################
### 2. Data Input/Import and Output/Export
##########################################################

# 2.1. Read in data from an R package
# 2.2. Write out/Export data from R workspace to local directory
# 2.3. Read in data from a file stored in your local directory 



##########################################################
### 2.1. Read in data from an R package
##########################################################

# Illustration: Forbes2000 data; url: http://www.forbes.com
# Source: Forbes.com, New York, 2004
# Data set on 2000 global companies from 2004 collected
# by Forbes magazine. It is included in an R package "HSAUR"
#A Handbook of Statistical Analyses Using R

install.packages("HSAUR")
require(HSAUR)

data()  # lists all data
#OR# 
# list the data sets in the HSAUR package
try(data(package="HSAUR"))  

# The function data searches for a data object called 
# "Forbes2000" in the package "HSAUR". 
# If the search is successful, it attaches the data object to the 
# R global environment and makes the data available in the R workspace.
#
data("Forbes2000",package="HSAUR")  
help(Forbes2000)  # Description of data in a new window

# List all objects stored in global environment
ls()     

# Check first and/or last portions of the data using head and/or tail
head(Forbes2000)
tail(Forbes2000)



##############################################################
### 2.2. Write out/Export data from R workspace to a file in 
### your local directory
##############################################################

# Save as a csv file
write.table(Forbes2000,"C:\\Users\\jhump\\Desktop\\Newer\\StatI\\Forbes2000.csv",
 append=FALSE,quote=FALSE,sep=",",dec=".",row.names=TRUE,col.names=NA)

# Save as a txt file (tab delimited)
write.table(Forbes2000,"C:\\Users\\jhump\\Desktop\\Newer\\StatI\\Forbes2000.txt",
 append=FALSE,quote=FALSE,sep="\t",dec=".",row.names=TRUE,col.names=NA)


##############################################################
### 2.3. Read in data from a file stored in your local directory
##############################################################

# Currently, four formats of data files are supported in R:
# files ending �.tab�, �.txt� or �.TXT� are read using read.table 
# files ending �.csv� or �.CSV� are read using read.table or read.csv
# files ending �.R� or �.r� are sourced into R workspace. 
# files ending �.RData� or �.rda� are loaded in 

# Read/import data from a csv file, using read.csv
Forbes2000.csv.import <- read.csv("C:\\Users\\jhump\\Desktop\\Newer\\StatI\\Forbes2000.csv",
  sep=",",row.names=1,fill=TRUE)
head(Forbes2000.csv.import)
tail(Forbes2000.csv.import)

# Read/import data from a txt file, using read.table
Forbes2000.txt.import <- read.table("C:\\Users\\jhump\\Desktop\\Newer\\StatI\\Forbes2000.txt",
  header=T,sep="\t",fill=TRUE)
head(Forbes2000.txt.import)
tail(Forbes2000.txt.import)

##########################################################
##########################################################






##########################################################
### 3. Data Structures 
##########################################################

# A data frame is a list of variables, each of the same length.
# Variables can be of different types or modes, viz.,
# numeric, character, factor, etc.

class(Forbes2000)
# Variables in the data frame Forbes2000
names(Forbes2000)
# Variable rank: ranking of the company, stored as an integer variable.
class(Forbes2000[,"rank"]) # comma holds place for the row in the table
class(Forbes2000$rank)  # or use this command
# Note: [ ] indicate subset of a larger object. Here we ask for a subset of
# the second dim, or col.
class(Forbes2000[,"country"])
class(Forbes2000[,"assets"])
str(Forbes2000) # type and details for all variables (structure)

# Forbes2000 is an object of class data.frame of 
# dimension 2000 (no. of rows) by 8 (no. of cols.)
dim(Forbes2000)
nrow(Forbes2000)
ncol(Forbes2000)
length(Forbes2000$rank)   # length of a vector

# Variable sales in Forbes2000: sales of a company in billions USD
class(Forbes2000$sales)
Forbes2000$sales[1:3]
summary(Forbes2000$sales)
mean(Forbes2000$sales)
median(Forbes2000$sales)
var(Forbes2000$sales)
sd(Forbes2000$sales)
#range function gives two values, max and min
range_sales=range(Forbes2000$sales)[2]-range(Forbes2000$sales)[1]
range_sales

# sort the variable in ascending order
sorted_profits=sort(Forbes2000$profits)
sorted_profits[1:5]

summary(Forbes2000$profits)
summary(sorted_profits) # should be same as above

# Variable name in Forbes2000: name of the company.
# (unique names of all 2000 companies)

class(Forbes2000$name)
Forbes2000$name[1:3]

# Variable category in Forbes2000: factor with 27 levels describing 
# the category into which a company falls. 
# Factor objects are used to depict nominally scaled categorical variable 
# (there is no ordering among the levels)

class(Forbes2000[,"category"])
nlevels(Forbes2000[,"category"])
l<-levels(Forbes2000[,"category"])
levels(Forbes2000[,"category"])[1:3]

# table function with categorical variables.
# Frequency of how many companies fall into each category level
table(Forbes2000[,"category"])
c<-rep(0,27)
for(i in 1:27){
c[i]<-length(which(Forbes2000[,"category"]==l[i]))}
o<-order(c)
cbind(l[o],c[o])

# Attach a data set, then refer to variables directly by name 
attach(Forbes2000)   
summary(sales)    # you need not say Forbes2000$sales

# Detach a data set after use
detach(Forbes2000) 





##########################################################
### 4. Data Visualization
##########################################################

# 4.1. Univariate Plots for a numerical variable
# 4.2. Many Plots in the same window


# R graphics package loads when R is started

# To open a new graphics window for a new plot
# Windows OS function to open a new graphics window 
windows()
# For a MAC, use quartz()



attach(Forbes2000) # use data Forbes2000

##########################################################
### 4.1. Univariate plots for a numerical variable
##########################################################

# Density histogram
hist(sales,main="Forbes2000: Freq. Histogram of Sales")
#Stem and leaf plot
stem(sales[which(sales>100)])

# Frequency histogram
windows()   # new graphics window
hist(sales,main="Forbes2000: Density Histogram of Sales",freq=F)

#Boxplot
windows()
boxplot(sales,main="Forbes2000:Boxplot of Sales", horizontal=TRUE)
summary(sales)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0     2.0     4.4     9.7     9.5   256.3 
#The mean is above the third quartile

fence<-9.5+1.5*(9.5-2)
length(which(sales>=fence)) #212
sd(sales) #18
length(which(sales>=9.7+3*18)) #35
sd(sort(sales)[1:1800]) #4.78
mean(sort(sales)[1:1800]) #5.35
length(which(sales>=5.35+3*4.78)) #229
 
 
### Scatterplot
windows()
plot(assets,profits,main="Plot of profits vs assets") #need to spread tem out for small values
plot(log(assets),profits,main="Plot of profits vs assets") #no pattern
plot(log(assets),sign(profits)*log(abs(profits)),main="Plot of profits vs assets") #slightly increasing pattern



##########################################################
### 4.2. Many Plots in the Same Window
##########################################################

# 2x1 grid of 2 figures on the same page for variable sales 
windows()
par(mfrow=c(2,2))
hist(sales,main="Forbes2000: Histogram of Sales")
boxplot(sales,main="Forbes2000: Boxplot of Sales")


#2x2 grid of 4 figures on the same page
# If there is evidence of right skewness in variables,
# try taking log of variables and check for symmetry.
# Note: you must check that sales values are all positive.

# (sales>0) is a logical operator.
table(sales>0)
windows()
par(mfrow=c(2,2))
hist(sales,col="red",main="Forbes2000: Histogram of Sales")
boxplot(sales,main="Forbes2000: Boxplot of Sales")
hist(log(sales),col="red",main="Forbes2000: Histogram of Log Sales")
boxplot(log(sales),main="Forbes2000: Boxplot of Log Sales")

#clear workspace
rm(list=ls())

############################################################
## 4.3 Example from statlab
## https://www.stat.berkeley.edu/~statlabs/about.html
############################################################
babies<-read.table("https://www.stat.berkeley.edu/~statlabs/data/babies.data",header=TRUE)
attach(babies)
#The 999 values in bwt denote missing values. To replace 999 with NA.
replace(bwt,bwt==999,NA)
replace(gestation,gentation==999,NA)
gestation[which(gestation==999)]<-NA
weight[which(weight==999)]<-NA
#The 9 values in smoke denote missing values. To replace 9 with NA and recode 2 and 3 as 0, for nonsmoker:
  ismoke<- replace(smoke,smoke==2 |smoke==3,0)
ismoke[ismoke==9]<-NA
#To compute the average birthweight for smokers and nonsmokers.
tapply(bwt,ismoke,mean)
#To select babies whose mother smoked.
smokerbabes<-babies[is.na(ismoke) | ismoke==1,]
#To count babies according to whether they are premature or low birthweight:
  table(cut(gestation,4),bwt<90)
#To make a quantile-quantile plot of bwt for smokers versus nonsmokers
qqplot(bwt[ismoke==0],bwt[ismoke==1])
abline(0,1)
#To make a gamma(5,1) quantile plot of bwt
ps<-ppoints(length(bwt))
plot(quantile(bwt,ps),qgamma(ps,5,1))
#To put box and whisker plots of bwt, one for each smoking level, on the same plot
boxplot(bwt~ismoke)
########################################################################
#4.4 Example of Qualitative data
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
 
