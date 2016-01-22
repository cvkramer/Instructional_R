#Bring in Functions
source('~/Documents/ITP/Ankenmann - Sampling/FunctionS.R')

################################## Inits ##################################
dat <- read.csv("/home/cvkramer/Documents/ITP/Ankenmann - Sampling/FoR.csv")

data <- rep(dat$Age,10000)
#Sampling Information
seed <- 14                #Randomizer Seed Value
ss <- 20                  #Sample Size
reps <- c(100,1000,10000) #Number of Sample Draws
sys.chg <- 7              #Systematic Change

#Data to be Analyzed
col <- 3                  #2-Gender     3-Age

#Plot Setting
fflag <- 0                #0-Percentage 1-Frequencies

#Auto Settings Based on Initial Settings
x<-rnorm(1000000, mean(data), sd(data)/sqrt(ss))
d.x <- density(x)

if(col==2){ xmax <- 3 ; brk=.5 ; prob.den <- FALSE }else{  xmax <- 70 ; brk=.25 ; prob.den <- TRUE}

############# Initilize Containers #############
#Generate empty matrices
data_1  <- matrix(,nrow=ss,ncol=reps[1])
data_1r <- matrix(,nrow=ss,ncol=reps[1])
data_1s <- matrix(,nrow=ss,ncol=reps[1])

data_2  <- matrix(,nrow=ss,ncol=reps[2])
data_2r <- matrix(,nrow=ss,ncol=reps[2])
data_2s <- matrix(,nrow=ss,ncol=reps[2])

data_3  <- matrix(,nrow=ss,ncol=reps[3])
data_3r <- matrix(,nrow=ss,ncol=reps[3])
data_3s <- matrix(,nrow=ss,ncol=reps[3])

#Generate empty vectors filled with NAs
mage_1  <- rep(NA,reps[1])
mage_1s <- rep(NA,reps[1])
mage_1r <- rep(NA,reps[1])

mage_2  <- rep(NA,reps[2])
mage_2s <- rep(NA,reps[2])
mage_2r <- rep(NA,reps[2])

mage_3  <- rep(NA,reps[3])
mage_3s <- rep(NA,reps[3])
mage_3r <- rep(NA,reps[3])

vals_1  <- rep(NA,ss*reps[1])
vals_1r <- rep(NA,ss*reps[1])
vals_1s <- rep(NA,ss*reps[1])

vals_2  <- rep(NA,ss*reps[2])
vals_2r <- rep(NA,ss*reps[2])
vals_2s <- rep(NA,ss*reps[2])

vals_3  <- rep(NA,ss*reps[3])
vals_3r <- rep(NA,ss*reps[3])
vals_3s <- rep(NA,ss*reps[3])

################################## Sampling ##################################
set.seed(seed)

#### sample loop
#Sample from Table 10.2 collect individual samples into columns of a matrix
#This done with three different sampling values

for(i in 1:reps[1]){
  data_1[,i]  <- sample(x=data,size=ss,replace=FALSE)
  data_1r[,i] <- sample(x=data,size=ss,replace=TRUE)
  data_1s[,i] <- systematic_sample(data,ss,sys.chg)
}

for(i in 1:reps[2]){
  data_2[,i]  <- sample(x=data,size=ss,replace=FALSE)
  data_2r[,i] <- sample(x=data,size=ss,replace=TRUE)
  data_2s[,i] <- systematic_sample(data,ss,sys.chg) 
}

for(i in 1:reps[3]){
  data_3[,i]  <- sample(x=data,size=ss,replace=FALSE)
  data_3r[,i] <- sample(x=data,size=ss,replace=TRUE)
  data_3s[,i] <- systematic_sample(data,ss,sys.chg)
}

################################## Collect Means ##################################

#Collect Means from Samplings into single container, do this for all three samplings.
for(i in 1:reps[1]){
  mage_1[i]  <- mean(data_1[,i])
  mage_1r[i] <- mean(data_1r[,i])
  mage_1s[i] <- mean(data_1s[,i])
}

for(i in 1:reps[2]){
  mage_2[i]  <- mean(data_2[,i])
  mage_2r[i] <- mean(data_2r[,i])
  mage_2s[i] <- mean(data_2s[,i])
}

for(i in 1:reps[3]){
  mage_3[i]  <- mean(data_3[,i])
  mage_3r[i] <- mean(data_3r[,i])
  mage_3s[i] <- mean(data_3s[,i])
}

################################## Collect Values ##################################
#This pools all the sample sets into one pool.
## For each observation from some specified sample size,
##   For each nth sample set
##       Put observation into container.

for(i in 1:ss){for(j in 1:reps[1]){vals_1[j+(i-1)*reps[1]]  <- data_1[i,j]}}
for(i in 1:ss){for(j in 1:reps[1]){vals_1r[j+(i-1)*reps[1]] <- data_1r[i,j]}}
for(i in 1:ss){for(j in 1:reps[1]){vals_1s[j+(i-1)*reps[1]] <- data_1s[i,j]}}

for(i in 1:ss){for(j in 1:reps[2]){vals_2[j+(i-1)*reps[2]]  <- data_2[i,j]}}
for(i in 1:ss){for(j in 1:reps[2]){vals_2r[j+(i-1)*reps[2]] <- data_2r[i,j]}}
for(i in 1:ss){for(j in 1:reps[2]){vals_2s[j+(i-1)*reps[2]] <- data_2s[i,j]}}

for(i in 1:ss){for(j in 1:reps[3]){vals_3[j+(i-1)*reps[3]]  <- data_3[i,j] }}
for(i in 1:ss){for(j in 1:reps[3]){vals_3r[j+(i-1)*reps[3]] <- data_3r[i,j]}}
for(i in 1:ss){for(j in 1:reps[3]){vals_3s[j+(i-1)*reps[3]] <- data_3s[i,j]}}

################################## Calculate Stats ##################################
#Make vectors of the mean of sampling technique means, then place into a dataframe.
wr_means<-round(c(mean(mage_1),mean(mage_2),mean(mage_3),mean(data)),2)
r_means<-round(c(mean(mage_1r),mean(mage_2r),mean(mage_2s),mean(data)),2)
s_means<-round(c(mean(mage_1s),mean(mage_2s),mean(mage_3s),mean(data)),2)
means<-data.frame(wr=wr_means, r=r_means, s=s_means, row.names=c(reps[1],reps[2],reps[3],"Pop"))

#Make vectors of the standard deviation of sampling technique standard deviations, then place into a dataframe.

wr_sd<-round(c(sd(mage_1),sd(mage_2),sd(mage_3),sd(data)/sqrt(20)),2)
r_sd<-round(c(sd(mage_1r),sd(mage_2r),sd(mage_3r),sd(data)/sqrt(20)),2)
s_sd<-round(c(sd(mage_1s),sd(mage_2s),sd(mage_3s),sd(data)/sqrt(20)),2)
sds<-data.frame(wr=wr_sd, r=r_sd, s=s_sd, row.names=c(reps[1],reps[2],reps[3],"Pop"))


################################## Getting Output ##################################
#Really only one function to use.  DEFAULTS: graphs(in_name="temp", tofile="FALSE")
#tofile suppresses printing graphs, instead printing them to a pdf named as specified in in_name var.
  #in_name will be the name of the file, only useful if tofile is made to be TRUE
  
#print(paste("seed=",seed,";  sample size=",ss,";  systematic change=",sys.chg,";  Freq?=",fflag,";  sample replications=",reps,sep=""))
#graphs("Complete",tofile=TRUE)
#graphs()

#library(ggplot2)
#g1<-gen_gg(vals_1,  "100 Samplings Without Replacement")
#g2<-gen_gg(vals_1s, "100 Samplings With Replacement")
#g3<-gen_gg(vals_1r, "100 Systematic Samplings")
#multiplot(g1,g2,g3,rows=3)


#c(0,.6)
  