#reading EPI data
EPI_data <- read.csv("D:/titta/RPI2024/Courses/Data Analytics/Assignment 1/epi2024results06022024.csv")
View(EPI_data)

attach(EPI_data) #Set default object

fix(EPI_data)#launches a simple data editor

tf<-is.na(EPI.new) # records True values if the value is NA
E <-EPI.new[!tf] # filters out NA values, new array

# stat
summary(EPI.new)
fivenum(EPI.new,na.rm=TRUE) #na.rm used to ignore NA values

# stem and leaf plots
stem(EPI.new)
hist(EPI.new)


hist(EPI.new, seq(20., 80., 1), prob=TRUE)
lines(density(EPI.new,na.rm=TRUE,bw=1.),col="blue")
rug(EPI.new, col = "red") # rug marks the individual data points with small tick marks along the axes.

hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines (density(EPI.new,na.rm=TRUE,bw="SJ")) #"SJ" stands for the Sheather-Jones method, which is a rule-of-thumb bandwidth selector for kernel density estimation. 
rug(EPI.new)

#Displaying the distribution of data based on five summary statistics: the minimum, first quartile (Q1), median (Q2), third quartile (Q3), and maximum
boxplot(EPI.new, APO.new)
sd(EPI.new, na.rm = FALSE)
x<-seq(20,80,1)
q<-dnorm(x,mean=42, sd=5,log=FALSE) #dnom density of the normal distribution.
lines(x,q)
lines(x,.4*q)
ln<-dnorm(x,mean=65, sd=5,log=FALSE)
lines(x,.12*q)


plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE) #cumulative density function #empirical cumulative distribution function (ECDF)

qqnorm(EPI.new); qqline(EPI.new) #Q-Q (Quantile-Quantile) plot

#Make a Q-Q plot against the generating distribution by:
qqplot(rnorm(ppoints(250)), EPI.new, xlab= "Q-Q plot for norm dsn")
qqline(EPI.new)

qqplot(rt(ppoints(250), df= 5), EPI.new, xlab= "Q-Q plot for t dsn")
qqline(EPI.new)






#APO data
ta<-is.na(APO.new) # records True values if the value is NA
A <-APO.new[!ta] # filters out NA values, new array

# stat
summary(APO.new)
fivenum(APO.new,na.rm=TRUE) #na.rm used to ignore NA values

# stem and leaf plots
stem(APO.new)
hist(APO.new)


hist(APO.new, seq(0., 100., 2.5), prob=TRUE)
lines(density(APO.new,na.rm=TRUE,bw=2.5),col="blue")
rug(APO.new, col = "red") # rug marks the individual data points with small tick marks along the axes.

hist(APO.new, seq(0., 100., 2.5), prob=TRUE)
lines (density(APO.new,na.rm=TRUE,bw="SJ")) #"SJ" stands for the Sheather-Jones method, which is a rule-of-thumb bandwidth selector for kernel density estimation. 
rug(APO.new)

#Displaying the distribution of data based on five summary statistics: the minimum, first quartile (Q1), median (Q2), third quartile (Q3), and maximum
boxplot(APO.new)

sd(APO.new, na.rm = FALSE)

x<-seq(0., 100., 2.5)
q<-dnorm(x,mean=65, sd=22.26,log=FALSE) #dnom density of the normal distribution.
lines(x,q)
lines(x,.4*q)
ln<-dnorm(x,mean=70, sd=22.26,log=FALSE)
lines(x,.12*q)


plot(ecdf(APO.new), do.points=FALSE, verticals=TRUE) #cumulative density function #empirical cumulative distribution function (ECDF)

qqnorm(APO.new); qqline(APO.new) #Q-Q (Quantile-Quantile) plot

#Make a Q-Q plot against the generating distribution by:
qqplot(rnorm(ppoints(250)), APO.new, xlab= "Q-Q plot for norm dsn")
qqline(APO.new)

qqplot(rt(ppoints(250), df= 5), APO.new, xlab= "Q-Q plot for t dsn")
qqline(APO.new)


#WRS data
tw<-is.na(WRS.new) # records True values if the value is NA
W <-WRS.new[!tw] # filters out NA values, new array

# stat
summary(WRS.new)
fivenum(WRS.new,na.rm=TRUE) #na.rm used to ignore NA values

# stem and leaf plots
stem(WRS.new)
hist(WRS.new)


hist(WRS.new, seq(0., 100., 2.5), prob=TRUE)
lines(density(WRS.new,na.rm=TRUE,bw=2.5),col="blue")
rug(WRS.new, col = "red") # rug marks the individual data points with small tick marks along the axes.

hist(WRS.new, seq(0., 100., 2.5), prob=TRUE)
lines (density(WRS.new,na.rm=TRUE,bw="SJ")) #"SJ" stands for the Sheather-Jones method, which is a rule-of-thumb bandwidth selector for kernel density estimation. 
rug(WRS.new)

#Displaying the distribution of data based on five summary statistics: the minimum, first quartile (Q1), median (Q2), third quartile (Q3), and maximum
boxplot(WRS.new)

sd(WRS.new, na.rm = FALSE)

x<-seq(0., 100., 2.5)
q<-dnorm(x,mean=42.16, sd=27.34,log=FALSE) #dnom density of the normal distribution.
lines(x,q)
lines(x,.4*q)
ln<-dnorm(x,mean=70, sd=27.34,log=FALSE)
lines(x,.12*q)


plot(ecdf(WRS.new), do.points=FALSE, verticals=TRUE) #cumulative density function #empirical cumulative distribution function (ECDF)

qqnorm(WRS.new); qqline(WRS.new) #Q-Q (Quantile-Quantile) plot

#Make a Q-Q plot against the generating distribution by:
qqplot(rnorm(ppoints(250)), WRS.new, xlab= "Q-Q plot for norm dsn")
qqline(WRS.new)

qqplot(rt(ppoints(250), df= 5), WRS.new, xlab= "Q-Q plot for t dsn")
qqline(WRS.new)
