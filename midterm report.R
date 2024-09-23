########################
# Midterm Report
# YIMING SHEN (20891774)
########################

# Analysis Part

#(1)
# estimate of the proportion of tweets in the study population that contain your chosen keyword
# (observed tweets containing ketwords) / (sample size)
# sample size 'n'
n <- nrow(midtermdataset20891774)
n
# observed tweets with keywords 'y' (covid==1)
table(midtermdataset20891774$covid)

# 15% likelihood interval estimate for the proportion of tweets with keywords
# relative likelihood function for binomial
BinoRLF <- function(theta, n, y, thetahat) {
  exp(y*log(theta/thetahat)+(n-y)*log((1-theta)/(1-thetahat)))
}

y <- 494
theta <- seq(0.43,0.57,0.001)
thetahat <- y/n
plot(theta, BinoRLF(theta, n, y, thetahat),type = "l",
     main="Relative likelihood function for Binomial with n=982, y=494",las=1,
     xlab=expression(theta),ylab=expression(paste("R(",theta,")")))
abline(h=0.15, col="red", lwd=1)
# lower and upper bounds for the 15% likelihood interval
uniroot(function(x) BinoRLF(x, n, y, thetahat) - 0.15, lower=0.465, upper=0.48)$root
uniroot(function(x) BinoRLF(x, n, y, thetahat) - 0.15, lower=0.525, upper=0.54)$root

# an estimate of the probability that at least half of these tweets will contain your chosen keyword
p <- 494/982
p.half <- 1 - pnorm(24,50*p,sqrt(p*(1-p)*50))
p.half

#(2)
# table for the number of media for first.tweet
table(midtermdataset20891774$media[midtermdataset20891774$first.tweet==1])
# table for the number of media for not first.tweet
table(midtermdataset20891774$media[midtermdataset20891774$first.tweet==0])

# mode and mean for the number of media for first.tweet
# mode can be obtained from summary above
mean(midtermdataset20891774$media[midtermdataset20891774$first.tweet==1])
# mode and mean for the number of media for not first.tweet
# mode can be obtained from summary above
mean(midtermdataset20891774$media[midtermdataset20891774$first.tweet==0])

#(3)
rt <- midtermdataset20891774$retweets
retweets.log <- log(rt+1)

# five number summary for retweets.log
round(summary(retweets.log),2)

# skewness for retweets.log
skewness <- function(x) {(sum((x - mean(x))^3)/length(x))/
    (sum((x - mean(x))^2)/length(x))^(3/2)}
round(skewness(retweets.log),2)

# assume retweets.log follows and Exponential distribution
# maximum likelihood estimate of parameter
n <- 982
max_le <- sum(retweets.log)/n
max_le

# superimposed plot of the relative frequency historgram of retweets.log
library(MASS)
truehist(retweets.log, main="Relative Frequency Histogram of retweets.log", 
         xlab="log( retweets+1 )",
         ylab="Relative Frequency",
         ylim=c(0,0.4), xlim=c(0,10),las=1,col="dodgerblue3",nbins=20)
curve(dexp(x,1/max_le),col="red",add=TRUE,lwd=1.5)

#(4)
# five number summary for retweets with 'covid'
rt.covid <- midtermdataset20891774$retweets[midtermdataset20891774$covid==1]
round(summary(rt.covid),2)

# five number summary for retweets without 'covid'
rt.nocovid <- midtermdataset20891774$retweets[midtermdataset20891774$covid==0]
round(summary(rt.nocovid),2)

# maximum likelihood estimate for lambda0 for without 'covid'
# total of log(retweets  + 1) for tweets without 'covid' / total number of tweets without 'covid'
n0 <- length(rt.nocovid)
n0
rt.nocovid.log <- log(rt.nocovid+1)
max_le_0 <- sum(rt.nocovid.log)/n0
max_le_0

# maximum likelihood estimate for lambda1 for with 'covid'
# total of log(retweets + 1) for tweets with 'covid' / total number of tweets with 'covid'
n1 <- length(rt.covid)
n1
rt.covid.log <- log(rt.covid+1)
max_le_1 <- sum(rt.covid.log)/n1
max_le_1

# 95% confidence interval for lambda0
# hatoflambda +- a*(hatoflambda/sqrt(n))

a <- qnorm(0.975,0,1)
# lower bound
lb0 <- max_le_0-a*(max_le_0/sqrt(n0))
# upper bound
ub0 <- max_le_0+a*(max_le_0/sqrt(n0))
lb0
ub0

# 95% confidence interval for lambda1
lb1 <- max_le_1-a*(max_le_1/sqrt(n1))
ub1 <- max_le_1+a*(max_le_1/sqrt(n1))
lb1
ub1

# side-by-side boxplot of retweets.log for with 'covid' and without 'covid'
boxplot(rt.nocovid.log,
        rt.covid.log,
        col=c("dodgerblue3","red"),
        xlab="Type of tweets",
        ylab="log( retweets+1 )",
        names=c("tweets not containing 'covid'","tweets containing 'covid'"),
        main="Boxplot of retweets.log for tweets without 'covid' and tweets with 'covid'")

#(5)
# time.of.day converted to hours
time.of.day.hour <- midtermdataset20891774$time.of.day/3600

# five-number summary for the number of likes received by tweets during 9:00-12:00
likes.am <- midtermdataset20891774$likes[time.of.day.hour>=9 & time.of.day.hour<=12]
round(summary(likes.am),2)

# five_number summary for the number of likes received by tweets during 12:00-15:00
likes.pm <- midtermdataset20891774$likes[time.of.day.hour>12 & time.of.day.hour<=15]
round(summary(likes.pm),2)

# assume Gaussian for the number of likes during 9:00-12:00 and 12:00-15:00
# MLE for Mu in am and pm
Mu.am_MLE <- mean(likes.am)
Mu.am_MLE
Mu.pm_MLE <- mean(likes.pm)
Mu.pm_MLE

# MLE for Sigma in am and pm
Sigma.am_MLE <- sd(likes.am)
Sigma.am_MLE
Sigma.pm_MLE <- sd(likes.pm)
Sigma.pm_MLE

# 95% confidence interval for Mu in am and pm
# using (mean of y) +- b*(s/sqrt(n))
n.am <- length(likes.am)
b.am <- qt(0.975,n.am-1)
Mu.am_lb <- Mu.am_MLE - b.am*(Sigma.am_MLE/sqrt(n.am))
Mu.am_lb
Mu.am_ub <- Mu.am_MLE + b.am*(Sigma.am_MLE/sqrt(n.am))
Mu.am_ub

n.pm <- length(likes.pm)
b.pm <- qt(0.975,n.pm-1)
Mu.pm_lb <- Mu.pm_MLE - b.pm*(Sigma.pm_MLE/sqrt(n.pm))
Mu.pm_lb
Mu.pm_ub <- Mu.pm_MLE + b.pm*(Sigma.pm_MLE/sqrt(n.pm))
Mu.pm_ub

# 90% confidence interval for Sigma in am and pm
# using [sqrt((n-1)*s^2/d),sqrt((n-1)*s^2)/c)]
d.am <- qchisq(0.95,n.am-1)
c.am <- qchisq(0.05,n.am-1)
Sigma.am_lb <- sqrt((n.am-1)*Sigma.am_MLE^2/d.am)
Sigma.am_lb
Sigma.am_ub <- sqrt((n.am-1)*Sigma.am_MLE^2/c.am)
Sigma.am_ub

d.pm <- qchisq(0.95,n.pm-1)
c.pm <- qchisq(0.05,n.pm-1)
Sigma.pm_lb <- sqrt((n.pm-1)*Sigma.pm_MLE^2/d.pm)
Sigma.pm_lb
Sigma.pm_ub <- sqrt((n.pm-1)*Sigma.pm_MLE^2/c.pm)
Sigma.pm_ub

# (6)
# examine how often different provincial health agencies use information retweet

# estimate of the parameter = (total number of retweets used by a agency) / (total number of tweets sent)
# 95% confidence interval = estinate +- a*sqrt((estimate*(1-estimate))/n)
# where
a <- qnorm(0.975,0,1)
# Alberta
table(midtermdataset20891774$is.retweet[midtermdataset20891774$username=="@GoAHealth"])
# 95% confidence interval
# from table
p.ab <- 164/(164+15)
l.ab <- p.ab - a*sqrt(p.ab*(1-p.ab)/(164+15))
u.ab <- p.ab + a*sqrt(p.ab*(1-p.ab)/(164+15))
l.ab
u.ab

# BC
table(midtermdataset20891774$is.retweet[midtermdataset20891774$username=="@PHSAofBC"])
# 95% confidence interval
p.bc <- 57/(67+57)
l.bc <- p.bc - a*sqrt(p.bc*(1-p.bc)/(67+57))
u.bc <- p.bc + a*sqrt(p.bc*(1-p.bc)/(67+57))
l.bc
u.bc

# Newfoundland and Labrador
table(midtermdataset20891774$is.retweet[midtermdataset20891774$username=="@HCS_GovNL"])
# 95% confidence interval
p.nl <- 155/(155+45)
l.nl <- p.nl - a*sqrt(p.nl*(1-p.nl)/(155+45))
u.nl <- p.nl + a*sqrt(p.nl*(1-p.nl)/(155+45))
l.nl
u.nl

# Nova Scotia @HealthNS
table(midtermdataset20891774$is.retweet[midtermdataset20891774$username=="@HealthNS"])
# 95% confidence interval
p.ns <- 31/(31+86)
l.ns <- p.ns - a*sqrt(p.ns*(1-p.ns)/(31+86))
u.ns <- p.ns + a*sqrt(p.ns*(1-p.ns)/(31+86))
l.ns
u.ns

# Prince Edward Island @Health_PEI
table(midtermdataset20891774$is.retweet[midtermdataset20891774$username=="@Health_PEI"])
# 95% confidence interval
p.pei <- 1/(44+1)
l.pei <- p.pei - a*sqrt(p.pei*(1-p.pei)/(44+1))
u.pei <- p.pei + a*sqrt(p.pei*(1-p.pei)/(44+1))
l.pei
u.pei

# Ontario @ONThealth
table(midtermdataset20891774$is.retweet[midtermdataset20891774$username=="@ONThealth"])
# # 95% confidence interval
p.on <- 122/(27+122)
l.on <- p.on - a*sqrt(p.on*(1-p.on)/(122+27))
u.on <- p.on + a*sqrt(p.on*(1-p.on)/(122+27))
l.on
u.on

# Quebec @sante_qc
table(midtermdataset20891774$is.retweet[midtermdataset20891774$username=="@sante_qc"])
# 95% confidence interval
p.qc <- 1/(39+1)
l.qc <- p.qc - a*sqrt(p.qc*(1-p.qc)/(39+1))
u.qc <- p.qc + a*sqrt(p.qc*(1-p.qc)/(39+1))
l.qc
u.qc

# Saskatchewan 
table(midtermdataset20891774$is.retweet[midtermdataset20891774$username=="@SaskHealth"])
# 95% confidence interval
p.sk <- 31/(97+31)
l.sk <- p.sk - a*sqrt(p.sk*(1-p.sk)/(97+31))
u.sk <- p.sk + a*sqrt(p.sk*(1-p.sk)/(97+31))
l.sk
u.sk














