########################
# Final Report
# YIMING SHEN (20891774)
########################

# plan part
# sample size 'n'
n <- nrow(midtermdataset20891774)
n
# summary statistics for each accounts
table(midtermdataset20891774$username)


# analysis part
# (1)

# urls.binary
# observed number of tweets containing url:y
table(midtermdataset20891774$urls.binary)
# mle of theta
y <- 355
thetahat <- y/n
thetahat

# 15% L.I for theta
# relative likelihood function for binomial
BinoRLF <- function(theta, n, y, thetahat) {
  exp(y*log(theta/thetahat)+(n-y)*log((1-theta)/(1-thetahat)))
}
theta <- seq(0.29,0.43,0.001)
plot(theta, BinoRLF(theta, n, y, thetahat),type = "l",
     main="Relative likelihood function for Binomial with n=982, y=355",las=1,
     xlab=expression(theta),ylab=expression(paste("R(",theta,")")))
abline(h=0.15, col="red", lwd=1)
# lower and upper bounds for the 15% likelihood interval
uniroot(function(x) BinoRLF(x, n, y, thetahat) - 0.15, lower=0.32, upper=0.34)$root
uniroot(function(x) BinoRLF(x, n, y, thetahat) - 0.15, lower=0.38, upper=0.40)$root

# H0: theta0=0.19
theta0 <- 0.19
# the likelihood ratio test stat:
lambda <- (-2)*log(BinoRLF(theta0, n, y, thetahat))
lambda
# p-value: from X^2(1) approximately
1-pchisq(lambda, 1)

# (2)

# for @ONThealth
time.of.day_o <- midtermdataset20891774$time.of.day[midtermdataset20891774$username=="@ONThealth"]
time.of.day.hour_o <- time.of.day_o/3600
n_o <- length(time.of.day.hour_o)
n_o

# for @GoAHealth
time.of.day_a <- midtermdataset20891774$time.of.day[midtermdataset20891774$username=="@GoAHealth"]
time.of.day.hour_a <- time.of.day_a/3600
n_a <- length(time.of.day.hour_a)
n_a

# check the goodness of fitting Gaussian model
# for @ONThealth
m_o <- mean(time.of.day.hour_o)
m_o
median(time.of.day.hour_o)

skewness <- function(x) {(sum((x - mean(x))^3)/length(x))/
    (sum((x - mean(x))^2)/length(x))^(3/2)}
kurtosis <- function(x) {(sum((x - mean(x))^4)/length(x))/
    (sum((x - mean(x))^2)/length(x))^2}

skewness(time.of.day.hour_o)
kurtosis(time.of.day.hour_o)

qqnorm(time.of.day.hour_o, main="QQplot for time.of.day.hour of @ONThealth",col="blue")
qqline(time.of.day.hour_o)

library(MASS)
# relative frequency histogram with superimposed Gaussian probability density function
truehist(time.of.day.hour_o,xlab="Time of day in hours",ylab="Relative Frequency",
         main = "Relative frequency histogram of time.of.day.hour for @ONThealth",
         nbins=15, ylim=c(0,0.25),
         las=1, col="dodgerblue3",density=25,angle=45)
curve(dnorm(x,m_o,sd(time.of.day.hour_o)), col = "red", add=TRUE, lwd=1.5)

# for @GoAHealth
m_a <- mean(time.of.day.hour_a)
m_a
median(time.of.day.hour_a)

skewness(time.of.day.hour_a)
kurtosis(time.of.day.hour_a)

qqnorm(time.of.day.hour_a, main="QQplot for time.of.day.hour of @GoAHealth",col="blue")
qqline(time.of.day.hour_a)

# relative frequency histogram with superimposed Gaussian probability density function
truehist(time.of.day.hour_a,xlab="Time of day in hours",ylab="Relative Frequency",
         main = "Relative frequency histogram of time.of.day.hour for @GoAHealth",
         nbins=15, ylim=c(0,0.25),
         las=1, col="dodgerblue3",density=25,angle=45)
curve(dnorm(x,m_a,sd(time.of.day.hour_a)), col = "red", add=TRUE, lwd=1.5)

# 99% c.i.: [sqrt((n-1)s^2/d),sqrt((n-1)s^2/c)]
# for mu_o
p <- 0.99
s2_o <- var(time.of.day.hour_o)
d_o <- qchisq((1+p)/2,n_o-1)
c_o <- qchisq((1-p)/2,n_o-1)
lb_o <- sqrt((n_o-1)*s2_o/d_o)
ub_o <- sqrt((n_o-1)*s2_o/c_o)
lb_o
ub_o

# for mu_a
p <- 0.99
s2_a <- var(time.of.day.hour_a)
d_a <- qchisq((1+p)/2,n_a-1)
c_a <- qchisq((1-p)/2,n_a-1)
lb_a <- sqrt((n_a-1)*s2_a/d_a)
ub_a <- sqrt((n_a-1)*s2_a/c_a)
lb_a
ub_a

# assume mu_0 = mu_a
# unpaired data with same variance
# point estiamte of difference mean: mean(y_o) - mean(y_a)
m_o-m_a
# 95% c.i.
p <- 0.95
sp2 <- (n_o-1)/(n_o+n_a-2)*s2_o + (n_a-1)/(n_o+n_a-2)*s2_a
b <- qt((1+p)/2,n_o+n_a-2)
sp <- sqrt(sp2)
lb <- m_o-m_a-b*sp*sqrt(1/n_o+1/n_a)
ub <- m_o-m_a+b*sp*sqrt(1/n_o+1/n_a)
lb
ub
# H0: mean(y_o)-mean(y_a)
# test stat
d <- (m_o-m_a-0)/(sp*sqrt((1/n_o)+(1/n_a)))
# p-value
2*(1-pt(d,n_o+n_a-2))
# check with t.test
t.test(time.of.day.hour_o,time.of.day.hour_a,var.equal = TRUE, conf.level = 0.95)



# (3)
retweets.log <- log(midtermdataset20891774$retweets+1)
likes.log <- log(midtermdataset20891774$likes+1)

# LSE
mod <- lm(likes.log~retweets.log)
summary(mod)

# check fitness
# scatterplot with fitted line
plot(retweets.log, likes.log, xlab="retweets.log",ylab="likes.log",
     main="Scatterplot of likes.log and retweets.log with fitted line",
     pch=1,cex=0.5,col="navy",las=1,lwd=1)
abline(coef(mod),lwd=2,lty=2,col="red")
# std residuals vs. fitted values
stdres <- rstandard(mod)
plot(fitted(mod),stdres, xlab="fitted values",ylab="Standardized Residuals",
     main="Std residuals vs. fitted values", 
     pch=1, col="navy",cex=0.5, las=1)
abline(h=0, lty=2, col="red", lwd=2)
# qqplot
qqnorm(stdres, main="qqplot of std residuals",
       xlab="G(0,1) Quantitiles", ylab="standardized residuals",
       pch=1,col="navy",cex=0.5)
qqline(stdres, lty=2, col="red", lwd=2)

# 95% confidence interval for slope
confint(mod, level=0.95)

# estimate and 90% prediction interval
int.log <- predict(mod, data.frame(retweets.log = log(30+1)), 
                   interval="prediction", level=0.9)
int <- exp(int.log) - 1
int

# 4

# assume poisson(theta)
# mle of theta
long.w <- midtermdataset20891774$long.words
thetahat <- mean(long.w)
thetahat
# approximately 95% c.i. for theta: thetahat +- a*sqrt(thetahat/n)
n <- 982
p <- 0.95
a <- qnorm((1+p)/2)
lb <- thetahat - a*sqrt(thetahat/n)
ub <- thetahat + a*sqrt(thetahat/n)
lb
ub

# likelihood ratio goodness of fit test
table(long.w)
# group data
long.w.grouped <- midtermdataset20891774$long.words
long.w.grouped[midtermdataset20891774$long.words >= 7] <- "7+"
# table of expected VS. observed
expected <- dpois(0:6, thetahat) * n
#round(expected,2)
e7 <- n-sum(expected)
e7
expected <- append(expected,e7)
observed <- table(long.w.grouped)
cbind(observed, round(expected,2))

# observed value of the likelihood ratio stat: lambda=2*sum(yi*log(yi/ei))
lambda <- 2*sum(observed*log(observed/expected))
lambda

# p-value
# d.f.=k-1-p, where k=8, p=1
df <- 8-1-1
1-pchisq(lambda, df)

# grouped barplot
barplot(rbind(observed, expected), beside=T, col=c("dodgerblue3","darkorchid"),
        density=75,angle=c(45,135),ylim=c(0,300),las=1,xlab="number of long words",
        ylab="tweets",
        main="Grouped barplot of the observed and expected frequencies")
legend("topright",legend=c("observed","expected"),fill=c("dodgerblue3","darkorchid"),
       density=75,angle=c(45,135))

# assume poisson fit well
# point estimate of the probability that the tweet contains at least 2 long words
# P(Y>=2) = 1 - P(Y=<1)
1-ppois(1,thetahat)

# (5)
# two-way table of observed and expected frequencies
observed <- table(midtermdataset20891774$hashtags.binary, midtermdataset20891774$media.binary)
observed
# row totals
r <- margin.table(observed, 1)
# column totals
c <- margin.table(observed, 2)
# expected = r*c/n
expected <- outer(r, c)/sum(observed)
expected

# the observed value of test stat
lambda <- 2*sum(observed*log(observed/expected))
lambda
# df <- (a-1)(b-1)
df <- (2-1)*(2-1)
#p-value
1-pchisq(lambda, df)

# (6)
# mle for each theta.i with 1<=i<=8
table(midtermdataset20891774$username)
# @GoAHealth
table(midtermdataset20891774$is.retweet[midtermdataset20891774$username=="@GoAHealth"])
thetahat1 <- 164/(164+15)
thetahat1
# @HCS_GovNL
table(midtermdataset20891774$is.retweet[midtermdataset20891774$username=="@HCS_GovNL"])
thetahat2 <- 155/(155+45)
thetahat2
# @Health_PEI
table(midtermdataset20891774$is.retweet[midtermdataset20891774$username=="@Health_PEI"])
thetahat3 <- 1/(44+1)
thetahat3
# @HealthNS
table(midtermdataset20891774$is.retweet[midtermdataset20891774$username=="@HealthNS"])
thetahat4 <- 31/(31+86)
thetahat4
# @ONThealth
table(midtermdataset20891774$is.retweet[midtermdataset20891774$username=="@ONThealth"])
thetahat5 <- 122/(122+27)
thetahat5
# @PHSAofBC
table(midtermdataset20891774$is.retweet[midtermdataset20891774$username=="@PHSAofBC"])
thetahat6 <- 57/(57+67)
thetahat6
# @sante_qc
table(midtermdataset20891774$is.retweet[midtermdataset20891774$username=="@sante_qc"])
thetahat7 <- 1/(39+1)
thetahat7
# @SaskHealth
table(midtermdataset20891774$is.retweet[midtermdataset20891774$username=="@SaskHealth"])
thetahat8 <- 31/(31+97)
thetahat8

# under the hypothesis, thetahat.i are all the same
# the mle of theta
table(midtermdataset20891774$is.retweet)
thetahat <- 562/(562+420)
thetahat

# pearson test
# expected and observed table
# expected.i <- n.i*thetahat and n can be obtained by previous table
expected1 <- thetahat*179
round(expected1,2)
expected2 <- thetahat*200
round(expected2,2)
expected3 <- thetahat*45
round(expected3,2)
expected4 <- thetahat*117
round(expected4,2)
expected5 <- thetahat*149
round(expected5,2)
expected6 <- thetahat*124
round(expected6,2)
expected7 <- thetahat*40
round(expected7,2)
expected8 <- thetahat*128
round(expected8,2)

# pearson test stat
observed_s <- c(164,155,1,31,122,57,1,31)
expected_s <- c(expected1,expected2,expected3,expected4,
                expected5,expected6,expected7,expected8)
d <-sum(((observed_s-expected_s)^2)/expected_s)
d
# p-value d~X2(d.f.=k-1-p) where k=8, p=1
d.f <- 8-1-1
1-pchisq(d,d.f)



