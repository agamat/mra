#Book I Chapter 3 - Probability and Statistics
#Sec. I.3.2.4 - Samples and Histograms
#Prepare the date
sp500 <- sp500_dec_2004_2005    #read in the daily quotes of S&P500 for period between 31/12/2004 to 30/12/2005 (see /data, source: finance.yahoo.com)
sp500 <- sp500[,c(1,5)]         #choose only date and the close price
sp500[,1] <- as.Date(sp500[,1]) #change format to Date
sp500 <- sp500[order(sp500[,1], decreasing = FALSE),] #reorder with date increasing
rets <- diff(log(sp500[,2]))    #calculate the log returns
sp500.rets <- data.frame(Date = sp500[-1,1], Log.rets = rets)
#Plot the data
par(mfrow = c(2,1))
plot(sp500, type = "l", lwd = 1, col = "red",
     main = "Daily returns of S&P500 (Dec-2004/Dec-2005)",
     cex.main = .75, cex.lab = .8)
grid()

bin.width <- 0.001
bins <- round(with(sp500.rets, max(Log.rets) - min(Log.rets)) / bin.width, 0)
hist(sp500.rets$Log.rets, prob = TRUE,
     main = "Log-rets of S&P500", breaks = bins,
     xlab = "", xlim = c(-0.02, 0.02), 
     cex.main = .75, cex.lab = .8, col = "dark blue")

par(mfrow = c(1,1))

#Sec. I.3.2.5 - Expected Value and Sample Mean
#numerical example for the convergence of sample's mean to the theoretical mean
set.seed(1234)
smpl.size <- 500

mean.vec <- data.frame(obs = 1, smpl.avg = mean(rnorm(1)))
for (i in 2:smpl.size){
  mean.vec <- rbind(mean.vec, c(i, mean(rnorm(i))))
}
plot(mean.vec, pch = 20, main = "Numerical example: Convergence to the theoretical mean along with sample's size",
     xlab = "Size of the sample", ylab = "Sample avg",
     cex.main = 0.75, cex.lab = 0.75)
for (i in seq(-1, 1, by = 0.5))
  abline(h = i, col = "grey", lwd = 1, lty = 2)
abline(h = 0, col = "red", lwd = 1, lty = 5)

#Sec. I.3.2.6 - Variance
#plot two distributions (Normal) with the same mean and different variances
par(mfrow = c(1, 1))
x.rng <- seq(-3,3, by = 0.01)
sd.1 <- 0.2
sd.2 <- 1
plot(x.rng, dnorm(x.rng, mean = 0, sd = sd.1), type = "l", col = "blue", lwd = 2,
     xlab = "x", ylab = "pdf's", main = "Two densities with the same expectation but different standard deviation",
     cex.lab = 0.75, cex.main = 0.75)
lines(x.rng, dnorm(x.rng, mean = 0, sd = sd.2), col = "green", lwd = 2)
for (i in seq(0, 2, by = 0.5))
  abline(h = i, col = "grey", lty = 5)
lab.1 <- paste("N(0;",sd.1,")", sep = "")
lab.2 <- paste("N(0;",sd.2,")", sep = "")
legend(-3, 2, legend = c(lab.1, lab.2), cex = 0.6, lty = 1, col = c("blue","green"))

#numerical example for the convergence of sample's variance to the theoretical variance
set.seed(1234)
smpl.size <- 500

var.vec <- data.frame(obs = 1, smpl.var = var(rnorm(1)))
for (i in 2:smpl.size){
  var.vec <- rbind(var.vec, c(i, var(rnorm(i))))
}
plot(var.vec, pch = 20, main = "Numerical example: Convergence to the theoretical variance along with sample's size",
     xlab = "Size of the sample", ylab = "Sample var",
     cex.main = 0.75, cex.lab = 0.75)
for (i in seq(0, 3, by = 0.5))
  abline(h = i, col = "grey", lwd = 1, lty = 2)
abline(h = 1, col = "red", lwd = 1, lty = 5)

#Sec. I.3.2.7 - Skewness and Kurtosis
#define the functions for sample skewness and kurtosis
skewness <- function(x, na.rm = TRUE){
  x <- x[!is.na(x)]
  n <- length(x)
  (n - 1)^(-1) * sum(((x - mean(x)) / sd(x))^3)
}
kurtosis <- function(x, na.rm = TRUE){
  x <- x[!is.na(x)]
  n <- length(x)
  (n - 1)^(-1) * sum(((x - mean(x)) / sd(x))^4) - 3
}

sample <- c(-3, 1, 4, -2, 0, 5, 6, 2, -3, -1, 0, -3, -2, 4, 7)
#function for calculating the moments of the sample
smpl.moments <- function(x){
                list(mean = mean(x), 
                     sd = sd(x), 
                     skewness = skewness(x), 
                     kurtosis = kurtosis(x))
}
smpl.moments(sample) #calculate the moments of the sample (mean, sd, skewness and kurtosis)

par(mfrow = c(1,2))

#first graph - examples of normal, lepto- and platykurtic distributions
x.rng <- seq(-5, 5, len = 100)
y.lept  <- dnorm(x.rng, mean = 0, sd = .5)
y.norm  <- dnorm(x.rng, mean = 0, sd = 1)
y.platy <- dnorm(x.rng, mean = 0, sd = 1.5)
lept.kurt <- kurtosis(y.lept)
norm.kurt <- kurtosis(y.norm)
platy.kurt <- kurtosis(y.platy)
plot(x.rng, y.lept, type = "l", col = "red", lwd = 2,
     ylab = "", xlab = "x", cex.lab = 0.75,
     main = "Different types of densities", cex.main = 0.75)
lines(x.rng, y.norm, col = "black", lwd = 2)
lines(x.rng, y.platy, col = "blue", lwd = 2)
lgnd.names <- c("leptocurtic", "normal","platycurtic")
for (i in seq(0, 0.8, by = 0.2))
  abline(h = i, col = "grey", lty = 5)
legend(min(x.rng), 0.8, legend = lgnd.names, cex = .5, lty = 1, lwd = 2, col = c("red","black","blue"))

#second graph - positively skewed distribution (example of Gamma dist.)
x.vals <- seq(0, 6, by = .01)
shape.par <- 2
scale.par <- 0.8
y.vals <- dgamma(x.vals, shape = shape.par, scale = scale.par)
skew <- round(skewness(y.vals), 2)
y.lab <- paste("Gamma(", shape.par, ";", scale.par, ")", sep = "")
main.lab <- paste("Positively skewed dist. Skewness =", skew)
plot(x.vals, y.vals, type = "l", col = "red",
     ylab = y.lab, xlab = "x", cex.lab = 0.75,
     main = main.lab, cex.main = 0.75)

#Sec. I.3.2.8 - Quantiles, Quartiles and Percentiles
par(mfrow = c(2,1))
curve(dnorm(x, mean = 0, sd = 1), xlim = c(-3, 3),
     main = "Continous density N(0,1)", ylab = "PDF", xlab = "",
     cex.main = .75, cex.lab = .75)
quartile.x <- qnorm(0.05, , mean = 0, sd = 1)
x.cord <- c(-3, seq(-3, quartile.x, len = 100), quartile.x)
y.cord <- c(0, dnorm(seq(-3, quartile.x, len = 100), mean = 0, sd = 1), 0)
polygon(x.cord, y.cord, col = "grey")
abline(v = qnorm(0.05, mean = 0, sd = 1), lty = 5, col = "red")
curve(pnorm(x, mean = 0, sd = 1), xlim = c(-3, 3),
     main = "Continous distribution N(0,1)", ylab = "CDF", xlab = "",
     cex.main = .75, cex.lab = .75)
quartile.x <- qnorm(0.05, , mean = 0, sd = 1)
x.cord <- c(-3, seq(-3, quartile.x, len = 100), quartile.x)
y.cord <- c(0, pnorm(seq(-3, quartile.x, len = 100), mean = 0, sd = 1), 0)
polygon(x.cord, y.cord, col = "grey")
abline(v = qnorm(0.05, mean = 0, sd = 1), lty = 5, col = "red")

#Sec. I.3.3.1 - Binominal distribution
par(mfrow = c(1, 1))
trials <- 10
#calculate probabilities for different prob. of success
p1 <- dbinom(0:10, trials, 0.1) 
p2 <- dbinom(0:10, trials, 0.25)
p3 <- dbinom(0:10, trials, 0.5)
#create the matrix with density values
bin.probs <- matrix(c(p1, p2, p3), ncol = 3)
colnames(bin.probs) <- c("p=0.1", "p=0.25", "p=0.5")
rownames(bin.probs) <- 0:10
apply(bin.probs, 2, sum) #check if all probabilities sum to 1
#plot the densities
barplot(t(bin.probs), beside = TRUE, col = c("blue", "red", "green"), legend = TRUE,
        main = "Some binominal density functions", ylab = "Prob.", xlab = "No of successes")

#Example I.3.4 - Evolution of an asset price
start.price <- 50
days <- 4
prob.up <- 0.7
up.mov <- 1.25
dw.mov <- 1/up.mov
#calculate price/paths/prob if the stock moves up for 4 consecutive days
price <- start.price * up.mov ^ 4 * dw.mov ^ (days - 4)
paths <- choose(days, 4)
prob <- dbinom(4, days, prob.up)
result <- data.frame(Price = price, No.of.Paths = paths, Prob = prob)

#calculate price/paths/prob if the stock moves up for 3,2, ... days
for (i in 3:0){
  price <- start.price * up.mov ^ i * dw.mov ^ (days - i)
  paths <- choose(days, i)
  prob <- dbinom(i, days, prob.up)
  result <- rbind(result, c(price, paths, prob))
}
result #show the result

#Sec.I.3.3.2 - Poisson and Exponential Distributions
mu <- 2.5
pois.prob <- matrix(dpois(0:10, lambda = mu), ncol = 1)
colnames(pois.prob) <- "Poiss(mu=2.5)"
rownames(pois.prob) <- 0:10
barplot(t(pois.prob), main = "Density of Poisson distribution (mu = 2.5)",
        xlab = "No of occurences", ylab = "Probability", col = "lightblue")
t(pois.prob) #Densisties of Poisson distribution

#Sec. I.3.3.3 - Uniform Distribution
draws <- 1000
x.cord <- runif(draws)
y.cord <- runif(draws)
plot(x.cord, y.cord, pch = 19,
     xlab = "", ylab = "",
     xlim = c(0, 1), ylim = c(0, 1), 
     cex = 0.7, main = paste(draws,"uniformely distributed points (X,Y) ~ U[0,1]"))
grid()

#Sec. I.3.3.4 - Normal Distribution
plot(function(x) dnorm(x), -7.5, 6.5, ylab = "", lwd = 2, col = "blue",
     main = "Two normal densities", cex.lab = 0.75)
curve(dnorm(x, mean = 1, sd = 2), add = TRUE, lwd = 2, col = "red", cex.lab = 0.75)
grid()
legend(-7.5, 0.4, legend = c("N(0,1)","N(1,4)"), lwd = 2, col = c("blue", "red"), cex = 0.75)
abline(v = 0:1, lwd = 1, lty = 5, col = c("blue", "red"))

#Sec. I.3.3.4 - Lognormal Distribution
plot(function(x) dlnorm(x), 0, 6, ylab = "", lwd = 2, col = "blue",
     main = "Two normal densities", cex.lab = 0.75, ylim = c(0, 0.7))
grid()

#Sec. I.3.3.5 - Lognormal Distribution
#Sec. I.3.3.6 - Normal Mixture Distributions
#Sec. I.3.3.7 - Student t Distribution
par(mfrow = c(1, 1), mar = c(5,4,4,2)+0.1)
plot(function(x) dt(x, df = 3), -6, 6, col = "grey60", lwd = 2, ylim = c(0, 0.45),
     xlab = "", ylab = "",
     main = "Comparison of Student t densities and standard normal", cex.main = 0.8)
curve(dt(x, df = 6), col = "grey", lwd = 2, add = TRUE)
curve(dnorm(x, mean = 0, sd = 1), lwd = 2, add = TRUE)

abline(h = seq(0, 0.5, 0.1), col = "light blue", lty = 3)
legend("topleft", inset = 0.01, 
       legend = c("t-Dist(df = 3)","t-Dist(df = 6)", "N(0;1)"), 
       lwd = 2, lty = 1, col = c("grey60", "grey", "black"),
       cex = 0.75)

plot(function(x) dnorm(x, 0, sqrt(1.5)), -6, 6, 
     col = "red", lwd = 2, ylim = c(0, 0.45), ann = FALSE)
curve(dt(x, df = 6), lwd = 2, add = TRUE, col = "blue")
abline(h = seq(0, 0.4, 0.1), col = "grey", lty = 3)
legend("topleft", inset = 0.01, 
       legend = c("Normal with var.=1.5", "t-Dist(df = 6)"), 
       lwd = 2, lty = 1, col = c("red", "blue"),
       cex = 0.75)
title(main = "Comparison of Student t density and normal with same variance", cex.main = 0.8) 

#Sec. I.3.3.8 - Sampling Distributions
#Chi-square with k degrees of freedom
par(mfrow = c(1,2))
#densities
plot(function (x) dchisq(x, df = 1), 0, 15, ann = FALSE, lwd = 2, col = "brown")
curve(dchisq(x, df = 2), add = TRUE, lwd = 2, col = "red")
curve(dchisq(x, df = 3), add = TRUE, lwd = 2, col = "blue")
curve(dchisq(x, df = 4), add = TRUE, lwd = 2, col = "green")
curve(dchisq(x, df = 6), add = TRUE, lwd = 2, col = "yellow")
curve(dchisq(x, df = 9), add = TRUE, lwd = 2, col = "grey")
grid()
legend.txt <- c("df = 1","df = 2","df = 3", "df = 4", "df = 6", "df = 9")
color.list <- c("brown", "red", "blue", "green", "yellow", "grey")
legend("topright", inset = 0.1, title = "Chi-sq", legend = legend.txt, 
       col = color.list, lty = 1, lwd = 2, cex = 0.7)
title(main = "Different Chi-square density functions", cex.main = 0.7)
#cumulative distribution functions
plot(function (x) pchisq(x, df = 1), 0, 15, ann = FALSE, lwd = 2, col = "brown")
curve(pchisq(x, df = 2), add = TRUE, lwd = 2, col = "red")
curve(pchisq(x, df = 3), add = TRUE, lwd = 2, col = "blue")
curve(pchisq(x, df = 4), add = TRUE, lwd = 2, col = "green")
curve(pchisq(x, df = 6), add = TRUE, lwd = 2, col = "yellow")
curve(pchisq(x, df = 9), add = TRUE, lwd = 2, col = "grey")
grid()
legend.txt <- c("df = 1","df = 2","df = 3", "df = 4", "df = 6", "df = 9")
color.list <- c("brown", "red", "blue", "green", "yellow", "grey")
legend("bottomright", inset = 0.1, title = "Chi-sq", legend = legend.txt, 
       col = color.list, lty = 1, lwd = 2, cex = 0.7)
title(main = "Different Chi-square cumulative distribution functions", cex.main = 0.7)
par(mfrow = c(1, 1))

opar <- par(no.readonly = TRUE)
draws <- 1e5
par(mfrow = c(3,1))
y.lim <- c(0, 0.15)
hist(rchisq(draws, df = 5), col = "light blue", breaks = 50, 
     prob = TRUE, xlim = c(0, 60), ylim = y.lim,
     xlab = "t-Distribution (df = 1)", main = "")
hist(rchisq(draws, df = 10), col = "blue", breaks = 50, 
     prob = TRUE, xlim = c(0, 60), ylim = y.lim,
     xlab = "t-Distribution (df = 10)", main = "")
hist(rchisq(draws, df = 25), col = "dark blue", breaks = 50, 
     prob = TRUE, xlim = c(0, 60), ylim = y.lim,
     xlab = "t-Distribution (df = 25)", main = "")
title(main = "Histograms for t-Distribution with different degrees of freedom", outer = TRUE, line = -1.5)
par(opar)
#Standard t with k degrees of freedom
#F-distribution

###+++++++++++++++++++++++++++++++++++++++++++
#++++ Review of probability distributions  +++
###+++++++++++++++++++++++++++++++++++++++++++

###+++ Continous probability distributions +++

#Parametrise the plotting area
#par(mfrow = c(1,2))
?layout
mat.lay <- matrix(c(2,2,0,4), 2, 2, byrow = TRUE)
mat.lay
layout(mat.lay)
layout.show(n = 4)
#Normal distribution
mean.1 = 0; sd.1 = 1
mean.2 = 1; sd.2 = 3
mean.3 = -1; sd.3 = 0.5
#Density functions
plot(function(x) dnorm(x, mean.1, sd.1), col = "blue", lwd = 2, lty = 1,
     xlim = c(-6, 6), ylim=c(0, 1), cex.axis = 0.7, ann = FALSE)
curve(dnorm(x, mean = mean.2, sd = sd.2), col = "red", add = TRUE, lwd = 2, lty =1)
curve(dnorm(x, mean = mean.3, sd = sd.3), col = "green", add = TRUE, lwd = 2, lty = 1)
title(main = "Different density functions for normal distribution", cex.main = 0.7)
abline(h = seq(0, 1, by = 0.2), col = "grey", lty = 3)
legend("topright", inset = 0.01, legend = c("N(0,1)","N(1,3)","N(-1,0.5)"), cex = 0.7,
       lty = 1, lwd = 2, col = c("blue","red","green"))
#Cumulative distribution function
plot(function(x) pnorm(x, mean.1, sd.1), col = "blue", lwd = 2, lty = 1,
     xlim = c(-6, 6), ylim=c(0, 1), cex.axis = 0.7, ann = FALSE)
curve(pnorm(x, mean = mean.2, sd = sd.2), col = "red", add = TRUE, lwd = 2, lty =1)
curve(pnorm(x, mean = mean.3, sd = sd.3), col = "green", add = TRUE, lwd = 2, lty = 1)
title(main = "Different density functions for normal distribution", cex.main = 0.7)
abline(h = seq(0, 1, by = 0.2), col = "grey", lty = 3)
legend("bottomright", inset = 0.01, legend = c("N(0,1)","N(1,3)","N(-1,0.5)"), cex = 0.7,
       lty = 1, lwd = 2, col = c("blue","red","green"))
#Randomly generated numbers
draws <- 100
plot(rnorm(draws, mean.1, sd.1), col = "blue", pch = 19, cex.axis = 0.7, ann = FALSE, cex = 0.7)
points(rnorm(draws, mean = mean.2, sd = sd.2), col = "red", pch = 19, cex = 0.7)
points(rnorm(draws, mean = mean.3, sd = sd.3), col = "green", pch = 19, cex = 0.7)
title(main = "Random numbers", cex.main = 0.7)
grid()
legend("topright", inset = 0.01, legend = c("N(0,1)","N(1,3)","N(-1,0.5)"),
       col = c("blue","red","green"), pch = 19, cex = 0.7)
