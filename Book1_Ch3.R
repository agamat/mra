#Book I Chapter 3 - Probability and Statistics
#Sec. I.3.2.4 - Samples and Histograms
#Prepare the date
sp500 <- sp500_dec_2004_2005    #read in the daily quotes of S&P500 (from www.finance.yahoo.com) for period between 31/12/2004 to 30/12/2005
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
