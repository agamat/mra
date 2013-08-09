f_x <- function(x, a, b){
  a * x + b
}

#define the domain and parameters of the linear function
x <- seq(-2.5, 2.5, len = 300); a <- 3; b <- 2
y <- f_x(x, a, b)
plot(x, y, 
     col = "red", 
     type = "l", 
     lwd = 2, 
     main = "Linear function", 
     xlab = "x-values", ylab = "y-values", cex.lab = 0.75)
grid()