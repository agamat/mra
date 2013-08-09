f_x <- function(x, a, b, c){
  a * x ^ 2 + b * x + c
}

#define the domain and parameters of the quadratic function
x <- seq(-2.5, 2.5, len = 300); a <- 4; b <- 3; c <- 2 
y <- f_x(x, a, b, c)
plot(x, y, 
     col = "red", 
     type = "l", 
     lwd = 2, 
     main = "Quadratic function", 
     xlab = "x-values", ylab = "y-values", cex.lab = 0.75)
grid()