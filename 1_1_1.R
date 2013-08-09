#Functions and graphs from chapter 1
#Fig.I.1.1 - linear funcion
f_x <- function(x){
  3 * x + 2
}
curve(f_x, -2.5, 2.5, 
     col = "red", 
     type = "l", 
     lwd = 2, 
     main = "Linear function", 
     xlab = "x-values", ylab = "y-values", cex.lab = 0.75)
abline(h = 0, v = 0, lty = 5, col = "black")
grid()

#Fig.I.1.2 - quadratic funcion
f_x <- function(x){
  4 * x ^ 2 + 3 * x + 2
}
curve(f_x, -2.5, 2.5,
     col = "red", 
     type = "l", 
     lwd = 2, 
     main = "Quadratic function", 
     xlab = "x-values", ylab = "y-values", cex.lab = 0.75,
     ylim = c(0, 35))
abline(h = 0, v = 0, lty = 5, col = "black")
grid()