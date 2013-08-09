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

#Fig.I.1.3 - reciprocal function
f_x <- function(x){
  1 / x
}
curve(f_x, -3, 3,
      col = "red", 
      type = "l", 
      lwd = 2, 
      main = "Reciprocal function", 
      xlab = "x-values", ylab = "y-values", cex.lab = 0.75)
abline(h = 0, v = 0, lty = 5, col = "black")
grid()

#Convergence to e
x <- seq(0.1, 10, len = 50)
conv.exp <- function(x){
  (1 + 1 / x) ^ x
}
plot(x, conv.exp(x), type = "p", col = "red", 
     main = "Convergence to exp(x)",
     xlab = "x", ylab = "f(x) = exp(x)",
     ylim = c(0, 3),
     cex.lab = 0.75
     )
abline(h = exp(1), col = "blue", lwd = 2)
grid()

#Fig.I.1.5 - The exponential function
curve(exp, -3, 3,
      main = "The exponential function",
      cex.lab = 0.75, col = "red")
abline(h = 0, v = 0, col = "grey", lty = 5)
grid()

#Fig.I.1.6 - The natural logarithm
curve(log, 0, 3,
      main = "The natural logarithmic function",
      cex.lab = 0.75, col = "red")
abline(h = 0, v = 0, col = "grey", lty = 5)
grid()

#Fig.I.1.8 - Two functions
f_1 <- function(x) {
  x^3 - 7*x^2 + 14*x - 8
}
f_2 <- function(x) {
  10 - 0.5*(x^2) + sqrt(log(1+x^2)) - 4*log(x)/exp(x)
}
curve(f_1, 0, 5, col = "blue", main = "Two functions")
curve(f_2, 0, 5, col = "red", add = TRUE)
abline(h = 0, v = 0, lty = 5, col = "grey")
grid()
legend(names = c("Function(a)","Function(b)"))