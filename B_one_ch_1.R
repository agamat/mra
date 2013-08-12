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
curve(f_1, 0.1, 5, col = "blue", 
      main = "Two functions", ylab = "values")
curve(f_2, 0.1, 5, col = "red", add = TRUE)
abline(h = 0, v = 0, lty = 5, col = "grey")
grid()
leg.txt <- c("f_1", "f_2")
legend(0, 7.5, leg.txt, lty = 1, col = c("blue","red"))

#Sec.I.1.3.4 - Stationary points
#define the initial funtion
initF <- function(x){
  x^3 - 7*x^2 + 14*x - 8
}
#calculate (define) the derivative of the initial function
deriv <- function(x){
  3*x^2 - 14*x + 14
}
#plotting procedures
curve(initF, 0, 5,
      main = "Roots of quadratic function", xlab = "x", ylab = "y",
      col = "red", lwd = 2,
      cex.lab = 0.75)
curve(deriv, add = TRUE, col = "green", lty = 2)
abline(h = 0, col = "grey", lty = 4)
#calculate the roots of the derivate (stationary points)
root_1 <- uniroot(deriv, c(0, 3))$root
root_2 <- uniroot(deriv, c(3, 4))$root
points(root_1, 0, col = "dark green")
points(root_2, 0, col = "dark green")
abline(v = root_1, lwd = 1, lty = 4, col = "grey")
abline(v = root_2, lwd = 1, lty = 4, col = "grey")

#Sec.I.1.3.4 - Stationary points
#define the initial funtion
initF <- function(x){
  x^2*log(x)
}
#calculate (define) the derivative of the initial function
fstDeriv <- function(x){
  2*x*log(x) + x
}
sndDeriv <- function(x){
  2*log(x) + 3
}
#plotting procedures
curve(initF, 0, 1.5,
      main = "Finding saddle points", xlab = "x", ylab = "y",
      ylim = c(-1, 1),
      col = "red", lwd = 2,
      cex.lab = 0.75)
curve(fstDeriv, add = TRUE, col = "dark green", lty = 2)
curve(sndDeriv, add = TRUE, col = "green", lty = 2)
abline(h = 0, col = "grey", lty = 4)
#calculate the roots of the derivate (stationary points)
root_1 <- uniroot(deriv, c(0.2, 1))$root
root_2 <- uniroot(deriv, c(3, 4))$root
points(root_1, 0, col = "dark green")
points(root_2, 0, col = "dark green")
abline(v = root_1, lwd = 1, lty = 4, col = "grey")
abline(v = root_2, lwd = 1, lty = 4, col = "grey")

#Sec.I.1.3.5 - Integration (Fig. I.1.9)
f_1 <- function(x) {
  2*x + x^2 
}
curve(f_1, 0, 5,
      main = "The definite integral",
      xlab = "x", ylab = "f(x)",
      cex.lab = 0.75)
grid()
x.rng <- c(2, seq(2,4, len = 100), 4)
y.rng <- c(0, f_1(seq(2,4,len = 100)), 0)
polygon(x.rng, y.rng, col = "grey", border = "grey")
