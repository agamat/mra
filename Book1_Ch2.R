#Sec. I.2.2.1 - Basic terminology
elem <- c(-1, 0.4, -0.5, 0, 3, 2)
A <- matrix(elem, nrow = 2) #create matrix object
A.trans <- t(A) #transpose the matrix

#Sec. I.2.2.2 - Laws of Matrix Algebra
A <- matrix(c(1, 3, 2, 4), nrow = 2) #define square matrix
B <- matrix(c(5, 7, 6, 8), nrow = 2) #define square matrix

C <- A + B #matrix addition
zero.mat <- matrix(c(0, 0, 0, 0), nrow = 2) #define zero matrix

C + zero.mat #should result in C matrix
A + (- A) # should result in zero.mat

A <- matrix(c(0, -1, 1, 2), nrow = 2)
2 * A #multiply the matrix by a scalar

A <- matrix(c(1,2,3), nrow = 3)
B <- matrix(c(-1, 1, 2), nrow = 3)
sum(A * B) #dot product of two non-zero vectors

A <- matrix(c(1, 2, 1), nrow = 3)
B <- matrix(c(-1, 1, -1), nrow = 3)
sum(A * B) #orthogonal vectors

A <- matrix(c(0, 1, 1, 2, -1, 0), nrow = 2)
B <- matrix(c(1, 0 , 2, 0, 0, -1, 2, 1, 0, 1, -1, 2), nrow = 3)
A %*% B #matrix product

A <- matrix(1:9, nrow = 3)
B <- matrix(10:18, nrow = 3)
A %*% B 
B %*% A #non-commutative

x <- 1:9
y <- 10:18
A <- diag(x) #define diagonal matrix
B <- diag (y) #define diagonal matrix
A %*% B
B %*% A #diagonal matrices are commutative 

A <- matrix(1:9, nrow = 3)
B <- diag(10:18, nrow = 3)
t(A %*% B)
t(B) %*% t(A)

#Sec. I.2.2.3 - Singular Matrices
I <- diag(rep(1,4))
A <- matrix(1:16, nrow = 4)
I %*% A
A %*% I

A <- matrix(c(1, -0.25, -0.25, 1), nrow = 2) #define the non-singular matrix
solve(A) #inverse the matrix
solve(A) %*% A #matrix product of inversed matrix and the original produces the identity matrix
A %*% solve(A)

#Sec. I.2.2.4 - Determinants
A <- matrix(c(1, 0, 1, 2, 2, 1, 0, 2, 1, -1, 0, 1, 0, 2, 1, 1), nrow = 4)
det(A) #this is the singular matrix therefore the inverse of matrix A does not exist
