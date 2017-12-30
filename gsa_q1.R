# Question 1: Write a program which reads a K*K transition matrix
# and a 1*K initial distribution from a file and outputs N elements
# of a Markov chain
# i.e., given the initial distribution pi0, transition matrix P, and
# number of steps N
# assume that the states are {1, ..., K}, where K is the length of 
# pi0, and P is the K*K transition matrix
Markov <- function(N, pi0, P){
  K <- length(pi0)
  X <- matrix(0, 1, N)
  a <- sample(x = c(1:K), size = 1, replace=TRUE, prob = pi0)
  X[1] <- a
  for (i in 2:N){
    a <- sample(x = c(1:K), size = 1, replace = TRUE, prob = P[a,])
    X[i] <- a
  }
  return(X)
}
