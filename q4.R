# Question 4: Implement the forward algorithm with scaling to 
# calculate the likelihood of an emitted sequence given the model, 
# reading the emitted sequence from a file

forward.algorithm <- function(ptrans, pemit, pi, Y){
  # transition matrix ptran, intial distribution pi, emission 
  # proabilities pemit and emitted sequence Y
  J <- dim(ptrans)[1]
  N <- length(Y)
  alpha.hat.mat <- matrix(0, N, J)
  hid.states <- dimnames(pemit)[[1]]
  emit.states <- dimnames(pemit)[[2]]
  alpha.hat.mat[1,] <- pemit[,Y[1]] * pi
  alpha.hat.mat[1,] <- alpha.hat.mat[1,]/sum(alpha.hat.mat[1,])
  for (i in 2:N){
    alpha.hat.mat[i,] <- pemit[,Y[i]] * alpha.hat.mat[(i-1),] %*% 
      ptrans
    alpha.hat.mat[i,] <- alpha.hat.mat[i,]/sum(alpha.hat.mat[i,])
  }
  C <- rep(0, N)
  C[1] <- sum(pemit[,Y[1]]*pi)
  for (j in 2:N){
    C[j] <- sum(pemit[,Y[i]] * alpha.hat.mat[(i-1),] %*% ptrans)
  }
  p <- prod(C)
  return(p)
}

hid.states <- c("1", "2")
emit.states <- c("1", "2", "3", "4", "5")
hid.trans <- matrix(c(0.8, 0.1, 0.2, 0.9), 2, 2,
                    dimnames = list(hid.states, hid.states))
init.dist <- c(0.5, 0.5)
names(init.dist) <- hid.states

emit.probs <- matrix(c(0.2, 0, 0.5, 0.1, 0.2, 0.4, 0.1, 0.4, 0, 
                       0.1), 2, 5, dimnames = list(hid.states,
                                                   emit.states))
disc.hmm <- hmm(hid.trans, init.dist, emit.probs, 115)

forward.algorithm(hid.trans, emit.probs, init.dist, disc.hmm$obs)
