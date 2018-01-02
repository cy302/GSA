# Question2: Write a program which reads a sequence of state indices
# (i.e. elements of {1,..,K}) an dinfers a maximum likelihood Markov
# chain transition matrix and initial distribution

Max.likelihood.para <- function(X, K){
  freq <- P <- matrix(0, K, K)
  pi <- rep(0, K)
  X.new <- cbind(X[1:(length(X)-1)], X[2:length(X)])
  for (i in 1:length(X.new)){
    freq[X.new[i,1], X.new[i.2]] = freq[X.new[i,1], X.new[i.2]]+1
  }
  for (i in 1:K){
    s <- sum(freq[i,])
    P[i,] <- freq[i,]/s
  }
  pi[X[1]] <- 1
  return(list("Maximum likelihood Markov 
              chain transition matrix" = P, "Maximum likelihood 
              Markov chain initial distribution" = pi))
}
