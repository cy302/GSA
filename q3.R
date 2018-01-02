# Question 3: Implement a HMM by modifying your program from 
# Question 1, adding an emmited variable at each point in the chain.

hmm <- function(ptrans, pi, pemit, N){
  # simulate observed (emission) sequence from an HMM with
  # transition matrix ptran, intial distribution pi, emission 
  # proabilities pemit and length of simulated data N.
  
  K <- c(1:length(pi))
  hidden <- matrix(0, 1, N)
  a <- sample(x = K, size = 1, replace=TRUE, prob = pi)
  hidden[1] <- a
  for (i in 2:N){
    a <- sample(x = K, size = 1, replace = TRUE, prob = ptrans[a,])
    hidden[i] <- a
  }
  
  if (is.matrix(pemit)){
    emit.states <- colnames(pemit)
    observed <- lapply(hidden, function(x) {sample(emit.states,
                                            1, prob = pemit[x,])})
  }
  if (is.function(pemit)){
    observed <- lapply(hidden, pemit)
  }
  return(list("hid" = hidden, "obs" = unlist(observed)))
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

par(mar=c(4, 4, 2, 2)+0.5, tck=0.02, mex=0.7, lwd=1.5, pty="m")
plot(1:115, disc.hmm$obs, type="h", xlab = "", ylab="discrete HMM")
lines(1:115, disc.hmm$hid, type="h", col=2)
