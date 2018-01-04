# Question 5: Doownload chromosome III of the Saccharomyces
# cerevisiae (yeast) genome sequence from Ensembl or NCBI (Uniprot).
# Calculate its GC content (the fraction of bases which are G or C)
# in 100 bp windows. Choose an appropriate binning scheme to
# represent the GC content of each window such that the resulting
# sequence corresponds to an emission sequence for the model in q3.
# Calculate the log likelihood of this GC sequence under the model 
# in q3.

setwd("~/Desktop/MPhilComputationalBiology/GSA/")
chromosome <- readLines('BK006937.fasta.txt')
chromosome <- chromosome[-grep(">", chromosome)]
whole.sequence <- rep("", length(chromosome)*60)

binning <- function(x){
  if (x <= 0.2){
    return(1)
  }
  else if (x <= 0.4){
    return(2)
  }
  else if (x <= 0.6){
    return(3)
  }
  else if (x <= 0.8){
    return(4)
  }
  else{
    return(5)
  }
}

for (i in 1:length(chromosome)){
  l <- unlist(strsplit(chromosome[i],split = c()))
  whole.sequence[(1+(i-1)*60):(i*60)] <- l
}

bin <- rep(0, as.integer(length(whole.sequence)/100))

for (i in 1:(length(bin)-1)){
  s <- whole.sequence[(1+100*(i-1)):(100*i)]
  percentage <- (length(which(s=="G"))+length(which(s=="C")))/100
  bin[i] <- binning(percentage)
}
s <- whole.sequence[(1+100*(length(bin)-1)):length(bin)]
bin[length(bin)] <- binning((length(which(s=="G"))+
                               length(which(s=="C")))/length(s))

log.likelihood <- sum(log(forward.algorithm(hid.trans, emit.probs,
                                    init.dist, bin)$C))
# log.likelihood = -4915.303
