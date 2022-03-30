pval = -1
pvalVector = NULL
DEBUG = TRUE
numOfSignfPval = 0
signifLevel = 0.05
numOfPvalToGenerate = 100

#generate random pvalue with a Gaussian (truncated to [0,1]) 
#with parameters mean and sd 
generateSimulatedPvalue <- function(mean, sd) {
  pval = rnorm(1,mean,sd)
  while(pval>1 | pval<0){
    pval = rnorm(1, mean, sd)
  }
  if(DEBUG){
    message(cat("Randomly generated pvalue is: ", pval))
  }
}

for(i in 1:numOfPvalToGenerate){
  generateSimulatedPvalue(0.15,0.25)
  pvalVector = c(pvalVector, pval) 
}

for(pval in pvalVector){
  if(pval <= signifLevel){
    numOfSignfPval = numOfSignfPval+1
  }
}
message(cat("Number of significative pvalues before correction: ", numOfSignfPval))

pvalVector= p.adjust(pvalVector, method = "bonferroni", n = length(pvalVector))
numOfSignfPval=0

for(pval in pvalVector){
  if(pval <= signifLevel){
    numOfSignfPval = numOfSignfPval+1
  }
}
message(cat("Number of significative pvalues before correction: ", numOfSignfPval))
