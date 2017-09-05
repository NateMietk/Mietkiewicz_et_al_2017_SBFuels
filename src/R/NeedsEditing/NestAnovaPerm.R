nest.anova.perm <- function(Y, Xmain, Xnes, nperm=0)
  #
  ##########################################################################
#
# Nested anova with a main factor and a nested factor.
# Balanced design, i.e., equal replication in each level of the nested factor.
#
# Arguments
#
#   Y     : Vector containing the response variable.
#
#   Xmain : Vector or data.frame containing the values of the main factor for
#           each observation.
#
#   Xnes  : Vector or data.frame containing the values of the nested factor for
#           each observation.
#
# Note    : Xmain and Xres are transformed to factors inside the function.
#
#                             Daniel Borcard and Pierre Legendre, September 2007
#
##########################################################################
#
## Example from Sokal & Rohlf (1981, 1995), Box 10.1
#
# Var   = c(58.5,59.5,77.8,80.9,84.0,83.6,70.1,68.3,69.8,69.8,56.0,54.5, 50.7,49.3,63.8,65.8,56.6,57.5,77.8,79.2,69.9,69.2,62.1,64.5)
# Xmain = c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3)
# Xnes  = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12)
#
# nest.out = nest.anova.perm(Var, Xmain, Xnes, nperm=999)
#
##########################################################################
{
  a.fac = as.factor(Xmain)
  b.fac = as.factor(Xnes)
  
  dat = as.data.frame(cbind(Y, a.fac, b.fac))
  
  # Check model balance (equal replication in each level of the nested factor)
  balance = var(as.vector(table(dat[,2])))
  if(balance > 0)
    stop("Design unbalanced. This function can only handle balanced designs.")
  
  # The nesting is done by %in% in 'lm'
  anova.res = anova(lm(Y ~ a.fac + b.fac %in% a.fac))
  if(nrow(anova.res)<3) cat("Problem with the data, probably no replicates",'\n',"Permutation test on the main factor only",'\n')
  
  # Recompute parametric F-statistic and associated probability for the main factor
  F.main = anova.res[1,3] / anova.res[2,3]
  P.main = pf(F.main, anova.res[1,1], anova.res[2,1], lower.tail=FALSE)
  anova.res[1,4] = F.main
  anova.res[1,5] = P.main
  
  # Permutation test (if requested)
  
  if(nperm>0){
    
    n = nrow(dat)
    vec = seq(1:n)
    nblock = length(levels(a.fac))
    nobs.block = nrow(dat)/nblock
    k = nrow(anova.res) - 1
    Pperm = c(rep(0,k), NA)
    
    # Permutation test of the nested factor. The data are permuted within the levels
    # of the main factor. Done only if replicates are available in the nested factor.
    GEn = NA
    if(nrow(anova.res)>=3){
      
      # First, make sure that the data are ordered on the main factor.
      data2 = dat[order(dat[,2]),]
      
      # Restricted permutation
      GEn = 1
      for(i in 1:nperm){
        vecperm = restrictedPerm(nobs.block, nblock, n, restPerm=TRUE, vec)
        Y.perm = data2[order(vecperm),1]
        anova.perm = anova(lm(Y.perm ~ a.fac + b.fac %in% a.fac))
        if(anova.perm[2,4] >= anova.res[2,4]) GEn = GEn + 1
      }
      Pperm[2] = GEn/(nperm+1)
    }
    
    # Permutation test of the main factor
    GEm = 1
    for(j in 1:nperm){
      Y.perm = sample(Y, n)
      anova.perm = anova(lm(Y.perm ~ a.fac + b.fac %in% a.fac))
      F.main.perm = anova.perm[1,3] / anova.perm[2,3]
      if(F.main.perm >= F.main) GEm = GEm + 1
    }
    Pperm[1] = GEm/(nperm+1)
    
    anova.res  = data.frame(anova.res, Pperm)
    colnames(anova.res) = c("Df", "Sum Sq", "Mean Sq", "F value", "Prob(param)", "Prob(perm)")
    note = "Nested anova, parametric and permutation tests"
    
  }
  
  # return(anova.res)
  
  if(nperm <= 0) {
    note = "Nested anova, parametric tests"
    return(list(anova.type=note,anova.table=anova.res))
  } else {
    return(list(anova.type=note, nperm=nperm, anova.table=anova.res))
  }
  
}
