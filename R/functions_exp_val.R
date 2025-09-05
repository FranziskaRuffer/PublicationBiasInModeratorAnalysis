#' Functions to obtain the expected effect sizes given publication bias
#' Parameter:   
#' * PB = Publication Bias... proportion of non-significant effect sizes published
#' * g = true effect size as standardized mean difference in form of Hedges' g
#' * N = total sample size (N = nT + nC)
#' * Zcv = critical Z value (used to calculate the critical value ycv)
#' * vg = sampling variance Hedges' g
#' * tau2 = (residual) between-study variance 
#' * I2 = porportion of within- to (residual) between-study variance
#' * E_sig = Expected effect size upper truncated normal distribution (significant effect sizes)
#' * E_nsig = Expected effect size lower truncated normal distribution (nonsignificant effect sizes)
#' * E = expected effect size given publication bias 
#' * p_ns = probability of a non-significant effect size


#' Function to obtain the expected value given publication bias for one true effect size g (Hedges' g)
exp_val <- function(PB, Zcv, g, N, I2=NA, tau2=NA){
  
  #balanced sample size in compared groups (N/2)
  nT <- nC <- N/2
  
  #exact sampling variance Hedges' g ( Viechtbauer (2007, Eq. 22))
  if(N >120){
    cm <- 1-(3/(4*(N-2)-1))
  }else{
    cm <- gamma((N-2)/2) /
      (sqrt((N-2)/2) * gamma((N-3)/2))
  }
  
  vg <- ((cm)^2 * (N-2) * 
           (1 +  nT*nC/(nT + nC) * g^2 )
  ) / (
    (N-4) * (nT*nC / (nT + nC))  ) - g^2
  
  #critical value
  ycv <- Zcv*sqrt(vg)
  
  #heterogeneity 
  if(is.na(tau2)==TRUE){
    tau2 <- (I2 * vg) / (1-I2)
  } else { I2 <- tau2/(vg + tau2)}
  
  #probability of obtaining a non-significant result
  p_ns <- pnorm((ycv-g)/sqrt(vg+tau2), lower.tail = TRUE) 
  
  #expectation significant studies
  E_sig <- g + sqrt(vg+tau2)* dnorm((ycv-g)/sqrt(vg+tau2)) / pnorm((ycv-g)/sqrt(vg+tau2), lower.tail = FALSE) 
  
  #expectation nonsignificant studies
  E_nsig <- g - sqrt(vg+tau2)* dnorm((ycv-g)/sqrt(vg+tau2)) / pnorm((ycv-g)/sqrt(vg+tau2), lower.tail = TRUE) 
  
  #expectation given publication bias
  E <- (PB * p_ns * E_nsig + (1-p_ns) * E_sig) / 
    (PB * p_ns + (1-p_ns))
  
  return(data.frame(N, vg, tau2, I2, PB, g, ycv, E_sig, E_nsig, E))
}


