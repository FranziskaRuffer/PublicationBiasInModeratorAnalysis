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
#' exp_val()
#'
#' @description Function to obtain the expected value given publication bias for one true effect size g (Hedges' g)
#' @param PB amount of publication bias between 0 (extreme publication bias) and 1 (no publication bias)
#' @param Zcv critical Z value defined by the normal quantile function. For instance Zcv = qnorm(.975).
#' @param g True effect size as standardized mean difference (we used Hedges' g)
#' @param N Primary study total sample size
#' @param I2 heterogeneity estimate in the meta-analysis (not necessary when tau2 is specified)
#' @param tau2 heterogeneity estimate in the meta-analysis (not necessary when I2 is specified)
#' @return This function returns a single numeric value of the expected effect size given publication bias for a given effect size
#' @export
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


#' Function to obtain the expected value given publication bias for a true effect size g (Hedges' g)
#' with a given primary study sample size in a meta-analysis. Compared to the exp_val() function, the
#' true effect sizes and sample sizes of other studies in the meta-analysis are taken into account when
#' calculating the residual $$\tau^2$$ for a given residual $$I^2$$.
#' exp_val_MA
#'
#' @description Function to obtain the expected value given publication bias for a true effect size g (Hedges' g)
#' with a given primary study sample size in a meta-analysis. Compared to the exp_val() function, the
#' true effect sizes and sample sizes of other studies in the meta-analysis are taken into account when
#' calculating the residual $$\tau^2$$ for a given residual $$I^2$$.
#' @param PB amount of publication bias between 0 (extreme publication bias) and 1 (no publication bias)
#' @param Zcv critical Z value defined by the normal quantile function. For instance Zcv = qnorm(.975).
#' @param N Primary study total sample size of a given study
#' @param Nvec Vector of all primary study total sample sizes in the meta-analysis
#' @param I2 heterogeneity estimate in the meta-analysis (not necessary when tau2 is specified)
#' @param g True effect size as standardized mean difference (we used Hedges' g)
#' @param N Primary study total sample size
#' @param I2 residual heterogeneity estimate in the meta-analysis 
#' @param x1 moderator value of the given study in the meta-analysis
#' @param x1vec vector with moderator values of all studies in the meta-analysis
#' @param beta0 true intercept parameter of the meta-analysis
#' @param beta1 true slope parameter (moderator effect) of the meta-analysis
#' @return This function returns a single numeric value of the expected effect size given publication bias for a given effect size in a meta-analysis.
#' @export
exp_val_continuous <- function(PB, Zcv, N, Nvec, I2, x1, x1vec, beta0, beta1){
  
  #true effect size
  g <- beta0 + beta1*x1
  gvec <- beta0 + beta1*x1vec
  
  #balanced sample size in compared groups (N/2)
  nT <- ceiling(N/2)
  nC <- floor(N/2)
  nTvec <- ceiling(Nvec/2)
  nCvec <- floor(Nvec/2) 
  
  if(sum(Nvec >200)>=1){
    cmvec <- 1-(3/(4*(Nvec-2)-1))
    cm <- 1-(3/(4*(N-2)-1))
  }else{
    cmvec <- gamma((Nvec-2)/2) /
      (sqrt((Nvec-2)/2) * gamma((Nvec-3)/2))
    cm <- gamma((N-2)/2) /
      (sqrt((N-2)/2) * gamma((N-3)/2))
  }
  #sampling variance of single study 
  vg <- ((cm)^2 * (N-2) * 
           (1 +  nT*nC/(nT + nC) * g^2 )
  ) / (
    (N-4) * (nT*nC / (nT + nC))  ) - g^2
  
  #vector of sampling variances for all studies
  vgvec <- ((cmvec)^2 * (Nvec-2) * 
              (1 +  nTvec*nCvec/(nTvec + nCvec) * gvec^2 )
  ) / (
    (Nvec-4) * (nTvec*nCvec / (nTvec + nCvec))  ) - gvec^2
  
  #critical value
  ycv <- Zcv*sqrt(vg)
  
  ### Compute typical within-study variance of these two studies. Equation (9) in
  w <- c(1/vgvec)
  k <- length(w)
  # Takkouche et al. (1999, 2013)
  typ_v_T <- k * (1/sum(w))  #equal to vg if k = 1, or if all N are the same size
  
  #heterogeneity 
  tau2 <- (I2 * typ_v_T) / (1-I2)
  
  #probability of obtaining a non-significant result
  p_ns <- pnorm((ycv-g)/sqrt(vg+tau2), lower.tail = TRUE) 
  
  #expectation significant studies
  E_sig <- g + sqrt(vg+tau2)* dnorm((ycv-g)/sqrt(vg+tau2)) / pnorm((ycv-g)/sqrt(vg+tau2), lower.tail = FALSE) 
  
  #expectation nonsignificant studies
  E_nsig <- g - sqrt(vg+tau2)* dnorm((ycv-g)/sqrt(vg+tau2)) / pnorm((ycv-g)/sqrt(vg+tau2), lower.tail = TRUE) 
  
  #expectation given publication bias
  E <- (PB * p_ns * E_nsig + (1-p_ns) * E_sig) / 
    (PB * p_ns + (1-p_ns))
  
  return(data.frame(N, vg, typ_v_T, tau2, I2, PB, g, x1, beta0, beta1, ycv, E_sig, E_nsig, E))
}


#' weighted-least squares estimate of betas using E[g|PB] instead of observed effect sizes
#'
#' @noRd
betas_PB <- function(data){
  W <- diag(1/(data$vg+data$tau2))
  X <- matrix(c(rep(1,nrow(data)),data$x1), byrow = F, ncol =2)
  betas_PB <- solve(t(X)%*%W%*%X) %*% t(X)%*%W%*%data$E
  return(betas_PB)
}



#' Calculates difference in E1-E2 (needed for solve for g2)
#'
#' @noRd
diffE <- function(N1, N2, g1, g2, Zcv, PB1, PB2, I2){
  E1 <- exp_val(PB=PB1, Zcv=Zcv, g=g1, N=N1, I2=I2)$E
  E2 <- exp_val(PB=PB2, Zcv=Zcv, g=g2, N=N2, I2=I2)$E
  diff <- E1-E2
  return(diff)
}


#' Solves for the true effect size g2 in group2, s.t. E1=E2 (hidden moderator effect)
#'
#' @noRd
sol.g2 <- function(N1, N2, g1,  Zcv, PB1, PB2, I2=0){
  f_sol <- uniroot(diffE , N1 = N1, N2 = N2, g1 = g1, Zcv = Zcv,
                   PB1=PB1, PB2 = PB2, I2=I2, interval = c(0.00001, g1+3))
  return(as.numeric(f_sol$root))
  
}


#' function to flatten nested lists
#'
#' @noRd
flattenlist     <- function(x){  
  more_lists <- sapply(x, function(first_x) class(first_x)[1]=="list")
  out        <- c(x[!more_lists], unlist(x[more_lists], recursive=FALSE))
  if(sum(more_lists)){ 
    Recall(out)
  }else{
    return(out)
  }
}

