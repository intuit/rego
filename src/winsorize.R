source(file.path(REGO_HOME, "/src/logger.R"))

winsorize <- function(x, beta = 0.025) 
{
  # Return l(x) = min(delta_+, max(delta_-, x)) where delta_- and delta_+ are
  # the beta and (1 - beta) quantiles of the data distribution {x_i}.  The 
  # value for beta reflects ones prior suspicions concerning the fraction of
  # such outliers.
  x.n  <- length(x)
  if ( beta < 0 || beta > 0.5 ) {
    error(logger, paste("winsorize: invalid argument beta =", beta))
  }
  if ( length(x) < 1 || length(which(is.na(x) == FALSE)) == 0) {
    error(logger, paste("winsorize: invalid argument x -- length =", length(x), "NAs =", length(which(is.na(x)))))
  }
  quant <- quantile(x, probs = c(beta, 1.0 - beta), na.rm = T)
  x.min2keep <- quant[1]
  x.max2keep <- quant[2]
  x.copy <- x
  x.copy[which(x < x.min2keep)] <- x.min2keep
  x.copy[which(x > x.max2keep)] <- x.max2keep
  return(list(x = x.copy, min2keep = x.min2keep, max2keep = x.max2keep))
}
