linear_interpolation <- function(x, y, xout) {
  yout <- numeric(length(xout))
  for (j in 1:length(xout)) {
    found <- FALSE
    for (i in 1:(length(x) - 1)) {
      if (xout[j] >= x[i] && xout[j] <= x[i + 1]) {
        # Apply the linear interpolation formula
        yout[j] <- y[i] + (xout[j] - x[i]) * (y[i + 1] - y[i]) / (x[i + 1] - x[i])
        found <- TRUE
        break
      }
    }
    # If xout[j] is out of bounds of the provided x values, we can't interpolate
    if (!found) {
      yout[j] <- NA 
    }
  }
  return(yout)
}


lagrange_interpolation <- function(x, y, xout) {
  n <- length(x)
  
  # Function to compute l_k(x)
  compute_lk <- function(k, x_val) {
    prod_val <- 1
    for (i in 1:n) {
      if (i != k) {
        prod_val <- prod_val * (x_val - x[i]) / (x[k] - x[i])
      }
    }
    return(prod_val)
  }
  
  yout <- numeric(length(xout))
  for (j in 1:length(xout)) {
    sum_val <- 0
    for (k in 1:n) {
      sum_val <- sum_val + y[k] * compute_lk(k, xout[j])
    }
    yout[j] <- sum_val
  }
  
  return(yout)
}