# Assume 'z', 'test7', and 'T' are defined in R as appropriate vectors or matrices

z <- as.numeric(mnistdata$test7[55, ])
dist <- numeric(10)

for (k in 1:10) {
  dist[k] <- sqrt(sum((z - T[k, ])^2))
}


# Step b
classifier <- function(A, T) {
  n <- nrow(A)
  labels <- numeric(n)
  
  for (i in 1:n) {
    d <- 0
    for (k in 1:10) {
      dist[k] <- sqrt(sum((A[i, ] - T[k, ])^2))
      if (dist[k] < dist[d+1]) {
        d <- k - 1
      }
    }
    
    labels[i] <- d
  }
  
  return(labels)
}