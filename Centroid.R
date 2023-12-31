# Assume 'z', 'test7', and 'T' are defined in R as appropriate vectors or matrices

z <- as.numeric(mnistdata$test7[55, ])
dist <- numeric(10)

for (k in 1:10) {
  dist[k] <- sqrt(sum((z - T[k, ])^2))
}


# Step B
classifier <- function(A, T, n) {
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


# Step C
class_count <- function (class, digit) {
  correct <- 0
  for (i in 1:length(class)) {
    if (class[i] == digit) {
      correct <- correct+1
    }
  }
  score <- correct/length(class)
  
  return(score)
}