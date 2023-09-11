# Perform Singular Value Decomposition
svd_result <- svd(t(mnistdata$train3), nu = 5, nv = 0)

# Extract the U matrix
U3 <- svd_result$u

# Get the size of U3
size_U3 <- dim(U3)



# Set the basis length
basis_len <- 5

# Create a 3D array to store the singular vectors for each digit (0 to 9)
Us <- array(0, dim = c(28*28, basis_len, 10))

# Loop through each digit from 0 to 9
for (k in 1:10) {
  # Get the name of the variable for the current digit
  s <- paste0('train', k-1)

  # Get the object from mnistdata
  obj <- mnistdata[[s]]

  A <- as.matrix(obj)
  
  # Perform Singular Value Decomposition
  svd_result <- svd(t(A), nu = basis_len, nv = 0)
  
  # Extract the U matrix
  U <- svd_result$u
  
  # Store U in the Us array
  Us[, , k] <- U
}


# Step 2b
pca_count <- function(A, T, k) {
  labels <- numeric(k)
  
  for (k in 1:n) {
    d <- 0
    for (i in 1:10) {
      residual <- A - T
      
      dist_pca[i] <- sqrt(sum(residual^2))
      
      if (dist_pca[i] < dist_pca[d+1]) {
        d <- i - 1
      }
    }
    
    labels[i] <- d
  }
  
  return(labels)
}





