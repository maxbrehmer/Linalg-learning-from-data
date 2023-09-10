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
  
  # Check if the variable exists in the mnistdata list or data frame
  if (s %in% names(mnistdata)) {
    # Get the object from mnistdata
    obj <- mnistdata[[s]]
    
    # Check the structure and type of obj
    if (is.numeric(obj)) {
      # If obj is already numeric, no need to convert
      A <- as.matrix(obj)
    } else {
      # If obj is not numeric, you may need to perform appropriate conversion
      # Example: A <- as.matrix(as.double(obj))
      cat("Variable", s, "is not numeric and cannot be converted to double.\n")
    }
    
    # Perform further operations on A as needed
  } else {
    cat("Variable", s, "does not exist in mnistdata.\n")
  }
  
  # Evaluate the variable and convert it to a double matrix
  #A <- as.matrix(double(get(s)))
  
  # Perform Singular Value Decomposition
  svd_result <- svd(t(A), nu = basis_len, nv = 0)
  
  # Extract the U matrix
  U <- svd_result$u
  
  # Store U in the Us array
  Us[, , k] <- U
}







