viewdigit <- function(digit) {
  # Reshape to 28 by 28 image
  a1 <- matrix(as.double(digit), nrow = 28, ncol = 28)
  
  # Rescale
  a1 <- (a1 - min(a1)) * (256 / max(a1))
  digitImage <- 256 - a1
  
  # Create the image
  image(t(apply(digitImage, 2, rev)), col = gray(0:255 / 255), axes = FALSE)
  box()
}