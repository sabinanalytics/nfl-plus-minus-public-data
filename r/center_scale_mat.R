# Load necessary library
library(tidyverse)

# Step 1 and 2: Center and scale X, and store the means and standard deviations
center_and_scale <- function(X) {
  # Calculate means and standard deviations
  means <- colMeans(X, na.rm = TRUE)
  sds <- apply(X, 2, sd, na.rm = TRUE)
  
  # Center and scale
  X_scaled <- scale(X, center = means, scale = sds)
  
  # Return a list containing the scaled matrix, means, and standard deviations
  list(X_scaled = X_scaled, means = means, sds = sds)
}

# # Apply the function to X
# result <- center_and_scale(X)
# X_scaled <- result$X_scaled
# means <- result$means
# sds <- result$sds

# Step 4: Function to transform back to the original scale
transform_back <- function(X_scaled, means, sds) {
  # Multiply by the standard deviation and add the mean
  sweep(sweep(X_scaled, 2, sds, "*"), 2, means, "+")
}
# 
# # Example usage: Transform back
# X_original <- transform_back(X_scaled, means, sds)
# 
# # Verify that X_original is the same as X (allowing for numerical precision differences)
# all.equal(X, X_original)