# Define the differential equation
dydx <- function(x, y) {
  -2 * x - y
}

# Runge-Kutta method for the first step
runge_kutta <- function(x0, y0, h) {
  k1 <- h * dydx(x0, y0)
  k2 <- h * dydx(x0 + 0.5 * h, y0 + 0.5 * k1)
  k3 <- h * dydx(x0 + 0.5 * h, y0 + 0.5 * k2)
  k4 <- h * dydx(x0 + h, y0 + k3)
  
  y0 + (k1 + 2*k2 + 2*k3 + k4) / 6
}

# Predictor-Corrector function
predictor_corrector <- function(x, y, y_prev, h) {
  # Predictor (Adams-Bashforth 2-step method)
  y_pred <- y + h * (1.5 * dydx(x, y) - 0.5 * dydx(x - h, y_prev))
  
  # Corrector (Adams-Moulton 2-step method)
  y_corr <- y + h * (0.5 * dydx(x + h, y_pred) + 0.5 * dydx(x, y))
  
  return(y_corr)
}

# Initial values
x0 <- 0
y0 <- -1

# Step length
h <- 0.1

# Initialize matrix to store results
results <- matrix(nrow = 4, ncol = 2)
colnames(results) <- c("x", "y")
rownames(results) <- c("Step 1", "Step 2", "Step 3", "Step 4")

# Calculate y at x = 0.1 using Runge-Kutta method
y_at_0_1 <- runge_kutta(x0, y0, h)

# Use Predictor-Corrector for the next steps
y_at_0_2 <- predictor_corrector(0.1, y_at_0_1, y0, h)
y_at_0_3 <- predictor_corrector(0.2, y_at_0_2, y_at_0_1, h)
y_at_0_4 <- predictor_corrector(0.3, y_at_0_3, y_at_0_2, h)

# Populate the matrix with results
results[1, ] <- c(0.1, y_at_0_1)
results[2, ] <- c(0.2, y_at_0_2)
results[3, ] <- c(0.3, y_at_0_3)
results[4, ] <- c(0.4, y_at_0_4)

# Print the matrix
print(results)
