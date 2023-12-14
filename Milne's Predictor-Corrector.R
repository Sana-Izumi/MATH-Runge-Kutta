# Define the differential equation
dydx <- function(x, y) {
  x * y + 2 * y
}

# Runge-Kutta method for initial steps
runge_kutta <- function(f, x0, y0, h, steps) {
  x <- x0
  y <- y0
  for (i in 1:steps) {
    k1 <- h * f(x, y)
    k2 <- h * f(x + 0.5 * h, y + 0.5 * k1)
    k3 <- h * f(x + 0.5 * h, y + 0.5 * k2)
    k4 <- h * f(x + h, y + k3)
    y <- y + (k1 + 2 * k2 + 2 * k3 + k4) / 6
    x <- x + h
  }
  return(y)
}

# Milne's Predictor-Corrector method
milne_predictor_corrector <- function(f, x, y, h) {
  # Predictor step
  y_pred <- y[1] + 4 * h / 3 * (2 * f(x[2], y[2]) - f(x[3], y[3]) + 2 * f(x[4], y[4]))
  
  # Corrector step
  y_corr <- y[3] + h / 3 * (f(x[4] + h, y_pred) + 4 * f(x[4], y[4]) + f(x[3], y[3]))
  
  return(y_corr)
}

# Initial values
x0 <- 0
y0 <- 1
h <- 0.05

# Calculate initial values using Runge-Kutta method
y_values <- numeric(4)
y_values[1] <- y0
for (i in 2:4) {
  y_values[i] <- runge_kutta(dydx, x0, y_values[i-1], h, 1)
  x0 <- x0 + h
}

# Apply Milne's Predictor-Corrector method
y_at_0_20 <- milne_predictor_corrector(dydx, c(0, 0.05, 0.10, 0.15), y_values, h)

# Print the result
cat("y at x = 0.20:", y_at_0_20, "\n")
