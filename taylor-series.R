# Define the function and its derivatives
f <- function(x, y) {
  -2 * x - y
}

f1 <- function(x, y) {
  -2 - f(x, y)
}

f2 <- function(x, y) {
  -f1(x, y)
}

f3 <- function(x, y) {
  -f2(x, y)
}

# Taylor series function
taylor_series <- function(x0, y0, h, steps) {
  x <- x0
  y <- y0
  
  for (i in 1:steps) {
    y <- y + h * f(x, y) + (h^2 / 2) * f1(x, y) + (h^3 / 6) * f2(x, y) + (h^4 / 24) * f3(x, y)
    x <- x + h
  }
  
  return(y)
}

# Initial values
x0 <- 0
y0 <- -1

# Step length
h <- 0.1

# Calculate y at x = 0.1
y_at_0_1 <- taylor_series(x0, y0, h, 1)

# Use y at x = 0.1 as the initial value for calculating y at x = 0.2
y_at_0_2 <- taylor_series(0.1, y_at_0_1, h, 1)

# Print the results
cat("y at x = 0.1:", y_at_0_1, "\n")
cat("y at x = 0.2:", y_at_0_2, "\n")
