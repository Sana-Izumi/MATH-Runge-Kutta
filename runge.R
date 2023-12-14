
f = function(x,y){-2*x-y}
h = 0.1; x0 = 0; y0 = -1
k1 = h*f(x0,y0); k2 = h*f(x0 + 0.5*h, y0 + 0.5*k1)
k3 = h*f(x0 + 0.5*h, y0 + 0.5*k2); k4 = h*f(x0 + h, y0 + k3)
y1 = y0 + (1/6)*(k1 + 2*k2 + 2*k3 + k4)
data.frame(k1,k2,k3,k4,y1)

f = function(x,y){-2*x-y}
h = 0.1; x1 = 0.1; y1 = -0.9145125
k1 = h*f(x1, y1); k2 = h*f(x1 + 0.5*h, y1 + 0.5*k1)
k3 = h*f(x1 + 0.5*h, y1 + 0.5*k2); k4 = h*f(x1 + h, y1 + k3)
y2 = y1 + (1/6)*(k1 + 2*k2 + 2*k3 + k4)
data.frame(k1,k2,k3,k4,y2)



runge_kutta = function(f,x0,y0,xend,h){
  x_vals = seq(x0, xend, by = h)
  y_vals = numeric(length(x_vals))
  y_vals[1] = y0
  
  for (i in 1:(length(x_vals) - 1)) {
    k1 = h*f(x_vals[i], y_vals[i])
    k2 = h*f(x_vals[i] + 0.5*h, y_vals[i] + 0.5*k1)
    k3 = h*f(x_vals[i] + 0.5*h, y_vals[i] + 0.5*k2)
    k4 = h*f(x_vals[i] + h, y_vals[i] + k3)
    y_vals[i+1] = y_vals[i] + (1/6)*(k1 + 2*k2 + 2*k3 + k4)
  }
  return(data.frame(x = x_vals, y = y_vals))
}

# Parameters
x0 = 0
y0 = -1
xend = 0.2
h = 0.1

# Runge-Kutta calculation
results = runge_kutta(f, x0, y0, xend, h)
plot(results$x, results$y, type = 'o', col = 'blue', 
     main = 'Runge-Kutta Method', xlab = 'x', ylab = 'y')


