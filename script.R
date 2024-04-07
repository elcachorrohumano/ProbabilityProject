library(mvtnorm)
# Definir los parámetros de la distribución normal bivariada
mu <- c(0, 0)
sigma <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)

# Generar una malla de valores para graficar la función de densidad
x <- seq(-3, 3, length.out = 50)
y <- seq(-3, 3, length.out = 50)
X <- expand.grid(x, y)

# Calcular la función de densidad
f <- dmvnorm(X, mean = mu, sigma = sigma)

# Graficar la función de densidad
persp(x, y, matrix(f, nrow = 50), xlab = "x", ylab = "y", zlab = "Densidad")


# Definir los parámetros de la distribución ji-cuadrada
df <- c(1, 5, 10)

# Generar una secuencia de valores para graficar la función de densidad
x <- seq(0, 20, length.out = 100)

# Calcular la función de densidad para cada valor de df
f <- lapply(df, function(d) dchisq(x, df = d))

# Graficar las funciones de densidad
plot(x, f[[1]], type = "l", xlab = "x", ylab = "Densidad", main = "Distribución Ji-cuadrada")
lines(x, f[[2]], col = "red")
lines(x, f[[3]], col = "blue")
legend("topright", legend = paste("df =", df), col = c("black", "red", "blue"), lty = 1)




