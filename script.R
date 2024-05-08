library(mvtnorm)
library(ggplot2)
library(dplyr)

######################### Normal Bivariada ####################################

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


############################### Ji-Cuadrada ####################################

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
legend("topright", legend = paste("Grados de Libertad =", df), col = c("black", "red", "blue"), lty = 1)

############################### T-Student ####################################

# Definir los grados de libertad para la distribución t
df <- c(1, 5, 10, 30)

# Generar una secuencia de valores para graficar la función de densidad
x <- seq(-4, 4, length.out = 1000)

# Calcular la función de densidad para cada valor de df
f <- lapply(df, function(d) dt(x, df = d))

# Graficar las funciones de densidad en el mismo plot
plot(x, f[[1]], type = "l", log = "y", xlab = "x", ylab = "Densidad", main = "Distribución t de Student", col = "black")
lines(x, f[[2]], col = "red")
lines(x, f[[3]], col = "blue")
lines(x, f[[4]], col = "green")

# Agregar leyenda
legend("topright", legend = paste("gl =", df), col = c("black", "red", "blue", "green"), lty = 1)

################################# F ###########################################

# Definir los grados de libertad para la distribución F
df1 <- c(5, 10, 30)
df2 <- c(15, 20, 40)

# Generar una secuencia de valores para graficar la función de densidad
x <- seq(0, 5, length.out = 1000)

# Calcular la función de densidad para cada par de valores de df
f <- sapply(1:length(df1), function(i) df(x, df1[i], df2[i]))

# Graficar las funciones de densidad en el mismo plot
plot(x, f[,1], type = "l", log = "y", xlab = "x", ylab = "Densidad", main = "Distribución F", col = "black")
lines(x, f[,2], col = "red")
lines(x, f[,3], col = "blue")

# Agregar leyenda
legend("topright", legend = paste("gl =", df1, "/", df2), col = c("black", "red", "blue"), lty = 1)

############################# Cambio de variable ###############################

#### Distribución F

# Definir los grados de libertad para las variables Chi-cuadrado
df1 <- 5
df2 <- 10

# Generar muestras aleatorias de las variables Chi-cuadrado
n_samples <- 10000
samples1 <- rchisq(n_samples, df = df1)
samples2 <- rchisq(n_samples, df = df2)

# Calcular la distribución F (Fisher-Snedecor)
f_distribution <- (samples1 / samples2) * (df2 / df1)

# Crear el primer plot: histograma de las variables Chi-cuadrado
hist_plot <- ggplot() +
  geom_histogram(aes(x = samples1, fill = "Ji_Cuadrada (5 gl)"), bins = 30, alpha = 0.8) +
  geom_histogram(aes(x = samples2, fill = "Ji_Cuadrada (10 gl)"), bins = 30, alpha = 0.8) +
  scale_fill_manual(values = c("Ji_Cuadrada (5 gl)" = "#377EB8", "Ji_Cuadrada (10 gl)" = "#E41A1C")) +
  labs(title = "Distribución Chi-cuadrado",
       x = "Valor",
       y = "Frecuencia") +
  theme_minimal()

# Crear el segundo plot: distribución F (Fisher-Snedecor)
fdist_plot <- ggplot() +
  geom_histogram(aes(x = f_distribution, fill = "Distribución F"), bins = 30, alpha = 0.8) +
  scale_fill_manual(values = c("Distribución F" = "#4DAF4A")) +
  labs(title = "Distribución F (Fisher-Snedecor)",
       x = "Valor",
       y = "Frecuencia") +
  theme_minimal()

# Mostrar los plots
print(hist_plot)
print(fdist_plot)

#### Distribución T de Student

# Definir los grados de libertad para la variable Chi-cuadrado
df <- 5
# Generar muestras aleatorias de las variables
n_samples <- 10000
samples_norm <- rnorm(n_samples)
samples_ji <- rchisq(n_samples, df = df)

# Calcular la distribución F (Fisher-Snedecor)
t_student <- (samples_norm / sqrt(samples_ji / df))

# Crear el primer plot: histograma de las variables Chi-cuadrado y normal estándar
hist_plot <- ggplot() +
  geom_histogram(aes(x = samples_ji, fill = "Ji_Cuadrada (5 gl)"), bins = 30, alpha = 0.8) +
  geom_histogram(aes(x = samples_norm, fill = "Normal Estandar"), bins = 30, alpha = 0.8) +
  scale_fill_manual(values = c("Ji_Cuadrada (5 gl)" = "#377EB8", "Normal Estandar" = "#E41A1C")) +
  labs(title = "Ji-Cuadrada y Normal Estandar",
       x = "Valor",
       y = "Frecuencia") +
  theme_minimal()

# Crear el segundo plot: distribución F (Fisher-Snedecor)
fdist_plot <- ggplot() +
  geom_histogram(aes(x = t_student, fill = "Distribución T"), bins = 30, alpha = 0.8) +
  scale_fill_manual(values = c("Distribución T" = "#4DAF4A")) +
  labs(title = "Distribución T",
       x = "Valor",
       y = "Frecuencia") +
  theme_minimal()

# Mostrar los plots
print(hist_plot)
print(fdist_plot)

################################### CONVERGENCIA ##############################

#### Uniforme

# Definir el tamaño de la muestra
sample_size <- 1000

# Número de valores de n para probar
n_values <- c(10, 50, 100, 500, 1000)

# Generar muestras de Xn para diferentes valores de n
samples <- lapply(n_values, function(n) runif(sample_size, 1/n, 1))

# Generar muestra de X (distribución uniforme U(0,1))
X <- runif(sample_size, 0, 1)

# Crear un data frame para almacenar los datos
df <- data.frame(X = X)

# Agregar las muestras de Xn al data frame
for (i in 1:length(samples)) {
  df[paste0("X", n_values[i])] <- samples[[i]]
}

# Función para graficar las distribuciones
plot_distributions <- function(df, n_values) {
  # Crear una lista de gráficos usando ggplot2
  plots <- lapply(n_values, function(n) {
    ggplot(df, aes(x = get(paste0("X", n)))) +
      geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5, color = "black", fill = "blue") +
      stat_function(fun = dunif, args = list(min = 1/n, max = 1), color = "red") +
      labs(x = "X", y = "Density") +  # Cambio de título del eje x y y
      theme_minimal() +
      scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
      scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, by = 0.5)) +
      theme(plot.title = element_blank(),  # Quitar título de la gráfica
            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))  # Ajustar margen del título del eje y
  })
  return(plots)
}

# Crear los gráficos para diferentes valores de n
plots <- plot_distributions(df, n_values)

# Mostrar los gráficos en una sola fila
multiplot <- function(..., plotlist = NULL, cols) {
  require(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  cols = cols
  rows = ceiling(numPlots/cols)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(rows, cols)))
  for (i in 1:numPlots) {
    matchidx <- as.data.frame(which(rownames(viewport(layout = grid.layout(rows, cols))) == paste("p", i, sep = "")))
    print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, layout.pos.col = matchidx$col))
  }
}

# Mostrar los gráficos
multiplot(plotlist = plots, cols = 3)

### Convergencia Binomial

# Definir el tamaño de la muestra
sample_size <- 1000

# Número de valores de n para probar
n_values <- c(5, 10, 20, 50, 100)

# Valor de lambda
lambda <- 5

# Generar muestras de Xn para diferentes valores de n
samples <- lapply(n_values, function(n) rbinom(sample_size, n, lambda/n))

# Generar muestra de X (distribución de Poisson con lambda = 5)
X <- rpois(sample_size, lambda)

# Crear un data frame para almacenar los datos
df <- data.frame(X = X)

# Agregar las muestras de Xn al data frame
for (i in 1:length(samples)) {
  df[[paste0("X", n_values[i])]] <- samples[[i]]
}

# Función para graficar las distribuciones
plot_distributions <- function(df, n_values, lambda) {
  # Crear una lista de gráficos usando ggplot2
  plots <- lapply(n_values, function(n) {
    ggplot(df, aes(x = get(paste0("X", n)))) +
      geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5, color = "black", fill = "lightblue") +
      stat_function(fun = dpois, args = list(lambda = lambda), color = "red") +
      labs(x = "X", y = "Density") +
      theme_minimal() +
      scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 2)) +
      scale_y_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, by = 0.05)) +
      theme(plot.title = element_blank(),
            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))
  })
  return(plots)
}

# Crear los gráficos para diferentes valores de n
plots <- plot_distributions(df, n_values, lambda)

# Mostrar los gráficos en una sola fila
multiplot <- function(..., plotlist = NULL, cols) {
  require(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  cols = cols
  rows = ceiling(numPlots/cols)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(rows, cols)))
  for (i in 1:numPlots) {
    matchidx <- as.data.frame(which(rownames(viewport(layout = grid.layout(rows, cols))) == paste("p", i, sep = "")))
    print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, layout.pos.col = matchidx$col))
  }
}

# Mostrar los gráficos
multiplot(plotlist = plots, cols = 3)

### Geométrica

# Definir el tamaño de la muestra
sample_size <- 1000

# Número de valores de n para probar
n_values <- c(2, 5, 10, 20, 50)

# Valor de lambda
lambda <- 2

# Generar muestras de Yn para diferentes valores de n
samples_Yn <- lapply(n_values, function(n) rgeom(sample_size, lambda/n))

# Generar muestras de Xn para diferentes valores de n
samples_Xn <- lapply(n_values, function(n) samples_Yn[[which(n_values == n)]] / n)

# Generar muestra de X (distribución exponencial con lambda = 2)
X <- rexp(sample_size, lambda)

# Crear un data frame para almacenar los datos
df <- data.frame(X = X)

# Agregar las muestras de Xn al data frame
for (i in 1:length(samples_Xn)) {
  df[[paste0("X", n_values[i])]] <- samples_Xn[[i]]
}

# Función para graficar las distribuciones
plot_distributions <- function(df, n_values, lambda) {
  # Crear una lista de gráficos usando ggplot2
  plots <- lapply(n_values, function(n) {
    ggplot(df, aes(x = get(paste0("X", n)))) +
      geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5, color = "black", fill = "lightgreen") +
      stat_function(fun = dexp, args = list(rate = lambda), color = "red") +
      labs(x = "X", y = "Density") +
      theme_minimal() +
      scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) +
      scale_y_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, by = 0.1)) +
      theme(plot.title = element_blank(),
            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))
  })
  return(plots)
}

# Crear los gráficos para diferentes valores de n
plots <- plot_distributions(df, n_values, lambda)

# Mostrar los gráficos en una sola fila
multiplot <- function(..., plotlist = NULL, cols) {
  require(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  cols = cols
  rows = ceiling(numPlots/cols)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(rows, cols)))
  for (i in 1:numPlots) {
    matchidx <- as.data.frame(which(rownames(viewport(layout = grid.layout(rows, cols))) == paste("p", i, sep = "")))
    print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, layout.pos.col = matchidx$col))
  }
}

# Mostrar los gráficos
multiplot(plotlist = plots, cols = 3)

############################# Ley debil de los grandes numeros ####################

# Definir el tamaño de la muestra
sample_size <- 1000

# Número de valores de n para probar
n_values <- c(10, 50, 100, 500, 1000)

# Probabilidad de éxito
p <- 0.3

# Generar muestras de Xn para diferentes valores de n
samples <- lapply(n_values, function(n) rbinom(sample_size, n, p) / n)

# Crear un data frame para almacenar los datos
df <- data.frame(matrix(unlist(samples), nrow = sample_size, byrow = TRUE))
names(df) <- paste0("X", n_values)

# Función para graficar las distribuciones
plot_distributions <- function(df, n_values, p) {
  # Crear una lista de gráficos usando ggplot2
  plots <- lapply(n_values, function(n) {
    ggplot(df, aes(x = get(paste0("X", n)))) +
      geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5, color = "black", fill = "lightblue") +
      geom_vline(xintercept = p, color = "red", linetype = "dashed", size = 1) +
      labs(x = "X", y = "Density") +
      theme_minimal() +
      scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
      scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 1)) +
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
  })
  return(plots)
}

# Crear los gráficos para diferentes valores de n
plots <- plot_distributions(df, n_values, p)

# Mostrar los gráficos en una sola fila
multiplot <- function(..., plotlist = NULL, cols) {
  require(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  cols = cols
  rows = ceiling(numPlots/cols)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(rows, cols)))
  for (i in 1:numPlots) {
    matchidx <- as.data.frame(which(rownames(viewport(layout = grid.layout(rows, cols))) == paste("p", i, sep = "")))
    print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, layout.pos.col = matchidx$col))
  }
}

# Título principal
main_title <- "Ley débil de los grandes números para variables aleatorias de Bernoulli"

# Mostrar el título principal y los gráficos
grid.newpage()
grid.text(main_title, x = 0.5, y = 0.95, just = "top", gp = gpar(fontsize = 20, fontface = "bold"))
multiplot(plotlist = plots, cols = 3)

##################### Teorema del límite central ################################

##### Continua (normal)

# Definir el tamaño de la muestra
sample_size <- 1000

# Número de valores de n para probar
n_values <- c(2, 5, 10, 20, 50)

# Generar muestras de Xn para diferentes valores de n
samples <- lapply(n_values, function(n) replicate(sample_size, mean(rnorm(n))))

# Crear un data frame para almacenar los datos
df <- data.frame(matrix(unlist(samples), nrow = sample_size, byrow = TRUE))
names(df) <- paste0("X", n_values)

# Función para graficar las distribuciones
plot_distributions <- function(df, n_values) {
  # Crear una lista de gráficos usando ggplot2
  plots <- lapply(n_values, function(n) {
    ggplot(df, aes(x = get(paste0("X", n)))) +
      geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5, color = "black", fill = "lightblue") +
      stat_function(fun = dnorm, color = "red") +
      labs(x = "X", y = "Density") +
      theme_minimal() +
      scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, by = 0.5)) +
      scale_y_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, by = 0.1)) +
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
  })
  return(plots)
}

# Crear los gráficos para diferentes valores de n
plots <- plot_distributions(df, n_values)

# Mostrar los gráficos en una sola fila
multiplot <- function(..., plotlist = NULL, cols) {
  require(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  cols = cols
  rows = ceiling(numPlots/cols)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(rows, cols)))
  for (i in 1:numPlots) {
    matchidx <- as.data.frame(which(rownames(viewport(layout = grid.layout(rows, cols))) == paste("p", i, sep = "")))
    print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, layout.pos.col = matchidx$col))
  }
}

# Título principal
main_title <- "Teorema del Límite Central para variables aleatorias continuas"

# Mostrar el título principal y los gráficos
grid.newpage()
grid.text(main_title, x = 0.5, y = 0.95, just = "top", gp = gpar(fontsize = 20, fontface = "bold"))
multiplot(plotlist = plots, cols = 3)

##### Discreta (binomial)

# Definir el tamaño de la muestra
sample_size <- 1000

# Número de valores de n para probar
n_values <- c(5, 10, 20, 50, 100)

# Probabilidad de éxito
p <- 0.3

# Generar muestras de Xn para diferentes valores de n
samples <- lapply(n_values, function(n) rbinom(sample_size, n, p) / n)

# Crear un data frame para almacenar los datos
df <- data.frame(matrix(unlist(samples), nrow = sample_size, byrow = TRUE))
names(df) <- paste0("X", n_values)

# Función para graficar las distribuciones
plot_distributions <- function(df, n_values, p) {
  # Crear una lista de gráficos usando ggplot2
  plots <- lapply(n_values, function(n) {
    ggplot(df, aes(x = get(paste0("X", n)))) +
      geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5, color = "black", fill = "lightblue") +
      stat_function(fun = dnorm, args = list(mean = p, sd = sqrt(p * (1 - p) / n)), color = "red") +
      labs(x = "X", y = "Density") +
      theme_minimal() +
      scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
      scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 1)) +
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
  })
  return(plots)
}

# Crear los gráficos para diferentes valores de n
plots <- plot_distributions(df, n_values, p)

# Mostrar los gráficos en una sola fila
multiplot <- function(..., plotlist = NULL, cols) {
  require(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  cols = cols
  rows = ceiling(numPlots/cols)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(rows, cols)))
  for (i in 1:numPlots) {
    matchidx <- as.data.frame(which(rownames(viewport(layout = grid.layout(rows, cols))) == paste("p", i, sep = "")))
    print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, layout.pos.col = matchidx$col))
  }
}

# Título principal
main_title <- "Teorema del Límite Central para variables aleatorias discretas"

# Mostrar el título principal y los gráficos
grid.newpage()
grid.text(main_title, x = 0.5, y = 0.95, just = "top", gp = gpar(fontsize = 20, fontface = "bold"))
multiplot(plotlist = plots, cols = 3)



