# Test di ipotesi unilaterale per la media di un campione

# Supponiamo di voler testare se la media di un campione di studenti in un esame 
# di matematica è superiore a 70. 
# Ipotesi nulla (H0): la media del campione è uguale a 70.
# Ipotesi alternativa (H1): la media del campione è maggiore di 70.
# I voti sono normalmente distribuiti con varianza pari a 16.

# A. Definizione del campione e calcolo della statistica test

campione <- c(70, 71, 73, 76, 75, 65, 69, 79, 74, 77)  # Voti degli studenti
n <- length(campione)  # Numero di osservazioni nel campione
sigma <- sqrt(16)  # Deviazione standard nota (radice quadrata della varianza)

x_bar <- mean(campione)  # Media campionaria

# Calcolo della statistica test z:
# z = (x̄ - μ) / (σ / sqrt(n))
stat_test <- (x_bar - 70) / (sigma / sqrt(n))

# B. Calcolo del p-value

# Poiché stiamo effettuando un test unilaterale a destra,
# calcoliamo la probabilità di ottenere un valore maggiore di stat_test nella distribuzione normale standard
p_value <- pnorm(stat_test, lower.tail = FALSE)

# C. Confronto con il valore critico

# Il valore critico z[alpha] per un livello di significatività del 5%
z_critico <- qnorm(0.05, lower.tail = FALSE)  # Quantile della distribuzione normale

# D. Creazione del grafico della distribuzione normale standardizzata con aree critiche

library(ggplot2)

# Creazione di un dataframe per la distribuzione normale standardizzata
z <- seq(-4, 4, length.out = 1000)
y <- dnorm(z)
df <- data.frame(z = z, y = y)

ggplot(df, aes(x = z, y = y)) +
    geom_line()  +  # Traccia la curva della PDF
    geom_segment(aes(y = 0, yend = y, x = 0, xend = 0), 
                 linetype = "dashed", color = "grey70") +  # Linea verticale al centro
    scale_x_continuous(breaks = c(0, z_critico, stat_test), 
                       labels = c(0, expression(z[1-alpha]),
                                  expression(z[bar(x)]))) +  # Etichette sugli assi
    geom_ribbon(data = df[df$z < z_critico,], aes(ymin = 0, ymax = y), 
                fill = "palegreen", alpha = 0.5) +  # Area di accettazione
    geom_ribbon(data = df[df$z > z_critico,], aes(ymin = 0, ymax = y), 
                fill = "salmon", alpha = 0.5) +  # Area critica
    geom_ribbon(data = df[df$z > stat_test, ], aes(ymin = 0, ymax = y), 
                fill = "tomato", alpha = 0.5) +  # Area corrispondente a stat_test
    theme_minimal() +
    labs(x = "z values", y = "Standardized Normal PDF",
         title = "Test di ipotesi per la media campionaria",
         subtitle = "Distribuzione normale standardizzata con aree critiche")
