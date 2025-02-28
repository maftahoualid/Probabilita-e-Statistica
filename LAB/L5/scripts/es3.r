# Analisi della distribuzione normale: PDF, CDF e istogramma di valori casuali
library(ggplot2)
# Definizione dei parametri della distribuzione normale
mu <- 100       # Media della distribuzione
sigma <- 15     # Deviazione standard
xmin <- 70      # Valore minimo per i grafici di PDF e CDF
xmax <- 130     # Valore massimo per i grafici di PDF e CDF
n = 100         # Numero di punti per i grafici PDF e CDF
k = 10000       # Numero di valori casuali generati per l'istogramma

# A. Creazione di una sequenza di valori x tra xmin e xmax per valutare la PDF e la CDF
x <- seq(xmin, xmax, length.out = n)

# B. Generazione di k numeri casuali da una distribuzione normale con media mu e deviazione sigma
r_numbers <- rnorm(k, mean = mu, sd = sigma)

# C. Creazione di un grafico con tre pannelli: PDF, CDF e istogramma dei valori casuali

# Creazione di un dataframe con i valori della funzione di densità di probabilità (PDF)
# e della funzione di distribuzione cumulativa (CDF) per la distribuzione normale
df_norm <- data.frame(x = x, 
    pdf = dnorm(x, mean = mu, sd = sigma),  # Calcolo della PDF
    cdf = pnorm(x, mean = mu, sd = sigma)) # Calcolo della CDF

# Creazione del grafico della PDF
# - geom_line(): disegna la curva della densità di probabilità
# - labs(): aggiunge etichette per assi e titolo
g_pdf <- ggplot(df_norm, aes(x = x, y = pdf)) + 
    geom_line() + 
    labs(title = "Distribuzione Normale",
         subtitle = paste("Media =", mu, "SD =", sigma),
         y = "Funzione di densità di probabilità (PDF)")

# Creazione del grafico della CDF
# - geom_line(): disegna la curva della distribuzione cumulativa
g_cdf <- ggplot(df_norm, aes(x = x, y = cdf)) + 
    geom_line() + 
    labs(title = "Distribuzione Normale",
         subtitle = paste("Media =", mu, "SD =", sigma),
         y = "Funzione di distribuzione cumulativa (CDF)")

# Creazione dell'istogramma dei numeri casuali generati dalla distribuzione normale
# - geom_histogram(): crea un istogramma con 20 bin
# - after_stat(density): normalizza le frequenze
# - labs(): aggiunge etichette e titolo
g_hist <- ggplot(data.frame(values = r_numbers), aes(x = values)) + 
    geom_histogram(mapping = aes(y = after_stat(density)), bins = 20) + 
    labs(x = "Valori", y = "Densità relativa", 
         title = paste("Istogramma di", k, "numeri generati casualmente"),
         subtitle = paste("Distribuzione Normale: Media =", mu, "SD =", sigma))

# Combinazione dei tre grafici in un unico layout
library(patchwork)
g_pdf + g_cdf + g_hist
