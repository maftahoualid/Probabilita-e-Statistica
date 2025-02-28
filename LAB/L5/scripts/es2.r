# Analisi della distribuzione normale: PDF, CDF e quantili

# A. Calcolo e grafico della funzione di densità di probabilità (PDF) per una
#    distribuzione normale con media 3 e deviazione standard 2

# Definiamo una sequenza di valori x nell'intervallo [-1, 7] con 100 punti
x <- seq(-1, 7, length.out = 100)

# Calcoliamo la densità di probabilità (PDF) per ogni valore di x
pdf <- dnorm(x, mean = 3, sd = 2)

# Creiamo un grafico della PDF utilizzando ggplot2
# - geom_line(): disegna la curva della densità di probabilità
# - labs(): aggiunge etichette per gli assi e il titolo del grafico

library(ggplot2)
ggplot(data.frame(x = x, pdf = pdf), aes(x = x, y = pdf)) +
    geom_line() + 
    labs(y = "Funzione di densità di probabilità (PDF)", 
         title = "Distribuzione Normale",
         subtitle = "Media = 3, Deviazione Standard = 2")

# B. Calcolo e grafico della funzione di distribuzione cumulativa (CDF)

# Calcoliamo la distribuzione cumulativa (CDF) per ogni valore di x
cdf <- pnorm(x, mean = 3, sd = 2)

# Creiamo un grafico della CDF
# - geom_line(): disegna la curva della distribuzione cumulativa
# - labs(): aggiunge etichette per gli assi e il titolo del grafico

ggplot(data.frame(x = x, cdf = cdf), aes(x = x, y = cdf)) +
    geom_line() + 
    labs(y = "Funzione di distribuzione cumulativa (CDF)", 
         title = "Distribuzione Normale",
         subtitle = "Media = 3, Deviazione Standard = 2")

# C. Determinare i quantili corrispondenti ai valori di probabilità 0.025 e 0.975
#    Questi valori corrispondono ai percentili del 2.5% e del 97.5%, ovvero gli
#    intervalli critici di una distribuzione normale standardizzata.

# La funzione qnorm() calcola il quantile associato a una probabilità specifica
qnorm(p = c(0.025, 0.975), mean = 3, sd = 2)
