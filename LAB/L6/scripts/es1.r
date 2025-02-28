# Approssimazione di una variabile binomiale con una variabile normale

# Supponiamo che X1, X2... Xn siano una sequenza di variabili casuali indipendenti e identicamente
# distribuite secondo una distribuzione di Bernoulli con probabilità di successo p. 
# La somma X = X1 + X2 + ... + Xn segue una distribuzione binomiale con n prove e probabilità di successo p.
# Quando n è grande, possiamo approssimare X con una distribuzione normale di media np e varianza np(1-p).

# A. Dimostrazione che una variabile casuale normale può approssimare una variabile binomiale
#    Utilizziamo diversi numeri di prove: n1 = 5, n2 = 10, n3 = 30.

p <- 0.8  # Probabilità di successo
n1 <- 5   # Numero di prove
n2 <- 10  # Numero di prove
n3 <- 30  # Numero di prove

# Calcolo della media per ciascuna distribuzione binomiale
mu1 <- n1 * p  # Media per n1
mu2 <- n2 * p  # Media per n2
mu3 <- n3 * p  # Media per n3

# Calcolo della varianza per ciascuna distribuzione binomiale
var1 <- n1 * p * (1 - p)  # Varianza per n1
var2 <- n2 * p * (1 - p)  # Varianza per n2
var3 <- n3 * p * (1 - p)  # Varianza per n3

# B. Creazione di una figura con 6 pannelli (2 righe x 3 colonne):
#    - La prima riga mostra le funzioni di massa di probabilità (PMF) binomiali
#    - La seconda riga mostra le funzioni di distribuzione cumulativa (CDF) binomiali con le relative normali

library(ggplot2)

# Creazione di dataframe per rappresentare PMF e CDF per n1, n2 e n3

df1 <- data.frame("x" = 0:n1,
    "pdf_bin" = dbinom(0:n1, size = n1, p = p),  # PMF della binomiale
    "cdf_bin" = pbinom(0:n1, size = n1, p = p),  # CDF della binomiale
    "pdf_norm" = dnorm(0:n1, mean = mu1, sd = sqrt(var1)),  # PDF della normale approssimante
    "cdf_norm" = pnorm(0:n1, mean = mu1, sd = sqrt(var1)))  # CDF della normale approssimante

# Stessa procedura per n2 e n3
df2 <- data.frame("x" = 0:n2,
    "pdf_bin" = dbinom(0:n2, size = n2, p = p),
    "cdf_bin" = pbinom(0:n2, size = n2, p = p),
    "pdf_norm" = dnorm(0:n2, mean = mu2, sd = sqrt(var2)),
    "cdf_norm" = pnorm(0:n2, mean = mu2, sd = sqrt(var2)))

df3 <- data.frame("x" = 0:n3,
    "pdf_bin" = dbinom(0:n3, size = n3, p = p),
    "cdf_bin" = pbinom(0:n3, size = n3, p = p),
    "pdf_norm" = dnorm(0:n3, mean = mu3, sd = sqrt(var3)),
    "cdf_norm" = pnorm(0:n3, mean = mu3, sd = sqrt(var3)))

# Creazione dei grafici delle PMF
# - geom_point(): mostra i punti della distribuzione binomiale
# - geom_segment(): collega ogni punto all'asse x per migliorare la leggibilità
# - geom_line(): traccia la curva della normale approssimante

g_pdf1 <- ggplot(data = df1, aes(x = x, y = pdf_bin)) +
    geom_point(aes(color = "Binomiale")) + 
    geom_segment(aes(xend = x, y = 0, yend = pdf_bin, color = "Binomiale")) +
    geom_line(aes(y = pdf_norm, col = "Normale")) + 
    theme(legend.position = "top") + 
    labs(title = "Approssimazione Normale della Binomiale", 
         subtitle = paste(n1, "prove, p =", p), y = "PDF")

# Stessa procedura per gli altri due grafici della PMF
g_pdf2 <- ggplot(data = df2, aes(x = x, y = pdf_bin)) +
    geom_point(aes(color = "Binomiale")) + 
    geom_segment(aes(xend = x, y = 0, yend = pdf_bin, color = "Binomiale")) +
    geom_line(aes(y = pdf_norm, col = "Normale")) + 
    theme(legend.position = "top") + 
    labs(title = "Approssimazione Normale della Binomiale", 
         subtitle = paste(n2, "prove, p =", p), y = "PDF")

g_pdf3 <- ggplot(data = df3, aes(x = x, y = pdf_bin)) +
    geom_point(aes(color = "Binomiale")) + 
    geom_segment(aes(xend = x, y = 0, yend = pdf_bin, color = "Binomiale")) +
    geom_line(aes(y = pdf_norm, col = "Normale")) + 
    theme(legend.position = "top") + 
    labs(title = "Approssimazione Normale della Binomiale", 
         subtitle = paste(n3, "prove, p =", p), y = "PDF")

# Creazione dei grafici delle CDF
# - geom_step(): disegna la CDF binomiale a gradini
# - geom_line(): traccia la CDF della normale approssimante

g_cdf1 <- ggplot(data = df1, aes(x = x, y = cdf_bin)) +
    geom_step(aes(color = "Binomiale")) + 
    geom_line(aes(y = cdf_norm, color = "Normale")) +
    theme(legend.position = "top") + 
    labs(title = "Approssimazione Normale della Binomiale", 
         subtitle = paste(n1, "prove, p =", p), y = "CDF")

# Stessa procedura per gli altri due grafici della CDF
g_cdf2 <- ggplot(data = df2, aes(x = x, y = cdf_bin)) +
    geom_step(aes(color = "Binomiale")) + 
    geom_line(aes(y = cdf_norm, color = "Normale")) +
    theme(legend.position = "top") + 
    labs(title = "Approssimazione Normale della Binomiale", 
         subtitle = paste(n2, "prove, p =", p), y = "CDF")

g_cdf3 <- ggplot(data = df3, aes(x = x, y = cdf_bin)) +
    geom_step(aes(color = "Binomiale")) + 
    geom_line(aes(y = cdf_norm, color = "Normale")) +
    theme(legend.position = "top") + 
    labs(title = "Approssimazione Normale della Binomiale", 
         subtitle = paste(n3, "prove, p =", p), y = "CDF")

# Creazione della figura con 6 pannelli
library(cowplot)
plot_grid(g_pdf1, g_pdf2, g_pdf3, g_cdf1, g_cdf2, g_cdf3, nrow = 2, ncol = 3)
