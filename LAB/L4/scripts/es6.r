# Esempi di distribuzioni binomiali e di Poisson con visualizzazione grafica

# Carichiamo le librerie necessarie per la visualizzazione grafica e la gestione dei dati
library(ggplot2)
library(patchwork)
library(reshape2)
library(RColorBrewer)

# A. Distribuzione binomiale con p = 0.5 e p = 0.8, n = 10

# Creiamo un dataframe per la distribuzione binomiale con p = 0.5
# - La colonna PMF rappresenta la funzione di massa di probabilità P(X = x)
# - La colonna CDF rappresenta la funzione di distribuzione cumulativa P(X ≤ x)

df_0.5 <- data.frame(
    x = 0:10,
    PMF = dbinom(0:10, size = 10, prob = 0.5),
    CDF = pbinom(0:10, size = 10, prob = 0.5)
)

# Creazione del grafico della distribuzione binomiale con ggplot2
# - geom_segment(): aggiunge segmenti verticali per rappresentare la PMF
# - geom_point(): aggiunge punti sui segmenti per evidenziare la PMF
# - geom_step(): aggiunge la funzione di distribuzione cumulativa (CDF) come una curva a gradini
# - scale_x_continuous(): definisce i valori dell'asse x
# - scale_color_manual(): assegna colori personalizzati alle curve per distinguerle

p05 <- ggplot(df_0.5, aes(x = x)) +
    geom_segment(aes(xend = x, y = 0, yend = PMF, color = "PMF")) +
    geom_point(aes(y = PMF, color = "PMF")) +
    geom_step(aes(y = CDF, color = "CDF")) +
    scale_x_continuous(breaks = 0:10) +
    scale_color_manual(values = c("cyan4", "brown3")) +
    labs(color = "Funzione", x = "Numero di successi", y = "Probabilità",
         title = "Distribuzione binomiale", subtitle = "n = 10, p = 0.5")

# Creiamo un dataframe per la distribuzione binomiale con p = 0.8

df_0.8 <- data.frame(
    x = 0:10,
    PMF = dbinom(0:10, size = 10, prob = 0.8),
    CDF = pbinom(0:10, size = 10, prob = 0.8)
)

# Creazione del grafico per p = 0.8 con le stesse caratteristiche di p05
p08 <- ggplot(df_0.8, aes(x = x)) + 
    geom_segment(aes(xend = x, y = 0, yend = PMF, color = "PMF")) +
    geom_point(aes(y = PMF, color = "PMF")) +
    geom_step(aes(y = CDF, color = "CDF")) +
    scale_color_manual(values = c("cyan4", "brown3")) +
    scale_x_continuous(breaks = 0:10) +
    labs(color = "Funzione", x = "Numero di successi", y = "Probabilità",
         title = "Distribuzione binomiale", subtitle = "n = 10, p = 0.8")

# Combinazione dei due grafici con patchwork per confrontare le distribuzioni
p05 / p08 + patchwork::plot_layout(guides = "collect")

# B. Confronto tra distribuzioni binomiali con diversi valori di p

# Creiamo un dataframe per confrontare diverse distribuzioni binomiali

df_many_p <- data.frame(x = 0:10,
    p0.1 = dbinom(0:10, size = 10, prob = 0.1),
    p0.3 = dbinom(0:10, size = 10, prob = 0.3),
    p0.5 = dbinom(0:10, size = 10, prob = 0.5),
    p0.7 = dbinom(0:10, size = 10, prob = 0.7),
    p0.9 = dbinom(0:10, size = 10, prob = 0.9))

df_many_p_long <- melt(df_many_p, id.vars = "x")

# Creazione del grafico delle distribuzioni binomiali con diversi valori di p
# - facet_grid(): suddivide il grafico in più pannelli in base alla variabile
# - geom_col(): genera un istogramma per ogni valore di probabilità
# - scale_fill_manual(): assegna colori distinti per ogni distribuzione

ggplot(df_many_p_long, aes(x = x)) + 
    facet_grid(~ variable) + 
    geom_col(aes(y = value, fill = variable)) +
    theme(legend.position = "bottom") +
    scale_fill_manual(values = brewer.pal(5, "Set1")) +
    scale_x_continuous(breaks = 0:10) +
    labs(x = "Numero di successi", y = "Funzione di massa di probabilità",
         fill = "Probabilità")

# C. Esempio di distribuzione di Poisson

# Creiamo un dataframe per la distribuzione di Poisson con lambda = 2
df_2 <- data.frame(
    x = 0:10,
    PMF = dpois(0:10, lambda = 2),
    CDF = ppois(0:10, lambda = 2)
)

# Creazione del grafico della distribuzione di Poisson con lambda = 2
p2 <- ggplot(df_2, aes(x = x)) +
    geom_segment(aes(xend = x, y = 0, yend = PMF, color = "PMF")) +
    geom_point(aes(y = PMF, color = "PMF")) +
    geom_step(aes(y = CDF, color = "CDF")) +
    scale_color_manual(values = c("cyan4", "brown3")) +
    scale_x_continuous(breaks = 0:10) +
    labs(color = "Funzione", x = "Numero di eventi", y = "Probabilità",
         title = "Distribuzione di Poisson", subtitle = "lambda = 2")

# Creiamo un dataframe per la distribuzione di Poisson con lambda = 5
df_5 <- data.frame(
    x = 0:15,
    PMF = dpois(0:15, lambda = 5),
    CDF = ppois(0:15, lambda = 5)
)

# Creazione del grafico per lambda = 5
p5 <- ggplot(df_5, aes(x = x)) +
    geom_segment(aes(xend = x, y = 0, yend = PMF, color = "PMF")) +
    geom_point(aes(y = PMF, color = "PMF")) +
    geom_step(aes(y = CDF, color = "CDF")) +
    scale_color_manual(values = c("cyan4", "brown3")) +
    labs(color = "Funzione", x = "Numero di eventi", y = "Probabilità",
         title = "Distribuzione di Poisson", subtitle = "lambda = 5")

# Combinazione dei due grafici di Poisson con patchwork
p2 / p5 + patchwork::plot_layout(guides = "collect")
