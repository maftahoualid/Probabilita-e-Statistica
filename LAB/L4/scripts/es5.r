# Analisi della distribuzione ipergeometrica per il numero di studenti che sanno programmare in R

# In un sondaggio su 500 studenti universitari, 240 hanno dichiarato di sapere programmare in R.
# Consideriamo la variabile casuale X, che indica il numero di studenti in un campione casuale di 50
# che sanno programmare in R.

# A. Calcolo della funzione di massa di probabilità di X e generazione del grafico

# Definiamo le variabili:
N <- 500  # Numero totale di studenti
k <- 50   # Campione selezionato
m <- 240  # Studenti che sanno programmare in R
n <- N - m  # Studenti che non sanno programmare in R

# Definiamo i possibili valori di X (numero di successi nel campione)
x <- 0:min(k, m)

# Creazione di un dataframe con la funzione di massa di probabilità
df_hyper <- data.frame("x" = x, 
    "PMF" = dhyper(x = x, m = m, n = n, k = k))

library(ggplot2)
# Creazione del grafico della funzione di massa di probabilità
# - geom_segment() crea segmenti verticali per ogni valore di X
# - geom_point() evidenzia i punti della distribuzione

ggplot(df_hyper, aes(x = x, y = PMF)) + 
    geom_segment(aes(xend = x, yend = 0)) +
    geom_point() +
    labs(x = "Numero di studenti che sanno programmare",
         title = "PMF per variabile ipergeometrica",
         subtitle = "N = 500, k = 50, m = 240, n = 260")

# B. Calcolo della probabilità che almeno 30 studenti nel campione sappiano programmare in R
# - Sommiamo le probabilità da X = 30 fino al massimo valore possibile
sum(df_hyper$PMF[df_hyper$x >= 30])
# Alternativamente, possiamo calcolare 1 - P(X ≤ 29) con phyper()
1 - phyper(29, m, n, k)

# C. Calcolo del valore atteso di X
# Il valore atteso della distribuzione ipergeometrica è dato da E[X] = k * (m/N)
50 * (240 / 500)  # Calcolo teorico

# Simulazione empirica del valore atteso
mean(rhyper(nn = 100000, m = m, n = n, k = k))  # Simulazione con 100000 campioni
