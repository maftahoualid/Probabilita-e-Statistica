# Dimostrazione dell'approssimazione di una distribuzione binomiale con una distribuzione di Poisson

# A. Definizione delle distribuzioni binomiali e calcolo del parametro λ per la distribuzione di Poisson

# Definiamo i valori di n (numero di prove) e p (probabilità di successo)
n <- c(20, 30, 40, 100)
p <- c(1/4, 1/6, 1/8, 1/20)

# Il valore atteso di una distribuzione binomiale è dato da E(X) = n * p,
# che rappresenta il parametro λ della distribuzione di Poisson approssimante.
# Più n è grande e p è piccolo, più l'approssimazione è accurata.
lambda <- n * p  # Calcolo dei valori di λ per ciascuna distribuzione binomiale

# B. Confronto tra le distribuzioni binomiali e la distribuzione di Poisson

# Inizializziamo una matrice vuota con righe da 0 a 20 e 4 colonne
# (una per ciascuna distribuzione binomiale).
PMF <- matrix(NA, nrow = 21, ncol = 4)

# Calcoliamo la funzione di massa di probabilità per ciascuna distribuzione binomiale
# e popoliamo la matrice. La funzione dbinom calcola P(X = x) per una binomiale.
for(i in 1:4){ 
    PMF[,i] <- dbinom(x = 0:20, size = n[i], prob = p[i])
}

# Convertiamo la matrice in un data frame per facilitare la visualizzazione
PMF <- as.data.frame(PMF)

# Rinominiamo le colonne per riflettere i parametri delle distribuzioni binomiali
colnames(PMF) <- paste("Binomial", n, round(p, 2), sep = "_")

# Aggiungiamo una colonna con la PMF di una distribuzione di Poisson con λ = 5
# La funzione dpois calcola P(X = x) per una distribuzione di Poisson con parametro λ.
PMF$Poisson_5 <- dpois(0:20, lambda = 5)

# Aggiungiamo una colonna con il numero di successi da 0 a 20
PMF$x <- 0:20

# Convertiamo il data frame in formato long per ggplot2 per poter plottare più distribuzioni insieme
library(reshape2) 
df_to_plot <- melt(PMF, id.vars = "x")

# Creazione del grafico con ggplot2
library(ggplot2)
library(RColorBrewer) # Per utilizzare palette di colori

ggplot(data = df_to_plot, aes(x = x, y = value, color = variable)) +
    geom_line() + # Tracciamo le curve delle funzioni di massa di probabilità
    
    # scale_color_manual personalizza i colori delle curve e assegna etichette leggibili
    scale_color_manual(
        labels = c("Bn(20,0.25)", "Bn(30,1/6)", 
                   "Bn(40,1/8)", "Bn(100,0.05)", "P(5)"), # Etichette delle curve
        values = c(brewer.pal(4, name = "PRGn"), "red") # Colorazione delle curve
    ) +
    labs(x = "Numero di Successi", y = "Funzione di Massa di Probabilità",
         title = "Approssimazione di Binomiale con Poisson",
         subtitle = "Confronto tra distribuzioni binomiali e Poisson(5)") +
    theme_minimal()
