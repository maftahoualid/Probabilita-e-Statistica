# Teorema del limite centrale: distribuzione delle medie campionarie

# Il teorema del limite centrale afferma che, man mano che la dimensione del campione aumenta,
# la distribuzione della media campionaria di variabili casuali indipendenti e identicamente distribuite
# (iid) tende a una distribuzione normale, indipendentemente dalla distribuzione originale delle variabili.

# A. Simulazione di medie campionarie da una distribuzione uniforme (0,1)
#    Il codice genera istogrammi delle medie di n = 1, 2, 3, 5, 10 e 100 variabili uniformi.

N <- 10000 # Numero di simulazioni
n <- c(1, 2, 3, 5, 10, 100) # Dimensioni del campione

# Creiamo una matrice vuota per memorizzare le medie campionarie
uniform_means <- matrix(NA, nrow = N, ncol = length(n))

# Generiamo i campioni casuali e calcoliamo le loro medie
for(i in 1:length(n)) {  # Iteriamo per ogni valore di n
    unif_values <- NULL
    for(j in 1:n[i])  # Generiamo n[i] variabili uniformi per ogni simulazione
        unif_values <- cbind(unif_values, runif(N))
    uniform_means[, i] <- rowMeans(unif_values)  # Calcoliamo la media campionaria
}

# B. Creazione di una figura con 6 pannelli (3 righe x 2 colonne):
#    - Ogni pannello mostra un istogramma delle medie campionarie per diversi n.

library(ggplot2)
library(patchwork)  # Libreria per combinare più grafici

plot_list <- list()  # Lista per memorizzare i grafici

for(i in 1:length(n)) {
    plot_list[[i]] <- ggplot(
        data = data.frame("SampleMean" = uniform_means[, i]), 
        aes(x = SampleMean)) + 
        geom_histogram(aes(y = after_stat(density)), bins = 40) +  # Istogramma normalizzato
        labs(title = paste("Numero di Uniformi:", n[i]))
}

# Creiamo una figura con 6 pannelli
plot_grid(plotlist = plot_list, nrow = 3, ncol = 2)
