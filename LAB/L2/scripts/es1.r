# A. Creazione del progetto Lab2 con la stessa struttura di Lab1

# Il progetto Lab2 dovrebbe avere una struttura organizzata con le directory:
# - scripts: per gli script R
# - plots: per le immagini generate
# - data: per i file di dati
# Supponiamo di lavorare all'interno del progetto

# B. Installazione e caricamento del dataset palmerpenguins

# Il pacchetto palmerpenguins contiene un dataset con misurazioni di pinguini.
# Possiamo installarlo con install.packages(), oppure caricare un file .RData
# dalla directory data usando load().

# Metodo 1: Installazione del pacchetto (se necessario) e caricamento del dataset
# install.packages("palmerpenguins")
# data("penguins")
# penguins

# Metodo 2: Caricamento da file RData già salvato nella cartella data
load("./data/penguins.RData")

# C. Calcolo di media, deviazione standard e mediana per le variabili numeriche

# Verifichiamo le variabili numeriche nel dataset con str()
str(penguins)

# Calcoliamo le statistiche descrittive per bill_length_mm
mean(penguins$bill_length_mm, na.rm = TRUE)  # Media
sd(penguins$bill_length_mm, na.rm = TRUE)    # Deviazione standard
median(penguins$bill_length_mm, na.rm = TRUE) # Mediana

# Calcoliamo le statistiche descrittive per bill_depth_mm
mean(penguins$bill_depth_mm, na.rm = TRUE)
sd(penguins$bill_depth_mm, na.rm = TRUE)
median(penguins$bill_depth_mm, na.rm = TRUE)

# D. Creazione di una funzione per calcolare media e deviazione standard

# La funzione stat_auto accetta un vettore e restituisce media e deviazione standard
stat_auto <- function(vec, removeNA = TRUE){
    avg <- mean(vec, na.rm = removeNA)
    stddev <- sd(vec, na.rm = removeNA)
    output <- list("Mean" = avg, "SD" = stddev)
    return(output)
}

# Applichiamo la funzione alle variabili numeriche
stat_auto(penguins$bill_length_mm)
stat_auto(penguins$bill_depth_mm)

# E. Creazione di una funzione manuale per calcolare media e deviazione standard

# La funzione stat_manual calcola manualmente media e deviazione standard
# senza usare mean() e sd(), sfruttando length(), sum() e na.omit()
stat_manual <- function(vec, removeNA = TRUE){
    if(removeNA == TRUE)
        vec <- na.omit(vec)
    n <- length(vec)
    avg <- sum(vec)/n  # Calcolo della media
    
    # Calcolo della deviazione standard passo per passo
    deviations <- vec - avg
    squared_deviations <- deviations^2
    summed_squared_deviations <- sum(squared_deviations)
    variance <- summed_squared_deviations / (n - 1)
    stddev <- sqrt(variance)
    
    # Alternativamente, possiamo calcolare tutto in un'unica riga:
    # stddev <- sqrt(sum((vec - avg)^2) / (n - 1))
    
    output <- list("Mean" = avg, "SD" = stddev)
    return(output)
}

# Testiamo la funzione sulle variabili numeriche
stat_manual(vec = penguins$bill_length_mm, removeNA = TRUE)
stat_manual(vec = penguins$bill_depth_mm, removeNA = TRUE)
