# Test di ipotesi per il confronto tra due medie campionarie

# A. Caricamento del dataset e separazione dei gruppi di confronto

library(ggplot2)  # Carichiamo la libreria ggplot2 per la visualizzazione

data <- read.csv("./data/scores.csv")  # Caricamento del dataset contenente i punteggi di due gruppi

# Separiamo i punteggi di due gruppi distinti
score_group1 <- data$Score[data$Group == "A"]  # Gruppo A
score_group2 <- data$Score[data$Group == "B"]  # Gruppo B

# Calcoliamo la dimensione campionaria di ciascun gruppo
n1 <- length(score_group1)
n2 <- length(score_group2)

# Calcoliamo la media e la deviazione standard per ciascun gruppo
mean1 <- mean(score_group1)
mean2 <- mean(score_group2)
sd1 <- sd(score_group1)
sd2 <- sd(score_group2)

# B. Creazione di un istogramma per visualizzare la distribuzione dei punteggi nei due gruppi

ggplot(data, aes(x = Score, fill = Group)) + 
    geom_histogram(position = "dodge", bins = 20, alpha = 0.7) + 
    labs(title = "Distribuzione dei punteggi nei due gruppi", x = "Punteggio", y = "Frequenza")

# C. Test di ipotesi per la differenza tra due medie campionarie

# Ipotesi nulla (H0): le due medie sono uguali
# Ipotesi alternativa (H1): le due medie sono diverse

# Calcoliamo i gradi di libertà per il test t di Student
df <- n1 + n2 - 2

# Deviazione standard poolizzata (varianza combinata)
pooled_sd <- sqrt(((n1-1) * sd1^2 + (n2-1) * sd2^2) / df)

# Calcoliamo la statistica t
# t = (media1 - media2) / (sp * sqrt(1/n1 + 1/n2))
t_stat <- (mean1 - mean2) / (pooled_sd * sqrt(1/n1 + 1/n2))

# Determiniamo i valori critici per un test bilaterale al 95%
critical_values <- qt(c(1-0.05/2, 0.05/2), df = df, lower.tail = FALSE)

# D. Calcolo del p-value per il test

# Il p-value ci dice la probabilità di ottenere un valore così estremo o più se l'ipotesi nulla fosse vera
p_value <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)

# E. Confronto con il test t eseguito direttamente in R

# Confrontiamo i risultati ottenuti manualmente con quelli della funzione t.test()
t.test(score_group1, score_group2, var.equal = TRUE)
