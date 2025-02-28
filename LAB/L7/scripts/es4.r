# Test t di Student per confrontare la pressione sanguigna tra due gruppi di età

# Il dataset contiene i valori della pressione sanguigna sistolica per due gruppi:
# - Persone di età compresa tra 30 e 40 anni
# - Persone di età compresa tra 50 e 60 anni
# L'obiettivo è verificare se esiste una differenza significativa tra le medie dei due gruppi.

# A. Caricamento dei dati e calcolo delle statistiche descrittive

data <- read.csv(file = "./data/blood_pressure.csv")  # Caricamento del dataset

# Separazione delle due categorie di età
SBP_young <- data$SBP[data$Age == "30-40"]  # Pressione sanguigna per 30-40 anni
SBP_adult <- data$SBP[data$Age == "50-60"]  # Pressione sanguigna per 50-60 anni

# Calcolo della media per ciascun gruppo
avg_young <- mean(SBP_young)
avg_adult <- mean(SBP_adult)

# Calcolo della deviazione standard per ciascun gruppo
sd_young <- sd(SBP_young)
sd_adult <- sd(SBP_adult)

# B. Creazione dell'istogramma delle due distribuzioni di pressione sanguigna

library(ggplot2)

ggplot(data, aes(x = SBP, color = Age, fill = Age)) + 
    geom_histogram(position = position_identity(), alpha = 0.3) + 
    labs(x = "Systolic Blood Pressure", y = "Frequency", 
         title = "Distribuzione della Pressione Sanguigna Sist. per Età")

# C. Calcolo della statistica t per il test di ipotesi

n_young <- length(SBP_young)  # Numero di osservazioni nel primo gruppo
n_adult <- length(SBP_adult)  # Numero di osservazioni nel secondo gruppo

df <- n_young + n_adult - 2  # Gradi di libertà per il test t

# Deviazione standard poolizzata per due gruppi con varianze uguali
pooled_sd <- sqrt(((n_young-1) * sd_young^2 + (n_adult-1) * sd_adult^2) / df)

# Statistica t calcolata manualmente
t_stat <- (avg_young - avg_adult) / (pooled_sd * sqrt(1/n_young + 1/n_adult))

# Valori critici della distribuzione t per un test bilaterale al 95%
critical_values <- qt(c(1-0.05/2, 0.05/2), df = df, lower.tail = FALSE)

# D. Calcolo del p-value e confronto con il test t di R

# Calcolo del p-value con la funzione pt()
p_value <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)

# Confronto con il test t eseguito direttamente in R
t.test(x = SBP_young, y = SBP_adult, var.equal = TRUE)

# E. Verifica delle assunzioni del test t: normalità e omogeneità delle varianze

# Test di normalità di Shapiro-Wilk per ogni gruppo
shapiro.test(SBP_young)  # OK se p-value > 0.05
shapiro.test(SBP_adult)  # OK se p-value > 0.05

# Test di omogeneità delle varianze
var.test(SBP_young, SBP_adult)  # Se p-value < 0.05, le varianze non sono uguali

# F. Se le varianze non sono uguali, eseguire il test t con varianze diseguali

if (var.test(SBP_young, SBP_adult)$p.value < 0.05) {
    t.test(x = SBP_young, y = SBP_adult, var.equal = FALSE)  # Test t con varianze diseguali
}
