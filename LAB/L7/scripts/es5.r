# Test t di Student per confrontare la pressione sanguigna tra due gruppi di età

# Il dataset contiene i valori della pressione sanguigna sistolica per due gruppi:
# - Persone di età compresa tra 30 e 40 anni
# - Persone di età compresa tra 50 e 60 anni
# L'obiettivo è verificare se esiste una differenza significativa tra le medie dei due gruppi.

# A. Caricamento dei dati e calcolo delle statistiche descrittive

# Caricamento del dataset da file CSV
# Il file contiene due colonne: 'Age' (categoria di età) e 'SBP' (pressione sistolica)
data <- read.csv(file = "./data/blood_pressure.csv")

# Separazione delle due categorie di età per effettuare confronti statistici
SBP_young <- data$SBP[data$Age == "30-40"]  # Pressione sanguigna per il gruppo 30-40 anni
SBP_adult <- data$SBP[data$Age == "50-60"]  # Pressione sanguigna per il gruppo 50-60 anni

# Calcolo della media della pressione sanguigna per ciascun gruppo
avg_young <- mean(SBP_young)
avg_adult <- mean(SBP_adult)

# Calcolo della deviazione standard per ciascun gruppo
sd_young <- sd(SBP_young)
sd_adult <- sd(SBP_adult)

# B. Creazione dell'istogramma delle due distribuzioni di pressione sanguigna

library(ggplot2)

# Creazione di un istogramma con i due gruppi sovrapposti
# - geom_histogram(): costruisce un istogramma per ogni gruppo
# - position_identity(): evita l'impilamento, sovrapponendo le distribuzioni
# - alpha = 0.3: rende le barre semi-trasparenti per migliorare la visibilità delle sovrapposizioni

ggplot(data, aes(x = SBP, color = Age, fill = Age)) + 
    geom_histogram(position = position_identity(), alpha = 0.3, bins = 30) + 
    labs(x = "Systolic Blood Pressure", y = "Frequency", 
         title = "Distribuzione della Pressione Sanguigna Sist. per Età")

# C. Calcolo della statistica t per il test di ipotesi

# Numero di osservazioni in ciascun gruppo
n_young <- length(SBP_young)
n_adult <- length(SBP_adult)

# Gradi di libertà per il test t
# Il test t per due campioni indipendenti con varianze uguali usa df = n1 + n2 - 2
df <- n_young + n_adult - 2

# Calcolo della deviazione standard poolizzata (varianza combinata tra i due gruppi)
# La formula combina le due varianze pesandole per le dimensioni dei campioni
pooled_sd <- sqrt(((n_young-1) * sd_young^2 + (n_adult-1) * sd_adult^2) / df)

# Calcolo della statistica t per confrontare le due medie
# La statistica t misura la differenza tra le due medie in unità di errore standard
t_stat <- (avg_young - avg_adult) / (pooled_sd * sqrt(1/n_young + 1/n_adult))

# Determinazione dei valori critici per un test bilaterale al 95% (alpha = 0.05)
critical_values <- qt(c(1-0.05/2, 0.05/2), df = df, lower.tail = FALSE)

# Se la statistica t è più estrema dei valori critici, rifiutiamo H0

# D. Calcolo del p-value e confronto con il test t di R

# Il p-value indica la probabilità di ottenere una differenza almeno così estrema
# se l'ipotesi nulla fosse vera (mu1 = mu2)
p_value <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)

# Confronto con il test t eseguito direttamente in R
# Questo comando esegue lo stesso test, ma in modo automatico
t.test(x = SBP_young, y = SBP_adult, var.equal = TRUE)

# E. Verifica delle assunzioni del test t: normalità e omogeneità delle varianze

# Test di normalità di Shapiro-Wilk per ciascun gruppo
# Se p-value > 0.05, possiamo assumere che i dati siano distribuiti normalmente
shapiro.test(SBP_young)  # OK se p-value > 0.05
shapiro.test(SBP_adult)  # OK se p-value > 0.05

# Test di omogeneità delle varianze (Levene's Test o F-test)
# Se p-value < 0.05, significa che le varianze non sono uguali
var.test(SBP_young, SBP_adult)

# F. Se le varianze non sono uguali, eseguire il test t con varianze diseguali

# Se il test sulle varianze mostra che sono diverse, usiamo il test t di Welch
if (var.test(SBP_young, SBP_adult)$p.value < 0.05) {
    t.test(x = SBP_young, y = SBP_adult, var.equal = FALSE)  # Test t con correzione di Welch
}
