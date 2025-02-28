# Test di ipotesi per la varianza di un processo produttivo

# Un produttore afferma che la varianza del suo processo produttivo è inferiore a 10.
# Effettuiamo un test di ipotesi per valutare questa affermazione.

# A. Calcolo di un intervallo di confidenza unilaterale inferiore per la varianza (95% confidence)

# Definizione del dataset (valori osservati del processo produttivo)
data <- c(48.1, 50.6, 47.5, 54.8, 51.0, 47.5, 51.5, 52.2, 51.7, 49.1)

alpha <- 0.05  # Livello di significatività
n <- length(data)  # Numero di osservazioni nel campione
S2 <- var(data)  # Varianza campionaria

# Costruzione dell'intervallo di confidenza inferiore per la varianza
# La distribuzione chi-quadrato viene utilizzata per calcolare l'intervallo di confidenza
confidence_interval <- c(0, (n-1) * S2 / qchisq(1-alpha, df = n-1, lower.tail = FALSE))

# B. Calcolo del valore p per il test di ipotesi

# Ipotesi nulla (H0): sigma^2 >= 10
# Ipotesi alternativa (H1): sigma^2 < 10

stat_test <- (n-1) * S2 / 10  # Statistica del test chi-quadrato
p_value <- pchisq(stat_test, df = n-1, lower.tail = TRUE)  # Calcolo del valore p

# C. Ripetizione dei calcoli usando la funzione varTest() del pacchetto EnvStats

# Il pacchetto EnvStats fornisce una funzione diretta per eseguire il test sulla varianza
library(EnvStats)
varTest_result <- varTest(x = data, conf.level = 0.95, alternative = "less", sigma.squared = 10)

# Decisione sul test di ipotesi
# Se il valore p è maggiore di alpha, accettiamo l'ipotesi nulla, ovvero
# la varianza del processo produttivo non è significativamente inferiore a 10.

if (p_value > alpha) {
    decision <- "Accettiamo l'ipotesi nulla: la varianza del processo produttivo è compatibile con 10 o più."
} else {
    decision <- "Rifiutiamo l'ipotesi nulla: la varianza del processo produttivo è significativamente inferiore a 10."
}

# Output dei risultati
decision
confidence_interval
p_value
varTest_result
