# Stima della probabilità di successo di una moneta truccata usando la funzione di verosimiglianza

# Supponiamo di avere un campione di 20 osservazioni ottenute dal lancio di una moneta con 
# probabilità di successo incognita. Vogliamo stimare questa probabilità usando la funzione di verosimiglianza.

# Generazione di 20 lanci casuali di una moneta truccata con probabilità di successo p = 0.4
set.seed(123)
coin_results <- c(0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1)  # 1 = successo, 0 = insuccesso

# A. Definizione e visualizzazione della funzione di verosimiglianza

# La funzione di verosimiglianza per la distribuzione binomiale è definita come:
# L(p) = p^successi * (1 - p)^fallimenti
# dove:
# - successi è il numero di volte che si è ottenuto 1
# - fallimenti è il numero di volte che si è ottenuto 0

lik <- function(data, p) {
    n <- length(data)  # Numero totale di lanci
    successes <- sum(data)  # Numero di successi
    failures <- n - successes  # Numero di fallimenti
    likelihood <- p^successes * (1 - p)^failures  # Calcolo della funzione di verosimiglianza
    return(likelihood)
}

# La funzione di log-verosimiglianza è la trasformazione logaritmica della funzione di verosimiglianza:
# logL(p) = successi * log(p) + fallimenti * log(1 - p)

llik <- function(data, p) {
    n <- length(data)  # Numero totale di lanci
    successes <- sum(data)  # Numero di successi
    failures <- n - successes  # Numero di fallimenti
    loglikelihood <- successes * log(p) + failures * log(1 - p)  # Calcolo della log-verosimiglianza
    return(loglikelihood)
}

# Creazione di una sequenza di valori per p, tra 0 e 1, con incrementi di 0.01
par_values <- seq(0, 1, 0.01)

# Creazione di un dataframe con i valori di verosimiglianza e log-verosimiglianza per ogni p
likelihood_values <- data.frame(
    p = par_values, 
    likelihood = lik(coin_results, par_values),  # Calcolo della verosimiglianza
    loglikelihood = llik(coin_results, par_values)  # Calcolo della log-verosimiglianza
)

# Visualizzazione del grafico della funzione di verosimiglianza
library(ggplot2)
ggplot(likelihood_values, aes(x = p, y = likelihood)) +
    geom_line() +
    labs(title = "Funzione di verosimiglianza", x = "Probabilità di successo (p)", y = "L(p)")

# Visualizzazione del grafico della funzione di log-verosimiglianza
ggplot(likelihood_values, aes(x = p, y = loglikelihood)) +
    geom_line() +
    labs(title = "Funzione di log-verosimiglianza", x = "Probabilità di successo (p)", y = "logL(p)")

# C. Stima della massima verosimiglianza per il parametro incognito

# La funzione nllik() calcola il valore negativo della log-verosimiglianza
# in modo che possa essere minimizzata dalla funzione optim()
nllik <- function(data, p){
    return(-llik(data, p))  # Negazione della log-verosimiglianza
}

# Uso del metodo Brent per trovare il valore di p che massimizza la log-verosimiglianza
estimate <- optim(par = 0.5, fn = nllik, data = coin_results, method = "Brent", lower = 0, upper = 1)

# Stima empirica della probabilità di successo come media dei risultati ottenuti
empirical_estimate <- mean(coin_results)

# Visualizzazione del punto stimato sul grafico della log-verosimiglianza
ggplot(likelihood_values, aes(x = p, y = -loglikelihood)) +
    geom_line() + 
    geom_point(aes(x = estimate$par, y = estimate$value, color = "MLE")) +
    labs(title = "Massima verosimiglianza", x = "Probabilità di successo (p)", y = "-logL(p)")
