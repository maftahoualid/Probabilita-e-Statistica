# Stima dei parametri di una distribuzione normale usando il metodo della massima verosimiglianza

# A. Generazione di un campione casuale da una distribuzione normale con media nota (mu) e deviazione standard nota (sigma)

set.seed(123)  # Impostiamo il seed per garantire la riproducibilità
population_mean <- 173.5  # Media della popolazione
population_sd <- 4.9  # Deviazione standard della popolazione
sample_data <- rnorm(100, mean = population_mean, sd = population_sd)  # Generiamo 100 valori casuali

# B. Definizione della funzione di log-verosimiglianza negativa per stimare i parametri della distribuzione normale

# La funzione calcola la somma negativa dei logaritmi delle probabilità di ciascun valore osservato
# dato un valore ipotetico di media (mu) e deviazione standard (sigma).
# Questa funzione sarà minimizzata con 'optim()' per trovare i valori ottimali dei parametri.

nllik <- function(data, params){
    mu <- params[1]  # Estrazione del valore ipotetico di mu (media)
    sigma <- params[2]  # Estrazione del valore ipotetico di sigma (deviazione standard)
    -sum(dnorm(data, mean = mu, sd = sigma, log = TRUE))  # Calcolo della log-verosimiglianza negativa
}

# C. Stima dei parametri con il metodo della massima verosimiglianza

# Usiamo la funzione 'optim()' per trovare i valori di mu e sigma che massimizzano la verosimiglianza.
# Poiché 'optim()' minimizza le funzioni, minimizziamo la log-verosimiglianza negativa.

# Tentativo iniziale senza specificare il metodo di ottimizzazione
fit <- optim(
    par = c(160, 10),  # Valori iniziali per mu e sigma
    fn = nllik,  # Funzione da minimizzare (-log-verosimiglianza)
    data = sample_data,  # Dati osservati
    lower = c(150, 1), upper = c(200, 20))  # Limiti inferiori e superiori per i parametri

# Poiché otteniamo un avviso che indica che i limiti possono essere usati solo con il metodo "L-BFGS-B",
# ripetiamo l'ottimizzazione specificando il metodo corretto.

fit <- optim(
    par = c(160, 10),
    fn = nllik,
    data = sample_data,
    method = "L-BFGS-B",  # Metodo appropriato per usare limiti
    lower = c(150, 1), upper = c(200, 20))

# Estrazione dei valori stimati per mu e sigma
mu_est <- fit$par[1]
cat("Stima MLE per la media:", mu_est, "\n")
sigma_est <- fit$par[2]
cat("Stima MLE per la deviazione standard:", sigma_est, "\n")

# D. Confronto tra la deviazione standard campionaria e la stima MLE di sigma

# La funzione 'sd()' in R calcola la deviazione standard campionaria (S), che è una stima corretta per sigma.
# Tuttavia, la stima MLE di sigma sottostima leggermente il valore corretto.
# Per ottenere la stima corretta dalla MLE, dobbiamo moltiplicare per sqrt(n/(n-1)).

sample_sd <- sd(sample_data)  # Deviazione standard corretta
sigma_adjusted <- sigma_est * sqrt(100/99)  # Correzione della stima MLE

cat("Deviazione standard campionaria:", sample_sd, "\n")
cat("Stima MLE corretta:", sigma_adjusted, "\n")
