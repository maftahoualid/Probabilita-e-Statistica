# Intervalli di confidenza per la media di una distribuzione normale

# A. Costruzione di un intervallo di confidenza al 95% (two-sided) per la media

# Supponiamo che il valore ricevuto in una trasmissione segua una distribuzione normale 
# con varianza nota (sigma^2 = 4). Lo stesso segnale è stato inviato 9 volte e i valori ricevuti sono:

data <- c(5, 8.5, 12, 15, 7, 9, 7.5, 6.5, 10.5)  # Campione di dati
n <- length(data)  # Numero di osservazioni
sigma <- 2  # Deviazione standard nota
alpha <- 0.05  # Livello di significatività

# Calcoliamo il valore critico della distribuzione normale standardizzata per un IC al 95%
z <- qnorm(alpha/2, lower.tail = FALSE)

# Calcoliamo la media campionaria
x_bar <- mean(data)

# Calcoliamo l'intervallo di confidenza al 95% (two-sided)
CI_95_two = c(x_bar - z * sigma / sqrt(n), x_bar + z * sigma / sqrt(n))
CI_95_two  # Stampa dell'intervallo di confidenza

# B. Calcolo dell'intervallo di confidenza inferiore al 95% (one-sided)

# Per un IC inferiore, utilizziamo un unico valore critico di z per la coda superiore
z <- qnorm(alpha, lower.tail = FALSE)

# Costruiamo l'intervallo inferiore
CI_95_low = c(-Inf, x_bar + z * sigma / sqrt(n))
CI_95_low  # Stampa dell'intervallo inferiore

# C. Costruzione di un intervallo di confidenza al 99% (two-sided) per la media

alpha = 0.01  # Nuovo livello di significatività
z <- qnorm(alpha/2, lower.tail = FALSE)  # Valore critico per un IC al 99%

# Calcoliamo l'intervallo di confidenza al 99%
CI_99_two = c(x_bar - z * sigma / sqrt(n), x_bar + z * sigma / sqrt(n))
CI_99_two  # Stampa dell'intervallo di confidenza

# D. Intervallo di confidenza al 95% con deviazione standard sconosciuta

# Supponiamo ora che la deviazione standard della popolazione non sia nota e sia stimata dai dati.
# In questo caso, utilizziamo la distribuzione t di Student al posto della distribuzione normale.

s <- sd(data)  # Deviazione standard campionaria
alpha = 0.05

# Valore critico della distribuzione t di Student
# Il numero di gradi di libertà è dato da n - 1
t <- qt(alpha/2, df = n - 1, lower.tail = FALSE)

# Calcoliamo l'intervallo di confidenza al 95% con deviazione standard sconosciuta
CI_95_two_unk = c(x_bar - t * s / sqrt(n), x_bar + t * s / sqrt(n))
CI_95_two_unk  # Stampa dell'intervallo di confidenza con deviazione standard stimata
