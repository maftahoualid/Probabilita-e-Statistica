# Analisi di un sito web: Poisson per visite, esponenziale per durata e normale per fatturato

# Il numero di visite giornaliere segue una distribuzione di Poisson con una media di 120 visite al giorno.
# La durata di ciascuna visita segue una distribuzione esponenziale con un tempo medio di 5 minuti.
# Il 20% dei visitatori effettua un acquisto e il valore medio dello scontrino è di 50€ con deviazione standard 10€.

# A. Calcolo della probabilità di avere almeno 120 visite in un giorno

# X = numero di visite giornaliere
# X ~ Pois(120)
# ppois(q, lambda, lower.tail = FALSE) calcola P(X ≥ q)

ppois(119, lambda = 120, lower.tail = FALSE)

# B. Calcolo del valore atteso e della varianza per la durata media di una visita

# Y = durata di una visita in minuti
# La media della distribuzione esponenziale è data da 1/lambda, dove lambda è il tasso degli eventi
# Y ~ Exp(1/5) significa che il tempo medio è 5 minuti e la varianza è 25

# E[Y] = 5, V[Y] = 5^2 = 25

set.seed(123)
a_thousand_visits <- rexp(1000, rate = 1/5)
mean(a_thousand_visits)  # Stima empirica della media
var(a_thousand_visits)   # Stima empirica della varianza

# C. Creazione del grafico della PDF per la durata di una visita

library(ggplot2)

# Creazione di un dataframe per rappresentare la funzione di densità di probabilità
# - x: sequenza di valori da 0 a 25 con incrementi di 0.1 minuti
# - y: densità di probabilità calcolata tramite la funzione dexp()

df <- data.frame(x = seq(0, 25, 0.1),
                 y = dexp(seq(0, 25, 0.1), rate = 1/5))

# Creazione del grafico della distribuzione esponenziale
# - geom_line(): traccia la curva della funzione di densità di probabilità
# - labs(): aggiunge etichette e titolo
# - aes(x = x, y = y): assegna l'asse X alla durata della visita e l'asse Y alla densità di probabilità

ggplot(df, aes(x = x, y = y)) + 
    geom_line() + 
    labs(title = "Durata di una visita",
         y = "PDF", x = "Durata (min)")

# D. Calcolo della probabilità che una visita duri tra 5 e 10 minuti

pexp(q = 10, rate = 1/5) - pexp(q = 5, rate = 1/5)

# E. Calcolo della probabilità di osservare almeno 20 acquisti su 100 visite

# S = il cliente effettua un acquisto (Bernoulli con p = 0.2)
# T = numero totale di acquisti su 100 visite segue una distribuzione Bin(100, 0.2)

pbinom(19, size = 100, prob = 0.2, lower.tail = FALSE)

# F. Calcolo della probabilità di osservare un numero di acquisti compreso tra diversi intervalli

# La spesa media per acquisto segue una distribuzione normale con media 50 e deviazione standard 10.

pnorm(60, mean = 50, sd = 10) - pnorm(40, mean = 50, sd = 10)  # Tra 40 e 60 acquisti
pnorm(70, mean = 50, sd = 10) - pnorm(30, mean = 50, sd = 10)  # Tra 30 e 70 acquisti
pnorm(80, mean = 50, sd = 10) - pnorm(20, mean = 50, sd = 10)  # Tra 20 e 80 acquisti

# G. Simulazione di 1000 giorni medi e confronto tra ricavo medio giornaliero e ricavo atteso

# Z = valore dell'acquisto ~ Normale(50, 10)

set.seed(123)

visitors <- rpois(1000, 120)  # Generazione di 1000 giorni con numero di visite da Poisson
buying_visitors <- round(0.2 * visitors)  # Il 20% dei visitatori effettua un acquisto

daily_revenue <- NULL
for(i in 1:1000){
    daily_revenue[i] <- sum(rnorm(buying_visitors[i], mean = 50, sd = 10))    
}

# Media del ricavo giornaliero simulato
mean(daily_revenue)

# Ricavo atteso teorico
# Poiché X e Z sono indipendenti:
# E[0.2 * X * Z] = 0.2 * E[X] * E[Z] = 0.2 * 120 * 50 = 1200€
