# A. Plot della funzione di massa di probabilità per la distribuzione binomiale con n=18 
#    e p=1/3. Calcolo delle probabilità richieste:
#    - P(X = 3)
#    - P(X ≥ 3)
#    - P(1 ≤ X < 5)
#    - P(X ≥ 15)

p <- 1/3
n <- 18

# Creazione di un dataframe con i valori della distribuzione binomiale
# - x: numero di successi
# - PMF: probabilità di ciascun numero di successi

df_to_plot <- data.frame(
    x = 0:n,
    PMF = dbinom(0:n, size = n, prob = p)
)

library(ggplot2)
# Grafico della funzione di massa di probabilità
# - geom_col(): grafico a barre per rappresentare la distribuzione
# - scale_x_continuous(): etichette sull'asse x per ogni valore da 0 a n

ggplot(df_to_plot, aes(x = x, y = PMF)) + 
    geom_col() +
    scale_x_continuous(breaks = 0:n) +
    labs(x = "Number of Successes", y = "Probability Mass Function",
         title = "Binomial Distribution",
         subtitle = paste("n =", n, "p =", round(p, digits = 2)))

# Calcolo delle probabilità richieste
# P(X = 3): probabilità che X sia esattamente 3
# - dbinom(x, size, prob): restituisce la probabilità esatta per X = x nella distribuzione binomiale
dbinom(x = 3, size = n, prob = p)

# P(X ≥ 3): probabilità che X sia maggiore o uguale a 3
# - pbinom(q, size, prob) calcola P(X ≤ q), quindi per P(X ≥ 3) usiamo 1 - P(X ≤ 2)
1 - pbinom(q = 2, size = n, prob = p) # Alternativa: pbinom(q = 2, size = n, prob = p, lower.tail = FALSE)
# - Alternativa: sommare direttamente le probabilità di X da 3 a 18
dbinom(x = 3:18, size = n, prob = p) |> sum()

# P(1 ≤ X < 5): probabilità che X sia compreso tra 1 e 4
# - pbinom(q, size, prob) restituisce P(X ≤ q), quindi calcoliamo P(X ≤ 4) - P(X ≤ 0)
pbinom(q = 4, size = n, prob = p) - pbinom(q = 0, size = n, prob = p)
# - Alternativa: sommare direttamente le probabilità di X = 1, 2, 3, 4
dbinom(1:4, n, p) |> sum()

# P(X ≥ 15): probabilità che X sia maggiore o uguale a 15
# - Usiamo pbinom(q, size, prob, lower.tail = FALSE) per ottenere P(X ≥ 15)
pbinom(14, n, p, lower.tail = FALSE)
# - Alternativa: sommare direttamente le probabilità di X = 15, 16, 17, 18
dbinom(15:18, n, p) |> sum()

# B. Plot della funzione di distribuzione cumulativa per la distribuzione di Poisson con λ = 3
#    e calcolo delle probabilità richieste:
#    - P(X = 3)
#    - P(X ≥ 3)
#    - P(1 ≤ X < 5)
#    - P(X ≥ 15)

lambda <- 3

# Creazione di un dataframe per la distribuzione cumulativa di Poisson
# - x: valori della distribuzione di Poisson
# - CDF: funzione di distribuzione cumulativa per ogni x

df_to_plot <- data.frame(
    x = 0:10,
    CDF = ppois(0:10, lambda = lambda)
)

# Grafico della funzione di distribuzione cumulativa
# - geom_step(): rappresenta la distribuzione cumulativa con linee a gradini
# - scale_x_continuous(): etichette per i valori di x
# - scale_y_continuous(): imposta i limiti per l'asse Y tra 0 e 1

ggplot(df_to_plot, aes(x = x, y = CDF)) + 
    geom_step() +
    scale_x_continuous(breaks = 0:10) +
    scale_y_continuous(limits = c(0,1)) +
    labs(x = "x", y = "P(X ≤ x)",
         title = "Poisson Cumulative Distribution Function",
         subtitle = paste("lambda =", lambda))

# Calcolo delle probabilità richieste
# P(X = 3): probabilità che X sia esattamente 3
# - dpois(x, lambda): restituisce la probabilità esatta di X = x nella distribuzione di Poisson
dpois(x = 3, lambda = lambda)

# P(X ≥ 3): probabilità che X sia maggiore o uguale a 3
# - ppois(q, lambda) calcola P(X ≤ q), quindi per P(X ≥ 3) usiamo 1 - P(X ≤ 2)
1 - ppois(q = 2, lambda = lambda) # Alternativa: ppois(q = 2, lambda = lambda, lower.tail = FALSE)

# P(1 ≤ X < 5): probabilità che X sia compreso tra 1 e 4
# - ppois(q, lambda) restituisce P(X ≤ q), quindi calcoliamo P(X ≤ 4) - P(X ≤ 0)
ppois(q = 4, lambda = lambda) - ppois(q = 0, lambda = lambda)
# - Alternativa: sommare direttamente le probabilità di X = 1, 2, 3, 4
dpois(1:4, lambda) |> sum()

# P(X ≥ 15): probabilità che X sia maggiore o uguale a 15
# - Usiamo ppois(q, lambda, lower.tail = FALSE) per ottenere P(X ≥ 15)
ppois(14, lambda = lambda, lower.tail = FALSE)
