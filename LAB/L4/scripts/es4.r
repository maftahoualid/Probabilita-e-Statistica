# Simulazione e analisi della distribuzione di Poisson per il numero di clienti in un negozio

# Supponiamo che il numero di clienti che visitano un negozio al dettaglio segua una distribuzione
# di Poisson con una media di 5 clienti all'ora.

# A. Probabilità che in un'ora non ci siano clienti
# La funzione dpois calcola P(X = k) per una distribuzione di Poisson

dpois(0, 5) 

# B. Probabilità che ci siano almeno 3 clienti in un'ora
# Per ottenere P(X ≥ 3), calcoliamo 1 - P(X ≤ 2) con ppois

1 - ppois(2, 5) 

# C. Probabilità che ci siano esattamente 7 clienti in un'ora
dpois(7, 5)

# D. Calcolo del valore atteso della spesa di un cliente
# Se ogni cliente spende un importo casuale tra 10€ e 50€, il valore atteso
# è dato dalla media della distribuzione uniforme tra 10 e 50.

(50 + 10) / 2  # Media teorica di una distribuzione uniforme
mean(runif(100000, min = 10, max = 50))  # Stima empirica con campionamento casuale

# E. Numero atteso di clienti in 8 ore di lavoro
# Poiché la media oraria è 5, moltiplichiamo per 8 ore:

8 * 5  # Calcolo teorico
mean(8 * rpois(100000, 5))  # Simulazione empirica con 100000 giorni lavorativi

# F. Simulazione dei clienti e delle loro spese in una giornata di lavoro

set.seed(123)
hours <- 8

# Simuliamo il numero di clienti in ogni ora usando rpois()
customers <- rpois(hours, 5)

# Creiamo un vettore per memorizzare le spese dei clienti
customer_expenses <- NULL 

# Per ogni ora, generiamo le spese dei clienti come valori casuali tra 10€ e 50€
for(customer_hour in customers){
    customer_expenses <- c(customer_expenses, runif(customer_hour, 10, 50))
}

# Creazione di un dataframe per organizzare i dati simulati
# - "ID": Assegna un identificativo univoco a ogni cliente
# - "hour": Registra l'ora in cui il cliente ha effettuato l'acquisto
# - "expense": Indica l'importo speso da ciascun cliente

df_to_plot <- data.frame(
    "ID" = paste("Customer", 1:sum(customers)),
    "hour" = rep(1:hours, times = customers),  
    "expense" = customer_expenses
)

# Creazione del grafico con ggplot2
# - facet_grid(~ hour) suddivide il grafico per ogni ora di lavoro
# - geom_col() crea un istogramma della spesa per ogni cliente
# - theme() personalizza la visualizzazione

ggplot(df_to_plot, aes(x = ID, y = expense)) + 
    facet_grid(~ hour, scales = "free_x", space = "free_x", 
        labeller = label_both) + 
    geom_col() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
