# Generazione di numeri casuali e analisi per i giorni della settimana

# Numero di osservazioni casuali da generare
N = 100
# Numero di giorni in una settimana
days = 7

# Creazione di una matrice vuota con N righe e 7 colonne (una per ogni giorno)
data = matrix(0, ncol = days, nrow = N);

# Riempimento della matrice con valori casuali
# - Ogni colonna rappresenta un giorno della settimana
# - I valori sono generati con una sinusoide più un termine casuale
for (day in seq_len(days)) {
    data[, day] <- 7 + 6*sin(day/5) + runif(N) # runif(N) genera numeri casuali tra 0 e 1
}

# I dati sono organizzati in formato "wide", con ogni colonna rappresentante un giorno.
# Per facilitarne l'analisi, li convertiamo in formato "long" concatenandoli in un'unica colonna.
data_as_vector <- c(data)

# Creazione di un vettore con i nomi dei giorni della settimana
day_of_the_week <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

# Creazione di un data frame contenente i valori generati e il corrispondente giorno della settimana
data_long <- data.frame("Value" = data_as_vector, 
                        "Day" = rep(day_of_the_week, each = N))

# Alternativa con il pacchetto reshape2 (commentata):
# library(reshape2)
# data_long <- melt(data)

# Visualizzazione dei dati con ggplot2
library(ggplot2)

ggplot(data = data_long, aes(x = Day, y = Value)) + 
    geom_boxplot() +  # Creazione del boxplot per ogni giorno della settimana
    coord_flip() +    # Ruotiamo il grafico per leggibilità
    scale_x_discrete(limits = rev(day_of_the_week)) + # Ordiniamo i giorni dal lunedì alla domenica
    labs(y = "Randomly created values",
         x = "Days of the week", 
         title = "Randomly generated values",
         subtitle = "Grouped by day of the week")
