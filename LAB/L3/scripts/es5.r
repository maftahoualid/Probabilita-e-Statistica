# Simulazione della probabilità che nessuna coppia di persone abbia lo stesso compleanno

# A. Probabilità che 4 persone abbiano compleanni distinti

n <- 4 # Numero di persone

d <- 365 # Giorni in un anno

# Numero di modi in cui possiamo scegliere 4 date distinte su 365 giorni
# - choose(d, n): seleziona 4 giorni distinti tra 365 (combinazioni)
# - factorial(n): considera tutte le permutazioni possibili di quei 4 giorni
n_Bday <- choose(d, n) * factorial(n)

# Calcolo della probabilità dividendo per il numero totale di combinazioni possibili del calendario (365^4)
P_different_Bday <- n_Bday / d^n

# B. Calcolo della probabilità per un numero variabile di persone (da 1 a 100)

people <- 1:100 # Sequenza di persone da considerare
d <- 365 # Giorni in un anno

# Inizializzazione di un vettore per memorizzare le probabilità
P_different_Bday <- vector(mode = "numeric", length = length(people))

# Ciclo per calcolare la probabilità per ciascun numero di persone
for(n in people)
    P_different_Bday[n] <- choose(d, n) * factorial(n) / d^n

# Creazione del dataframe per la visualizzazione dei dati
df_to_plot <- data.frame("N_people" = people, 
                         "P_different_Bday" = P_different_Bday)

# Grafico della probabilità che nessuna coppia condivida il compleanno
# - aes(x = N_people, y = P_different_Bday): assegna gli assi
# - geom_line(): traccia una linea per rappresentare l'andamento

ggplot(data = df_to_plot, aes(x = N_people, y = P_different_Bday)) + 
    geom_line() +
    labs(title = "Probability of all different birthdays",
         x = "Number of people in the room", 
         y = "Probability")
