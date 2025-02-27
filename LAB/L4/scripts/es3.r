# Simulazione e visualizzazione della distribuzione binomiale

# A. Generazione di 1000 numeri casuali da una distribuzione binomiale con n=9
#    e p=0.8. Ogni numero generato rappresenta il numero di successi su 9 prove.

set.seed(456) # Impostiamo il seed per garantire la riproducibilità
values <- rbinom(1000, size = 9, prob = 0.8) # Generazione dei valori binomiali

# Visualizziamo la tabella di frequenza dei valori generati
table(values)

# B. Creazione del grafico della probabilità sperimentale

library(ggplot2)

# Creazione del grafico
# - aes(x = values): definisce la variabile sull'asse X (numero di successi)
# - geom_bar(aes(y = ..prop..)): crea un istogramma delle frequenze relative
#   utilizzando il valore ..prop.., che normalizza i conteggi rispetto al totale
# - scale_x_continuous(breaks = 3:9): imposta i valori specifici sull'asse X
# - labs(): aggiunge titolo e etichette agli assi

g_values <- ggplot(data = data.frame("values" = values), 
    mapping = aes(x = values)) +
    geom_bar(aes(y = ..prop..)) +  
    scale_x_continuous(breaks = 3:9) +
    labs(x = "Numero di successi", y = "Probabilità sperimentale",
         title = "1000 lanci da Bn(9,0.8)")

# Visualizzazione del grafico
g_values 

# C. Aggiunta della vera funzione di massa di probabilità al grafico

# Creiamo un nuovo data frame con la PMF teorica della distribuzione binomiale
# - dbinom(3:9, 9, 0.8): calcola la probabilità esatta per ogni valore possibile
# - geom_point(): aggiunge i punti della PMF teorica in rosso
# - scale_color_manual(): personalizza il colore della legenda
# - theme(legend.position = "top"): posiziona la legenda sopra il grafico
# - labs(color = ""): rimuove il titolo della legenda per pulizia

true_pmf <- data.frame("x" = 3:9, "PMF" = dbinom(3:9, 9, 0.8))

g_values + 
    geom_point(data = true_pmf, aes(x = x, y = PMF, color = "PMF Teorica")) +
    scale_color_manual(values = "red") +
    theme(legend.position = "top") +
    labs(color = "")
