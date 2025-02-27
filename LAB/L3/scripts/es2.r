# Simulazione e verifica delle probabilità con il lancio di un dado equo in R

# A. Simulazione del lancio di un dado e verifica della probabilità dell'evento E = {2,3}

# Numero di esperimenti da eseguire
n <- 1e5 
# Definizione dell'evento di interesse (uscita del numero 2 o 3)
E <- c(2, 3) 
# Spazio campionario del dado (valori possibili)
support <- 1:6 

# Impostiamo un seed per garantire la riproducibilità degli esperimenti
set.seed(123) 

# Simulazione del lancio del dado: estraiamo n valori casuali tra 1 e 6
res <- sample(support, size = n, replace = TRUE)

# Visualizzazione dei primi 6 risultati
head(res) 

# Inizializzazione della variabile per contare le occorrenze dell'evento E
nE <- NULL 

# Verifica della presenza dell'evento in ogni lancio
for(i in 1:n)
    nE[i] <- sum(res[i] == E) 

# La probabilità dell'evento è data dal rapporto tra il numero di successi e il numero totale di esperimenti
pE <- cumsum(nE) / (1:n) 

# Visualizziamo la probabilità stimata dopo n lanci
pE[n] 

# B. Grafico dei primi 40 risultati del lancio del dado

# Utilizzo della funzione plot() base di R
# - Asse X: numero di lanci effettuati (da 1 a 40)
# - Asse Y: risultati ottenuti (da 1 a 6, valori del dado)
plot(x = 1:40, 
     y = res[1:40], 
     main = "Tossing a fair dice", 
     xlab = "First 40 launches",
     ylab = "Results")

# Creazione del dataframe per ggplot
df <- data.frame("index" = seq_along(res), "res" = res, "pE" = pE)

# Utilizzo di ggplot per visualizzare i primi 40 risultati del dado
ggplot(data = df[1:40,], aes(x = index, y = res)) + 
    geom_point() +
    labs(title = "Tossing a fair dice", x = "First 40 launches", y = "Results")

# C. Convergenza della probabilità sperimentale al valore teorico (1/3)

# Tracciamo la probabilità cumulativa rispetto ai lanci effettuati
# - log = "x": imposta l'asse X in scala logaritmica per mostrare meglio la convergenza
plot(x = 1:n, y = pE, type = "l", log = "x") 
# Aggiungiamo una linea orizzontale corrispondente al valore teorico della probabilità
abline(h = 1/3, col = "red")

# Visualizzazione con ggplot
# - geom_line(): disegna la curva della probabilità cumulativa
# - geom_hline(): aggiunge una linea orizzontale alla probabilità teorica
# - scale_x_continuous(trans = "log10"): imposta l'asse X in scala logaritmica
ggplot(data = df, aes(x = index, y = pE)) + 
    geom_line() + 
    geom_hline(aes(yintercept = 1/3), linetype = "dashed", color = "red") +
    scale_x_continuous(trans = "log10") + 
    labs(title = "Probability of event E = {2, 3}", 
         subtitle = "From launch 1 to launch 10^5",
         x = "Launches", y = "Estimated Probability")
