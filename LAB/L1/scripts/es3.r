# A. Caricamento del dataset sunspot.year dal pacchetto datasets

# Il dataset sunspot.year contiene il numero medio annuo di macchie solari 
# osservate dal 1700 al 1988. Lo carichiamo nell'ambiente di lavoro con data().
data("sunspot.year")
# Visualizzazione del dataset per verificarne il contenuto.
sunspot.year

# B. Consultazione della documentazione del dataset e creazione del vettore degli anni

# Per ottenere informazioni dettagliate sul dataset, utilizziamo la funzione help().
help("sunspot.year")
# Creiamo un vettore di anni corrispondente all'intervallo temporale del dataset (1700-1988).
year <- 1700:1988

# C. Creazione di una variabile sunspot contenente i valori del dataset

# Estraiamo i valori delle macchie solari dal dataset e li assegniamo alla variabile sunspot.
sunspot <- sunspot.year

# D. Creazione di un data frame contenente le variabili year e sunspot

# Organizziamo i dati in un data frame per facilitarne la gestione e l'analisi.
df <- data.frame(year = year,
                 sunspot = sunspot)

# E. Creazione di un grafico a linee del numero di macchie solari rispetto agli anni

# Creiamo un grafico a linee per mostrare l'andamento del numero di macchie solari nel tempo.
plot(df$year, df$sunspot, type = "l", xlab = "Anno", ylab = "Macchie solari")

# F. Sovrapposizione di punti rossi asterisco al grafico con la funzione points()

# Rappresentiamo i singoli valori con asterischi rossi per migliorarne la leggibilità.
plot(df$year, df$sunspot, type = "l", xlab = "Anno", ylab = "Macchie solari")
points(x = df$year, y = df$sunspot, pch = "*", col = "red")

# G. Aggiunta di un titolo al grafico 'Sunspots by year'

# Inseriamo un titolo descrittivo al grafico per una migliore comprensione.
plot(df$year, df$sunspot, type = "l", xlab = "Anno", ylab = "Macchie solari",
     main = "Numero di macchie solari per anno")
points(x = df$year, y = df$sunspot, pch = "*", col = "red")

# H. Creazione di una colonna con 3 pannelli per visualizzare:
#    1. Il grafico a linee
#    2. Un barplot del numero di macchie solari
#    3. Un istogramma del numero di macchie solari

# Impostiamo il layout grafico per visualizzare tre grafici sovrapposti.
par(mfrow = c(3, 1))

# Grafico a linee con punti sovrapposti.
plot(df$year, df$sunspot, type = "l", xlab = "Anno", ylab = "Macchie solari",
     main = "Numero di macchie solari per anno")
points(x = df$year, y = df$sunspot, pch = "*", col = "red")

# Creazione di un barplot per rappresentare visivamente il numero di macchie solari per anno.
barplot(as.vector(df$sunspot), main = "Numero di macchie solari - Barplot")

# Creazione di un istogramma per visualizzare la distribuzione del numero di macchie solari.
hist(df$sunspot, main = "Distribuzione delle macchie solari", xlab = "Numero di macchie solari")

# I. Salvataggio del grafico nella directory ./plots come immagine .png

# Salviamo il grafico generato come immagine PNG nella cartella ./plots.
png(filename = "./plots/plot_es3.png")

par(mfrow = c(3, 1))

plot(df$year, df$sunspot, type = "l", xlab = "Anno", ylab = "Macchie solari",
     main = "Numero di macchie solari per anno")
points(x = df$year, y = df$sunspot, pch = "*", col = "red")

barplot(as.vector(df$sunspot), main = "Numero di macchie solari - Barplot")

hist(df$sunspot, main = "Distribuzione delle macchie solari", xlab = "Numero di macchie solari")

# Chiusura del dispositivo grafico per salvare l'immagine generata.
dev.off()

# J. Salvataggio del dataframe come file CSV nella directory ./data

# Salviamo il dataframe in un file CSV nella cartella ./data per un'eventuale analisi successiva.
write.csv(df, file = "./data/sunspots.csv")
