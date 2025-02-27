# Simulazione del lancio di due dadi e analisi delle frequenze

# A. Generazione di N = 10.000 numeri casuali per simulare il lancio di un dado

N <- 10^4 # Numero di lanci
set.seed(123) # Fissiamo il seed per garantire la riproducibilità
# Generiamo i risultati del primo dado

dice1 <- sample(1:6, size = N, replace = TRUE)

# B. Simulazione del lancio di un secondo dado

# Generiamo i risultati del secondo dado

dice2 <- sample(1:6, size = N, replace = TRUE)

# C. Analisi delle frequenze assolute e relative

# Creazione di un dataframe con le frequenze assolute del primo dado
# La funzione table() conta le occorrenze di ogni numero da 1 a 6
# La colonna "Freq" rappresenta il numero di volte in cui è uscito ciascun valore
df_dice1 <- as.data.frame(table(dice1))

# Grafico delle frequenze assolute del primo dado con geom_col()
# - aes(x = dice1, y = Freq): assegna gli assi X e Y
# - geom_col(): crea le barre proporzionali alla frequenza

ggplot(data = df_dice1, aes(x = dice1, y = Freq)) + 
    geom_col() +
    labs(title = "Absolute frequency of Dice 1", x = "Dice outcome", y = "Absolute Frequency")

# Grafico delle frequenze relative del primo dado
# - y = Freq/N normalizza le frequenze rispetto al numero di esperimenti

ggplot(data = df_dice1, aes(x = dice1, y = Freq/N)) + 
    geom_col() +
    labs(title = "Relative frequency of Dice 1", x = "Dice outcome", y = "Relative Frequency")

# Alternativa con geom_bar() per frequenze assolute e relative
ggplot(data = as.data.frame(dice1), aes(x = dice1)) + 
    geom_bar() +
    labs(title = "Absolute frequency using geom_bar()", x = "Dice outcome", y = "Absolute Frequency")

ggplot(data = as.data.frame(dice1), aes(x = dice1)) + 
    geom_bar(aes(y = ..prop..)) +
    labs(title = "Relative frequency using geom_bar()", x = "Dice outcome", y = "Relative Frequency")

# D. Analisi della somma dei due dadi

# Calcolo della somma senza for loop, utilizzando rowSums
dice_sum <- rowSums(cbind(dice1, dice2))

# Calcolo della somma con un ciclo for (alternativa meno efficiente)
dice_sum <- vector(mode = "numeric", N)
for(i in 1:N)
    dice_sum[i] <- dice1[i] + dice2[i]

# Calcolo delle frequenze relative della somma dei due dadi
# La funzione table() conta le occorrenze di ogni possibile somma
dice_sum_rel <- prop.table(table(dice_sum)) 
# Alternativa equivalente: 
dice_sum_rel <- table(dice_sum)/N

# Grafico della distribuzione della somma dei due dadi
# - x = dice_sum: risultati della somma
# - y = Freq: frequenza relativa della somma

ggplot(data = as.data.frame(dice_sum_rel), aes(x = dice_sum, y = Freq)) +
    geom_col() + 
    labs(x = "Sum of two dice", y = "Relative frequency", 
         title = "Distribution of the sum of two dice")
