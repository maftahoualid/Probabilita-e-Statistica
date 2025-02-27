# A. Creazione di vettori utilizzando l'operatore colon (:) e la funzione seq()

# Creazione di un vettore contenente i numeri da 1 a 10 usando l'operatore ':'
1:10
# Creazione dello stesso vettore utilizzando la funzione seq()
seq(1, 10, 1)

# Creazione di un vettore che esclude determinati elementi utilizzando l'indicizzazione negativa
(1:12)[-c(1, 3:6, 8:11)]
# Creazione dello stesso vettore usando la funzione seq()
seq(2, 12, 5)

# B. Creazione di una matrice 4x2 di zeri e modifica di una riga

# Creazione di una matrice 4x2 inizializzata con valori zero
mymat <- matrix(0, 4, 2)
# Sostituzione della seconda riga con i valori 3 e 6
mymat[2,] <- c(3,6)

# C. Creazione di un vettore x con 20 punti equidistanti tra -pi e pi, 
#    calcolo della funzione seno per tali punti e realizzazione di un grafico

# Creazione di un vettore x con 20 valori equidistanti tra -pi e pi
x <- seq(-pi, pi, length.out = 20)
# Calcolo del valore del seno per ciascun valore di x
y <- sin(x)
# Creazione di un grafico a linee dei valori ottenuti
plot(x, y, type = "l")

# D. Creazione di una matrice 4x6 di numeri interi casuali tra -5 e 5
#    e creazione di una seconda matrice con i valori assoluti

# Creazione della matrice usando runif() e round()
mat <- matrix(round(runif(4*6, -5, 5)), nrow = 4, ncol = 6)
# Alternativamente, creazione della matrice usando sample()
mat <- matrix(sample(x = -5:5, size = 4*6, replace = TRUE), nrow = 4, ncol = 6)
# Creazione della matrice contenente i valori assoluti
tmat_pos <- abs(mat)

# E. Creazione di un grafico della funzione esponenziale exp(x) 
#    con valori di x tra -2 e 2 con step di 0.1

# Creazione del vettore x con valori tra -2 e 2 con incremento di 0.1
x <- seq(-2, 2, 0.1)
# Creazione del grafico con titolo e etichette per gli assi
plot(x, exp(x), main = "Exponential function", 
     xlab = "x", ylab = expression(e^x))

# F. Creazione di un vettore x con valori da 1 a 100 con step di 5,
#    calcolo della radice quadrata e creazione di un grafico e un barplot

# Creazione del vettore x
x <- seq(1, 100, 5)
# Calcolo della radice quadrata di ogni elemento in x
y <- sqrt(x)
# Configurazione della finestra grafica per visualizzare due grafici affiancati
par(mfrow = c(1, 2))
# Creazione di un grafico a dispersione
plot(x, y)
# Creazione di un grafico a barre
barplot(y, names.arg = x)
