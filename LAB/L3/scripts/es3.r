# Simulazione e verifica delle probabilità con il lancio di un dado equo in R

# A. Simulazione del lancio di un dado e verifica dell'indipendenza di eventi

# Definizione degli eventi di interesse
A <- c(1, 2)  # Evento A: uscita del numero 1 o 2
B <- c(2, 3, 6)  # Evento B: uscita del numero 2, 3 o 6
C <- c(1, 4, 5)  # Evento C: uscita del numero 1, 4 o 5

# Numero di esperimenti da eseguire
n <- 10^5 

# Simulazione del lancio del dado: estraiamo n valori casuali tra 1 e 6
set.seed(123) 
res <- sample(x = 1:6, size = n, replace = TRUE)

# Inizializzazione delle variabili per contare le occorrenze degli eventi
nA <- nB <- nC <- nAB <- nBC <- numeric(n)

# Popoliamo i vettori con le occorrenze degli eventi
nA <- is.element(res, A)
nB <- is.element(res, B)
nC <- is.element(res, C)
nAB <- is.element(res, intersect(A, B))
nBC <- is.element(res, intersect(B, C))

# Calcolo delle probabilità cumulative
# cumsum(nA): Conta quante volte l'evento A è accaduto fino a ogni lancio.
# (1:n): Normalizza il conteggio dividendo per il numero totale di esperimenti fino a quel punto.
pA <- cumsum(nA) / (1:n)  # Probabilità cumulativa di A
pB <- cumsum(nB) / (1:n)  # Probabilità cumulativa di B
pC <- cumsum(nC) / (1:n)  # Probabilità cumulativa di C
pAcB <- cumsum(nAB) / cumsum(nB) # Probabilità condizionata P(A|B) 
pBcC <- cumsum(nBC) / cumsum(nC) # Probabilità condizionata P(B|C)

# B. Verifica dell'indipendenza di A e B e della dipendenza di B e C

# Creazione di un dataframe con i risultati
df <- data.frame("index" = 1:n, pA, pB, pC, pAcB, pBcC)

# Grafico della convergenza di P(A) e P(A|B)
ggplot(df) + 
    geom_line(aes(x = index, y = pA, color = "P(A)")) +  # Linea della probabilità di A
    geom_line(aes(x = index, y = pAcB, color = "P(A|B)")) +  # Linea della probabilità condizionata P(A|B)
    geom_hline(aes(yintercept = 1/3), linetype = "dashed", color = "red") +  # Linea orizzontale della probabilità teorica
    scale_x_continuous(trans = "log10") +  # Asse X in scala logaritmica
    scale_color_manual(name = "Probabilities", values = c("purple", "green")) +
    theme(legend.position = "bottom") + 
    labs(y = "P(A) and P(A|B)", x = "Launches",
         title = "Independence verification of A and B")

# Grafico della convergenza di P(B) e P(B|C)
ggplot(df) + 
    geom_line(aes(x = index, y = pB, color = "P(B)")) +  # Linea della probabilità di B
    geom_line(aes(x = index, y = pBcC, color = "P(B|C)")) +  # Linea della probabilità condizionata P(B|C)
    geom_hline(aes(yintercept = 1/2), linetype = "dashed", color = "red") +  # Linea della probabilità teorica
    scale_x_continuous(trans = "log10") +  # Asse X in scala logaritmica
    scale_color_manual(name = "Probabilities", values = c("blue", "orange")) +
    theme(legend.position = "bottom") + 
    labs(y = "P(B) and P(B|C)", x = "Launches",
         title = "Dependence verification of B and C")
