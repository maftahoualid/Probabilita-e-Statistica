# Creazione e utilizzo di funzioni per operazioni matematiche di base in R

# A. Creazione del progetto Lab3 con la stessa struttura di Lab1 e Lab2

# Il progetto dovrebbe contenere le directory:
# - scripts: per gli script R
# - plots: per le immagini generate
# - data: per i file di dati

# B. Funzione per calcolare la somma dei numeri interi da 1 a n

# Definizione della funzione runsum()
# - n: numero fino a cui sommare
# - La funzione utilizza un ciclo for per sommare tutti gli interi da 1 a n
runsum <- function(n){
    runsum = 0 # Variabile per memorizzare la somma
    for(i in 1:n) # Iterazione da 1 a n
        runsum = runsum + i # Aggiornamento della somma
    return(runsum) # Ritorna il risultato finale
}

# Test della funzione con n = 10
runsum(10)

# Verifica con la funzione built-in sum()
sum(1:10)

# C. Funzione per calcolare il fattoriale di un numero (n!)

# Definizione della funzione myfact()
# - n: numero di cui calcolare il fattoriale
# - Il ciclo for moltiplica i numeri da 1 a n
myfact <- function(n){
    runprod = 1 # Variabile per il prodotto
    for(i in 1:n) # Iterazione da 1 a n
        runprod = runprod * i # Moltiplicazione iterativa
    return(runprod) # Ritorna il risultato finale
}

# Test della funzione con n = 10
myfact(10)

# Verifica con la funzione built-in factorial()
factorial(10)

# D. Implementazione della funzione fattoriale in modo ricorsivo

# Definizione della funzione myfact_recursive()
# - Se n > 1, chiama ricorsivamente se stessa con n-1
# - Quando n raggiunge 1, ritorna 1
myfact_recursive <- function(n){
    if((n - 1) > 0) # Condizione di ricorsione
        return(n * myfact_recursive(n - 1)) # Chiamata ricorsiva
    else
        return(1) # Caso base: quando n è 1, ritorna 1
}

# Test della funzione ricorsiva con n = 5
myfact_recursive(5)

# Verifica con la funzione built-in factorial()
factorial(5)
