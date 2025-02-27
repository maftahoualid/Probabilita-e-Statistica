# A. Creazione dei vettori con le ore del giorno e le temperature registrate

# Creiamo un vettore contenente le ore del giorno a intervalli di 3 ore,
# dove 0 rappresenta la mezzanotte e 21 l'ultima misurazione della giornata.
hours <- seq(0, 21, 3) 

# Creiamo un vettore contenente le temperature registrate a ogni ora specificata.
temperatures <- c(55.5, 52.4, 52.6, 55.7, 75.6, 77.7, 70.3, 66.6)

# B. Creazione di un grafico dei dati con simboli '+' di colore nero

# Impostiamo il layout grafico predefinito
par(mfrow = c(1,1))

# Creiamo un grafico scatter con simboli '+' per rappresentare le temperature
plot(hours, temperatures, pch = "+")

# C. Modifica dell'intervallo dell'asse X per migliorare la visualizzazione

# Il punto a 0 ore cade sull'asse Y, rendendolo difficile da vedere.
# Modifichiamo l'intervallo dell'asse X con xlim per migliorare la leggibilità.
plot(hours, temperatures, pch = "+", xlim = c(-1, 22))

# D. Creazione di una funzione per convertire misure da centimetri a pollici

# Definiamo una funzione che converte una misura espressa in cm in pollici
# Sapendo che 2.54 cm equivalgono a 1 pollice.
cm2inches <- function(cm){
    return(cm / 2.54)
}

# E. Salvataggio del grafico in un file PDF di dimensioni 10x10 cm

# Utilizziamo la funzione pdf() per creare un file PDF nella cartella ./plots,
# impostando le dimensioni del grafico in pollici.
pdf(file = "./plots/plot_es4.pdf", 
    width = cm2inches(10), 
    height = cm2inches(10))

# Creiamo il grafico con l'intervallo X corretto.
plot(hours, temperatures, pch = "+", xlim = c(-1, 22))

# Chiudiamo il dispositivo grafico per salvare il file PDF.
dev.off()
