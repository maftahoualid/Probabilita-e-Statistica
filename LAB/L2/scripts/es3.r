library(palmerpenguins)
library(ggplot2)

# Caricamento del dataset penguins dal pacchetto palmerpenguins
data("penguins")
penguins

# A. Creazione di un istogramma della distribuzione della lunghezza delle pinne

# Utilizziamo geom_histogram() per visualizzare la distribuzione della lunghezza delle pinne,
# colorando ogni specie con un colore diverso.
# - aes(x = flipper_length_mm): indica la variabile sulla quale costruire l'istogramma
# - fill = species: assegna un colore diverso a ciascuna specie
# - y = after_stat(density): normalizza l'istogramma per mostrare la densità anziché il conteggio assoluto
#   (in pratica, mostra la distribuzione relativa dei dati piuttosto che la frequenza assoluta)
# - alpha = 0.5: imposta la trasparenza per evitare sovrapposizioni troppo marcate
# - position = "identity": mantiene le distribuzioni sovrapposte senza impilamento
flipper_hist <- ggplot(data = penguins, aes(x = flipper_length_mm)) +
    geom_histogram(aes(fill = species, y = after_stat(density)), alpha = 0.5, 
        position = "identity") +
    scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
    labs(x = "Flipper length (mm)",
         y = "Density",
         title = "Penguin flipper lengths distribution")

# Visualizzazione del grafico
flipper_hist

# B. Calcolo delle statistiche descrittive per la lunghezza delle pinne suddivisa per specie

# Creiamo una funzione per calcolare media, mediana, varianza e deviazione standard
# - df: dataframe di input
# - var: nome della variabile su cui calcolare le statistiche
# - groupVariable: variabile che definisce i gruppi (es. specie di pinguino)
# - groupName: valore specifico della variabile di raggruppamento (es. "Gentoo")
stats_on_groups <- function(df, var, groupVariable, groupName){
    groups <- df[, groupVariable] # Estraiamo la colonna delle specie
    groupIndex <- which(groups == groupName) # Identifichiamo le righe corrispondenti alla specie scelta
    vec <- df[groupIndex, var] # Selezioniamo i valori della variabile di interesse per il gruppo scelto
    return(list(
        Mean = mean(vec, na.rm = TRUE),
        Median = median(vec, na.rm = TRUE),
        SD = sd(vec, na.rm = TRUE),
        Var = var(vec, na.rm = TRUE)
    ))
}

# Calcoliamo le statistiche per ogni specie di pinguino
stats_on_groups(as.data.frame(penguins), var = "flipper_length_mm", groupVariable = "species", groupName = "Gentoo")
stats_on_groups(as.data.frame(penguins), var = "flipper_length_mm", groupVariable = "species", groupName = "Adelie")
stats_on_groups(as.data.frame(penguins), var = "flipper_length_mm", groupVariable = "species", groupName = "Chinstrap")

# C. Creazione di un boxplot della lunghezza delle pinne

# Usiamo geom_boxplot() per visualizzare la distribuzione della lunghezza delle pinne per ogni specie
# - aes(x = species, y = flipper_length_mm): definisce l'asse X con la specie e l'asse Y con la variabile numerica
# - color = species: assegna un colore diverso a ciascuna specie per il boxplot
# - geom_jitter(): aggiunge punti dispersi lungo l'asse X per migliorare la leggibilità dei dati
# - scale_color_manual(): assegna colori personalizzati per ogni specie
flipper_box <- ggplot(data = penguins, aes(x = species, y = flipper_length_mm, color = species)) +
    geom_boxplot(outlier.alpha = 0) +
    geom_jitter(alpha = 0.5) + 
    scale_color_manual(values = c("darkorange", "purple", "cyan4")) +
    labs(x = "Species", y = "Flipper length (mm)",
         title = "Penguin flipper lengths distribution")

# Visualizzazione del boxplot
flipper_box

# D. Calcolo dei quartili per i pinguini Gentoo
# - quantile(): calcola i quartili
# - probs = c(0.25, 0.5, 0.75): specifica i quantili da calcolare (Q1, Q2, Q3)
# - na.rm = TRUE: rimuove i valori NA prima del calcolo
quantile(penguins$flipper_length_mm[penguins$species == "Gentoo"], 
         probs = c(0.25, 0.5, 0.75),
         na.rm = TRUE)

# E. Calcolo del 40° percentile per i pinguini Adelie
# - probs = 0.4: indica che vogliamo calcolare il 40° percentile
quantile(penguins$flipper_length_mm[penguins$species == "Adelie"], 
         probs = 0.4,
         na.rm = TRUE)
