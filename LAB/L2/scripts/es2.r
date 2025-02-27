library(palmerpenguins)
library(ggplot2)

# Caricamento del dataset penguins dal pacchetto palmerpenguins
data("penguins")
penguins

# A. Trasformazione di una variabile numerica in una categorica

# Consideriamo la lunghezza delle pinne (flipper_length_mm)
# e la dividiamo in classi di 10mm

# Estrapoliamo la variabile flipper_length_mm ignorando i valori mancanti (NA)
flipper <- penguins$flipper_length_mm[!is.na(penguins$flipper_length_mm)]

# Determiniamo il range minimo e massimo dei valori della lunghezza delle pinne
# per stabilire i limiti delle classi
range(flipper)

# Creiamo i limiti delle classi con intervalli di 10mm
classes <- seq(170, 235, 10)

# Suddividiamo i valori in classi specifiche usando la funzione cut()
# - breaks = classes: definisce gli intervalli
# - include.lowest = TRUE: include il valore più basso nella prima classe
# - right = FALSE: gli intervalli sono chiusi a sinistra, ovvero [a, b)
flipper_cut <- cut(flipper, 
                   breaks = classes, 
                   include.lowest = TRUE, 
                   right = FALSE)

# Visualizziamo le prime righe della variabile categorizzata
head(flipper_cut)

# B. Creazione di una tabella di frequenza e trasformazione in data frame

# Creiamo una tabella che mostra la frequenza assoluta di ogni classe
flipper_df <- data.frame(table(flipper_cut))

# Rinominiamo le colonne per rendere il dataset più leggibile
colnames(flipper_df) <- c("flipper_length_mm", "absolute_freq")

# C. Calcolo delle frequenze relative e cumulative

# Calcoliamo la frequenza cumulativa assoluta, cioè il conteggio progressivo delle osservazioni
flipper_df$cumulative_absolute_freq <- cumsum(flipper_df$absolute_freq)

# Calcoliamo la frequenza relativa, ovvero la proporzione di ogni classe rispetto al totale
flipper_df$relative_freq <- flipper_df$absolute_freq / 
                            sum(flipper_df$absolute_freq)

# Calcoliamo la frequenza cumulativa relativa, cioè la somma progressiva delle frequenze relative
flipper_df$cumulative_relative_freq <- cumsum(flipper_df$relative_freq)

# D. Creazione di un grafico con ggplot2

# Creiamo un grafico a barre per rappresentare le frequenze delle classi
# - geom_col(): genera un grafico a barre
# - geom_text(): aggiunge etichette con le percentuali sopra ogni barra
# - nudge_y = 3: sposta leggermente il testo verso l'alto per una migliore leggibilità
ggplot(flipper_df, aes(x = flipper_length_mm, y = absolute_freq)) + 
    geom_col() + 
    geom_text(aes(label = paste0(round(relative_freq * 100, digits = 1), "%")), 
              nudge_y = 3) +
    labs(x = "Flipper length (10mm classes)", y = "Absolute frequency", 
         title = "Flipper lengths distribution",
         subtitle = "Percentage over the bars")
