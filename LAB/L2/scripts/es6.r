# Analisi del dataset palmerpenguins con statistiche descrittive e visualizzazioni

library(palmerpenguins)
library(ggplot2)
library(corrplot)

# Caricamento del dataset penguins dal pacchetto palmerpenguins
data("penguins")

# Esplorazione iniziale del dataset
# - head(): Mostra le prime righe
# - str(): Mostra la struttura delle variabili
# - summary(): Fornisce statistiche riassuntive
head(penguins)
str(penguins)
summary(penguins)

# Salvataggio del dataset in un file .RData per future analisi
save(penguins, file = "./data/penguins.RData")

# Per caricare nuovamente il dataset:
load(file = "./data/penguins.RData")

# A. Analisi delle isole e della distribuzione delle specie

# Creiamo una tabella per vedere quante isole ci sono e quanti pinguini
# sono presenti in ciascuna. Inoltre, analizziamo se le specie coabitano.
table(penguins$species, penguins$island)

# B. Rappresentazione grafica della distribuzione dei pinguini per isola

# Creiamo un grafico a barre con ggplot2:
# - aes(x = island): specifica l'asse X con il nome dell'isola
# - geom_bar(): crea un istogramma con il conteggio delle osservazioni
# - aes(fill = species): riempie le barre con colori diversi per ogni specie
# - position_dodge(): separa le barre per specie all'interno di ogni isola
# - scale_fill_manual(): assegna colori specifici alle specie

ggplot(data = penguins, aes(x = island)) + 
    geom_bar(aes(fill = species), 
             position = position_dodge(preserve = "single")) +
    labs(x = "Island", y = "Frequency", fill = "Species",
         title = "Penguins by Island") + 
    scale_fill_manual(values = c("darkorange", "purple", "cyan4"))

# C. Scatter plot per la lunghezza delle pinne vs massa corporea

# Creiamo un grafico a dispersione con ggplot2:
# - aes(x = flipper_length_mm, y = body_mass_g): assegna le variabili agli assi
# - geom_point(aes(color = species, shape = species)): rappresenta i punti con colori e forme diverse per specie
# - scale_color_manual(): assegna colori specifici alle specie
# - theme(legend.position = "bottom"): posiziona la legenda sotto il grafico

ggplot(data = penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
    geom_point(aes(color = species, shape = species), size = 3, alpha = 0.8) +
    scale_color_manual(values = c("darkorange", "purple", "cyan4")) +
    labs(title = "Flipper length and body mass",
         subtitle = "Colored by Adelie, Chinstrap and Gentoo Penguins",
         x = "Flipper length (mm)",
         y = "Body mass (g)",
         color = "Penguin species",
         shape = "Penguin species") +
    theme(legend.position = "bottom")

# D. Calcolo delle correlazioni tra variabili numeriche

# Selezioniamo solo le variabili numeriche del dataset
penguins_numeric <- penguins[, 3:6]

# Calcoliamo la matrice di correlazione tra le variabili numeriche
cors <- cor(penguins_numeric, use = "complete.obs")

# Creiamo un grafico della matrice di correlazione
corrplot::corrplot(corr = cors)

# E. Scatter plot per lunghezza e profondità del becco con regressione lineare

# Creiamo un grafico a dispersione per visualizzare la relazione tra
# lunghezza e profondità del becco.
# - geom_point(): rappresenta i punti colorandoli per specie
# - geom_smooth(method = "lm", color = "black"): aggiunge una linea di regressione globale
# - geom_smooth(aes(color = species), method = "lm"): aggiunge linee di regressione separate per specie
# - scale_color_manual(): assegna colori specifici alle specie

ggplot(data = penguins, aes(x = bill_depth_mm, y = bill_length_mm)) + 
    geom_point(aes(color = species)) +
    geom_smooth(method = "lm", color = "black") +
    geom_smooth(aes(color = species), method = "lm") + 
    scale_color_manual(values = c("darkorange", "purple", "cyan4")) + 
    labs(x = "Bill depth (mm)", y = "Bill length (mm)",
         color = "Species", title = "Bill length vs depth", 
         subtitle = "Overall and colored by species")
