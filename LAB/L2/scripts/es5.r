library(palmerpenguins)
library(ggplot2)
library(corrplot)

# Caricamento del dataset penguins dal pacchetto palmerpenguins
data("penguins")
head(penguins)

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
# - aes(x = body_mass_g, y = flipper_length_mm): assegna le variabili agli assi
# - geom_point(aes(color = sex)): rappresenta i punti colorandoli per sesso
# - facet_grid(~ species): suddivide il grafico per specie
# - labs(): definisce le etichette degli assi e il titolo

ggplot(data = penguins, aes(x = body_mass_g, y = flipper_length_mm)) + 
    geom_point(aes(color = sex)) + 
    facet_grid(~ species) +
    labs(x = "Body mass (g)", y = "Flipper length (mm)", color = "Sex",
         title = "Flipper length vs. Body mass", 
         subtitle = "Colored by Sex, Faceted by species")

# D. Calcolo delle correlazioni tra variabili numeriche

# Selezioniamo solo le variabili numeriche del dataset
penguins_numeric <- penguins[, 3:6]

# Calcoliamo la matrice di correlazione tra le variabili numeriche
cors <- cor(penguins_numeric, use = "complete.obs")
cors

# Creiamo un grafico della matrice di correlazione
corrplot::corrplot(corr = cors)

# E. Calcolo della correlazione senza usare cor()

# Selezioniamo due variabili numeriche
var1 <- penguins_numeric$bill_length_mm
var2 <- penguins_numeric$bill_depth_mm

# Metodo 1: utilizzo di cov() e sd()
r_short <- cov(var1, var2, use = "complete.obs") / 
    (sd(var1, na.rm = TRUE) * sd(var2, na.rm = TRUE))

# Metodo 2: calcolo manuale della correlazione
v1 <- na.omit(var1)
v2 <- na.omit(var2)
covariance <- sum((v1 - sum(v1)/length(v1)) * (v2 - sum(v2)/length(v2))) /
    (length(v1) - 1)
sd1 <- sqrt(sum((v1 - sum(v1)/length(v1)) ^ 2) / (length(v1) - 1))
sd2 <- sqrt(sum((v2 - sum(v2)/length(v2)) ^ 2) / (length(v2) - 1))
r_long <- covariance / (sd1 * sd2)

# F. Scatter plot per lunghezza e profondità del becco con regressione lineare

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
