# Analisi di regressione lineare: relazione tra lunghezza della pinna e peso nei pinguini

# A. Caricamento del dataset e selezione di un campione casuale di 10 pinguini

library(palmerpenguins)  # Carichiamo il dataset dei pinguini

df <- penguins  # Assegniamo il dataset a una variabile

set.seed(123)  # Fissiamo il seed per riproducibilità

# Selezioniamo casualmente 10 pinguini e manteniamo solo le variabili di interesse
penguins10 <- df[sample(1:nrow(df), size = 10), 
                 c("body_mass_g", "flipper_length_mm")]

# Il dataset contiene i pesi e le lunghezze delle pinne di 10 pinguini:

weights <- c(4100, 3800, 4300, 4550, 3775, 2900, 4600, 5400, 4100, 4500)
flipper_lengths <- c(216, 191, 210, 205, 199, 187, 209, 228, 210, 211)

# B. Stima manuale dei coefficienti della regressione lineare

# Costruiamo un dataframe con i dati selezionati
data <- data.frame(weights = weights, flipper_lengths = flipper_lengths)
n <- nrow(data)  # Numero di osservazioni

# Calcolo del coefficiente angolare (slope) della retta di regressione
# Formula della stima dei minimi quadrati:
# beta_1 = (Σ xi yi - n * x̄ * ȳ) / (Σ xi^2 - n * x̄^2)

slope <- (sum(flipper_lengths * weights) - n * mean(flipper_lengths) * mean(weights)) /
    (sum(flipper_lengths^2) - n * mean(flipper_lengths)^2)

# Calcolo dell'intercetta della retta di regressione
# Formula: beta_0 = ȳ - beta_1 * x̄
intercept <- mean(weights) - slope * mean(flipper_lengths)

# C. Stima dei coefficienti della regressione usando la funzione lm() e confronto con i risultati manuali

# Creiamo il modello di regressione con la funzione lm()
model <- lm(weights ~ flipper_lengths, data = data)

# Visualizziamo i coefficienti stimati automaticamente e confrontiamoli con quelli manuali
coef(model)

# D. Creazione del grafico di dispersione con la retta di regressione stimata manualmente

library(ggplot2)

ggplot(data, aes(x = flipper_lengths, y = weights)) + 
    geom_point() +  # Rappresentazione dei punti (peso vs lunghezza pinna)
    geom_abline(aes(intercept = intercept, slope = slope), color = "blue") +  # Retta di regressione stimata manualmente
    labs(title = "Regressione tra lunghezza della pinna e peso",
         x = "Lunghezza della pinna (mm)",
         y = "Peso (g)")

# E. Sostituzione della retta di regressione con il metodo geom_smooth()

# geom_smooth() aggiunge una curva di regressione stimata automaticamente con lm()
ggplot(data, aes(x = flipper_lengths, y = weights)) + 
    geom_point() +  # Punti di dispersione
    geom_smooth(method = "lm", formula = y ~ x, color = "red") +  # Retta stimata da ggplot2
    labs(title = "Regressione con geom_smooth()",
         x = "Lunghezza della pinna (mm)",
         y = "Peso (g)")
